open Base
open Lwt.Syntax
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

let f = Printf.sprintf

let code_wrap str = f "```\n%s\n```" str

let string_match ~regexp ?(pos = 0) string =
  try
    let (_ : int) = Str.search_forward (Str.regexp regexp) string pos in
    true
  with Stdlib.Not_found -> false

let rec fold_string_matches ~regexp ~f ~init ?(pos = 0) string =
  if string_match ~regexp ~pos string then
    let pos = Str.match_end () in
    f (fun () -> fold_string_matches ~regexp ~f ~init ~pos string)
  else init

let map_string_matches ~regexp ~f string =
  fold_string_matches ~regexp
    ~f:(fun rest ->
      let v = f () in
      v :: rest () )
    ~init:[] string

let iter_string_matches ~regexp ~f string =
  fold_string_matches ~regexp ~f:(fun rest -> f () ; rest ()) ~init:() string

let pr_from_branch branch =
  if string_match ~regexp:"^pr-\\([0-9]*\\)$" branch then
    (Some (Str.matched_group 1 branch |> Int.of_string), "pull request")
  else (None, "branch")

let first_line_of_string s =
  if string_match ~regexp:"\\(.*\\)\n" s then Str.matched_group 1 s else s

let remove_between s i j =
  String.sub ~pos:0 ~len:i s ^ String.sub s ~pos:j ~len:(String.length s - j)

let trim_comments comment =
  let rec aux comment begin_ in_comment =
    if not in_comment then
      try
        let begin_ = Str.search_forward (Str.regexp "<!--") comment 0 in
        aux comment begin_ true
      with Stdlib.Not_found -> comment
    else
      try
        let end_ = Str.search_forward (Str.regexp "-->") comment begin_ in
        aux (remove_between comment begin_ (end_ + 3)) 0 false
      with Stdlib.Not_found -> comment
  in
  aux comment 0 false

let strip_quoted_bot_name ~github_bot_name body =
  (* If someone says "`@coqbot minimize foo`", (with backticks), we
     don't want to treat that as them tagging coqbot, so we adjust
     the tagging to "@`coqbot minimize foo`" so that the matching
     below doesn't pick up the name *)
  Str.global_replace
    (Str.regexp (f "\\(`\\|<code>\\)@%s:? " @@ Str.quote github_bot_name))
    (f "@\\1%s " @@ Str.quote github_bot_name)
    body

let%expect_test "strip_quoted_bot_name" =
  Stdio.printf "%s\n"
    (strip_quoted_bot_name ~github_bot_name:"coqbot"
       {|>this didn't produce a pipeline for some reason\r\n\r\nI think that this is normal. @herbelin was maybe expecting that adding the `request: full CI` label would trigger a new run immediately, but the semantics is that this label will produce such a full CI run at the next update (next push) of this PR. Cf. the [documentation](https://github.com/coq/coq/blob/master/CONTRIBUTING.md#understanding-automatic-feedback):\r\n\r\n>you can request a full run of the CI by putting the `request: full CI` label before pushing to your PR branch, or by commenting `@coqbot: run full CI` after having pushed. |} ) ;
  [%expect
    {| >this didn't produce a pipeline for some reason\r\n\r\nI think that this is normal. @herbelin was maybe expecting that adding the `request: full CI` label would trigger a new run immediately, but the semantics is that this label will produce such a full CI run at the next update (next push) of this PR. Cf. the [documentation](https://github.com/coq/coq/blob/master/CONTRIBUTING.md#understanding-automatic-feedback):\r\n\r\n>you can request a full run of the CI by putting the `request: full CI` label before pushing to your PR branch, or by commenting @`coqbot run full CI` after having pushed. |}]

let github_repo_of_gitlab_project_path ~gitlab_mapping ~gitlab_domain
    ~gitlab_repo_full_name =
  let full_name_with_domain = gitlab_domain ^ "/" ^ gitlab_repo_full_name in
  let github_full_name =
    match Hashtbl.find gitlab_mapping full_name_with_domain with
    | Some value ->
        value
    | None ->
        Stdio.printf
          "Warning: No correspondence found for GitLab repository %s.\n"
          full_name_with_domain ;
        gitlab_repo_full_name
  in
  match Str.split (Str.regexp "/") github_full_name with
  | [owner; repo] ->
      (owner, repo)
  | _ ->
      failwith
        (f "Could not split repository full name %s into (owner, repo)."
           github_full_name )

let parse_gitlab_repo_url ~http_repo_url =
  if not (string_match ~regexp:"https?://\\([^/]*\\)/\\(.*/.*\\)" http_repo_url)
  then Error (f "Could not parse GitLab repository URL %s." http_repo_url)
  else Ok (Str.matched_group 1 http_repo_url, Str.matched_group 2 http_repo_url)

let parse_gitlab_repo_url_and_print ~http_repo_url =
  match parse_gitlab_repo_url ~http_repo_url with
  | Ok (gitlab_domain, gitlab_repo_full_name) ->
      Stdio.printf "GitLab domain: \"%s\"\n" gitlab_domain ;
      Stdio.printf "GitLab repository full name: \"%s\"\n" gitlab_repo_full_name
  | Error msg ->
      Stdio.print_endline msg

let%expect_test "http_repo_url_parsing_coq" =
  parse_gitlab_repo_url_and_print ~http_repo_url:"https://gitlab.com/coq/coq" ;
  [%expect
    {|
     GitLab domain: "gitlab.com"
     GitLab repository full name: "coq/coq" |}]

let%expect_test "http_repo_url_parsing_mathcomp" =
  parse_gitlab_repo_url_and_print
    ~http_repo_url:"https://gitlab.inria.fr/math-comp/math-comp" ;
  [%expect
    {|
    GitLab domain: "gitlab.inria.fr"
    GitLab repository full name: "math-comp/math-comp" |}]

let%expect_test "http_repo_url_parsing_example_from_gitlab_docs" =
  parse_gitlab_repo_url_and_print
    ~http_repo_url:"https://gitlab.example.com/gitlab-org/gitlab-test" ;
  [%expect
    {|
    GitLab domain: "gitlab.example.com"
    GitLab repository full name: "gitlab-org/gitlab-test" |}]

let github_repo_of_gitlab_url ~gitlab_mapping ~http_repo_url =
  parse_gitlab_repo_url ~http_repo_url
  |> Result.map ~f:(fun (gitlab_domain, gitlab_repo_full_name) ->
         github_repo_of_gitlab_project_path ~gitlab_mapping ~gitlab_domain
           ~gitlab_repo_full_name )

let download_cps ~uri ~with_file =
  let open Lwt.Infix in
  let rec inner_download uri =
    let* resp, body = Client.get uri in
    match Response.status resp with
    | `OK ->
        let stream = Body.to_stream body in
        with_file (fun chan -> Lwt_stream.iter_s (Lwt_io.write chan) stream)
        >>= Lwt.return_ok
    | `Moved_permanently
    | `Found
    | `See_other
    | `Temporary_redirect
    | `Permanent_redirect -> (
      match Header.get_location (Response.headers resp) with
      | Some new_uri ->
          inner_download new_uri
      | None ->
          f "Redirected from %s, but no Location header found"
            (Uri.to_string uri)
          |> Lwt.return_error )
    | status_code ->
        f "HTTP request to %s failed with status code: %s" (Uri.to_string uri)
          (Code.string_of_status status_code)
        |> Lwt.return_error
  in
  inner_download uri

let download ~uri dest =
  download_cps ~uri ~with_file:(Lwt_io.with_file ~mode:Lwt_io.output dest)

let download_to ~uri chan =
  download_cps ~uri ~with_file:(fun write_to -> write_to chan)

let copy_stream ~src ~dst =
  let open Lwt.Infix in
  let buffer = Buffer.create 1024 in
  let rec aux () =
    Lwt_io.read_char_opt src
    >>= function
    | Some c ->
        Buffer.add_char buffer c ;
        Lwt_io.write_char dst c >>= aux
    | None ->
        Lwt.return (Buffer.contents buffer)
  in
  aux ()
