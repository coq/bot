open Base

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

let github_repo_of_gitlab_project_path ~gitlab_mapping gitlab_full_name =
  let github_full_name =
    match Hashtbl.find gitlab_mapping gitlab_full_name with
    | Some value ->
        value
    | None ->
        Stdio.printf
          "Warning: No correspondence found for GitLab repository %s.\n"
          gitlab_full_name ;
        gitlab_full_name
  in
  match Str.split (Str.regexp "/") github_full_name with
  | [owner; repo] ->
      (owner, repo)
  | _ ->
      failwith "Could not split github_full_name into (owner, repo)."

let github_repo_of_gitlab_url ~gitlab_mapping gitlab_repo_url =
  let owner, repo =
    if not (string_match ~regexp:".*:\\(.*\\)/\\(.*\\).git" gitlab_repo_url)
    then Stdio.printf "Could not match project name on repository url.\n" ;
    (Str.matched_group 1 gitlab_repo_url, Str.matched_group 2 gitlab_repo_url)
  in
  let repo_full_name = owner ^ "/" ^ repo in
  github_repo_of_gitlab_project_path ~gitlab_mapping repo_full_name
