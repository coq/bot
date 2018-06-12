let port =
  try
    "PORT" |> Sys.getenv |> int_of_string
  with
  | Not_found -> 8000

let username = Sys.getenv "USERNAME"
let password = Sys.getenv "PASSWORD"
let credentials = `Basic(username, password)

let gitlab_access_token = Sys.getenv "GITLAB_ACCESS_TOKEN"
let gitlab_header = [ "PRIVATE-TOKEN", gitlab_access_token ]

let repo_to_push_to = "coq/coq.git"

let project_api_preview_header =
  [ "Accept", "application/vnd.github.inertia-preview+json" ]

let ssh_push = Sys.file_exists "~/bot_rsa"

open Base
open Cohttp
open Cohttp_lwt_unix
open Lwt

let report_status command report code =
  print_string "Command \"";
  print_string command;
  print_string "\" ";
  print_string report;
  print_string " ";
  print_int code;
  print_endline "."

let execute_cmd command =
  Lwt_unix.system command >|= fun status ->
  match status with
  | Unix.WEXITED code ->
     report_status command "exited with status" code
  | Unix.WSIGNALED signal ->
     report_status command "was killed by signal number" signal
  | Unix.WSTOPPED signal ->
     report_status command "was stopped by signal number" signal

let cd_repo =
  "cd repo"

let git_fetch repo remote_ref =
  "git fetch " ^ repo ^ " " ^ remote_ref

let git_push repo =
  if ssh_push then
    "GIT_SSH_COMMAND='ssh -i ~/bot_rsa -o \"StrictHostKeyChecking=no\"'"
    ^ "git push git@gitlab.com:"
    ^ repo
  else
    "git push https://" ^ username ^ ":" ^ password ^ "@gitlab.com/" ^ repo

let git_force_push repo local_ref remote_branch_name =
  git_push repo ^ " +" ^ local_ref ^ ":refs/heads/" ^ remote_branch_name

let git_delete repo remote_branch_name =
  git_push repo ^ " :refs/heads/" ^ remote_branch_name

let remote_branch_name number = "pr-" ^ Int.to_string number

let (|&&) command1 command2 = command1 ^ " && " ^ command2

let string_match r s =
  try
    let _ = Str.search_forward r s 0 in
    true
  with
    Not_found -> false

let print_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  print_string "Response code: ";
  print_int code;
  print_newline ();
  print_string "Headers: ";
  resp |> Response.headers |> Header.to_string |> print_endline;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  print_endline "Body:";
  print_endline body

let headers header_list =
  Header.init ()
  |> (fun headers -> Header.add_list headers header_list)
  |> (fun headers -> Header.add headers "User-Agent" "coqbot")
  |> (fun headers -> Header.add_authorization headers credentials)

let send_request ~body ~uri header_list =
  let headers = headers header_list in
  print_endline "Sending request.";
  Client.post ~body ~headers uri >>= print_response

let analyze_milestone milestone =
  let open Yojson.Basic.Util in
  let milestone_title = milestone |> member "title" |> to_string in
  print_string "PR was part of milestone: ";
  print_endline milestone_title;
  let milestone_description =
    milestone |> member "description" |> to_string
  in
  let project_column_regexp =
    "https://github.com/[^/]*/[^/]*/projects/[0-9]+#column-\\([0-9]+\\)"
  in
  let regexp =
    "coqbot: backport to \\([^ ]*\\) (request inclusion column: "
    ^ project_column_regexp
    ^ "; backported colum: "
    ^ project_column_regexp
    ^ ")"
    |> Str.regexp
  in
  if string_match regexp milestone_description then
    let backport_to = Str.matched_group 1 milestone_description in
    let request_inclusion_column_id =
      Str.matched_group 2 milestone_description |> Int.of_string
    in
    let backported_column_id =
      Str.matched_group 3 milestone_description |> Int.of_string
    in
    Some (backport_to, request_inclusion_column_id, backported_column_id)
  else
    None

let add_pr_to_column pr_id column_id =
  let body =
    "{\"content_id\":"
    ^ Int.to_string pr_id
    ^ ", \"content_type\": \"PullRequest\"}"
    |> (fun body ->
      print_endline "Body:";
      print_endline body;
      body)
    |> Cohttp_lwt.Body.of_string
  in
  let uri =
    "https://api.github.com/projects/columns/"
    ^ Int.to_string column_id
    ^ "/cards"
    |> (fun url ->
      print_string "URL: ";
      print_endline url;
      url)
    |> Uri.of_string
  in
  send_request ~body ~uri project_api_preview_header

let mv_card_to_column card_id column_id =
  let body =
    "{\"position\":\"top\",\"column_id\":"
    ^ Int.to_string column_id
    ^ "}"
    |> (fun body ->
      print_endline "Body:";
      print_endline body;
      body)
    |> Cohttp_lwt.Body.of_string
  in
  let uri =
    "https://api.github.com/projects/columns/cards/"
    ^ Int.to_string card_id
    ^ "/moves"
    |> (fun url ->
      print_string "URL: ";
      print_endline url;
      url)
    |> Uri.of_string
  in
  send_request ~body ~uri project_api_preview_header

let pull_request_action json =
    let open Yojson.Basic.Util in
    let action = json |> member "action" |> to_string in
    print_string "Action: ";
    print_endline action;
    let json_pr = json |> member "pull_request" in
    let number = json_pr |> member "number" |> to_int in
    print_string "Number: #";
    print_int number;
    print_newline ();
    match action with
    | "opened" | "reopened" | "synchronize" ->
       print_endline "Action warrants fetch / push.";
       let pull_request_head = json_pr |> member "head" in
       let branch = pull_request_head |> member "ref" |> to_string in
       let commit = pull_request_head |> member "sha" |> to_string in
       let repo =
         pull_request_head |> member "repo" |> member "html_url" |> to_string
       in
       (fun () ->
         cd_repo
         |&& git_fetch repo branch
         |&& git_force_push repo_to_push_to commit (remote_branch_name number)
         |> execute_cmd)
       |> Lwt.async
    | "closed" ->
       print_endline "Branch will be deleted following PR closing.";
       (fun () ->
         cd_repo
         |&& git_delete repo_to_push_to (remote_branch_name number)
         |> execute_cmd)
       |> Lwt.async;
       (* let merged = json_pr |> member "merged" |> to_bool in *)
       (* TODO: if PR was closed without getting merged, remove the milestone *)
       (* TODO: if PR was merged in master without a milestone, post an alert *)
    | _ -> ()

let handle_json action default body =
  try
    let json = Yojson.Basic.from_string body in
    print_endline "JSON decoded.";
    action json
  with
  | Yojson.Json_error err ->
     prerr_string "Json error: ";
     prerr_endline err;
     default
  | Yojson.Basic.Util.Type_error (err, _) ->
     prerr_string "Json type error: ";
     prerr_endline err;
     default

let generic_get relative_uri ?(header_list=[]) ~default json_handler =
  let uri = "https://api.github.com/" ^ relative_uri |> Uri.of_string in
  let headers = headers header_list in
  Client.get ~headers uri
  >>= (fun (response, body) -> Cohttp_lwt.Body.to_string body)
  >|= (handle_json json_handler default)

let get_pull_request_info pr_number =
  generic_get
    ("repos/coq/coq/pulls/" ^ pr_number)
    ~default:None
    (fun json ->
      let open Yojson.Basic.Util in
      let pr_id = json |> member "id" |> to_int in
      let milestone = json |> member "milestone" in
      match analyze_milestone milestone with
      | Some (backport_to, request_inclusion_column, backported_colum) ->
         Some (pr_id, backport_to, request_inclusion_column, backported_colum)
      | None ->
         None
    )

let get_cards_in_column column_id =
  generic_get
    ("projects/columns/" ^ Int.to_string column_id ^ "/cards")
    ~header_list:project_api_preview_header
    ~default:[]
    (fun json ->
      let open Yojson.Basic.Util in
      json
      |> to_list
      |> List.filter_map ~f:(fun json ->
             let card_id = json |> member "id" |> to_int in
             let content_url =
               json
               |> member "content_url"
               |> to_string_option
               |> Option.value ~default:""
             in
             let regexp =
               "https://api.github.com/repos/.*/\\([0-9]*\\)"
               |> Str.regexp
             in
             if string_match regexp content_url then
               let pr_number = Str.matched_group 1 content_url in
               Some (pr_number, card_id)
             else
               None
           )
    )

let push_action json =
  let cache_list_cards = ref ("", []) in
  print_endline "Commit messages:";
  let open Yojson.Basic.Util in
  let base_ref = json |> member "ref" |> to_string in
  let commit_action commit =
    let commit_msg = commit |> member "message" |> to_string in
    print_endline commit_msg;
    let regexp_merge = Str.regexp "Merge PR #\\([0-9]*\\):" in
    let regexp_backport = Str.regexp "Backport PR #\\([0-9]*\\):" in
    if string_match regexp_merge commit_msg then (
      let pr_number = Str.matched_group 1 commit_msg in
      print_string "PR #";
      print_string pr_number;
      print_endline " was merged.";
      get_pull_request_info pr_number >>= (fun pr_info ->
        match pr_info with
        | Some (pr_id, backport_to, request_inclusion_column, backported_colum) ->
           if ("refs/heads/" ^ backport_to |> String.equal base_ref) then (
             print_endline "PR was merged into the backportig branch directly.";
             add_pr_to_column pr_id backported_colum
           )
           else (
             print_string "Backporting to ";
             print_string backport_to;
             print_endline " was requested.";
             add_pr_to_column pr_id request_inclusion_column
           )
        | None ->
           print_endline "Did not get any backporting info.";
           return ()
      )
    )
    else if string_match regexp_backport commit_msg then (
      let pr_number = Str.matched_group 1 commit_msg in
      print_string "PR #";
      print_string pr_number;
      print_endline " was backported.";
      get_pull_request_info pr_number
      >>= (function
           | Some (pr_id, backport_to, request_inclusion_column, backported_colum) ->
              (* Now we need to find the card id for this PR in the request inclusion column *)
              begin
                if !cache_list_cards |> fst |> String.equal backport_to |> not then
                  get_cards_in_column request_inclusion_column >|=
                    (fun cards ->
                      cache_list_cards := (backport_to, cards);
                      !cache_list_cards
                    )
                else return !cache_list_cards
              end >>= (fun (_, cards) ->
               match
                 List.find_map cards ~f:(fun (pr_number', card_id) ->
                     if String.equal pr_number pr_number'
                     then Some card_id
                     else None
                   )
               with
               | Some card_id ->
                  print_string "Moving card ";
                  print_int card_id;
                  print_endline " to colum ";
                  print_int backported_colum;
                  print_newline ();
                  mv_card_to_column card_id backported_colum
               | None ->
                  prerr_endline "Could not find a card for the backported PR.";
                  return ()

             )
           | None ->
              prerr_endline "Could not find backporting info for backported PR.";
              return ()
          )
    )
    else return ()
  in
  (fun () ->
    json |> member "commits" |> to_list |> Lwt_list.iter_s commit_action
  ) |> Lwt.async

let get_build_trace project_id build_id =
  let uri =
    "https://gitlab.com/api/v4/projects/" ^ Int.to_string project_id
    ^ "/jobs/" ^ Int.to_string build_id ^ "/trace"
    |> Uri.of_string
  in
  let headers = headers gitlab_header in
  Client.get ~headers uri
  >>= (fun (_response, body) -> Cohttp_lwt.Body.to_string body)

let job_action json =
  let open Yojson.Basic.Util in
  let build_status = json |> member "build_status" |> to_string in
  if String.equal build_status "failed" then
    let project_id = json |> member "project_id" |> to_int in
    let build_id = json |> member "build_id" |> to_int in
    print_string "Failed job ";
    print_int build_id;
    print_string " of project ";
    print_int project_id;
    print_endline ".";
    (fun () ->
      get_build_trace project_id build_id
      >|= (fun trace -> print_endline trace))
    |> Lwt.async
    (* TODO: retry job at:
       https://gitlab.com/api/v4/projects/{project_id}/jobs/{build_id}/retry
     *)
(* TODO: if build is a success and this was a documentation job and
   the PR was labelled documentation, post the link to the artifact
 *)

let callback _conn req body =
  let body = Cohttp_lwt.Body.to_string body in
  print_endline "Request received.";
  let handle_request action =
    (fun () -> body >|= handle_json action ()) |> Lwt.async;
    Server.respond_string ~status:`OK ~body:"" ()
  in
  match Uri.path (Request.uri req) with
  | "/pull_request" -> handle_request pull_request_action
  | "/push" -> handle_request push_action
  | "/job" -> handle_request job_action
  | _ -> Server.respond_not_found ()

let server =
  print_endline "Initializing repository";
  "mkdir -p repo && cd repo && git init" |> execute_cmd |> Lwt.ignore_result;
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () = Lwt_main.run server
