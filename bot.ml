open Cohttp
open Cohttp_lwt_unix
open Lwt

let repo_to_push_to = "git@gitlab.com:coq/coq"

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

let git =
  "cd ~/repo && GIT_SSH_COMMAND='ssh -i ~/bot_rsa -o \"StrictHostKeyChecking=no\"' git "

let git_fetch repo remote_ref =
  git ^ "fetch " ^ repo ^ " " ^ remote_ref

let git_force_push repo local_ref remote_branch_name =
  git ^ "push " ^ repo ^ " +" ^ local_ref ^ ":refs/heads/" ^ remote_branch_name

let git_delete repo remote_branch_name =
  git ^ "push " ^ repo ^ " :refs/heads/" ^ remote_branch_name

let remote_branch_name number = "pr-" ^ string_of_int number

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

let get_port () =
  try
    "PORT" |> Sys.getenv |> int_of_string
  with
  | Not_found -> 8000

(* Get the credentials using environment variables *)
let get_credentials () =
  try
    let username = Sys.getenv "USERNAME" in
    let password = Sys.getenv "PASSWORD" in
    Some (`Basic(username, password))
  with
  | Not_found -> None

let send_request ~body ~uri header_list =
  match get_credentials () with
  | Some credentials ->
     let headers =
       Header.init ()
       |> (fun headers -> Header.add_list headers header_list)
       |> (fun headers -> Header.add headers "User-Agent" "coqbot")
       |> (fun headers -> Header.add_authorization headers credentials)
     in
     Lwt.async (fun () ->
         print_endline "Sending request.";
         Client.post ~body ~headers uri >|= print_response
       )
  | None ->
     print_endline "No credentials found. Skipping"

let pull_request body =
  try
    let json = Yojson.Basic.from_string body in
    print_endline "JSON decoded.";
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
         git_fetch repo branch
         |&& git_force_push repo_to_push_to commit (remote_branch_name number)
         |> execute_cmd)
       |> Lwt.async
    | "closed" ->
       print_endline "Branch will be deleted following PR closing.";
       (fun () ->
         git_delete repo_to_push_to (remote_branch_name number)
         |> execute_cmd)
       |> Lwt.async;
       let merged = json_pr |> member "merged" |> to_bool in
       (* TODO: if PR was closed without getting merged, remove the milestone *)
       (* TODO: if PR was merged in master without a milestone, post an alert *)
       if merged then (
         print_endline "PR was merged.";
         let milestone = json_pr |> member "milestone" in
         let milestone_title = milestone |> member "title" |> to_string in
         print_string "PR was part of milestone: ";
         print_endline milestone_title;
         let milestone_description =
           milestone |> member "description" |> to_string
         in
         let regexp =
           "coqbot: backport to \\([^ ]*\\) (request inclusion column: https://github.com/[^/]*/[^/]*/projects/[0-9]+#column-\\([0-9]+\\))"
           |> Str.regexp
         in
         if string_match regexp milestone_description then (
           let backport_to = Str.matched_group 1 milestone_description in
           let column_id =
             Str.matched_group 2 milestone_description |> int_of_string
           in
           print_string "Backporting to ";
           print_string backport_to;
           print_endline " was requested.";
           let base = json_pr |> member "base" |> member "ref" |> to_string in
           if String.equal base backport_to then (
             print_string "But the PR was already merged into ";
             print_endline backport_to
           )
           else
             let global_id = json_pr |> member "id" |> to_int in
             let body =
               "{\"content_id\":"
               ^ string_of_int global_id
               ^ ", \"content_type\": \"PullRequest\"}"
               |> (fun body ->
                 print_endline "Body:";
                 print_endline body;
                 body)
               |> Cohttp_lwt.Body.of_string
             in
             let uri =
               "https://api.github.com/projects/columns/"
               ^ string_of_int column_id
               ^ "/cards"
               |> (fun url ->
                 print_string "URL: ";
                 print_endline url;
                 url)
               |> Uri.of_string
             in
             send_request ~body ~uri
               [ "Accept", "application/vnd.github.inertia-preview+json" ]
         )
       )
    | _ -> ()
  with
  | Yojson.Json_error err ->
     print_string "Json error: ";
     print_endline err
  | Yojson.Basic.Util.Type_error (err, _) ->
     print_string "Json type error: ";
     print_endline err

let callback _conn req body =
  let body = Cohttp_lwt.Body.to_string body in
  print_endline "Request received.";
  match Uri.path (Request.uri req) with
  | "/pull_request" ->
     Lwt.async (fun () -> body >|= pull_request);
     Server.respond_string ~status:`OK ~body:"" ()
  | _ ->
     Server.respond_not_found ()

let server =
  print_endline "Initializing repository";
  "mkdir -p repo && cd repo && git init" |> execute_cmd |> Lwt.ignore_result;
  let mode = `TCP (`Port (get_port ())) in
  Server.create ~mode (Server.make ~callback ())

let () = Lwt_main.run server
