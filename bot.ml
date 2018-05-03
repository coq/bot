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

let pull_request body =
  try
    let json = Yojson.Basic.from_string body in
    print_endline "JSON decoded.";
    let open Yojson.Basic.Util in
    let action = json |> member "action" |> to_string in
    print_string "Action: ";
    print_endline action;
    let number = json |> member "number" |> to_int in
    print_string "Number: #";
    print_int number;
    print_newline ();
    match action with
    | "opened" | "reopened" | "synchronize" ->
       print_endline "Action warrants fetch / push.";
       let pull_request_head = json |> member "pull_request" |> member "head" in
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
       |> Lwt.async
    | _ -> ()
  with
    Yojson.Json_error err ->
     print_string "Json error: ";
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

let get_port () =
  try
    int_of_string (Sys.getenv "PORT")
  with
  | Not_found -> 8000

let server =
  print_endline "Initializing repository";
  "mkdir -p repo && cd repo && git init" |> execute_cmd |> Lwt.ignore_result;
  let mode = `TCP (`Port (get_port ())) in
  Server.create ~mode (Server.make ~callback ())

let () = Lwt_main.run server
