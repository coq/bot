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

let git =
  "cd ~/repo && GIT_SSH_COMMAND='ssh -i ~/bot_rsa -o \"StrictHostKeyChecking=no\"' git "

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
       (fun () ->
         git
         ^ "fetch https://github.com/coq/coq refs/pull/"
         ^ string_of_int number
         ^ "/head && "
         ^ git
         ^ "push git@gitlab.com:coq/coq +FETCH_HEAD:refs/heads/pr-"
         ^ string_of_int number
         |> execute_cmd)
       |> Lwt.async
    | "closed" ->
       print_endline "Branch will be deleted following PR closing.";
       (fun () ->
         git
         ^ "push git@gitlab.com:coq/coq :refs/heads/pr-"
         ^ string_of_int number
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
