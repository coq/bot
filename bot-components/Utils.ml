open Base
open Bot_info
open Cohttp
open Cohttp_lwt_unix
open Lwt
open Zip

let f = Printf.sprintf

let string_match ~regexp string =
  try
    let _ = Str.search_forward (Str.regexp regexp) string 0 in
    true
  with Stdlib.Not_found -> false

let headers header_list user_agent =
  Header.init ()
  |> (fun headers -> Header.add_list headers header_list)
  |> fun headers -> Header.add headers "User-Agent" user_agent

let print_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Lwt_io.printf "Response code: %d.\n" code
  >>= fun () ->
  if code < 200 && code > 299 then
    resp |> Response.headers |> Header.to_string
    |> Lwt_io.printf "Headers: %s\n"
    >>= fun () ->
    body |> Cohttp_lwt.Body.to_string >>= Lwt_io.printf "Body:\n%s\n"
  else Lwt.return_unit

let send_request ~body ~uri header_list user_agent =
  let headers = headers header_list user_agent in
  Client.post ~body ~headers uri >>= print_response

let handle_json action body =
  try
    let json = Yojson.Basic.from_string body in
    (* print_endline "JSON decoded."; *)
    Ok (action json)
  with
  | Yojson.Json_error err ->
      Error (f "Json error: %s\n" err)
  | Yojson.Basic.Util.Type_error (err, _) ->
      Error (f "Json type error: %s\n" err)

let handle_zip action body =
  let open Lwt_result.Infix in
  Lwt_io.with_temp_file (fun (tmp_name, tmp_channel) ->
      let open Lwt.Infix in
      Lwt_io.write tmp_channel body
      >>= fun () ->
      Lwt_io.close tmp_channel
      >>= Lwt_preemptive.detach (fun () ->
              try
                let zip_entries =
                  let zf = Zip.open_in tmp_name in
                  let entries =
                    Zip.entries zf
                    |> List.filter ~f:(fun entry -> not entry.is_directory)
                    |> List.map ~f:(fun entry ->
                           (entry, Zip.read_entry zf entry) )
                  in
                  Zip.close_in zf ; entries
                in
                Ok zip_entries
              with Zip.Error (zip_name, entry_name, message) ->
                Error (f "Zip.Error(%s, %s, %s)" zip_name entry_name message) ) )
  >|= action

(* GitHub specific *)

let project_api_preview_header =
  [("Accept", "application/vnd.github.inertia-preview+json")]

let app_api_preview_header =
  [("Accept", "application/vnd.github.machine-man-preview+json")]

let api_json_header = [("Accept", "application/vnd.github+json")]

let github_header bot_info =
  [("Authorization", "bearer " ^ github_token bot_info)]

let headers_of_list = headers

(* when following a redirect from GitHub to Azure, passing along the
   Authorization header results in 403 Forbidden.  So we strip the
   headers when we recurse by default. *)
let rec client_get ?(follow_redirects = true)
    ?(include_headers_in_redirects = false) ~user_agent ~headers uri =
  Client.get ~headers uri
  >>= fun (resp, body) ->
  match Response.status resp with
  | `OK ->
      Lwt.return_ok body
  | `Moved_permanently
  | `Found
  | `See_other
  | `Temporary_redirect
  | `Permanent_redirect
    when follow_redirects -> (
      let headers =
        if include_headers_in_redirects then headers
        else headers_of_list [] user_agent
      in
      match Header.get_location (Response.headers resp) with
      | Some new_uri ->
          Lwt_io.printlf "Following redirect to %s" (Uri.to_string new_uri)
          >>= fun () ->
          client_get ~follow_redirects ~include_headers_in_redirects ~headers
            ~user_agent new_uri
      | None ->
          let msg =
            f "Redirected from %s, but no Location header found"
              (Uri.to_string uri)
          in
          Lwt.return_error msg )
  | status_code ->
      let msg =
        f "HTTP request to %s failed with status code: %s" (Uri.to_string uri)
          (Code.string_of_status status_code)
      in
      Lwt.return_error msg

let generic_get ~bot_info relative_uri ?(header_list = []) handler =
  let open Lwt_result.Infix in
  let uri = "https://api.github.com/" ^ relative_uri |> Uri.of_string in
  let user_agent = bot_info.github_name in
  let headers = headers (header_list @ github_header bot_info) user_agent in
  client_get ~headers ~user_agent uri
  >>= (fun body -> Cohttp_lwt.Body.to_string body |> Lwt_result.ok)
  >>= handler

let generic_get_json ~bot_info relative_uri ?(header_list = []) json_handler =
  generic_get ~bot_info relative_uri ~header_list (fun body ->
      body |> handle_json json_handler |> Lwt.return )

let generic_get_zip ~bot_info relative_uri ?(header_list = []) zip_handler =
  generic_get ~bot_info relative_uri ~header_list (handle_zip zip_handler)
