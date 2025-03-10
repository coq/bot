open Base
open Cohttp_lwt_unix
open Lwt
open Lwt.Syntax
open Utils

let github_headers token =
  [ ("Content-Type", "application/json")
  ; ("accept", "application/vnd.github.machine-man-preview+json")
  ; ("authorization", "Bearer " ^ token) ]

let rs256_sign ~key ~data =
  (* Taken from https://github.com/robur-coop/ocaml-letsencrypt *)
  let h = Digestif.SHA256.(to_raw_string (digest_string data)) in
  let pkcs1_digest = X509.Certificate.encode_pkcs1_digest_info (`SHA256, h) in
  Mirage_crypto_pk.Rsa.PKCS1.sig_encode ~key pkcs1_digest

let base64 = Base64.encode ~pad:false ~alphabet:Base64.uri_safe_alphabet

(* The following functions are largely based on https://github.com/Schniz/reason-pr-labels *)
let make_jwt ~key ~app_id =
  let header = {|{ "alg": "RS256" }|} in
  let issuedAt = Unix.time () |> Int.of_float in
  let payload =
    f {|{ "iat": %d, "exp": %d, "iss": %d }|} issuedAt
      (issuedAt + (60 * 8))
      app_id
  in
  match (base64 header, base64 payload) with
  | Ok h, Ok p -> (
      let data = h ^ "." ^ p in
      match rs256_sign ~key ~data |> base64 with
      | Ok signature ->
          Ok (data ^ "." ^ signature)
      | Error _ ->
          Error "Couldn't create JWT token" )
  | _, _ ->
      Error "Couldn't create JWT token"

let get ~bot_info ~token ~url =
  let* () = Lwt_io.printl ("Making get request to " ^ url) in
  let headers = headers (github_headers token) bot_info.Bot_info.github_name in
  let* _response, body = Client.get ~headers (Uri.of_string url) in
  Cohttp_lwt.Body.to_string body

let post ~bot_info ~body ~token ~url =
  let* () = Lwt_io.printl ("Making post request to " ^ url) in
  let headers = headers (github_headers token) bot_info.Bot_info.github_name in
  let body =
    (match body with None -> "{}" | Some json -> Yojson.to_string json)
    |> Cohttp_lwt.Body.of_string
  in
  let* _response, body =
    Cohttp_lwt_unix.Client.post ~body ~headers (Uri.of_string url)
  in
  Cohttp_lwt.Body.to_string body

let get_installation_token ~bot_info ~key ~app_id ~install_id :
    (string * float, string) Result.t Lwt.t =
  match make_jwt ~key ~app_id with
  | Ok jwt -> (
      let access_token_url =
        f "https://api.github.com/app/installations/%d/access_tokens" install_id
      in
      post ~bot_info ~body:None ~token:jwt ~url:access_token_url
      >|= fun resp ->
      try
        let json = Yojson.Basic.from_string resp in
        Ok
          (* Installation tokens expire after one hour, let's stop using them after 40 minutes *)
          ( Yojson.Basic.Util.(json |> member "token" |> to_string)
          , Unix.time () +. (40. *. 60.) )
      with
      | Yojson.Json_error err ->
          Error (f "Json error: %s" err)
      | Yojson.Basic.Util.Type_error (err, _) ->
          Error (f "Json type error: %s" err) )
  | Error e ->
      Lwt.return (Error e)

let get_installations ~bot_info ~key ~app_id =
  match make_jwt ~key ~app_id with
  | Ok jwt -> (
      get ~bot_info ~token:jwt ~url:"https://api.github.com/app/installations"
      >|= fun body ->
      try
        let json = Yojson.Basic.from_string body in
        let open Yojson.Basic.Util in
        Ok
          ( json |> to_list
          |> List.map ~f:(fun json ->
                 ( json |> member "account" |> member "login" |> to_string
                 , json |> member "id" |> to_int ) ) )
      with
      | Yojson.Json_error err ->
          Error (f "Json error: %s" err)
      | Yojson.Basic.Util.Type_error (err, _) ->
          Error (f "Json type error: %s" err) )
  | Error e ->
      Lwt.return (Error e)
