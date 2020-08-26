open Base
open Cohttp_lwt_unix
open Lwt
open Utils

let github_headers token =
  [ ("Content-Type", "application/json")
  ; ("accept", "application/vnd.github.machine-man-preview+json")
  ; ("authorization", "Bearer " ^ token) ]

let rs256_sign ~key ~data =
  (* Taken from https://github.com/mmaker/ocaml-letsencrypt *)
  let data = Cstruct.of_string data in
  let h = Mirage_crypto.Hash.SHA256.digest data in
  let pkcs1_digest = X509.Certificate.encode_pkcs1_digest_info (`SHA256, h) in
  Mirage_crypto_pk.Rsa.PKCS1.sig_encode ~key pkcs1_digest |> Cstruct.to_string

let base64 = Base64.encode ~pad:false ~alphabet:Base64.uri_safe_alphabet

(* The following functions are largely based on https://github.com/Schniz/reason-pr-labels *)
let make_jwt ~key ~app_id =
  let header = "{ \"alg\": \"RS256\" }" in
  let issuedAt = Unix.time () |> Int.of_float in
  let payload =
    f "{ \"iat\": %d, \"exp\": %d, \"iss\": %d }" issuedAt
      (issuedAt + (60 * 10))
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
  Stdio.print_endline ("Making get request to " ^ url) ;
  let headers = headers ~bot_info (github_headers token) in
  Client.get ~headers (Uri.of_string url)
  >>= fun (_response, body) -> Cohttp_lwt.Body.to_string body

let post ~bot_info ~body ~token ~url =
  Stdio.print_endline ("Making post request to " ^ url) ;
  let headers = headers ~bot_info (github_headers token) in
  let body =
    (match body with None -> "{}" | Some json -> Yojson.to_string json)
    |> Cohttp_lwt.Body.of_string
  in
  Cohttp_lwt_unix.Client.post ~body ~headers (Uri.of_string url)
  >>= fun (_response, body) -> Cohttp_lwt.Body.to_string body

let get_installation_token ~bot_info ~owner ~repo ~jwt :
    (string * float, string) Result.t Lwt.t =
  get ~bot_info ~token:jwt
    ~url:(f "https://api.github.com/repos/%s/%s/installation" owner repo)
  >>= (fun body ->
        try
          let json = Yojson.Basic.from_string body in
          let access_token_url =
            Yojson.Basic.Util.(json |> member "access_tokens_url" |> to_string)
          in
          post ~bot_info ~body:None ~token:jwt ~url:access_token_url
          >|= Result.return
        with
        | Yojson.Json_error err ->
            Lwt.return_error (f "Json error: %s" err)
        | Yojson.Basic.Util.Type_error (err, _) ->
            Lwt.return_error (f "Json type error: %s" err))
  >|= Result.bind ~f:(fun resp ->
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
              Error (f "Json type error: %s" err))

let get_installation_token ~bot_info ~key ~app_id ~owner ~repo =
  match make_jwt ~key ~app_id with
  | Ok jwt ->
      get_installation_token ~bot_info ~jwt ~owner ~repo
  | Error e ->
      Lwt.return (Error e)
