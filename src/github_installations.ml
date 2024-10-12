open Base
open Bot_components
open Helpers
open Lwt

let installation_ids : (string, int) Base.Hashtbl.t =
  Hashtbl.create (module String)

let installation_tokens : (int, string * float) Base.Hashtbl.t =
  Hashtbl.create (module Int)

let action_with_new_installation_token ~bot_info ~key ~app_id ~install_id action
    =
  (* Installation tokens expire after one hour, we stop using them after 40 minutes *)
  GitHub_app.get_installation_token ~bot_info ~key ~app_id ~install_id
  >>= function
  | Ok (install_token, expiration_date) ->
      let _ =
        Hashtbl.add installation_tokens ~key:install_id
          ~data:(install_token, expiration_date)
      in
      let bot_info : Bot_info.t =
        {bot_info with github_install_token= Some install_token}
      in
      action ~bot_info
  | Error err ->
      failwith
        (f
           "We did not manage to get an installation token for installation \
            %d: %s"
           install_id err)

let action_as_github_app_from_install_id ~bot_info ~key ~app_id ~install_id
    action =
  (* Executes an action with an installation token if the repository has
     the GitHub app installed.
     Generates a new installation token if the existing one has expired. *)
  match Hashtbl.find installation_tokens install_id with
  | Some (install_token, expiration_date) ->
      if Float.(expiration_date < Unix.time ()) then (
        Hashtbl.remove installation_tokens install_id ;
        action_with_new_installation_token ~bot_info ~key ~app_id ~install_id
          action )
      else
        let bot_info : Bot_info.t =
          {bot_info with github_install_token= Some install_token}
        in
        action ~bot_info
  | None ->
      action_with_new_installation_token ~bot_info ~key ~app_id ~install_id
        action

let action_as_github_app ~bot_info ~key ~app_id ~owner action =
  (* Executes an action with an installation token if the repository has
     the GitHub app installed.
     Generates a new installation token if the existing one has expired. *)
  match Hashtbl.find installation_ids owner with
  | Some install_id ->
      action_as_github_app_from_install_id ~bot_info ~key ~app_id ~install_id
        action
  | None -> (
      GitHub_app.get_installations ~bot_info ~key ~app_id
      >>= function
      | Ok installs -> (
        match
          installs
          |> List.find_map ~f:(fun (owner', install_id) ->
                 if String.equal owner owner' then Some install_id else None)
        with
        | Some install_id ->
            let _ = Hashtbl.add installation_ids ~key:owner ~data:install_id in
            action_as_github_app_from_install_id ~bot_info ~key ~app_id
              ~install_id action
        | None ->
            (* If the owner does not have the GitHub app installed,
               we execute the action with the github access token. *)
            action ~bot_info )
      | Error err ->
          failwith
            (f "We did not manage to get the list of installations: %s" err) )
