val receive_gitlab :
     secret:string
  -> Cohttp.Header.t
  -> string
  -> (bool * GitLab_types.msg, string) result
