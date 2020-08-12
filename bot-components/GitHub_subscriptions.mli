val receive_github :
     secret:string
  -> Cohttp.Header.t
  -> string
  -> (bool * GitHub_types.msg, string) result
