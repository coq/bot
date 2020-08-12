type msg =
  | JobEvent of GitLab_types.job_info
  | PipelineEvent of GitLab_types.pipeline_info
  | UnsupportedEvent of string

val receive_gitlab :
  secret:string -> Cohttp.Header.t -> string -> (bool * msg, string) result
