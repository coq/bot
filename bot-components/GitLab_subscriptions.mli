type job_info =
  { build_status: string
  ; build_id: int
  ; build_name: string
  ; commit: string
  ; branch: string
  ; repo_url: string
  ; project_id: int
  ; failure_reason: string option
  ; allow_fail: bool option }

type pipeline_info =
  {state: string; id: int; commit: string; branch: string; project_path: string}

type msg =
  | JobEvent of job_info
  | PipelineEvent of pipeline_info
  | UnsupportedEvent of string

val pr_from_branch : string -> int option * string

val job_info_of_json : Yojson.Basic.t -> job_info

val pipeline_info_of_json : Yojson.Basic.t -> pipeline_info

val receive_gitlab :
  secret:string -> Cohttp.Header.t -> string -> (bool * msg, string) result
