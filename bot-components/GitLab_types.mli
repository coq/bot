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
