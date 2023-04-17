type ci_common_info =
  { head_commit: string
  ; base_commit: string option
  ; branch: string
  ; http_repo_url: string
  ; project_id: int }

type 'a job_info =
  { build_status: string
  ; build_id: int
  ; build_name: string
  ; stage: string
  ; failure_reason: string option
  ; allow_fail: bool
  ; common_info: 'a }

type pipeline_info =
  { state: string
  ; pipeline_id: int
  ; common_info: ci_common_info
  ; variables: (string * string) list
  ; stages: string list
  ; builds: unit job_info list }
