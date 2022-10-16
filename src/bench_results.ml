open Base

module Table = struct
  type t = {table: string; count: int option}
end

let f = Printf.sprintf

type t =
  { summary_table: Table.t option
  ; slow_table: Table.t option
  ; fast_table: Table.t option
  ; slow_table_pdiff: Table.t option
  ; fast_table_pdiff: Table.t option }

let pp_count () count =
  match count with None -> "" | Some n -> Printf.sprintf "Top %d" n

let pp_table_with_header title table ~emoji =
  Markdown.header2
    (f "%s %a %s:\n%s" emoji pp_count table.Table.count title
       (Markdown.code table.table) )

let pp_table_details title table ~emoji =
  Markdown.details
    (f "%s %a %s" emoji pp_count table.Table.count title)
    (Markdown.code table.table)

let pp_summary_table_opt t =
  Option.map
    ~f:(pp_table_with_header "Bench Summary" ~emoji:":checkered_flag:")
    t.summary_table

let pp_slow_table_opt t =
  Option.map
    ~f:(pp_table_with_header "Slow Downs" ~emoji:":turtle:")
    t.slow_table

let pp_fast_table_opt t =
  Option.map
    ~f:(pp_table_with_header "Speed Ups" ~emoji:":rabbit:")
    t.fast_table

let pp_slow_table_pdiff_opt t =
  Option.map
    ~f:(pp_table_with_header "Slow Downs %%Diff" ~emoji:":snail:")
    t.slow_table_pdiff

let pp_fast_table_pdiff_opt t =
  Option.map
    ~f:(pp_table_with_header "Speed Ups %%Diff" ~emoji:":eagle:")
    t.fast_table_pdiff

let pp_details_slow_table_opt t =
  Option.map ~f:(pp_table_details "Slow Downs" ~emoji:":turtle:") t.slow_table

let pp_details_fast_table_opt t =
  Option.map ~f:(pp_table_details "Speed Ups" ~emoji:":rabbit:") t.fast_table

let bench_text t =
  [ pp_summary_table_opt t
  ; pp_slow_table_opt t
  ; pp_fast_table_opt t
  ; pp_slow_table_pdiff_opt t
  ; pp_fast_table_pdiff_opt t ]
  |> List.filter_opt |> String.concat ~sep:"\n"

let bench_comment_text ~gitlab_url ~check_url t =
  let summary_line =
    match check_url with
    | Ok url ->
        Some (Markdown.link_bullet ":spiral_notepad: Bench Check Summary" url)
    | _ ->
        None
  in
  [ pp_summary_table_opt t
  ; pp_details_slow_table_opt t
  ; pp_details_fast_table_opt t
  ; Some (Markdown.link_bullet ":chair: GitLab Bench Job" gitlab_url)
  ; summary_line ]
  |> List.filter_opt |> String.concat ~sep:"\n"

let result_example_1, result_example_2 =
  let summary_table = Some {Table.table= {|SUMMARY TABLE|}; count= None} in
  let slow_table = Some {Table.table= {|SLOW TABLE|}; count= Some 12} in
  let fast_table = Some {Table.table= {|FAST TABLE|}; count= Some 21} in
  let slow_table_pdiff =
    Some {Table.table= {|SLOW TABLE PDIFF|}; count= Some 23}
  in
  let fast_table_pdiff =
    Some {Table.table= {|FAST TABLE PDIFF|}; count= Some 34}
  in
  ( {summary_table; slow_table; fast_table; slow_table_pdiff; fast_table_pdiff}
  , { summary_table
    ; slow_table
    ; fast_table= None
    ; slow_table_pdiff= None
    ; fast_table_pdiff } )

let%expect_test "test printing of bench summary" =
  bench_text result_example_1 |> Stdio.print_string ;
  [%expect
    {|
## :checkered_flag:  Bench Summary:
```
SUMMARY TABLE
```
## :turtle: Top 12 Slow Downs:
```
SLOW TABLE
```
## :rabbit: Top 21 Speed Ups:
```
FAST TABLE
```
## :snail: Top 23 Slow Downs %%Diff:
```
SLOW TABLE PDIFF
```
## :eagle: Top 34 Speed Ups %%Diff:
```
FAST TABLE PDIFF
```
|}]

let%expect_test "test printing of bench summary with missing tables" =
  bench_text result_example_2 |> Stdio.print_string ;
  [%expect
    {|
## :checkered_flag:  Bench Summary:
```
SUMMARY TABLE
```
## :turtle: Top 12 Slow Downs:
```
SLOW TABLE
```
## :eagle: Top 34 Speed Ups %%Diff:
```
FAST TABLE PDIFF
```
|}]

let%expect_test "test printing of bench comment with check url" =
  bench_comment_text ~gitlab_url:"www.SOMEGITLAB.URL"
    ~check_url:(Ok "www.SOMECHECK.url") result_example_1
  |> Stdio.print_string ;
  [%expect
    {|
## :checkered_flag:  Bench Summary:
```
SUMMARY TABLE
```
<details>
<summary>:turtle: Top 12 Slow Downs</summary>

```
SLOW TABLE
```

</details>

<details>
<summary>:rabbit: Top 21 Speed Ups</summary>

```
FAST TABLE
```

</details>

- [:chair: GitLab Bench Job](www.SOMEGITLAB.URL)
- [:spiral_notepad: Bench Check Summary](www.SOMECHECK.url)
|}]

let%expect_test "test printing of bench comment with check url missing" =
  bench_comment_text ~gitlab_url:"www.SOMEGITLAB.URL"
    ~check_url:(Error "some error") result_example_1
  |> Stdio.print_string ;
  [%expect
    {|
## :checkered_flag:  Bench Summary:
```
SUMMARY TABLE
```
<details>
<summary>:turtle: Top 12 Slow Downs</summary>

```
SLOW TABLE
```

</details>

<details>
<summary>:rabbit: Top 21 Speed Ups</summary>

```
FAST TABLE
```

</details>

- [:chair: GitLab Bench Job](www.SOMEGITLAB.URL)
|}]

let%expect_test "test printing of bench comment with check url and tables \
                 missing " =
  bench_comment_text ~gitlab_url:"www.SOMEGITLAB.URL"
    ~check_url:(Ok "www.SOMECHECK.url") result_example_2
  |> Stdio.print_string ;
  [%expect
    {|
## :checkered_flag:  Bench Summary:
```
SUMMARY TABLE
```
<details>
<summary>:turtle: Top 12 Slow Downs</summary>

```
SLOW TABLE
```

</details>

- [:chair: GitLab Bench Job](www.SOMEGITLAB.URL)
- [:spiral_notepad: Bench Check Summary](www.SOMECHECK.url)
|}]
