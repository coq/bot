open Base

let f = Printf.sprintf

let header1 str = f "# %s" str

let header2 str = f "## %s" str

let header3 str = f "### %s" str

let code ?(lang = "") str = f "```%s\n%s\n```" lang str

let details summary text =
  f "<details>\n<summary>%s</summary>\n\n%s\n\n</details>\n" summary text

let link text url = f "[%s](%s)" text url

let link_bullet text url = f "- %s" (link text url)
