#!/usr/bin/env bash

# usage: coq_bug_minimizer.sh 'script' comment_thread_id comment_author github_token bot_name bot_domain owner repo coq_version ocaml_version

set -ex

if [ $# != 10 ]; then >&2 echo Bad argument count; exit 1; fi

script=$1
comment_thread_id=$2
comment_author=$3
token=$4
bot_name=$5
bot_domain=$6
owner=$7
repo=$8
coq_version=$9
ocaml_version=${10}
branch_id=$(($(od -A n -t uI -N 5 /dev/urandom | tr -d ' ')))
repo_name="coq-community/run-coq-bug-minimizer"
branch_name="run-coq-bug-minimizer-$branch_id"

wtree=$(mktemp -d)

git fetch "https://github.com/$repo_name.git" "refs/heads/master:$branch_name"
git worktree add "$wtree" "$branch_name"
pushd "$wtree"

printf "%s %s %s %s %s %s" "$comment_thread_id" "$comment_author" "$repo_name" "$branch_name" "$owner" "$repo" > coqbot-request-stamp
test -z "${coq_version}" || sed -i 's~^\(\s*\)[^:\s]*coq_version:.*$~\1coq_version: '"'${coq_version}'~" .github/workflows/main.yml
test -z "${ocaml_version}" || sed -i 's~^\(\s*\)[^:\s]*ocaml_version:.*$~\1ocaml_version: '"'${ocaml_version}'~" .github/workflows/main.yml
printf "%s\n" "$script" > coqbot.sh
sed -i 's/\r$//g' coqbot.sh
echo "https://$bot_domain/coq-bug-minimizer" > coqbot.url
git add .
git commit -m "$(printf "Added user script in coqbot.sh for %s in %s/%s\n\nComment Thread ID: %s\n" "$comment_author" "$owner" "$repo" "$comment_thread_id")"
git push --set-upstream "https://$bot_name:$token@github.com/$repo_name.git" "$branch_name"

popd
git worktree remove "$wtree"
