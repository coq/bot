#!/usr/bin/env bash

# usage: coq_bug_minimizer.sh comment_thread_id github_token bot_name bot_domain owner repo docker_image target opam_switch failing_urls passing_urls base head

set -ex

if [ $# != 13 ]; then >&2 echo Bad argument count; fi

comment_thread_id=$1
token=$2
bot_name=$3
bot_domain=$4
owner=$5
repo=$6
docker_image=$7
target=$8
opam_switch=$9
failing_urls=${10}
passing_urls=${11}
base=${12}
head=${13}
branch_id=$(($(od -A n -t uI -N 5 /dev/urandom | tr -d ' ')))
repo_name="coq-community/run-coq-bug-minimizer"
branch_name="run-coq-bug-minimizer-$branch_id"

wtree=$(mktemp -d)

git fetch "https://github.com/$repo_name.git" "refs/heads/master:$branch_name"
git worktree add "$wtree" "$branch_name"
pushd "$wtree"

printf "%s %s %s %s %s %s" "$comment_thread_id" "<>" "$repo_name" "$branch_name" "$owner" "$repo" > coqbot-request-stamp
sed -i 's~^\(\s*\)[^:\s]*custom_image:.*$~\1custom_image: '"'${docker_image}'~" .github/workflows/main.yml
echo "${target}" > coqbot.ci-target
echo "${opam_switch}" > coqbot.compiler
echo "${failing_urls}" >  coqbot.failing-artifact-urls
echo "${passing_urls}" >  coqbot.passing-artifact-urls
echo "${head}" > coqbot.failing-sha
echo "${base}" > coqbot.passing-sha
echo "https://$bot_domain/ci-minimization" > coqbot.url
git add .
git commit -m "Set up CI minimization run."
git push --set-upstream "https://$bot_name:$token@github.com/$repo_name.git" "$branch_name"

popd
git worktree remove "$wtree"
