#!/usr/bin/env bash

# usage: coq_bug_minimizer.sh comment_thread_id github_token bot_name bot_domain owner repo pr_number docker_image target opam_switch failing_urls passing_urls base head minimizer_extra_arguments [bug_file]

set -e

if [ $# != 15 ] && [ $# != 16 ]; then >&2 echo Bad argument count; exit 1; fi

comment_thread_id=$1
token=$2
bot_name=$3
bot_domain=$4
owner=$5
repo=$6
pr_number=$7
docker_image=$8
target=$9
ci_targets=${10}
opam_switch=${11}
failing_urls=${12}
passing_urls=${13}
base=${14}
head=${15}
minimizer_extra_arguments=${16}
bug_file=${17}
branch_id=$(($(od -A n -t uI -N 5 /dev/urandom | tr -d ' ')))
repo_name="coq-community/run-coq-bug-minimizer"
branch_name="run-coq-bug-minimizer-$branch_id"
nl=$'\n'
resumption_args=(
    "${docker_image}"
    "${target}"
    "${opam_switch}"
    "${failing_urls}"
    "${passing_urls}"
    "${base}"
    "${head}"
    "${minimizer_extra_arguments}"
)

if [ -f "${bug_file}" ]; then
    bug_file_contents="$(cat "${bug_file}")"
else
    bug_file_contents=""
fi

wtree=$(mktemp -d)

git fetch "https://github.com/$repo_name.git" "refs/heads/master:$branch_name"
git worktree add "$wtree" "$branch_name"
pushd "$wtree"

printf "%s %s %s %s %s %s %s" "$comment_thread_id" "<>" "$repo_name" "$branch_name" "$owner" "$repo" "$pr_number" > coqbot-request-stamp
sed -i 's~^\(\s*\)[^:\s]*custom_image:.*$~\1custom_image: '"'${docker_image}'~" .github/workflows/main.yml
echo "${target}" > coqbot.ci-target
echo "${opam_switch}" > coqbot.compiler
echo "${failing_urls}" >  coqbot.failing-artifact-urls
echo "${passing_urls}" >  coqbot.passing-artifact-urls
echo "${head}" > coqbot.failing-sha
echo "${base}" > coqbot.passing-sha
echo "${pr_number}" > coqbot.issue-number
printf "%s" "${minimizer_extra_arguments}" | tr ' ' '\n' > coqbot.extra-args
echo "https://$bot_domain/ci-minimization" > coqbot.url
echo "https://$bot_domain/resume-ci-minimization" > coqbot.resume-minimization-url
rm -f coqbot.resumption-args
for arg in "${resumption_args[@]}"; do
    echo "$(echo -n "$arg" | tr '\n' ' ')" >> coqbot.resumption-args
done
resumption=""
if [ ! -z "${bug_file_contents}" ]; then
    resumption=" resumption"
    echo "${bug_file_contents}" > bug.v
fi
git add .
git commit -m "Set up CI minimization${resumption} run for ${target}${nl}${nl}At ${owner}/${repo}@${head} over ${owner}/${repo}@${base}"
git push --set-upstream "https://$bot_name:$token@github.com/$repo_name.git" "$branch_name"

popd
git worktree remove "$wtree"
