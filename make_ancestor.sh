#!/usr/bin/env bash

# usage: make_ancestor.sh base head pr_title prnum
# PWD must be in the relevant git repo
# merges base in head if necessary

# exit with status code 10 if merge was unsuccessful

set -ex

if [ $# != 4 ]; then >&2 echo Bad argument count; fi

base=$1
head=$2
pr_title=$3
prnum=$4

basecommit=$(git rev-parse "$base")
headcommit=$(git rev-parse "$head")

wtree=$(mktemp -d)

( git worktree add "$wtree" "$head"
  pushd "$wtree"

  # We have to merge $head into $base as otherwise when $head is ahead
  # of $base merge will do nothing even with --no-ff.
  # We assume $base is never ahead of $head.
  # This means the "bot merge $base into $head" message is upside down
  # but since the bot matches on it we can't easily change it.
  git reset --hard "$basecommit"
  if ! git merge --no-ff "$headcommit" \
       -m "[CI merge] PR #$prnum: $pr_title" \
       -m "Bot merge $basecommit into $headcommit";
  then
      popd
      rm -rf "$wtree"
      git worktree prune
      exit 10
  fi
)
rm -rf "$wtree"
git worktree prune
