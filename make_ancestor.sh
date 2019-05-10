#!/usr/bin/env bash

# usage: make_ancestor.sh base head
# PWD must be in the relevant git repo
# merges base in head if necessary

set -ex

if [ $# != 2 ]; then >&2 echo Bad argument count; fi

base=$1
head=$2

basecommit=$(git rev-parse "$base")
headcommit=$(git rev-parse "$head")

wtree=$(mktemp -d)

( git worktree add "$wtree" "$head"
  pushd "$wtree"
  if ! git merge "$base" -m "Bot merge $basecommit into $headcommit";
  then
      popd
      rm -rf "$wtree"
      git worktree prune
      false
  fi
)
rm -rf "$wtree"
git worktree prune
