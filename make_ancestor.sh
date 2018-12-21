#!/usr/bin/env bash

# usage: make_ancestor.sh base ref
# PWD must be in the relevant git repo
# merges base in ref if necessary

set -ex

if [ $# != 2 ]; then >&2 echo Bad argument count; fi

base=$1
ref=$2

basecommit=$(git rev-parse "$base")
refcommit=$(git rev-parse "$ref")

wtree=$(mktemp -d)

( #git checkout --detach HEAD # if ref is already checked out add worktree errors
  git worktree add "$wtree" "$ref"
  pushd "$wtree"
  if ! git merge "$base" -m "Bot merge $basecommit into $refcommit";
  then
      popd
      rm -rf "$wtree"
      git worktree prune
      false
  fi
)
rm -rf "$wtree"
git worktree prune
