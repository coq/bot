#!/usr/bin/env bash

# usage: backport-pr.sh num ref
# PWD must be in the relevant git repo
# backports PR num to existing GitLab coq/coq ref

set -e

if [ $# != 2 ]; then >&2 echo Bad argument count; fi

num=$1
ref=$2

wtree=$(mktemp -d)

git fetch https://github.com/coq/coq master

if ! git log FETCH_HEAD --grep "Merge PR #${num}" | grep "." > /dev/null; then
    echo "PR #${num} does not exist."
    exit 1
fi

BRANCH=backport-pr-${num}
RANGE=$(git log FETCH_HEAD --grep "Merge PR #${num}" --format="%P" | sed 's/ /../')
MESSAGE=$(git log FETCH_HEAD --grep "Merge PR #${num}" --format="%s" | sed 's/Merge/Backport/')

git worktree add "$wtree" "$ref"
pushd "$wtree"

if git checkout -b "$BRANCH"; then
    if ! git cherry-pick -x "$RANGE"; then
        git status
        echo "Conflicts! Aborting..."
        git cherry-pick --abort
        popd
        rm -rf "$wtree"
        git worktree prune
        git branch -D "$BRANCH"
        exit 1
    fi
    git checkout -
else
    echo "Branch $BRANCH already exists! Aborting..."
    popd
    rm -rf "$wtree"
    git worktree prune
    exit 1
fi

if ! git diff --exit-code HEAD "${BRANCH}" -- "*.mli"; then
    echo "Some mli files are modified. Aborting..."
    popd
    rm -rf "$wtree"
    git worktree prune
    exit 1
fi

git merge --no-ff "${BRANCH}" -m "${MESSAGE}"
git branch -d "${BRANCH}"
