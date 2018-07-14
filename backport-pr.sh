#!/usr/bin/env bash

set -e

PRNUM="$1"
BACKPORT_TO="$2"

cd repo

git fetch https://github.com/coq/coq master

if ! git log FETCH_HEAD --grep "Merge PR #${PRNUM}" | grep "." > /dev/null; then
    echo "PR #${PRNUM} does not exist."
    exit 1
fi

BRANCH=backport-pr-${PRNUM}
RANGE=$(git log FETCH_HEAD --grep "Merge PR #${PRNUM}" --format="%P" | sed 's/ /../')
MESSAGE=$(git log FETCH_HEAD --grep "Merge PR #${PRNUM}" --format="%s" | sed 's/Merge/Backport/')

git fetch https://gitlab.com/coq/coq "staging-$BACKPORT_TO"

git checkout --detach FETCH_HEAD

if git checkout -b "$BRANCH"; then
    if ! git cherry-pick -x "$RANGE"; then
        git status
        echo "Conflicts! Aborting..."
        git cherry-pick --abort
        git checkout -
        git branch -d "$BRANCH"
        exit 1
    fi
    git checkout -
else
    echo "Branch $BRANCH already exists! Aborting..."
    exit 1
fi

if ! git diff --exit-code HEAD "${BRANCH}" -- "*.mli"; then
    echo "Some mli files are modified. Aborting..."
    exit 1
fi

git merge --no-ff "${BRANCH}" -m "${MESSAGE}"
git branch -d "${BRANCH}"
