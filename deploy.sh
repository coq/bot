#!/usr/bin/env bash

set -e

if [ -f bot.exe ]; then
    echo "Cannot overwrite bot.exe"
    exit 1
fi
cp _build/default/bot.exe .
if [ -f bot_rsa ]; then
    tar czf bot.tar.gz bot.exe backport-pr.sh Procfile bot_rsa
else
    tar czf bot.tar.gz bot.exe backport-pr.sh Procfile
fi
rm bot.exe
heroku builds:create --source-tar bot.tar.gz "$@"
