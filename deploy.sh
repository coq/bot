#!/usr/bin/env bash

set -e

if [ -f bot_rsa ]; then
    tar czf bot.tar.gz bot.exe make_ancestor.sh Procfile bot_rsa
else
    tar czf bot.tar.gz bot.exe make_ancestor.sh Procfile
fi
heroku builds:create --source-tar bot.tar.gz "$@"
