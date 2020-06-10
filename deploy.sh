#!/usr/bin/env bash

set -e

patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 bot.exe
if [ -f bot_rsa ]; then
    tar czf bot.tar.gz bot.exe make_ancestor.sh Procfile bot_rsa
else
    tar czf bot.tar.gz bot.exe make_ancestor.sh Procfile
fi
heroku builds:create --source-tar bot.tar.gz "$@"
