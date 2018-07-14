#!/usr/bin/env bash

set -e

if ! type heroku > /dev/null 2>&1; then

heroku(){
  docker run -it --rm -u $(id -u):$(id -g) -w "$HOME" \
    -v /etc/passwd:/etc/passwd:ro \
    -v /etc/group:/etc/group:ro \
    -v /etc/localtime:/etc/localtime:ro \
    -v /home:/home \
    -v /tmp:/tmp \
    -v /run/user/$(id -u):/run/user/$(id -u) \
    -v $(pwd):/workdir \
    -w /workdir \
    --name heroku \
    johnnagro/heroku-toolbelt "$@"
}

fi

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
