#!/usr/bin/env bash

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

tar czf bot.tar.gz bot.native Procfile bot_rsa
heroku builds:create --source-tar bot.tar.gz "$@"
