# A (Coq Development Team) bot written in OCaml #

This bot is triggered by a GitHub webhook.

For now, it is set to receive pull request events and react to "opened",
"reopened" and "synchronize" events by force-pushing the PR content to
a branch `pr-XXXX` on GitLab, to benefit of GitLab CI for pull requests.
It also reacts to "closed" events by deleting the corresponding branch
on GitLab.

The bot will need access to an SSH key without passphrase `bot_rsa`
to be able to push to GitLab.

## Build locally ##

Use `nix-shell` to get into an appropriate development environment or use
`opam` to install `cohttp`, `cohttp-lwt-unix` and `yojson`.

Use the following command to build:

```
ocamlfind ocamlopt -package cohttp.lwt -package yojson -linkpkg bot.ml -o bot.native
```

To run locally, use [ngrok](https://ngrok.io) to redirect a public address
to your local machine and set up the GitHub webhook accordingly.

## Deploy on Heroku ##

Heroku doesn't support OCaml and there is no up-to-date community-provided
support so we deploy binaries directly as explained here:
https://medium.com/cryptosense-tech/how-to-deploy-ocaml-on-heroku-9903548aafa5

Don't forget the `bot_rsa` file to be deployed together with the binary.

If on NixOS, a first step is to patch the binary to make it work on non-NixOS
platforms:

```
patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 bot.native
```

Futhermore, to get a working `heroku` command, I use the current workaround
(found in [NixOS/nixpkgs#25983](https://github.com/NixOS/nixpkgs/pull/25983)):

```
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
```

which requires docker to be enabled.
My `configuration.nix` contains the following:

```
virtualisation.docker.enable = true;
users.extraUsers."${my_user_name}".extraGroups = [ "docker" ];
```

Then (back to every Linux system):

```
heroku plugins:install heroku-builds
heroku apps:create --app my-coqbot
heroku buildpacks:set http://github.com/ryandotsmith/null-buildpack.git --app my-coqbot
./deploy.sh --app my-coqbot
```

## To-do ##

- [ ] More flexibility: make the repo addresses configurable.
- [ ] Automatic deployment to Heroku when new code is committed.
- [ ] A test-suite for the bot.
- [ ] More features.
