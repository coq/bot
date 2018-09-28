# A (Coq Development Team) bot written in OCaml #

This bot is triggered by a few GitHub webhooks.

## Build locally ##

Use `nix-shell` to get into an appropriate development environment or use
`opam` to install `cohttp`, `cohttp-lwt-unix` and `yojson`.

Use the following command to build:

```
dune build
```

To run locally, use [ngrok](https://ngrok.io) to redirect a public address
to your local machine and set up the GitHub webhook accordingly.

## Deploy on Heroku ##

There's currently no up-to-date OCaml buildpack for Heroku so we deploy
binaries directly as explained here:
https://medium.com/cryptosense-tech/how-to-deploy-ocaml-on-heroku-9903548aafa5

The first time you upload your app to Heroku run:

```
heroku plugins:install heroku-builds
heroku apps:create --app my-coqbot
heroku buildpacks:set http://github.com/ryandotsmith/null-buildpack.git --app my-coqbot
```

If you've built inside `nix-shell`, a first step before deploying is to patch
the binary to make it work on standard Linux platforms:

```
patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 _build/default/bot.exe
```

Then:

```
./deploy.sh --app my-coqbot
```

The bot will need access to an SSH key without passphrase `bot_rsa`
to be able to push to GitLab, unless the environment variables
`USERNAME` and `PASSWORD` are defined.

## To-do ##

- [ ] More flexibility: make the repo addresses configurable
      or turn this into an OCaml GitHub bot library.
- [ ] Automatic deployment to Heroku when new code is committed.
- [ ] A test-suite for the bot.
- [ ] Document existing features.
- [ ] More features.
