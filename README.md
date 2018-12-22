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

## Rely on @coqbot to synchronize your GitHub PRs with your GitLab project ##

One of the main features of this bot is its ability to listen to a pull request
GitHub webhook and push every new or updated pull request to a branch named
`pr-<num>` on GitLab.com so that projects can take advantage of GitLab CI for
their GitHub project, including for pull requests originating from forks.

Note that there are security issues with this setup if you have defined
(unprotected) secret variables on your GitLab repository.

To use this feature for your project, you should first create a repository on
GitLab.com whose URL *must* correspond to the GitHub project URL except for the
"lab" replacing the "hub" part.

The bot will only take care of mirroring the PRs and reporting status checks
back so you may still want to activate the mirroring feature for the main
branches. To do so, the easiest way is to choose the "CI/CD for external repo"
option when creating the GitLab repository.
However, choose to give the repo by URL rather than with the GitHub button,
because we won't need GitLab's own status check reporting feature. (If it is
already activated, you can disable this integration in the "Settings" /
"Integration" menu).

In your GitLab repository:
- go to "Settings" / "Members" to add [**@coqbot**](https://gitlab.com/coqbot)
  as a project member with "Developer" role (so that it can push new branches).
- go to "Settings" / "Integration" and create two webhooks: one with URL
  <https://coqbot.herokuapp.com/pipeline> that will only be triggered by
  pipeline events, and one with URL <https://coqbot.herokuapp.com/job> that
  will only be triggered by job events.

In your GitHub repository:
- go to "Settings" / "Collaborators & teams" to add [**@coqbot**](https://github.com/coqbot)
  as a collaborator (so that it can push status checks).
- go to "Settings" / "Webhooks" and add one webhook with URL
  <https://coqbot.herokuapp.com/pull_request> that will only be triggered by
  pull request events. Make sure you change the "content/type" value to
  "application/json".
