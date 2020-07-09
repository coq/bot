# Contributing #

This is a work in progress and your help is welcome, both in the form
of issues and pull requests.

When reporting a bug or requesting a feature, please be as specific as
possible, and be ready to follow up.  If you are not ready to follow
up to make your needs more explicit, or to help with testing, don't
bother requesting a new feature in the first place.

## Building locally ##

The best way to get the dependencies is to run `nix-shell` (see the
[Nix](https://nixos.org/nix/) documentation to learn more).

Without Nix, you'll need [opam 2.0](https://opam.ocaml.org/doc/Install.html)
installed on your system. Run `opam update` followed by
`opam switch create . -y --deps-only` to install locally the required
ocaml libraries.

Use the following command to build:

```
dune build --ignore-promoted-rules
```

If you want to update the GraphQL schema:

```
dune build
```

This call to `dune build` without the `--ignore-promoted-rules` option
requires that a file `bot-components/.github-token` be provided and
contain a single line with a GitHub API personal token (with no
specific permission).  It will use this token and the node package
`graphql-cli` to update the GitHub schema stored in
`bot-component/schema.json`, before building the project.
Get `graphql-cli` with `npm install graphql-cli@3.0.14 -g` if you're
not compiling in `nix-shell`.

## Testing locally ##

To test locally, we recommend using Docker for building and running
the bot as a local server and [ngrok](https://ngrok.com/) to redirect
webhooks coming from the web to this local server.

1. Create an `.env` file defining the [required environment
   variables](../README.md#how-to-deploy-a-new-instance).
2. Build the bot with: `docker build . -t coq-bot-local-test`
3. Run the bot with: `docker run -p 8080:8080 --env-file .env coq-bot-local-test:latest`
4. Make the server accessible from the web with: `ngrok http 8080`
5. Configure your repositories' webhooks with the ngrok URL.
