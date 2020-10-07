FROM ocaml/opam2:alpine-3.10-ocaml-4.10 AS builder

WORKDIR /src
COPY coq-bot.opam bot-components.opam ./

# Install the OCaml libraries needed to compile the program
RUN echo 'archive-mirrors: [ "https://opam.ocaml.org/cache" ]' >> ~/.opam/config \
  && opam repository set-url default http://opam.ocaml.org \
  && opam switch 4.10 \
  && echo 'pre-session-commands: [ "sudo" "apk" "add" depexts ]' >> ~/.opam/config \
  && opam install --deps-only .

COPY . .

RUN sudo chown -R opam:nogroup .
RUN opam install . --destdir /src/opam-install

# Store the dynamic dependencies of the server
RUN opam depext -ln coq-bot > /src/depexts-coq-bot
RUN opam depext -ln bot-components > /src/depexts-bot-components


FROM alpine:3.10 AS app

WORKDIR /app

COPY --from=builder /src/opam-install/bin/bot bot.exe
COPY make_ancestor.sh coq_bug_minimizer.sh run_ci_minimization.sh ./

RUN apk update \
  && apk add bash git \
  && addgroup coqbot \
  && adduser coqbot -DG coqbot

COPY --from=builder /src/depexts-coq-bot depexts-coq-bot
COPY --from=builder /src/depexts-bot-components depexts-bot-components

# Install the required dynamic dependencies
RUN cat depexts-coq-bot | xargs apk --update add
RUN cat depexts-bot-components | xargs apk --update add

EXPOSE 8000

CMD ["./bot.exe"]