# CodinGame 2020 Fall Challenge (OCaml submission)

This is my submission to the CodinGame contest "[Fall Challenge 2020][challenge]".  It got
me to the Bronze league but I believe it was a good basis for the Silver league.

## Build

This worked with OCaml 4.11.1.

```bash
opam install alcotest dune
dune build
dune runtest
```

This produces:

* `_build/default/answer.ml`: Assembled source code that could be submitted to the game server.
  the cards.

[challenge]: https://www.codingame.com/multiplayer/bot-programming/fall-challenge-2020
