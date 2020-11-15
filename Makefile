all:
	dune build @fmt --auto-promote || true
	dune build @all @runtest
