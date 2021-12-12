:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% DCG for parsing the input

edges([])     --> eos, !.
edges([E|Es]) --> edge(E), edges(Es).

edge(e(V, W)) --> string(VC), "-", string(WC), eol, !,
  { atom_codes(V, VC), atom_codes(W, WC) }.

% Main program

parse_input(Es) :-
  phrase_from_file(edges(Es), 'resources/demo.txt').

main :-
  parse_input(_).
