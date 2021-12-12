:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% Graph utils

edge(V, W, [e(V, W)|_]).
edge(V, W, [_|Es]) :- edge(V, W, Es).

% DCG for parsing the input

dcg_edges([])     --> eos, !.
dcg_edges([E|Es]) --> dcg_edge(E), dcg_edges(Es).

dcg_edge(e(V, W)) --> string(VC), "-", string(WC), eol, !,
  { atom_codes(V, VC), atom_codes(W, WC) }.

% Main program

parse_input(Es) :-
  phrase_from_file(dcg_edges(Es), 'resources/demo.txt').

main :-
  parse_input(_).
