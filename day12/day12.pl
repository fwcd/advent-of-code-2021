:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% Graph utils

edge(V, W, [e(V, W)|_]).
edge(V, W, [e(W, V)|_]).
edge(V, W, [_|Es]) :- edge(V, W, Es).

remove_edge(_, _, [], []).
remove_edge(V, W, [e(V, W)|Es], Es) :- !.
remove_edge(V, W, [e(W, V)|Es], Es) :- !.
remove_edge(V, W, [E|Es], [E|Fs]) :- remove_edge(V, W, Es, Fs).

% Graph simplification

small_node(V) :- atom_chars(V, [C|_]), char_type(C, lower).

mark_visit(V, Visited, [V|Visited]) :- small_node(V), !.
mark_visit(_, Visited, Visited).

dfs_path(end, _, _, [end]).
dfs_path(V, Es, Visited, [V|Path]) :-
  edge(V, W, Es),
  \+ member(W, Visited),
  mark_visit(V, Visited, Visited2),
  dfs_path(W, Es, Visited2, Path).

all_paths(Es, Paths) :-
  findall(Path, dfs_path(start, Es, [], Path), Paths).

% DCG for parsing the input

dcg_edges([])     --> eos, !.
dcg_edges([E|Es]) --> dcg_edge(E), dcg_edges(Es).

dcg_edge(e(V, W)) --> string(VC), "-", string(WC), eol, !,
  { atom_codes(V, VC), atom_codes(W, WC) }.

% Main program

parse_input(Es) :-
  phrase_from_file(dcg_edges(Es), 'resources/input.txt').

main :-
  parse_input(Es),

  all_paths(Es, Paths),
  length(Paths, Part1),
  print(['Part 1:', Part1]), nl.
