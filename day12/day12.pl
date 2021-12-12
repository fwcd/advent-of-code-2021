:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% Graph utils

% For convenience, we also use graphs to represent
% associative arrays, such as the 'Visited' dictionary later.

edge(V, W, [e(V, W)|_]).
edge(V, W, [e(W, V)|_]).
edge(V, W, [_|Es]) :- edge(V, W, Es).

remove_edge(_, _, [], []).
remove_edge(V, W, [e(V, W)|Es], Es) :- !.
remove_edge(V, W, [e(W, V)|Es], Es) :- !.
remove_edge(V, W, [E|Es], [E|Fs]) :- remove_edge(V, W, Es, Fs).

unsorted_nodes([], []).
unsorted_nodes([e(V, W)|Es], [V,W|Vs]) :- unsorted_nodes(Es, Vs).

nodes(Es, Ws) :- unsorted_nodes(Es, Vs), sort(Vs, Ws).

node(V, Es) :- nodes(Es, Vs), member(V, Vs).

start_or_end(start).
start_or_end(end).

small_node(V) :- atom_chars(V, [C|_]), char_type(C, lower).

% DFS-based path-finding algorithm

part1_mark_visit(V, Visited, [V|Visited]) :- small_node(V), !.
part1_mark_visit(_, Visited, Visited).

part1_dfs_path(end, _, _, [end]).
part1_dfs_path(V, Es, Visited, [V|Path]) :-
  edge(V, W, Es),
  \+ member(W, Visited),
  part1_mark_visit(V, Visited, NewVisited),
  part1_dfs_path(W, Es, NewVisited, Path).

part1_dfs_path(V, Es, Path) :-
  part1_dfs_path(V, Es, [], Path).

part1_paths(Es, Paths) :-
  findall(Path, part1_dfs_path(start, Es, Path), Paths).

part2_mark_visit(V, V, VisitedOnce, VisitedTwice, [V|VisitedOnce], VisitedTwice) :- \+ member(V, VisitedOnce), !.
part2_mark_visit(V, _, VisitedOnce, VisitedTwice, VisitedOnce, [V|VisitedTwice]) :- small_node(V), !.
part2_mark_visit(_, _, VisitedOnce, VisitedTwice, VisitedOnce, VisitedTwice).

part2_dfs_path(end, _, _, _, _, [end]).
part2_dfs_path(V, Es, TwiceVisitable, VisitedOnce, VisitedTwice, [V|Path]) :-
  edge(V, W, Es),
  \+ member(W, VisitedTwice),
  part2_mark_visit(V, TwiceVisitable, VisitedOnce, VisitedTwice, NewVisitedOnce, NewVisitedTwice),
  part2_dfs_path(W, Es, TwiceVisitable, NewVisitedOnce, NewVisitedTwice, Path).

part2_dfs_path(V, Es, Path) :-
  node(TwiceVisitable, Es),
  small_node(TwiceVisitable),
  \+ start_or_end(TwiceVisitable),
  part2_dfs_path(V, Es, TwiceVisitable, [], [], Path).

part2_paths(Es, SortedPaths) :-
  findall(Path, part2_dfs_path(start, Es, Path), Paths),
  sort(Paths, SortedPaths).

% DCG for parsing the input

dcg_edges([])     --> eos, !.
dcg_edges([E|Es]) --> dcg_edge(E), dcg_edges(Es).

dcg_edge(e(V, W)) --> string(VC), "-", string(WC), eol, !,
  { atom_codes(V, VC), atom_codes(W, WC) }.

% Main program

parse_input(Es) :-
  phrase_from_file(dcg_edges(Es), 'resources/input.txt').

println(X) :-
  print(X), nl.

main :-
  parse_input(Es),

  part1_paths(Es, Paths1),
  length(Paths1, Part1),
  println(['Part 1:', Part1]),

  part2_paths(Es, Paths2),
  length(Paths2, Part2),
  println(['Part 2:', Part2]).
