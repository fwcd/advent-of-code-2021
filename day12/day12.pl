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

start_or_end(start).
start_or_end(end).

small_node(V) :- atom_chars(V, [C|_]), char_type(C, lower).

% DFS-based path-finding algorithm

part1_mark_visit(V, Visited, [V|Visited]) :- small_node(V), !.
part1_mark_visit(_, Visited, Visited).

part1_should_visit(V, Visited) :-
  \+ member(V, Visited).

part1_dfs_path(end, _, _, [end]).
part1_dfs_path(V, Es, Visited, [V|Path]) :-
  edge(V, W, Es),
  part1_should_visit(W, Visited),
  part1_mark_visit(V, Visited, NewVisited),
  part1_dfs_path(W, Es, NewVisited, Path).

part2_mark_visit(V, VisitedOnce, VisitedTwice, VisitedOnce, [V|VisitedTwice]) :- small_node(V), member(V, VisitedOnce), !.
part2_mark_visit(V, VisitedOnce, VisitedTwice, [V|VisitedOnce], VisitedTwice) :- small_node(V), !.
part2_mark_visit(_, VisitedOnce, VisitedTwice, VisitedOnce, VisitedTwice).

part2_should_visit(V, VisitedOnce, VisitedTwice) :- (start_or_end(V); length(VisitedTwice, L), L >= 1), !, \+ member(V, VisitedOnce), \+ member(V, VisitedTwice).
part2_should_visit(V, _, VisitedTwice) :- \+ member(V, VisitedTwice).

part2_dfs_path(end, _, _, _, [end]).
part2_dfs_path(V, Es, VisitedOnce, VisitedTwice, [V|Path]) :-
  edge(V, W, Es),
  part2_should_visit(W, VisitedOnce, VisitedTwice),
  part2_mark_visit(V, VisitedOnce, VisitedTwice, NewVisitedOnce, NewVisitedTwice),
  part2_dfs_path(W, Es, NewVisitedOnce, NewVisitedTwice, Path).

% DCG for parsing the input

dcg_edges([])     --> eos, !.
dcg_edges([E|Es]) --> dcg_edge(E), dcg_edges(Es).

dcg_edge(e(V, W)) --> string(VC), "-", string(WC), eol, !,
  { atom_codes(V, VC), atom_codes(W, WC) }.

% Main program

parse_input(Es) :-
  phrase_from_file(dcg_edges(Es), 'resources/demo2.txt').

println(X) :-
  print(X), nl.

main :-
  parse_input(Es),

  findall(Path, part1_dfs_path(start, Es, [], Path), Paths1),
  length(Paths1, Part1),
  println(['Part 1:', Part1]),

  findall(Path, part2_dfs_path(start, Es, [], [], Path), Paths2),
  length(Paths2, Part2),
  println(['Part 2:', Part2]).
