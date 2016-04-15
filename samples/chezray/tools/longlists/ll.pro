%-*-Prolog-*-  
% ll indented on 11/30/2001 by 'JOLI' 1.0.

/* Apply atomic predicate or partially applied predicate to arg list */
apply(AtomicPred, ArgList) :-
  atom(AtomicPred), !,
  Goal =.. [AtomicPred|ArgList],
  call(Goal).
apply(PartialPred, ArgList) :-
  PartialPred =.. PredList,
  append(PredList, ArgList, CompleteList),
  Goal =.. CompleteList,
  call(Goal).

reverse(X, Y) :-
  var(X), !,
  nonvar(Y),
  reverse_dl(Y, X - []).
reverse(X, Y) :-
  reverse_dl(X, Y - []).

reverse_dl([A, B, C, D, E, F, G, H|Rest], Rev) :-
  R2 = [H, G, F, E, D, C, B, A|X],
  dl_append(R1 - R2, R2 - X, Rev), !,
  reverse_dl(Rest, R1 - R2).
reverse_dl(X, Y) :-
  reverse4(X, Y).
reverse_dl(X, Y) :-
  reverse3(X, Y).

reverse4([A, B, C, D|Rest], Rev) :-
  R2 = [D, C, B, A|X],
  dl_append(R1 - R2, R2 - X, Rev),
  reverse3(Rest, R1 - R2).

reverse3([A, B, C], [C, B, A|X] - X).
reverse3([A, B], [B, A|X] - X).
reverse3([A], [A|X] - X).
reverse3([], X - X).

dl_append(A - B, B - C, A - C).

append(X, Y, Z) :-
  nonvar(X),
  nonvar(Y), !,
  catenate(X, Y, Z).
append(X, Y, Z) :-
  appsplit(X, Y, Z).

appsplit([], X, X).
appsplit([H|T], W, [H|Z]) :-
  appsplit(T, W, Z).

catenate([A, B, C, D, E, F, G, H|Rest], Tail, 
        [A, B, C, D, E, F, G, H|SoFar]) :-
  catenate(Rest, Tail, SoFar), !.
catenate(X, Y, Z) :-
  catenate4(X, Y, Z).

catenate4([A, B, C, D|Rest], Tail, [A, B, C, D|SoFar]) :-
  catenate3(Rest, Tail, SoFar), !.
catenate4(X, Y, Z) :-
  catenate3(X, Y, Z).

catenate3([A, B, C], Tail, [A, B, C|Tail]).
catenate3([A, B], Tail, [A, B|Tail]).
catenate3([A], Tail, [A|Tail]).
catenate3([], Tail, Tail).

length(L, N) :-
  var(N),
  nonvar(L), !,
  get_length(L, Length),
  N is Length.
length(L, N) :-
  integer(N),
  make_length(L, N).

get_length([_, _, _, _, _, _, _, _|Rest], LRest + 8) :- !,
  get_length(Rest, LRest).
get_length(X, Y) :-
  get_length4(X, Y).

get_length4([_, _, _, _|Rest], LRest + 4) :- !,
  get_length3(Rest, LRest).
get_length4(X, Y) :-
  get_length3(X, Y).

get_length3([_, _, _], 3) :- !.
get_length3([_, _], 2) :- !.
get_length3([_], 1) :- !.
get_length3([], 0).

make_length([_, _, _, _, _, _, _, _|Rest], Length) :-
  Length > 7, !,
  Length1 is Length - 8,
  make_length(Rest, Length1).
make_length([_, _, _, _|Rest], Length) :-
  Length > 3, !,
  Length1 is Length - 4,
  make_length(Rest, Length1).
make_length([_, _|Rest], Length) :-
  Length > 1, !,
  Length1 is Length - 2,
  make_length(Rest, Length1).
make_length([X], 1).
make_length([], 0).

lastElement([_, _, _, _, _, _, _, Last], Last) :- !.
lastElement([_, _, _, _, _, _, _, _|Rest], Last) :- !,
  lastElement(Rest, Last).
lastElement([_, _, _, Last], Last) :- !.
lastElement([_, _, _, _|Rest], Last) :- !,
  lastElement(Rest, Last).
lastElement([_, Last], Last) :- !.
lastElement([_, _|Rest], Last) :- !,
  lastElement(Rest, Last).
lastElement([X], [X]).

last([X], X).
last([_|Y], Z) :-
  last(Y, Z).
