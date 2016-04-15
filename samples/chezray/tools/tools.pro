%-*-Prolog-*-

:- op(950, xfy, do).
:- op(940, xfy, in).

fugit(X):-                                      % tempus fugit while calling X
	Time1 is cputime,
	(call(X);true),
	Time2 is cputime,
	Interval is (Time2-Time1),
	write(Interval),write($ seconds$),nl.

/* Apply atomic predicate or partially applied predicate to arg list */
apply(AtomicPred, ArgList):-
	atom(AtomicPred), !,
	Goal =.. [AtomicPred|ArgList],
	call(Goal).
apply(PartialPred, ArgList):-
	PartialPred =.. PredList, 
	append(PredList, ArgList, CompleteList),
	Goal =.. CompleteList,
	call(Goal).

reverse(X, Y):-
	var(X), !,
	nonvar(Y),
	reverse_dl(Y, X-[]).
reverse(X, Y):- 
	reverse_dl(X, Y-[]).

reverse_dl([A, B, C, D, E, F, G, H|Rest], Rev):- 
	R2 = [H, G, F, E, D, C, B, A|X],
	dl_append(R1-R2, R2-X, Rev),!,
	reverse_dl(Rest, R1-R2).
reverse_dl(X, Y):- reverse4(X, Y).
reverse_dl(X, Y):- reverse3(X, Y).

reverse4([A, B, C, D|Rest], Rev):- 
	R2 = [D, C, B, A|X],
	dl_append(R1-R2, R2-X, Rev),
	reverse3(Rest, R1-R2).

reverse3([A, B, C], [C, B, A|X]-X). 
reverse3([A, B], [B, A|X]-X). 
reverse3([A], [A|X]-X). 
reverse3([],X-X).

dl_append(A-B, B-C, A-C).	

append(X,Y,Z):-  nonvar(X), nonvar(Y), !, catenate(X, Y, Z).
append(X,Y,Z):- appsplit(X,Y,Z).

appsplit([], X, X).
appsplit([H|T], W, [H|Z]) :- appsplit(T, W, Z).

catenate([A, B, C, D, E, F, G, H|Rest], Tail, [A, B, C, D, E, F, G, H|SoFar]):-
	catenate(Rest, Tail, SoFar), !.
catenate(X,Y,Z):- catenate4(X,Y,Z).

catenate4([A, B, C, D|Rest], Tail, [A, B, C, D|SoFar]):- 
	catenate3(Rest, Tail, SoFar), !.
catenate4(X,Y,Z):- catenate3(X,Y,Z).

catenate3([A, B, C], Tail, [A, B, C|Tail]).
catenate3([A, B], Tail, [A, B|Tail]).
catenate3([A], Tail, [A|Tail]).
catenate3([], Tail, Tail).

length(L,N):- var(N), nonvar(L), !, 
	get_length(L, Length),
	N is Length.
length(L,N):- integer(N), make_length(L, N).

get_length([_, _, _, _, _, _, _, _|Rest], LRest+8):- !,
	get_length(Rest, LRest).
get_length(X,Y):- get_length4(X,Y).
get_length4([_, _, _, _|Rest], LRest+4):- !,
	get_length3(Rest, LRest).
get_length4(X,Y):- get_length3(X,Y).
get_length3([_, _,_], 3):- !.
get_length3([_,_], 2):- !.
get_length3([_], 1):- !.
get_length3([], 0).

make_length([_, _, _, _, _, _, _, _|Rest], Length):- 
	Length > 7, !,
	Length1 is Length-8,
	make_length(Rest, Length1).
make_length([_, _, _, _|Rest], Length):-
	Length > 3, !,
	Length1 is Length-4,
	make_length(Rest, Length1).
make_length([_, _|Rest], Length):- 
	Length > 1, !,
	Length1 is Length-2,
	make_length(Rest, Length1).
make_length([X], 1).
make_length([], 0).

lastElement([_, _, _, _, _, _, _, Last], Last):- !.
lastElement([_, _, _, _, _, _, _, _|Rest], Last):- !,
	lastElement(Rest, Last).
lastElement([_, _, _, Last], Last):- !.
lastElement([_, _, _, _|Rest], Last):- !,
	lastElement(Rest, Last).
lastElement([ _, Last], Last):- !.
lastElement([_, _|Rest], Last):- !,
	lastElement(Rest, Last).
lastElement([X],[X]).

last([X],X).
last([_|Y], Z):- last(Y, Z).
/*
merge_sort(Rel, L, S ):-
	length(L, N),
	merge_sort1(N, Rel, L, S, [] ).

merge_sort1( 0, _, L, [], L ):-!.
merge_sort1( 1, _, [X|L], [X], L ):-!.
merge_sort1( N, Rel, L, S, R ):-	% N >= 2
	N1 is N >> 1,	N2 is N-N1,
	merge_sort1( N1, Rel,  L, S1, R1),	
	merge_sort1( N2, Rel, R1, S2, R),
	merge_2(S2, Rel, S1, S ).

merge_2([], _, S, S ):-!.
merge_2([X|L1], Rel, [Y|L2], [X|L] ):-
%	compare(Rel, X, Y),!,
	X @< Y,
	merge_2(L1, Rel, [Y|L2], L ).
merge_2(L1, Rel, [Y|L2], [Y|L] ):-
	merge_2(L2, Rel, L1, L ).

compare(<, X, Y):- X @< Y. 
compare(>, X, Y):- X @> Y. 
compare(=, X, Y):- X == Y. 
compare(>=, X, Y):- X @>= Y. 
compare(=<, X, Y):- X @=< Y. 
*/

member(X,[X|_]).
member(X,[_|Y]):- member(X,Y).

memberchk(X,[X|_]):- !.
memberchk(X,[_|Y]):- memberchk(X,Y).

memberList(X, [Y|Z]):-
	( Y == X ; memberList(X, Z)).

in(I, iota(X, Y):-  !,
	iota(X, Y, Z),
	in(I, Z).
in(I, X):- member(I, X).

do(while(Condition), Goals):- !,                          % 1
	Condition,
	repeat,
	call(Goals),
	not(Condition), !.
do(Goals, while(Condition)):-                             % 2
	nonvar(Goals),
	repeat,
	call(Goals),
	not(Condition).

iota(Start, Finish, Iota):-
	Finish =< Start,
	iotadown(Start, Finish, Iota).
iota(Start, Finish, Iota):-
	Finish >= Start,
	iotaup(Start, Finish, Iota).

iotaup(F, F, [F]):- !.
iotaup(A, F, [A|Iota1]):-
	A1 is A+1,
	iotaup(A1, F, Iota1).

iotadown(F, F, [F]):- !.
iotadown(A, F, [A|Iota1]):-
	A1 is A-1,
	iotadown(A1, F, Iota1).

flow(X):- put('>'),write(X),nl.
flow(X):- put('<'),write(X),nl,fail.

flow(X, Y, Y):- put('>'),write(X),nl.            % threaded for dcg rules
flow(X, Y, Y):- put('<'),write(X),nl,fail.





