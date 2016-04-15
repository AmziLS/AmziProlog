
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
do(Domain, (while(Condition), Goals)):-                   % 3
	call(Domain),
	Condition,
	call(Goals),
	not(Condition).
do(Domain, (Goals, while(Condition))):-                   % 4
	nonvar(Goals),
	call(Domain),
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
