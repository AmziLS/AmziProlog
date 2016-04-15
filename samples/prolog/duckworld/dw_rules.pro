
% Module definition for the 'rules' module.  The exported
% predicates can be called without qualification from the
% 'user' module.  The 'data' module is imported, so it's
% predicates can be referred to from this module. 

:- module(rules).
:- export([done/0, do/1, demons/0]).
:- import(data).

connect(X,Y) :-
        nextto(X,Y).
connect(X,Y) :-
        nextto(Y,X).

% end condition

done :-
	loc(you, house),
	loc(egg, you),
	write($ Thanks for getting the egg. $), nl.

% The demon predicates, waiting for certain situations to occur.

demons :-
	ducks,
	fox.

ducks :-
	loc(ducks, pen),
	loc(you, pen),
	move(ducks, yard),
	write($ The ducks have run into the yard. $), nl.
ducks.

fox :-
	loc(ducks, yard),
	loc(you, house),
	write($ The fox has taken a duck. $), nl.
fox.

% Even though 'data' is imported, the asserts and retracts
% of clauses in a separate module require the module
% qualification.  This is because asserts and retracts,
% by default, work in the current module, which would be
% 'rules' in this case.

move(Item, Place) :-
	retract( data:loc(Item, _) ),
	assert( data:loc(Item,Place) ).
	
% the commands that the user will type to manipulate the game.

do(goto(X)) :- !, goto(X).
do(chase(X)) :- !, chase(X).
do(take(X)) :- !, take(X).
do(look) :- !, look.
do(help) :- !, instructions.
do(quit) :- !.
do(listing) :- !, listing.
do(report) :- !, report.
do(X) :- write($unknown command$:X), nl, instructions.

goto(X) :-
	loc(you, L),
	connect(L, X),
	move(you, X),
	write($ You are in the $), write(X), nl.
goto(X) :-
	write($ You can't get there from here. $), nl.

chase(ducks) :-
	loc(ducks, L),
	loc(you, L),
	move(ducks, pen),
	write($ The ducks are back in their pen. $), nl.
chase(ducks) :-
	write($ No ducks here. $), nl.

take(X) :-
	loc(you, L),
	loc(X, L),
	move(X, you),
	write($ You now have the $), write(X), nl.
take(X) :-
	write($ There is no $), write(X), write($ here.$), nl.

look :-
	write($You are in the $),
	loc(you, L), write(L), nl,
	look_connect(L),
	look_here(L),
	look_have(you).

look_connect(L) :-
	write($You can go to: $), nl,
	connect(L, CONNECT),
	write($  $), write(CONNECT), nl,
	fail.
look_connect(_).

look_have(X) :-
	write($You have: $), nl,
	loc(THING, X),
	write($  $), write(THING), nl,
	fail.
look_have(_).

look_here(L) :-
	write($You can see: $), nl,
	loc(THING, L),
	THING \= you,
	write($  $), write(THING), nl,
	fail.
look_here(_).

report :-
        findall(X:Y, loc(X,Y), L),
        write(L), nl.
	
:- end_module(rules).
