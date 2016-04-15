% Modular Duck World Sample
% A single file version of Duck World.

% The modular version of Duck World has three
% modules.
%
% user - this is the default module, and contains
%        the definition of main/0 which is where
%        compiled, stand-alone programs start.
%
% rules - this is where all the rules that control
%         the game are coded.
%
% data - this is there the dynamic data that defines
%        the state of the game is kept
%
% The module definitions, bracketed by the directives
% :- module(M) and :- end_module(M) are used to define the
% modules and their imports and exports.
%
% The body directives, :- body(M) and :- end_body(M) are
% used to define the code.
%
% In the Amzi! implementation, it is permissable to skip
% the body directives and have all the code between the
% the module/end_module pair instead of coding separate
% body/end_body pairs of directives.
%
% It is not necessary to have the module definitions
% appear before any use of the module, even though
% they appear that way in this sample.
%

% Module definition for the 'rules' module

:- module(rules).
:- export([done/0, do/1, demons/0]).
:- end_module(rules).

% Module definition for the 'data' module.  nextto/2
% is defined in the 'data' body, but loc/2 is dynamically
% asserted/retracted from the other modules.

:- module(data).
:- export([nextto/2, loc/2]).
:- end_module(data).

% At this when reading the file, there is no 'active'
% module, so everything entered here is assumed to go
% in the default module named 'user'.

% This line indicates that the user module will import
% predicates from the 'rules' module.  This allows the
% code in the 'user' module to refer, without module
% qualification, to the predicates exported from 'rules'.

:- import(rules).

% The following predicates are all in 'user'.

main :-
        init_state,
	write($ Welcome to Duck World $),nl,
	instructions,
	write($ Go get an egg $),nl,
	go.

% These statement initialize three facts in the
% 'data' module. By default, assert/1 asserts
% in the current module, in this case, 'user'. So
% the module qualifier data: is used to specify
% exactly where the assertions are to go.
	
init_state :-
	assert(data:loc(egg,pen)),
	assert(data:loc(ducks,pen)),
	assert(data:loc(you,house)).

% Because 'rules' are imported by 'user', this
% predicate can refer directly to done/0, do/1,
% and demons/0 which are defined in 'rules'.

go :- done.
go :-
	write($>> $),
	read(X),
	X \= quit,
	do(X),
	demons,
	!, go.
go :- write($ Quitter $), nl.

instructions :-
	nl,
	write($You start in the house, the ducks and an egg$), nl,
	write($are in the pen.  You have to get the egg$), nl,
	write($without losing any ducks.$), nl,
	nl,
	write($Enter commands at the prompt as Prolog terms$), nl,
	write($ending in period:$), nl,
	write($  goto(X). - where X is a place to go to.$), nl,
	write($  take(X). - where X is a thing to take.$), nl,
	write($  chase(X). - chasing ducks sends them to the pen.$), nl,
	write($  look. - the state of the game.$), nl,
	write($  help. - this information.$), nl,
	write($  quit. - exit the game.$), nl,
	nl.

% Now the body of code for 'rules'.  We can also
% put import statements here, so at this point we
% declare the 'rules' will import 'data'.  This means
% the predicates in 'rules' can refer to the predicates
% in 'data' without qualification.

:- body(rules).
:- import(data).

connect(X,Y) :-
        nextto(X,Y).
connect(X,Y) :-
        nextto(Y,X).

% The demon predicates, waiting for certain situations to occur.

% end condition

done :-
	loc(you, house),
	loc(egg, you),
	write($ Thanks for getting the egg. $), nl.

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
	
:- end_body(rules).

% The data module has just the static data, defining
% the relationships between places, and the dynamic
% loc/2 data which is asserted and retracted from other
% parts of the program.

:- body(data).

nextto(pen, yard).
nextto(yard, house).

:- end_body(data).