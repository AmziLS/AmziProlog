% OOPS2 - A toy production system interpreter.  It uses a forward chaining,
%        data driven, rule based approach for expert system development.
%
% Version 2, the simplest version without LEX, MEA, or conflict sets
%
% author Dennis Merritt
% Copyright (c) Dennis Merritt, 1986

% operator definitions

:-op(800,xfx,==>).          % used to separate LHS and RHS of rule
:-op(500,xfy,:).            % used to separate attributes and values
:-op(810,fx,rule).          % used to define rule
:-op(700,xfy,#).            % used for unification instead of =

main :- welcome, supervisor.

welcome  :-
	nl,nl,
	write($         OOPS - A Toy Production System$),nl,nl,
	write($This is an interpreter for files containing rules coded in the$),nl,
	write($OOPS format.$),nl,nl,
	write($The => prompt accepts three commands:$),nl,nl,
	write($   load. -  prompts for name of rules file$),nl,
	write($            enclose in single quotes$),nl,
	write($   list. -  lists working memory$),nl,
	write($   go.   -  starts the inference$),nl,
	write($   exit. -  does what you'd expect$),nl,nl.

% the supervisor, uses a repeat fail loop to read and process commands
% from the user

supervisor :-
	repeat,
	write('=>'),
	read(X),
%  write(echo1-X),
	doit(X),
%  write(echo2-X),
	X = exit.

doit(X) :- do(X).

% actions to take based on commands

do(exit) :- !.
do(go) :-initialize,go,!.
do(load) :-load,!.
do(list) :- lst,!.       % lists all of working storage
do(list(X)) :- lst(X),!. % lists all which match the pattern
do(_) :- write('invalid command').

% loads the rules (Prolog terms) into the Prolog database

load :-
	write('Enter file name in single quotes (ex. ''room.okb''.): '),
	read(F),
	reconsult(F).            % loads a rule file into interpreter work space

% assert each of the initial conditions into working storage

initialize :-
	initial_data(X),
	assert_list(X).

% working storage is represented by database terms stored
% under the key "fact"

assert_list([]) :- !.
assert_list([H|T]) :-
	assertz( fact(H) ),
	!,assert_list(T).

% the main inference loop, find a rule and try it.  if it fired, say so
% and repeat the process.  if not go back and try the next rule.  when
% no rules succeed, stop the inference

go :-
	call(rule ID: LHS ==> RHS),
	try(LHS,RHS),
	write('Rule fired '),write(ID),nl,
	!,go.
go.

% find the current conflict set.

%conflict_set(CS) :-
%	bagof(rule ID: LHS ==> RHS,
%		[rule ID: LHS ==> RHS, match(LHS)],CS).

% match the LHS against working storage, if it succeeds process the
% actions from the RHS

try(LHS,RHS) :-
	match(LHS),
	process(RHS,LHS),!.

% recursively go through the LHS list, matching conditions against
% working storage

match([]) :- !.
match([N:Prem|Rest]) :-
	!,
	(fact(Prem);
	 test(Prem)),          % a comparison test rather than a fact
	match(Rest).
match([Prem|Rest]) :-
	(fact(Prem);    % condition number not specified
	 test(Prem)),
	match(Rest).

% various tests allowed on the LHS

test(not(X)) :-
	fact(X),
	!,fail.
test(not(X)) :- !.
test(X#Y) :- X=Y,!.
test(X>Y) :- X>Y,!.
test(X>=Y) :- X>=Y,!.
test(X<Y) :- X<Y,!.
test(X=<Y) :- X=<Y,!.
test(X = Y) :- X is Y,!.
test(member(X,Y)) :- member(X,Y),!.

% recursively execute each of the actions in the RHS list

process([],_) :- !.
process([Action|Rest],LHS) :-
	take(Action,LHS),
	!,process(Rest,LHS).

% if its retract, use the reference numbers stored in the Lrefs list,
% otherwise just take the action

take(retract(N),LHS) :-
	(N == all; integer(N)),
	retr(N,LHS),!.
take(A,_) :-take(A),!.

take(retract(X)) :- retract(fact(X)), !.
take(assert(X)) :- asserta(fact(X)),write(adding-X),nl,!.
take(X # Y) :- X=Y,!.
take(X = Y) :- X is Y,!.
take(write(X)) :- write(X),!.
take(nl) :- nl,!.
take(read(X)) :- read(X),!.
take(prompt(X,Y)) :- nl,write(X),read(Y),!.
take(member(X,Y)) :- member(X,Y), !.
take(list(X)) :- lst(X), !.

% logic for retraction

retr(all,LHS) :-retrall(LHS),!.
retr(N,[]) :-write('retract error, no '-N),nl,!.
retr(N,[N:Prem|_]) :- retract(fact(Prem)),!.
retr(N,[_|Rest]) :- !,retr(N,Rest).

retrall([]).
retrall([N:Prem|Rest]) :-
	retract(fact(Prem)),
	!, retrall(Rest).
retrall([Prem|Rest]) :-
	retract(fact(Prem)),
	!, retrall(Rest).
retrall([_|Rest]) :-		% must have been a test
	retrall(Rest).

% list all of the terms in working storage

lst :-
	fact(X),
	write(X),nl,
	fail.
lst :- !.

% lists all of the terms which match the pattern

lst(X) :-
	fact(X),
	write(X),nl,
	fail.
lst(_) :- !.

% utilities

member(X,[X|Y]).
member(X,[Y|Z]) :- member(X,Z).

