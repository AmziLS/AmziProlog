% Duck World - a demonstrative Amzi! Prolog application
%
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
% data - this is where the dynamic data that defines
%        the state of the game is kept
%
% The module definitions, bracketed by the directives
% :- module(M) and :- end_module(M) are used to define the
% modules and their imports and exports.
%
% The body directives, :- body(M) and :- end_body(M) are
% used to define the code.  This way a module can be
% defined in one file, but implemented in many others.
%
% In the Amzi! implementation, it is permissable to skip
% the body directives and have all the code between the
% the module/end_module pair instead of coding separate
% body/end_body pairs of directives.
%

% This line indicates that the user module will import
% predicates from the 'rules' module.  This allows the
% code in the 'user' module to refer, without module
% qualification, to the predicates exported from 'rules'.

:- import(rules).

% The code in this file is in module 'user'.  Because that
% is the default module, it does not need to be specified.
% The following predicates are all in 'user'.

main :-
	write(` Welcome to Duck World `),nl,
	instructions,
	write(` Go get an egg `),nl,
	go.

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
