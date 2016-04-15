/* PXMAIN - A Prolog main for running PROXS from
            the listener.

   PROXS is designed to provide services to any calling program.
   This front-end on PROXS is a pure scrolling interface Prolog
   front-end.  It is especially useful during the development
   phase if and when you decide to modify or experiment with
   PROXS.

*/

%:-op(900,xfy, :).

% The main control loop

main :-
	greeting,
	retractall(known(_,_,_)),
	repeat,
	write('> '),
	read_string(StrX),
	string_term(StrX, X),
	do(X),
	X == quit.

greeting :-
	write($This is the Prolog front end for PROXS\n$),
	native_help.

% All the commands

do(help) :- !, once( native_help ).
do(load) :- !, once( load_kb ).
do(solve) :- !, once( solve ).
do(goal(Goal)) :- !, once( goal(Goal) ).
do(how(Goal)) :- !, once( how(Goal) ).
do(whynot(Goal)) :- !, once( whynot(Goal) ).
do(clear) :- !, once( clear ).
do(quit).
do(X) :-
	write(X),
	write(' is not a legal command.'), nl,
	fail.

native_help :-
	write($Type help load solve goal(Goal) how(Goal) whynot(Goal) or quit.\n$),
	write($at the prompt.\n$).

load_kb :-
	write($Enter file name: $),
	read_string(StrF),
	string_atom(StrF, F),
	load_kb(F).

% The external predicates PROXS expects to be defined

px_write(X) :-
	write(X), nl.

px_clear.

px_ynprompt(X, Y) :-
	write(X),
	write($ (yes or no)? $),
	write('> '),
	read_string(StrY),
	string_term(StrY, Y).

px_menuprompt(Prompt, Menu, AnswerValue) :-
	nl, write(Prompt), nl,
	display_menu(Menu),
	write('Enter the number (or item) of choice> '),
	write('> '),
	read_string(StrX),
	string_term(StrX, X),
	pick_menu(X, AnswerValue, Menu).

display_menu(Menu) :-
	disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]) :-            % recursively write the head of
	write(N),write('  : '),write(Item),nl, % the list and disp_menu the tail
	NN is N + 1,
	disp_menu(NN,Rest).

pick_menu(N,Val,Menu) :-
	integer(N),                     % make sure they gave a number
	pic_menu(1,N,Val,Menu), !.      % start at one
pick_menu(Val,Val,_).             % if they didn't enter a number, use
	                                % what they entered as the value

pic_menu(_,_,none_of_the_above,[]).  % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).       % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]) :-
	NextCtr is Ctr + 1,                % try the next one
	pic_menu(NextCtr, N, Val, Rest).
