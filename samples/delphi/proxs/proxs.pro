 
/* PROXS - A simple shell for conversational
           expert systems with Prolog rules.

   The system uses Prolog rules for the domain, that
   are all of the form attribute(value) :- ...

   Certain attributes(value) pairs are askable, with either
   a yes/no prompt or a menu of choices.  The predicates
   ask and menuask do the asking, remembering the answers
   so they don't have to ask twice.

   It has its own 'Prolog in Prolog' interpreter, that
   makes it easy to add explanations to the system.  The
   explanations are:

	how - after a goal has been proved, how tells you how.
	why - when a question is being asked, why tells you why.
	whynot - after a goal has been proved, whynot tells
		you whynot some other goal.

   Entry points:

	load_kb(F) - Consults a knowledge base, F.
	goal(X) - Calls goal X.
	clear - Clears known facts.
	solve - Calls topgoal.
	xs_goals(L) - creates list L of valid goals.
	how_goals(L) - creates list L of valid results
		that can be queried with 'how'.
	whynot_goals(L) - creates list L of invalid
		results, that can be queried by 'whynot'.

   The calling program must provide these external predicates:

	px_write(S) - Outputs S to the user interface.
	px_clear - Clears the output display.
	px_ynprompt - Prompt user with proposed Attr:Val pair,
		getting back a yes or no (or why or quit).
	px_menuprompt - Prompt user with proposed Attr:Val pair,
		and a menu of choices.
   */ 

%:-op(900,xfy, :).

% Load a file with rules in the attribute(value) format.

load_kb(F) :-
	reconsult(F).

% Use as a goal the special predicate 'top_goal'

solve :-
	abolish(known/3),
	goal(top_goal(X)).
solve :-
	px_write($No match found$).

% Call our own 'Prolog in Prolog' prove.

goal(X) :-
	px_clear,
	catch( prove(X,[]), EX, ex_proc(EX) ),
	px_write(X).
goal(X) :-
	px_write($No match found$).

	ex_proc(EX) :-
		px_write(EX).

% Make a list of attrbute(value) type goals for menu
% choices in the calling program.

xs_goals(GListS) :-
	get_preds(P),
	build_glist(P, []+GList),
	sort(GList, GListS).

	build_glist([], G+G).
	build_glist([user:F/1|T], Ga+Gs) :-
		G =.. [F, 'X'],
		not ask_clause(G),
		!,
		string_term(StrG, G),
		build_glist(T, [StrG|Ga]+Gs).
	build_glist([_|T], Ga+Gs) :-
		build_glist(T, Ga+Gs).

% Make a list of goals known to be true for a menu
% of choices of 'how' goals in the calling program.

how_goals(HListS) :-
	set_flag(how,on),
	get_preds(P),
	build_hlist(P, []+HList),
	sort(HList, HListS),
	set_flag(how,off).
how_goals(_) :-
	set_flag(how,off).

	build_hlist([], G+G).
	build_hlist([user:F/1|T], Ga+Gs) :-
		G =.. [F, X],
		not ask_clause(G),
		prove(G, []), !,
		string_term(StrG, G),
		build_hlist(T, [StrG|Ga]+Gs).
	build_hlist([_|T], Ga+Gs) :-
		build_hlist(T, Ga+Gs).

% We're only interested in provable goals, so the ones
% with ask or menuask are ignored.

ask_clause(G) :-
	clause(G, Body),
	(functor(Body, ask, _) ; functor(Body, menuask, _)).

% And a menu of choices of 'whynot' goals.

whynot_goals(WNListS) :-
	set_flag(how,on),
	get_preds(P),
	build_wnlist(P, []+WNList),
	sort(WNList, WNListS),
	set_flag(how,off).
whynot_goals(_) :-
	set_flag(how,off).

	build_wnlist([], G+G).
	build_wnlist([user:F/1|T], Ga+Gs) :-
		G =.. [F, X],
		not ask_clause(G),
		findall(G,
			( clause(G,_), nonvar(X), not(prove(G,[])) ),
			L),
		add_wnlist(L, Ga+Gn),
		build_wnlist(T, Gn+Gs).
	build_wnlist([_|T], Ga+Gs) :-
		build_wnlist(T, Ga+Gs).

	add_wnlist([], Gn+Gn).
	add_wnlist([G|T], Ga+Gn) :-
		string_term(StrG, G),
		add_wnlist(T, [StrG|Ga]+Gn).

% ask and menuask use known/3 to remember answers.  clear clears them

clear :-
	abolish(known/3).

% The system needs to know yes or no on an attribute value pair.  ask/3
% checks to see if the question has already been asked, and if not pops
% the question and remembers the answer.
	
ask(Attribute,Value,_) :-
	known(yes,Attribute,Value),     % succeed if we know its true
	!.                              % and dont look any further
ask(Attribute,Value,_) :-
	known(_,Attribute,Value),       % fail if we know its false
	!, fail.

ask(Attribute,_,_) :-
	not multivalued(Attribute),
	known(yes,Attribute,_),         % fail if its some other value.
	!, fail.                        % the cut in clause #1 ensures
					% this is the wrong value
ask(A,V,Hist) :-
	not flag(how,on),
	repeat,
	px_ynprompt(A:V, Y),
	check_ans(Y, Hist),
	!,
	asserta(known(Y,A,V)),          % remember it so we dont ask again.
	Y = yes.                        % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes or no answer.  In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,Value,_,_) :-
	known(yes,Attribute,Value),     % succeed if we know
	!.
menuask(Attribute,_,_,_) :-
	known(yes,Attribute,_),         % fail if its some other value
	!, fail.

menuask(Attribute,AskValue,Menu,Hist) :-
	not flag(how,on),
	repeat,
	px_menuprompt(Attribute, Menu, AnswerValue),
	check_ans(AnswerValue, Hist),
	!,
	asserta(known(yes,Attribute,AnswerValue)),
	AskValue = AnswerValue.         % succeed or fail based on answer

% There are two special case answers.  'why' means display the history list
% up to this point, so we can see the chain of inference.  'quit' means abort
% the proof.

check_ans(why,Hist) :-
	px_clear,
	reverse(Hist, RHist),
	px_write($Trying to prove: $),
	px_write($$),
	write_hist($$, RHist), !, fail.
check_ans(quit,_) :-
	throw(exception(user_abort)).
check_ans(_,_).	

% Prolog in Prolog for explanations.  Prolog can be written in Prolog in
% just a few lines of code, using the clause/2 predicate that finds
% clauses in the dynamic database.  This ability lets you customize the
% Prolog inference for your own purposes.

% In this case there are two modifications.  The first is to recognize two
% special 'built-in' predicates, being ask and menuask.  The second is to
% maintain a second argument which is a history list of goals called so far.
% This history list is passed to ask and menuask, so it can be displayed if
% the user asks why.

% The logic is simple.  If you want to prove a goal, use clause/2 to find a clause
% that has that goal as its head.  Take the body of that clause, which is a comma-
% delimited list of goals, and prove each of its goals.  Normal Prolog recursion
% and backtracking make this search strategy work just like Prolog, of course.

% The syntax is a bit confusing because of the ambiguous use of the comma, both
% to separate arguments and as an infix operator between the goals of
% a clause.

prove(true,_) :- !.
prove((Goal,Rest),Hist) :-
	prov(Goal,[Goal|Hist]),
	prove(Rest,Hist).
prove(Goal,Hist) :-
	prov(Goal,[Goal|Hist]).

prov(true,_) :- !.
prov(menuask(X,Y,Z),Hist) :- menuask(X,Y,Z,Hist), !.
prov(ask(X,Y),Hist) :- ask(X,Y,Hist), !.
prov(Goal,Hist) :-
	clause(Goal,Body),
	prove(Body,Hist).

% Explanations.  The flag(how,on/off) toggle is used to prevent ask
% and menuask from asking during an explanation.  If it hasn't got the
% answer to a question already, it simply fails.

how(Goal) :-
	px_clear,
	clause(Goal,Body),
	px_write(Goal),
	set_flag(how,on),
	prove(Body,[]),
	set_flag(how,off),
	write_body(Body).

whynot(Goal) :-
	px_clear,
	clause(Goal,Body),
	write_line([Goal,$ fails because: $]),
	set_flag(how,on),
	explain(Body),
	set_flag(how,off).
whynot(_) :-
	set_flag(how,off).

explain(true).
explain((Head,Body)) :-
	!,
	check(Head),
	explain(Body).
explain(Clause) :-
	check(Clause).

check(H) :- prove(H, []), write_line([H, $ succeeds$]), !.
check(H) :- write_line([H, $ fails$]), fail.

% display the history list for why, and the body of a clause
% for how.

write_hist(_, [Ask]).
write_hist(Tab, [H|T]) :-
	write_line([Tab, H]),
	strcat(Tab, $  $, NextTab),
	write_hist(NextTab, T).

write_body((First,Rest)) :-
	write_line([$  $, First]),
	write_body(Rest).
write_body(Last) :-
	write_line([$  $, Last]).

write_line(L) :-
	flatten(L,LF),
	write_lin(LF, $$).
	
	write_lin([], Str) :-
		px_write(Str).
	write_lin([H|T], Str1) :-
		string_term(S, H),
		strcat(Str1, S, Str2),
		write_lin(T, Str2).

% Utilities

flatten([],[]) :- !.
flatten([[]|T],T2) :-
	flatten(T,T2), !.
flatten([[X|Y]|T], L) :-
	flatten([X|[Y|T]],L), !.
flatten([H|T],[H|T2]) :-
	flatten(T,T2).

reverse(F, R) :-
	rev(F, []+R).

	rev([], R+R) :- !.
	rev([H|T], Ra+R) :-
		rev(T, [H|Ra]+R). 

set_flag(F,V) :-
	retractall(flag(F,_)),
	assert(flag(F,V)).
