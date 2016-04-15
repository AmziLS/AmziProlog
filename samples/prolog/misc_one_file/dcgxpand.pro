%----------------------------------------------------------------------
% DCG Translator
%
% Copyright (c) 1987-1996 Amzi! inc.
% All rights reserved
%
% expand_term/2 - takes a DCG term as the first argument and unifies
%                 the second argument with the equivalent Prolog term.
%
% This is the code that is used internally by Amzi! Prolog to
% interpret DCG statements.  It converts DCG grammar rules into
% Prolog rules using difference lists.
%
% Difference lists are a way of representing items on a list that
% make it more efficient to process the list, in particular for parsing
% applications.  The pair of difference lists represents a list by,
% as the name implies, the difference between the lists.  So, for
% example, the normal list [a,b,c] would be represented as a pair
% of lists: [a,b,c,d,e,f...], [d,e,f...].
%
% So, a request to parse a list of words completely would ask for
% the difference list [word1, word2, word3, ...], [].  That is, get
% all of the words so nothing is left over.  This difference list
% could be fed to predicates that take off the front portion they
% recognize, and return the second difference list unified to what's
% left to be parsed.  This way the difference lists chain from the
% first predicate to the last.
%
% So, using pure difference lists, a Prolog program that parses
% English sentences might have this as the first clause:
%
% sentence(WordList, []) :-
%   noun(WordList, WhatsLeftAfterTheNoun),
%   verb(WhatsLeftAfterTheNoun, WhatsLeftAfterTheVerb),
%   noun(WhatsLeftAfterTheVerb, []).
%
% DCG, basically, is a shorthand syntax that eliminates the chained
% difference lists for parsing predicates.  This predicate,
% expand_term/2, is the one that takes the DCG syntax and puts in
% the chained difference lists.
%
% To test it:
%
% ?- expand_term(( sentence-->noun, verb, noun ), X).
% X = sentence(H1, Hn) :- noun(H1, H2), verb(H2, H3), noun(H3, Hn)
%
% See Clocksin & Mellish, _Programming in Prolog_ for a discussion
% of difference lists and DCG.
%
% This version of expand term was written by Ray Reeves.
%

my_expand_term((P0 --> Q0), (P :- Q)) :-
	left_hand_side(P0, P, S0, S),
	rhs(Q0, Q1, S0, S), 
	flatten$(Q1, Q).

left_hand_side((NT, Ts), P, S0, S) :- !,
	nonvar(NT),
	is_list(Ts),
	tag(NT, P, S0, S1),
	append(Ts, S0, S1).
left_hand_side(NT, P, S0, S) :-
	nonvar(NT),
	tag(NT, P, S0, S).

rhs((X1, X2), P, S0, S) :- !,
	rhs( X1, P1, S0, S1),
	rhs( X2, P2, S1, S),
	and(P1, P2, P).
rhs((X1 -> X2), (P1->P2), S0, S) :- !,
	rhs( X1, P1, S0, S1),
	rhs( X2, P2, S1, S).
rhs((X1;X2), (P1;P2), S0, S) :- !,
	or(X1, P1, S0, S),
	or(X2, P2, S0, S).
rhs({P}, (P), S, S) :- !.
rhs(!, !, S, S) :- !.
rhs([], true, S, S) :- !.
rhs([T|Ts], mydcg$terminal([T|Ts], S0, S), S0, S) :- !.
rhs(T, P, S0, S) :-                      % tag it
	tag(T, P, S0, S).

mydcg$terminal([T], S0, S):-         % if dcg_terminal exists, use it.
	(
	    dcg_terminal([T], S0, S),!;
	    S0 = [T|S]
	),!.
mydcg$terminal(Ts, S0, S):-
	(
	    dcg_terminal(Ts, S0, S),!;
	    append(Ts, S, S0)
	).
	    
or(X, P, S0, S):-
	rhs(X, X1, S1, S),
	or1(X1, P, S0, S1, S).

or1(X, P, S0, S1, S):- 
	S1 == S,!,
	and(S1 = S0, X, P).
or1(P, P, S, S, _).

and(true, P, P) :- !.
and( P, true, P) :- !.
and((X,Xs), Y, (X,P)) :- !,
	and(Xs, Y, P).
and( P, Q, (P, Q)).

tag(X, P, S0, S) :-
	X =.. [F|A],
	append(A, [S0, S], AX),
	P =.. [F|AX].

flatten$(A, A) :- var(A), !.
flatten$((A, B), C) :- !,
	flattenl(A, C, R),
	flatten$( B, R).
flatten$(A, A).

flattenl( A, (A, R), R) :- var(A), !.
flattenl( (A, B), C, R) :- !,
	flattenl(A, C, R1),
	flattenl(B, R1, R).
flattenl( A, (A,R), R).

append([], X, X).
append([H|T], W, [H|Z]) :- append(T, W, Z).



