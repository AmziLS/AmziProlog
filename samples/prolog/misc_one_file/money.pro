/*  MONEY.PRO -- Send more money puzzle
**
**  Copyright (c) 1993-1995, Amzi! inc.
**  All rights reserved
**
**  A generic solver of word arithmetic puzzles, like the classic:
**  
**     S E N D
**   + M O R E
**   ---------
**   M O N E Y
**
**  The object of this game is to assign a digit value to each letter
**  in SEND and MORE then add those numbers like a normal math problem
**  and get MONEY as the answer.
**
**  This program will solve any such problem, with the constraints that
**  the two numbers to be added have the same number of digits, and the
**  solution has one more digit.  None of the first digits can be 0.  You
**  can modify the program to be more general.
**
**  To run in the listener, type:
**    ?- main.
**  at the Puzzle prompt type in a puzzle in the form:
**		[S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y].
**
**  You can also run the solve/1 predicate directly from the listener.
**    ?- solve([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]).
**
**  Another one to try is [C,R,O,S,S] + [R,O,A,D,S] = [D,A,N,G,E,R].
**
**  As you try other random combinations, looking for interesting puzzles,
**  you'll find the hard part is coming up with one that has a unique
**  solution, as many random attempts turn out to have multiple solutions.
**  For example, this puzzle using our two dogs has many solutions:
**      [K,A,T,O] + [E,L,L,A] = [C,H,A,O,S].
*/

main :-
	write($Puzzle? $),
	read(Puzzle),
	solve(Puzzle),
	write(Puzzle),nl,
	fail.
main :- write($no more$),nl.

% By expressing the problem as three lists of uppercase letters (variables)
% joined by operators, the puzzle is a single prolog term and variables
% with the same name, such as the Es in send more money will be constrained
% to have the same value.

% This relationship of the variables is preserved through unification as
% the lists making up the words are passed down through levels of recursion
% to solve the problem.

% Backtracking to the permutation predicate causes different numbers to be
% assigned to the variables as the program searches for a solution.

% Solve by permuting the values of the letters and applying the constraints.
% if the constraints fail, go back and repermute until an answer is found.
% the Xs are the carry bits.

solve([A|At] + [B|Bt] = [C0,C|Ct]) :-
	permutation([A,B,C0,C], [0,1,2,3,4,5,6,7,8,9], L),
	A \= 0, B \= 0, C0 \= 0,
	(X = 0; X = 1),
	A + B + X =:= 10 * C0 + C,
	solve2(At+Bt=Ct,X,L).

solve2([A] + [B] = [C], X, L) :-
	permutation([A,B,C],L,_),
	(X = 0, A + B =:= C; X = 1, A + B =:= 10 + C).
solve2([A|At] + [B|Bt] = [C|Ct], X, L) :-
	permutation([A,B,C],L,L1),
	(X1 = 0; X1 = 1),
	(X = 0, A + B + X1 =:= C; X = 1, A + B + X1 =:= 10 + C),
	solve2(At + Bt = Ct,X1,L1).

% permutation assigns different values to the variables in the first
% list from the values in the second.  The third list is the list of
% unassigned values.  It works by simply deleting elements from the
% list using delete/3.  Because it is deleting an element which is an
% unbound variable, delete/3 simply deletes the next element and binds
% its value to the variable, thus providing a simple way to assign
% permuted values to a list of variables.  On backtracking, of course,
% delete simply binds the variable to the next element of the list and
% deletes it, thus eventually generating all permutations of a list.

permutation([],L,L).
permutation([A|X],Y,L) :- atomic(A), permutation(X,Y,L).
permutation([A|X],Y,L) :- remove(A,Y,Y1), permutation(X,Y1,L).

remove(A,[A|X],X).
remove(A,[B|X],[B|Y]) :- remove(A,X,Y).


