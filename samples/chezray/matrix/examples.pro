%-*-Prolog-*-

:- ensure_loaded(list).
:- import(list).
:- import(hll).
:- import(matrix).

fugit(X) :-                    % tempus fugit while finding all solutions of X
   Time1 is cputime,
   (call(X), fail;  true),
   Time2 is cputime,
   Interval is (Time2 - Time1),
   write(`Time for all solutions is: `), write(Interval), write(` seconds`), 
   nl.

fugit1(X) :-                    % tempus fugit while finding one solution of X
   Time1 is cputime,
   call(X), 
   Time2 is cputime,
   Interval is (Time2 - Time1),
   write(`Time for one solution is: `), write(Interval), write(` seconds`), 
   nl.

example1 :-
% D = [[-4,3,5],[2,-4,-3],[5,-2,-7]],         % -11
%  D = [[-2, 3, 2, -5], [3, -4, -5, 6], [4, -7, -6, 9], [-3, 5, 4, -10]], % -18
	D = [[1, 3, 4, 2], [4, 5, 6, 1], [3, 5, 8, 9], [4, 6, 2, 5]], % -302
	write(`D` = D), nl, nl,
	write(`Determinant by Laplace expansion:`), nl,
  fugit1(detL(D, NL)),
	write(`N` = NL), nl, nl,
	write(`Determinant by Pivotal condensation:`), nl,
  fugit1(detP(D, NP)),
	write(`N` = NP), nl, nl,
	write(`Determinant by Sweep operator:`), nl,
  fugit1(detS(D, NS)),
	write(`N` = NS), nl, nl.

example2:-
  D = [[1, 1, 1], [1, -1, 1], [2, -1, 1]],
  K = [2, -2, -1],
	write(`D` = D), nl,
	write(`K` = K), nl,
	write(`Form : |D  -K|`), nl,
	write(`       |  I  |`), nl,
	write(`Solve by pivotal condensation`), nl,
  solveEqns(D, K, N),                           % [1, 2, -1]
  write(N), nl.

example3:-
	D = [[1, 0, -2],[2, 3, 0],[1, 2, -1]],
	minor(D, 3, 3, M),
	write(`D` = D), write(`   Minor D(3,3)` = M), nl.

example4:-
	D = [[1, 0, -2],[2, 3, 0],[1, 2, -1]],
	minorsD(D, M),
	write(`D` = D), nl,
	write(`Minors of D` = M), nl.


example5:-
	A = [[1, 0, -2],[2, 3, 0],[1, 2, -1]],
	adj(A, AdjA),
	matMult(A, AdjA, K),
	write(`A` = A), nl,
	write(`Adjoint(A)` = AdjA), nl,
	write(`K` = K), nl.

example6:-
	A = [[1, 0, -2],[2, 3, 0],[1, 2, -1]],
	transpose(A, M),
	write(`A` = A), write(`   Transpose(A)` = M), nl.

solveEqns(D, K, X):-                        % solve by pivotal condensation
	erect(K, ColK),
	matKMult(-1, ColK, K1),
	cbind(D, K1, DK),
	DK = [Row1|_],
	unit(Row1, I),                         % make unit matrix conforming to Row1
	rbind(DK, I, DKI),                       % DKI is D augmented by -K and I
	pivCon(DKI, Vector),                          % pivotal condensation
	scale(Vector, X).

erect(List, Column):-
	hll:map(lambda(X, Y, Y = [X]), List, Column).

scale(Vector, NVector):-       % normalize by dividing thru by last element
	last(Vector, NotLast, [Last]),
   map(lambda([X], Y, Y is X/Last), NotLast, NVector).

last([X], [], X).                      
last([X|Xs], [X|Y], Z) :-               % split vector X into NotLast and Last
  last(Xs, Y, Z).

append1([], [X|Xs], [X|Xs]).
append1([], Z, [Z]).                          % allow scalar appendage
append1([A|X], Y, [A|Z]) :-
  append1(X, Y, Z).

unit(Row, I) :-                               % Row defines size of unit matrix
  hll:mapI(unit1(Row), 1, Row,  I).

unit1(Row1, N, _, RowI) :-
  hll:mapI(unit2(N), 1, Row1, RowI).

unit2(N, M, _, Y) :-
  (M =:= N -> Y = 1 ;  Y = 0).








