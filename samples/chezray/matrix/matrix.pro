%-*-Prolog-*-  
% matrix indented on 7/4/2002 by 'JOLI' 1.0.

:- op(700, xfx, mis).
:- op(300, yfx, sub).
:- op(200, yf, t).

:- module(matrix).

:- import(hll).

:- export([t/2, rbind/3, cbind/3, transpose/2, sweep/3, sweep/3, inverse/2]).
:- export([matAdd/3, matSub/3, matMult/3, matKMult/3]).
:- export( [minors/2, minor/4, minorsD/2, adj/2]).
:- export([ijth/4, detL/2, detP/2, detS/2, pivCon/2, abcds/5, abcd/5]).
:- export([normalize/3, rowI/6, elJ/8]).

nthcol(I, A, [X]):-
	nth(I, A, X).

mis(X, A + B):-                               % evaluate expressions
  AA mis A,
  BB mis B,
  matAdd(AA, BB, X).
mis(X, A - B):-                               % evaluate expressions
  AA mis A,
  BB mis B,
  matSub(AA, BB, X).
mis(X, A*B):-                                 % evaluate expressions
  AA mis A,
  BB mis B,
  mis_multiply(AA, BB, X).
mis(X, A sub [*, J]):- !,                     % Jth col
	map(nthcol(J)/3, A, X).	
mis(X, A sub [I, *]):- !,                     % Ith row
	nth(I, A, X).
mis(X, A sub [I, J]):-
	AA mis A,
	ijth(AA, I, J, X).
mis(X, A sub [I]):-                           % Ith elelment of vector
	list(A),
	nth(I, A, AI),
	(AI = [X] ; not list(AI), AI = X).
mis(X, A sub B):- !,
	fail.
mis(X, A t):-
  AA mis A,
  transpose(A, X).
mis(X, X).

mis_multiply(A, B, X):-
  matMult(A, B, X).
mis_multiply(A, B, X):-
  matKMult(A, B, X).
mis_multiply(A, B, X):-
  X is A*B.

is_matrix([[_|_]|_]).

t(A, B):-
  transpose(A, B).

rbind(A, B, C):-                              % row bind, A & B matrices
  A = [A11|_],
  B = [B11|_], !,
  list:length(A11, Cols),
  list:length(B11, Cols),
  list:append(A, B, C).
rbind(A, B, C):-                              % row bind A number
  number(A),
  B = [B1|_], !,
  kVector(A, B1, D),
  list:append([D], B, C).
rbind(A, B, C):-
  number(B),                                  % row bind B number
  A = [A1|_], !,
  kVector(B, A1, D),
  list:append(A, [D], C).

cbind(A, B, C):-                              % col bind, A & B matrices
  list:length(A, Rows),
  list:length(B, Rows), !,
  map2(append, A, B, C).
cbind(A, B, C):-                              % col bind A number
  number(A),
  B = [[_|_]|_], !,
  kVector(A, B, D),
  transpose(D, E),
  map2(append, E, B, C).
cbind(A, B, C):-                              % col bind B number
  number(B),
  A = [[_|_]|_], !,
  kVector(B, A, D),
  transpose(D, E),
  map2(append, A, E, C).

kVector(_, [], []).
kVector(K, [_|As], [K|Rest]):-             % makes a vector of Ks as long as A
  kVector(K, As, Rest).

empty([], []).
empty([_|As], [[]|Rest]):-                % makes a vector of []s as long as A
  empty(As, Rest).

appendRow([], [], []).       % each element of B appended to corr element of A
appendRow(C, [], [C]).
appendRow([], _, []).
appendRow([A|As], [B|Bs], [BA|C]):-
  append(B, [A], BA),
  appendRow(As, Bs, C).

transpose(A, B):-
  A = [[_|_]|_],
  empty(A, Empty),
  maptc(matrix:appendRow/3, A, Empty, B).

sweeps(A, Determinant, B):-
  sweepRange(1, 1, Determinant, A, B).

sweepRange(K, Acc, Det) -->
  sweep(K, Pivot), !,
  {Acc1 is Acc*Pivot},
  {K1 is K + 1},
  sweepRange(K1, Acc1, Det).
sweepRange(_, D, D) --> [].

sweep(K, Pivot, A, Sweep):-          % returns Pivot(K) and single Sweep(K, A)
  nth(K, A, RowAK),                           % row K of A
  nth(K, RowAK, Pivot),                      % pivot is Kth element of Kth row
  mapI(rowI(K, RowAK, Pivot)/6, 1, A, Sweep).

rowI(K, RowAK, Pivot, I, RowAI, RowBI):-      % compute row BI
  mapI(elJ(RowAK, RowAI, Pivot, K, I)/8, 1, RowAI, RowBI).

elJ(RowAK, RowAI, Pivot, K, I, J, AIJ, BIJ):- % compute BIJ
  (
     K = I ->
     (K = J -> BIJ is 1/Pivot ;  BIJ is AIJ/Pivot) ;

     (
        K = J ->
        BIJ is - AIJ/Pivot ;

        nth(J, RowAK, AKJ),
        nth(K, RowAI, AIK),
        BIJ is AIJ - AIK*AKJ/Pivot
     )
  ).

inverse(A, B):-
  sweeps(A, _, B).

matAdd(A, B, C):-
  map2mat(plus/3, A, B, C).

matSub(A, B, C):-
  map2mat(plus/3, C, B, A).

matMult(A, B, C):-
  A = [[_|_]|_],
  B = [[_|_]|_], !,
  transpose(B, Bt),
  map(multRow(Bt)/3, A, C), !.

multRow(RowBt, RowA, RowC):-
  map(inner(RowA), RowBt, RowC).

inner(A, B, C):-
  map2(times, A, B, P),
  foldr(plus, 0, P, C).

matKMult(K, B, C):-
  number(K),
  B = [[_|_]|_], !,
  mapmat(times(K)/3, B, C).

adj(A, AdjA):-
  mapIJmat(matrix:adjAIJ(A)/5, A, AdjA).

adjAIJ(A, I, J, Aij, AdjA):-                  % for each element
  minor(A, J, I, M),
  detL(M, Aji),                               % minor value
  (((I + J) /\ 1) =:= 0 -> AdjA = Aji ;  AdjA is - Aji).

minorsD(D, M):-
  mapI(minorsI(D), 1, D, M).

minorsI(D, I, RowI, M):-                      % for each row
  mapI(minorJ(D, I), 1, RowI, M).

minorJ(D, I, J, Aij, M):-                     % for each element
  minor(D, I, J, M).

minor(D, I, J, M):-
  Map1 = lambda(X, X =\= I),
  Map2 = lambda(Y, Y =\= J),
  filterI(Map1, 1, D, R),                     % filter rows from D index org 1
  map(filterI(Map2, 1), R, M).                % filter cols from R index org 1

minors1(D, Minors):-                          % minors of the 1st row
  list:length(D, Len),                        % number of rows
  range(1, Len, Range),
  map(minor(D, 1)/4, Range, Minors).

ijth(A, I, J, AIJ):-
  Map1 = lambda(X, X =:= I),
  Map2 = lambda(Y, Y =:= J),
  filterI(Map1, 1, A, [RowI]),                % extract row I
  filterI(Map2, 1, RowI, [AIJ]).              % extract element IJ

normalize(Scale, Vector, NVector):-
   map(lambda([X], Y, Y is X/Scale), Vector, NVector).

detL([[A, B], [C, D]], N):-                   % LaPlace
  N is A*D - B*C.
detL(D, N):-                                  % expand D by 1st row
  D = [Row1|Rows],                            % get 1st row
  minors1(D, Minors),                         % minors of the 1st row
  map(detL, Minors, DetMs),                   % determinants of those minors
  map2(times, Row1, DetMs, Terms),            % aij*|Aij|
  foldr(minus, 0, Terms, N), !.             % Sigma, apply cofactor signs here

pivot([Pivot|_], PivCol, PivCol, Pivot):-
  Pivot =\= 0, !.
pivot([A|As], PivColIn, PivColOut, Pivot):-   % pivot must not be 0
  PivCol1 is PivColIn + 1,
  pivot(As, PivRow, PivCol1, PivColOut, Pivot).

detP(Det, N):-
  detP([], Det, N).

detP([_|Pivots], UnNormal, Normal):-
  UnNormal = [[X]|_], !,                      % column vector
  mapI(lambda(N, P, C, C is P**N), 1, Pivots, Cs),
  foldr(times, 1, Cs, Coeff),
  map(normalize(Coeff)/3, [UnNormal], [[Normal]]).
detP(Pivots, Det, N):-
  Det = [Row1|Rows],
  pivot(Row1, 1, PivCol, A),                  % choose Pivot (A)
  Map = lambda(X, X =\= PivCol),
  filterI(Map, 1, Row1, Bs),                  % Bs is row1 without the pivot
  map(nth(PivCol), Det, [_|Cs]),        % Cs is pivot column without the pivot
  minor(Det, 1, PivCol, M),                 % pivot minor M, not always square
  map2(abcds(A, Bs), Cs, M, MM),              % for each row
  detP([A|Pivots], MM, N).

detS(D, N):-
  sweeps(D, N, _).

pivCon(Det, Det):-
  Det = [[_]|_], !.
pivCon(Det, Vector):-                         % pivotal condensatio
  Det = [Row1|Rows],
  pivot(Row1, 1, PivCol, A),             % choose non-zero Pivot(A) from row 1
  Map = lambda(X, X =\= PivCol),
  filterI(Map, 1, Row1, Bs),                  % Bs is row1 without the pivot
  map(nth(PivCol), Det, [_|Cs]),        % Cs is pivot column without the pivot
  minor(Det, 1, PivCol, M),                 % pivot minor M, not always square
  map2(abcds(A, Bs), Cs, M, MM),              % for each row
  pivCon(MM, Vector).                         % Vector length 1 if MM square

abcds(A, Bs, C, Row, N):-
  map2(abcd(A, C), Bs, Row, N).               % for each column

abcd(A, C, B, D, N):-                         % [[A, B], [C, D]]
  N is A*D - B*C.

append([], X, X).
append([H|T], W, [H|Z]):-
   append(T, W, Z).

:- end_module(matrix).
