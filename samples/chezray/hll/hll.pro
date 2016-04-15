%-*-Prolog-*-  
% hll indented on 7/2/2002 by 'JOLI' 1.0.

% Ray Reeves 2002
% Based on: Naish, Lee, Technical Report 96/2, U. Melbourne
% "Higher Order Logic Programming in Prolog"

:- module(hll).

:- import(list).

:- export([apply/2, apply/3, apply/4, range/3, range/4]).
:- export([map/3, mapI/4, map2/4, map2I/5, converse/4, compose/4]).
:- export([filter/3, filterI/4, foldr/4, foldl/5]). 
:- export([plus/3, minus/3, times/3]).
:- export([mapmat/3, mat2map/4, mapIJmat/3, map2IJmat/4, maptc/4, mapItc/5]).

:- metapredicate(apply(:, *)).
:- metapredicate(apply(:, *, *)).
:- metapredicate(apply(:,*, *, *)).
:- metapredicate(map(:, *, *)).
:- metapredicate(map(:, *, *)).
:- metapredicate(mapI(:, *, *, *)).
:- metapredicate(map2(:, *, *, *)).
:- metapredicate(map2I(:, *, *, *, *)).
:- metapredicate(maptc(:, *)).
:- metapredicate(mapItc(:, *, *)).
:- metapredicate(filter(:, *, *)).
:- metapredicate(filterI(:, *, *, *)).
:- metapredicate(foldr(:, *, *, *)).
:- metapredicate(foldl(:, *, *, *, *)).

cp(X):-
	current_predicate(X).

apply(M:lambda(X, Y), X):- !,
	call(Y).
apply(lambda(X, Y), X):- !,                   % final lambda application
  call(Y).
apply(F, Arg):-                               % /2 final application
  F =.. L,
  append(L, [Arg], LAR),
  FAR =.. LAR, !,
  call(FAR).

apply(F/A, Arg, Result):- !,                  % target arity A supplied
  functor(F, _, FArity),                      % get Functor and Arity
  Arity is FArity + 2,                        % arity of this try
  F =.. L,
  append(L, [Arg, Result], LAR),
  FAR =.. LAR, !,
  A >= Arity,
  (                                           % target exists and conforms
     A =:= Arity ->                           % it's callable now
     call(FAR) ;                              % call may fail

     append(L, [Arg], LA),                    % target not callable
     PA =.. LA,                               % return partial application
     Result = PA/A
  ).
apply(M:F/A, Arg, Result):- !,                % arity A and module M supplied
  functor(F, _, FArity),                      % get Functor and Arity
  Arity is FArity + 2,                        % arity of this try
  F =.. L,
  append(L, [Arg, Result], LAR),
  FAR =.. LAR, !,
  A >= Arity,
  (                                           % target exists and conforms
     A =:= Arity ->                           % it's callable now
     call(M:FAR) ;                            % call may fail

     append(L, [Arg], LA),                    % target not callable
     PA =.. LA,                               % return partial application
     Result = M:PA/A
  ).
apply(Module:F, Arg, Result):-  !,            % arity A not supplied
  functor(F, Functor, FArity),                % get Functor and Arity
  (
     Functor = lambda ->                      % a lambda expression
     applyLambda(FArity, F, Arg, Result) ;

     Arity is FArity + 2,                     % arity of this try
     F =.. L,
     L = [Name|_],                            % target name
     append(L, [Arg, Result], LAR),
     FAR =.. LAR, 
	  (current_module(M) ; M = amzi_system),    
     current_predicate(M:Name/A),             % see if target exists
     A >= Arity,
     (                                        % target exists and conforms
        A =:= Arity ->                        % it's callable now
        call(M:FAR) ;                         % call may fail

        append(L, [Arg], LA),                 % target not callable
        PA =.. LA,                            % return partial application
        Result = M:PA/A
     )
  ).
apply(F, Arg, Result):-                       % target arity A not supplied
  functor(F, Functor, FArity),                % get Functor and Arity
  (
     Functor = lambda ->                      % a lambda expression
     applyLambda(FArity, F, Arg, Result) ;

     Arity is FArity + 2,                     % arity of this try
     F =.. L,
     L = [Name|_],                            % target name
     append(L, [Arg, Result], LAR),
     FAR =.. LAR, 
	  (current_module(M) ; M = amzi_system),   
     current_predicate(M:Name/A),             % see if target exists
     A >= Arity,
     (                                        % target exists and conforms
        A =:= Arity ->                        % it's callable now
        call(FAR) ;                           % call may fail

        append(L, [Arg], LA),                 % target not callable
        PA =.. LA,                            % return partial application
        Result = PA/A
     )
  ).

apply(F, A, B, C):-                           % apply to A first, then B
  apply(F, A, FA),
  apply(FA, B, C).

applyLambda(3, Lambda, A, B):- !,
  arg(1, Lambda, A),                          % instantiate 1st lambda arg
  arg(2, Lambda, B),                          % result
  arg(3, Lambda, Body),                       % get body
  call(Body).
applyLambda(Arity, Lambda, A, B):-
  arg(1, Lambda, A),                          % instantiate 1st lambda arg
  appendArgs(2, Lambda, [lambda], BL),        % append all args except 1st
  B =.. BL.

appendArgs(N, Lambda, In, Out):-
  arg(N, Lambda, ArgN),                       % fails on completion
  append(In, [ArgN], Acc),
  N1 is N + 1,
  appendArgs(N1, Lambda, Acc, Out).
appendArgs(_, _, X, X).

range(From, To, Range):-
  F is From + 1,
  (
     From < To ->
     Range = [From|R],
     range(F, To, R) ;

     From =:= To,
     Range = [From]
  ).

range(From, To, Map, []):-
  From > To, !.
range(From, To, Map, [Y|Range]):-
  F is From + 1,
  copy_term(Map, Map1),
  (Map1 = lambda(From, Y, FXY) -> call(FXY) ;  true),
  range(F, To, Map, Range).

tc(E) --> [].
tc(E) -->
  apply(E),
  tc(E).

% map(f, [e1, e2, ... , en], [f e1, f e2, ... , f en])

map(_, [], []).
map(F, [A|As], [FA|FAs]):-
  copy_term(F, F1),
  apply(F, A, FA),
  map(F1, As, FAs).

mapI(apply(F, X), I, A, FA):- !,
  apply(F, X, FAX),
  mapI(FAX, I, A, FA).
mapI(_, _, Null, []):-
  Null == [], !.
mapI(F, I, [A|As], [FA|FAs]):-
  copy_term(F, F1),
  apply(F, I, A, FA),
  I1 is I + 1,
  mapI(F1, I1, As, FAs).

map2(_, [], _, []).
map2(_, _, [], []).
map2(F, [A|As], [B|Bs], [FAB|FABs]):-
  apply(F, A, B, FAB),
  map2(F, As, Bs, FABs).

map2I(apply(F, X), I, A, B, FAB):- !,
  apply(F, X, FAX),
  map2I(FAX, I, A, B, FAB).
map2I(_, _, [], _, []).
map2I(_, _, _, [], []).
map2I(F, I, [A|As], [B|Bs], [FAB|FABs]):-     % map2I/5
  copy_term(F, F1),
  apply(F, I, A, FA),
  I1 is I + 1,
  apply(FA, B, FAB),
  map2I(F1, I1, As, Bs, FABs).

maptc(F, []) --> [].
maptc(F, [A|As]) -->                          % apply F(A) to diff list
  apply(F, A),
  maptc(F, As).

mapItc(_, _, []) --> [].
mapItc(F, I, [A|As]) -->                      % apply F(I, A) to diff list
  apply(F, I, A),
  {I1 is I + 1},
  mapItc(F, I1, As).

% mapping matrices

mapmat(F, A, M):-
  map(doRow(F)/3, A, M).

doRow(F, RowA, RowM):-
  map(F, RowA, RowM).

map2mat(F, A, B, M):-
  map2(doRow2(F)/4, A, B, M).

doRow2(F, RowA, RowB, RowM):-
  map2(F, RowA, RowB, RowM).

mapIJmat(F, A, M):-
  mapI(doRowIJ(F)/4, 1, A, M).

doRowIJ(F, I, RowA, RowM):-
  mapI(apply(F, I), 1, RowA, RowM).

map2IJmat(F, A, B, M):-
  map2I(doRow2IJ(F)/4, 1, A, B, M).

doRow2IJ(F, I, RowA, RowB, RowM):-
  map2I(apply(F, I), 1, RowA, RowB, RowM).

converse(F, X, Y, FYX):-
  apply(F, Y, X, FYX).

compose(F, G, X, FGX):-
  apply(G, X, GX),
  apply(F, GX, FGX).

filter(_, [], []).
filter(P, [A0|As0], As):-
  (
     apply(P, A0) ->
     As = [A0|As1] ;                          % transfer A0

     As = As1                                 % skip A0
  ),
  filter(P, As0, As1).

filterI(_, _, [], []).
filterI(Filter, I, [A0|As0], As):-
  (                                           % filter with index as parameter
     not not apply(Filter, I) ->              % apply Filter to I
     As = [A0|As1] ;                          % transfer A0

     As = As1                                 % skip A0
  ),
  I1 is I + 1,
  filterI(Filter, I1, As0, As1).

% foldr(op, b, [e1, e2, ... , en], (e1 op (e2 op ( ... (en op b) ))))

foldr(F, B, [], B).
foldr(F, B, [A|As], R):-
  foldr(F, B, As, R1),
  apply(F, A, FA),
  apply(FA, R1, R).

% foldl(op, b, [e1, ... , en1, en], (en op (en1 op ( ... (e1 op b) ))))

foldl(_, _, [], A, A).
foldl(FC, FB, [X|Xs], A0, A):-
  apply(FC, X, A0, A1),
  foldl(FC, FB, Xs, A1, A).

less(A, B):-
  A < B.

eq(A, B):-
  A == B.

gr(A, B):-
  A > B.

plus([A], [B], [C]):- !,
  plus(A, B, C).
plus(A, B, C):-
  (
     number(A) ->
     (number(B) -> C is A + B ;  number(C), B is C - A) ;

     number(B),
     number(C),
     A is C - B
  ).

minus(A, B, C):-
  plus(C, B, A).

times(A, B, C):-
  (
     number(A) ->
     (number(B) -> C is A*B ;  number(C), B is C/A) ;

     number(B),
     number(C),
     A is C/B
  ).

:- end_module(hll).
