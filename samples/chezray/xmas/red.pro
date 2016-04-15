%-*-Prolog-*-  
% red indented on 6/12/1999 by 'JOLI' 1.0.


/* 
 * REDUCE by Ray Reeves 1989. 
 * A general purpose puzzle-solving kernel using the reduction methods
 * introduced by Sanjay Narain.
 */

:- op(650, yfx, =>>).                          % reduce the head only
:- op(650, yfx, >>>).                          % total reduction

[U|V] =>> [U|V].                               % a list reduces to itself
[] =>> [].
perm(L) =>> [] :-
   L =>> [], !.
perm(L) =>> [A|perm(B)] :-
   remove(L) =>> [A|B].
remove([Y, Z|L]) =>> X :-
   Y == Z, !,
   remove([Z|L]) =>> X.                        % skip duplicates
remove(L) =>> [U|V] :-
   L =>> [U|V].
remove(L) =>> [A, U|B] :-
   L =>> [U|V],
   remove(V) =>> [A|B].

iterate([X, []|_]) =>> [].
iterate(X) =>> [T1|iterate(T3)] :-
   T0 =.. X,                                   % list to functor
   functor(T0, GoalName, Arity),               % name and arity
   functor(T1, GoalName, Arity),               % blank copy  
   functor(T2, GoalName, Arity),               % blank copy 
   reduceArgs(1, Arity, T0, T1, T2),
   T2 =.. T3.                                  % functor to list
iterateF(T0, _) =>> [] :-
   arg(1, T0, []).                             % filtered iterator
iterateF(T0, Past) =>> 
[Template|iterateF(Goals, [Template|Past])] :-
   functor(T0, GoalName, Arity),               % N is arity of T0
   functor(Goals, GoalName, Arity),
   arg(1, T0, [Template|Future]),
   arg(1, Goals, Future),
   1 - Past - Template,                        % test
   unifyArgs([Template|Future], 2, Arity, T0, Goals, Past), % do label args
   arg(Arity, Goals, Tail),
   arg(Arity, T0, LastArg),                    % final arg (attributes)
   LastArg =>> [Template|Tail],                % match Head
   Arity - Past - Template.                    % test

% :- mode unifyArgs(?,+,+,?,?,+).

unifyArgs(Arg1, N, Arity, _, _, _) :-
   N >= Arity.                                 % 'label' args only
unifyArgs(Arg1, N, Arity, T0, Goals, Past) :-  
                                           % all args must unify with Template
   N < Arity,                                  % N is arg # being unified
   Arg1 = [Template|Future],                   % head of arg 1 is the template
   arg(N, Template, ATN),                      % template has same arity as T0
   arg(N, Goals, Tail),                       % build future args for iterateF 
   arg(N, T0, Arg),                            % process this arg here
   N1 is N + 1,
   (
      var(ATN) ->
      Arg =>> [Item|Tail],                     % reduced arg is a list
      (
         Item = [FirstItem|_] ->               % if item is a list ...
         FirstItem = Template,            % bind template to first Item and ...
         meld(Arg1, Item);                     % meld with Arg1 else ...

         Template = Item                       % bind template to item
      );

      Tail = Arg
   ),
   N - Past - Template,                        % partially apply filter
   unifyArgs(Arg1, N1, Arity, T0, Goals, Past). % process next arg

L >>> [] :-
   L =>> [].                                   % total reduction
L >>> [U|V] :-
   L =>> [U|Rest],
   Rest >>> V.

reduceArgs(N, Arity, _, T1, _) :-
   N > Arity,
   call(T1).
reduceArgs(N, Arity, T0, T1, T2) :-
   N =< Arity,                                 % =<
   arg(N, T0, Arg),                            % Nth arg of T0
   Arg =>> [Head|Tail],                        % reduce Arg to Head & Tail 
   arg(N, T1, Head),                           % Nth arg of T1
   arg(N, T2, Tail),                           % Nth arg of T2
   N1 is N + 1,
   reduceArgs(N1, Arity, T0, T1, T2).          % reduce the rest

meld(X, []).                                   % unifies list with shorter list
meld([X|Y], [X|Z]) :-
   meld(Y, Z).

solve(Problem, Solution) :-
   iterateF(Problem, []) >>> Solution.

displaySolutions(Solution) :-
   iterate([displaySolution, Solution]) >>> _.

comb(0, L, []).
comb(N, L, [X|L2]) :-
   N > 0,
   choose(L, [X|L1]),
   N1 is N - 1,
   comb(N1, L1, L2).

choose([Y, Y|Z], L) :- !,
   choose([Y|Z], L).
choose(L, L).
choose([_|Y], [X|Z]) :-
   choose(Y, [X|Z]).

insert(X, [], [X]) :- !.
insert(X, [X|Y], [X|Y]) :- !.
insert(X, [A|B], [A|B1]) :-
   X > A, !,
   insert(X, B, B1).
insert(X, L, [X|L]).

fugit(X) :-                    % tempus fugit while finding all solutions of X
   Time1 is cputime,
   (call(X), fail;  true),
   Time2 is cputime,
   Interval is (Time2 - Time1),
   write($Time for all solutions is: $), write(Interval), write($ seconds$), 
   nl.

member(X, [X|_]).
member(X, [_|Y]) :-
   member(X, Y).
