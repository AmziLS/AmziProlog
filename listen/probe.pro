% <PRE>
% probe.pro
% 
% Probe is designed to help tune Prolog code.  It
% provides a report of all the predicates called
% sorted by frequency.  It works with interpreted
% code.
% 
% To run the program from the listener:
% 
% ?- load(probe).
% ?- import(probe).    % its a module
% ?- [your list of programs to consult, with main/0 as starting point].
% ?- probe(main), tell('probe.txt'), report, told.
%
% probe.txt then has the sorted report.
%

:- ensure_loaded(list).

:- module(probe).
:- import(list).
:- export([
      probe/1,
      report/0 ]).

probe(M:G) :-
   !,
   amzi_system:get$env(B),
   pr_int(G, M, B).
probe(X) :-
   probe(user:X).
   
report :-
   findall(HITS-(M:F/A), hits(M,F,A,HITS), L),
   keysort(L, LS),
   reverse(LS, SL),
   write_list(SL, '\n').

%----------------------------------------------------------

pr_int((X, Y), M, B) :-
   !,
   pr_int(X, M, B),
   pr_int(Y, M, B).
pr_int((X -> Y;  Z), M, B) :-
   !,
   (pr_int(X, M, B) -> pr_int(Y, M, B);  pr_int(Z, M, B)).
pr_int((X;  Y), M, B) :-
   !,
   (pr_int(X, M, B);  pr_int(Y, M, B)).
pr_int(!, M, B) :-
   !,
   amzi_system:cut$env(B).
pr_int(true, _, _) :-
   !.
pr_int(G, M, _) :-
  (G = Mod : Goal -> true ;  Mod = M, Goal = G),
  amzi_system:is$code(Mod, Goal, CODE, META_DM), !,
  (
     CODE == 1 ->
     hit(Mod, Goal),
     call(Mod : Goal) ;

     amzi_system:get$env(B),
     (
        META_DM == 0 ->
        (Goal = call_nometa(Goal2) -> true ;  Goal2 = Goal) ;

        amzi_system:meta$convert(Mod, Goal, Goal2, META_DM)
     ),
     hit(Mod, Goal2),
     amzi_system:clause$(Mod, Goal2, Body, _, NextMod),
     (true; hit(Mod, Goal2),fail),
     pr_int(Body, NextMod, B)
  ).

hit(M, H) :-
   (H = call_nometa(H2) ->
      functor(H2, F, A)
      ;
      functor(H, F, A)
   ),
   (retract(hits(M,F,A,HITS1)) ->
      true
      ;
      HITS1 = 0 ),
   HITS2 is HITS1 + 1,
   assert(hits(M,F,A,HITS2)),
   !.
   
:- end_module(probe).
