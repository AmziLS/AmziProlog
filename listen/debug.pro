
%-*-Prolog-*-  
% debug indented on 3/5/2000 by 'JOLI' 1.0.


%-------------------------------------------------------------------
%  debug.pro - Amzi! Prolog Debugger
%
%  Copyright (c) 1992-2002 Amzi! inc. All Rights Reserved.
%
% $Log: debug.pro,v $
% Revision 1.1.1.1  2003/09/11 02:15:11  dennis
% Starting release 7.0
%
% Revision 1.20  2003/03/24 15:49:01  dennis
% getting read for 6.4.0 alpha
%
% Revision 1.19  2003/01/04 03:31:52  dennis
% pretrip commit
%
% Revision 1.18  2002/12/26 21:19:30  dennis
% Mary made both top-loops identical. Removed {sys}command flag from alis and
% set_debug(on). Only one clause of set_debug(on) needed. Removed extraneous
% "type quit to exit". Changed user_more export to /0.
%
% Revision 1.17  2002/07/20 00:57:30  dennis
% debugger bug fixes, and some retract bugs fixed
%
% Revision 1.16  2002/07/16 00:41:09  dennis
% minor changes
%
% Revision 1.15  2002/06/19 17:11:01  dennis
% fixed debugger to not display integer modules, use atom instead
%
% Revision 1.14  2002/06/09 03:07:58  dennis
% added locking
%
% Revision 1.13  2002/06/02 03:50:57  dennis
% all the XStr forms of logic server calls call strterm and grow the
% heap, so made new ExecProve and CallProve that to the strterm inside
% so that the heap can rolled back to before the Exec/Call.  Important
% that this be done in the Prove, so that if heapgc is encountered,
% the new heap is used for the rollback.
%
% Revision 1.12  2002/05/20 04:34:11  dennis
% final changes
%
% Revision 1.11  2002/05/15 16:59:10  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.10  2002/04/25 03:42:23  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.9  2002/03/07 04:37:43  dennis
% updated tutorial and duckworld samples
%
% Revision 1.8  2001/10/02 16:05:22  dennis
% changed streams, cleaner interface to lex, reimplemented
% readers as buffer between the two
%
% Revision 1.7  2001/08/01 20:18:00  ray
% removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
%
% Revision 1.6  2001/04/02 21:50:14  dennis
% got debugger working again
%
% Revision 1.5  2001/03/16 00:29:07  dennis
% compiled metapredicates
%
% Revision 1.4  2001/02/21 04:46:43  dennis
% debugger working, updated documentation for 6.1
%
% Revision 1.3  2001/02/08 22:56:46  dennis
% string bug fixes, modularized compiler and listener
%
% Revision 1.2  2001/02/05 03:11:45  dennis
% Fixed nasty heap GC bug, other minor fixes
%
% Revision 1.1.1.1  2000/12/29 02:18:06  dennis
% moved to a6
%
% Revision 1.6  2000/10/01 16:20:04  dennis
% cleaned up modules, ddb, got defined, abolish and friends working
%
% Revision 1.5  2000/09/28 03:24:40  dennis
% debugger working for command line listener
%
% Revision 1.4  2000/03/28 01:05:18  dennis
% merged Ray's changes with bigdig.  bigdig is at point where
% new Cell class is used, but there are no modules in the system.
%
% Revision 1.2.2.1  2000/02/26 20:56:14  dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now.  Also made member/2
% and friends built-ins as well as the bug predicates.
%
%
% Turning on the debugger causes the engine to use call$d for interpreted
% calls, rather than call$i.  call$d differs from call$i in that it
% reports on its progress as it goes.
%
% The debugger relies on a number of external predicates to display
% the reported information.
%
% The debugger exports various predicates that allow you to control
% debugging behavior.  The main predicate, set_debug, turns debugging
% on and off.  The leash predicates allow you to control leashing,
% and the spy predicates control spypoints.
%
% The externally supplied predicates are:
%
%   bug_message(String) - sends an informational string
%   bug_goal_report(Depth, Port, Goal, Num) - state of a call
%   bug_action(Action) - get the next user requested action
%   bug_noclause(Functor, Arity) - no clauses for Functor/Arity
%
% Metapredicate complications - Metapredicates force calls to 
% go through call$i, which means calls in the debugger code
% to metapredicates, such as retractall, cause endless loops.
% So the debugger must use variations on the calls that are not
% metapredicates.
%-------------------------------------------------------------------

%-------------------------------------------------------------------
% imports and exports
%

:- module(amzi_debugger).
:- import(amzi_listener).

:- export([
     leashport/1,
     leash/1,
     set_debug/1,
     set_creep/1,
     spypoint/1,
     spy/1
   ]).

%------------------------------------------------------------
%  Debugger Entry Point
%

set_debug_wide(on) :-
   set_mode(trace, on),
   set_creep(on),
   leash(all).

set_debug(on) :-
%   dbg_clause(amzi_listener, '{sys}command'), !,    % command line mode
   set_mode(trace, on),
   leash(all),
   set_creep(on),
   top$loopd($\n??- $),
   set_debug(off).
%set_debug(on) :-
%   set_mode(trace, on),
%   set_creep(on),
%   leash(all).
set_debug(off) :-
   set_mode(trace, off),
   set_creep(off).
   
%----------------------------------------------------------
%  Spy Points
%

%% spy(M:A/X) :-
%%    (var(M); var(A); var(X)),
%%    !,
%%    dbg_clause('{sys}spying'(M:A/X)).
spy(M:P/A) :-
%%    spy$(M, P, A, 1),
   !,
   dbg_assertz('{sys}spying'(M:P/A)).
spy(P/A) :-
   !,
   dbg_assertz('{sys}spying'(user:P/A)).
spy(M:P) :-
   !,
   dbg_assertz('{sys}spying'(M:P/_)).
spy(P) :-
   atom(P),
   !,
   dbg_assertz('{sys}spying'(user:P/_)).
spy([]).
spy([H|T]) :-
   spy(H),
   spy(T).
%% spy(A) :-
%%    atom(A),
%%    spy$$(A).

%% spy$$(A) :-
%%    dbg_retractall('{sys}spied$arity'(_)),          % new key
%% 
%% % *** need to rethink this ***

%% %   recorded(A, Term, _),
%%    ((Term = (H :- B)) -> true ;  Term = H),
%%    functor(H, A, Arity),
%%    dbg_not('{sys}spied$arity'(Arity)),     % not spied this arity
%%    dbg_asserta('{sys}spied$arity'(Arity)),
%%    spy(A/Arity),
%%    fail.
%% spy$$(_) :-
%%    dbg_clause('{sys}spied$arity'(_)),
%%    dbg_retractall('{sys}spied$arity'(_)).

nospy(M:P/A) :-
   !,
   dbg_retract('{sys}spying'(M:P/A)).
nospy(P/A) :-
   !,
   dbg_retract('{sys}spying'(user:P/A)).
nospy(M:P) :-
   !,
   dbg_retractall('{sys}spying'(M:P/_)).
nospy(P) :-
   atom(P),
   !,
   dbg_retractall('{sys}spying'(user:P/_)).
%% nospy(A/X) :-
%%    atom(A),
%%    number(X),
%%    spy$(A/X, 0),
%%    dbg_retractall('{sys}spying'(A/X)).
nospy([]).
nospy([H|T]) :-
   nospy(H),
   nospy(T).
%% nospy(A) :-
%%    atom(A),
%%    nospy$$(A).

%% nospy$$(A) :-
%%    dbg_retractall('{sys}spied$arity'(_)),          % new key
   
% *** need to rethink how spy/nospy works ***
%   recorded(A, Term, _),

%%    (Term = (H :- B) -> true ;  Term = H),
%%    functor(H, A, Arity),
%%    dbg_not('{sys}spied$arity'(Arity)),     % not spied this arity
%%    dbg_asserta('{sys}spied$arity'(Arity)),
%%    nospy(A/Arity),
%%    fail.
%% nospy$$(_) :-
%%    dbg_retractall('{sys}spied$arity'(_)).

nospyall :-
   dbg_retractall('{sys}spying'(_)).

spypoint(S) :-
   dbg_clause('{sys}spying'(S)).

%---------------------------------------------------------------------------
%  Leashing
%

leash(X) :-
   var(X), !,
   dbg_clause('{sys}leash'(X)).
leash([]) :-
   !,
   dbg_retractall('{sys}leash'(_)).
leash(all) :-
   !,
   leash(['FAIL', 'REDO', 'CALL', 'EXIT']).
leash(X) :-
   atomic(X),
   (leash$name(X, Xn) ;  Xn = X), !,
   dbg_retractall('{sys}leash'(_)),
   dbg_asserta('{sys}leash'(Xn)).
leash(L) :-
   list(L),
   dbg_retractall('{sys}leash'(_)),
   leash1(L).
   
leash(on, X) :-
   (leash$name(X, Port) ;  Port = X),
   (dbg_clause('{sys}leash'(Port)) ->
      true
      ;
      dbg_assertz('{sys}leash'(Port)) ).
leash(off, X) :-
   (leash$name(X, Port) ;  Port = X),
   (dbg_retract('{sys}leash'(Port)) ;  true).

leash1([]).
leash1([H|T]) :-
   (leash$name(H, Xn) ;  Xn = H), !,
   dbg_assertz('{sys}leash'(Xn)),
   leash1(T).

leashport(X) :-
   dbg_clause('{sys}leash'(X)).

leash$name(call, 'CALL').
leash$name(exit, 'EXIT').
leash$name(redo, 'REDO').
leash$name(fail, 'FAIL').

%---------------------------------------------------------------------
%  Debugger  (Uses lots of stack)
%

% Invocation number and Body goal
rbd$((A, B), M, N, L) :- !,
   rbd$(A, M, N, L),
   rbd$(B, M, N, L).
rbd$((X -> Y ;  Z), M, B, S) :- !,
   (rbd$(X, M, B, S) -> rbd$(Y, M, B, S) ;  rbd$(Z, M, B, S)).
% We should support ->/2 better, but there are problems with
% compiled code, this is a tricky business so this doesn't work
%rbd$((X -> Y), M, B, S) :- !,
%   rbd$(X, M, B, S) -> rbd$(Y, M, B, S).
rbd$((A ;  B), M, N, L) :- !,
   (rbd$(A, M, N, L) ;  rbd$(B, M, N, L)).
rbd$(!, M, N, L) :- !,
   report$('CALL', L, !),
   report$('EXIT', L, !),
   amzi_system:cut$env(N).
rbd$(true, _, _, _) :- !.
rbd$(fail, M, N, L) :- !,
   report$('CALL', L, fail),
   report$('FAIL', L, fail),
   fail.
rbd$(G, M, _, L) :-
   (G = Mod:Goal ->
       true
       ;
       Mod = M,
       Goal = G),
   amzi_system:is$code(Mod,Goal,CODE,META_DM),
%write(exit:is$code(Mod,Goal,CODE,META_DM)),nl,
   !,
   (CODE == 1 ->
       (report$('CALL', L, Mod:Goal); report$('FAIL', L, Mod:Goal), fail),
%write(calling:call(Mod:Goal)),nl,
       call(Mod:Goal),
       (report$('EXIT', L, Mod:Goal); report$('REDO', L, Mod:Goal), fail)
       ;
       amzi_system:get$env(B),
       (META_DM == 0 ->
           (Goal = call_nometa(Goal2) ->
              true
              ;
              Goal2 = Goal)
           ;
           meta$convert(Mod, Goal, Goal2, META_DM) ),
       (report$('CALL', L, Mod:Goal2); report$('FAIL', L, Mod:Goal2), fail),
       ( dbg_clause('{sys}skip') ->
           rud$i(Mod:Goal2, NextMod, B, L),
           exit_redo(L, Mod:Goal2)
           ;
           amzi_system:clause$(Mod, Goal2, Body, ClauseN, NextMod),
           report$('trying', L, Mod:Goal2, ClauseN),
           rud$d(Body, NextMod, B, L),
           exitcl_redo(L, Mod:Goal2, ClauseN)
        )
     ).
rbd$(G, M, _, L) :- 
   no_clause_warning(M, G, L),
   report$('FAIL', L, M:G),
   !, fail.

exit_redo(L, Mod:Goal2) :-
   ( report$('EXIT', L, Mod:Goal2) ;
     report$('REDO', L, Mod:Goal2), fail ).

exitcl_redo(L, Mod:Goal2, ClauseN) :-
   ( report$('EXIT', L, Mod:Goal2, ClauseN) ;
     report$('REDO', L, Mod:Goal2), fail ).

          
%% rbd$(G, CM, _, L) :-
%%    (G = Mod:Goal ->
%%       true
%%       ;
%%       Mod = CM,
%%       Goal = G),
%%    is$code(Mod,Goal),
%%    !,
%%    report$('CALL', L, Mod:Goal),
%%    run$code(L, Mod, Goal).
%% rbd$(G, M, _, L) :-
%%    get$env(N),
%%    rud$(G, M, N, L).

%% rud$(fail, M, N, L) :- !,
%%    report$('CALL', L, fail),
%%    report$('FAIL', L, fail),
%%    fail.                  % so it doesn't cause a careful moan

%% rud$(Goal, M, N, L) :-
%%    get$env(B),     % This is the NEW environment - do not move it !!
%%    (Goal = Mod:Head ->
%%        ThisMod = Mod
%%        ;
%%        ThisMod = M, 
%%        Head = Goal ),
%%    (is$meta(ThisMod, Head, DefMod) ->
%%        meta$convert(ThisMod, Head, Head2, DefMod)
%%        ;
%%        Head2 = Head),
%%   report$('Calling', L, ThisMod:Head2),
%%   bt$debug$assert('{sys}calling'),
%%   amzi_system:clause$(ThisMod, Head2, Body, ClauseNum, NextMod),
%%   call_or_redo(L, NextMod, Head2, ClauseNum),
%%   (
%%   ( dbg_clause('{sys}skip') ->
%%     rud$i(Body, NextMod, B, N, L) ;
%%     rud$d(Body, NextMod, B, N, L) ).
%% rud$(G, M, N, L) :- 
%%   no_clause_warning(M, G, L),
%%   report$('FAIL', L, M:G),
%%   !, fail.

% if skip is set, use call$i for subgoals

rud$i(G, M, B, L) :-
   dbg_retractall('{sys}skip'),
   set_mode(trace, off),
   amzi_system:rb$(G, M, B, [G]),
   set_mode(trace, on).
rud$i(G, M, B, L) :-
   set_mode(trace, on),
   fail.

rud$d(G, M, B, L) :-
   L1 is L + 1,
   rbd$(G, M, B, L1).
rud$d(G, M, B, L) :-
   fail.

no_clause_warning(M, G, L) :-
    (G = MOD:HEAD ->
       true;
       MOD = M, HEAD = G),
   functor(HEAD, F, N),
   functor(Gnaked, F, N),
   dbg_not(MOD,Gnaked),
   (atom(MOD) -> AMOD = MOD; amzi_system:module$index(AMOD, MOD)),
   bug_noclause(AMOD,F,N).
no_clause_warning(_, _, _).

%% bt$debug$assert(X) :- dbg_assertz(X).
%% bt$debug$assert(X) :- dbg_retract(X), fail.

%% call_or_redo(L, M, G, N) :-
%%    dbg_retract('{sys}calling'),
%%    !,
%%    report$('CALL', L, M:G, N).
%% call_or_redo(L, M, G, N) :-
%%    report$('REDO', L, M:G, N).

%-------------------------------------------------------------------
% copy of top$loop from alis.pro, doesn't trace itself,
% listen$ from listen.pro.
%

top$loopd(Promt) :-
   repeat,
   current_user(UserIn, UserOut, _),
   fflush(UserIn),
   fflush(UserOut),
   set_creep(on),
   write(UserOut, Promt), 
   fflush(UserOut),
   set_mode(trace, off),
   catch( (read(UserIn, X), listen$d(X)), E, except$(E)).

listen$d(X) :-
   set_mode(trace, on),
   tag(debug_abort),
   exp$listen$term(X, XE),
   do$(XE).


%-------------------------------------------------------------------
% report if either a spypoint, or creeping
% and get user response if appropriate
%

report$(P, I, G) :-
   report$(P, I, G, 0).

report$(Port, Invocation, Goal, Num) :- 
   (b_creeping ;  b_spypoint(Goal)),
   !,
   ensure_atom_modname(Goal, GoalA),
   %(write(calling: bug_goal_report),nl; write(failing: bug_goal_report), nl,fail),
   %bug_message(`**** hi Mary ****\n`);
   bug_goal_report(Port, Invocation, GoalA, Num),
   %(write(exitting: bug_goal_report),nl; write(redoing: bug_goal_report),nl, fail),
   dbg$response(Port, Invocation, Goal, Num).
report$(_, _, _, _).

ensure_atom_modname(M:FA, AMOD:FA) :-
   integer(M),
   !,
   amzi_system:module$index(AMOD, M).
ensure_atom_modname(Goal, Goal).

dbg$response(Port, Inv, Goal, Num) :-
   leashport(Port), !,
  %(write(calling: bug_action(Resp)),nl ; write(failing:bug_action(Resp)),nl, fail),
   bug_action(Resp),
  %(write(exiting: bug_action(Resp)),nl ; write(redoing:bug_action(Resp)),nl, fail),
   do$dbg$resp(Resp, pig(Port, Inv, Goal, Num)).
dbg$response(_, _, _, _) :-
   bug_action(nl).

%-------------------------------------------------------------------
% process user response
%

do$dbg$resp(0'c, _) :- !,
   set_creep(on).
do$dbg$resp(13, _) :- !,
   set_creep(on).
do$dbg$resp(10, _) :- !,
   set_creep(on).
do$dbg$resp(0'a, _) :-
   set_debug(off),
   cut_tag(debug_abort),
   fail.
do$dbg$resp(0'q, _) :-
   set_debug(off),
   cut_tag(debug_abort),
   fail.
do$dbg$resp(0'h, _) :- !,
   abort(2).
do$dbg$resp(0'n, _) :- !,
   nospyall,
   set_creep(off).
do$dbg$resp(0'l, _) :-
   set_creep(off), !.
do$dbg$resp(0's, pig(P, _, _, _)) :-
   (P = 'CALL' ;  P = 'REDO'),
   dbg_assertz('{sys}skip'),
   set_mode(trace, off), !.
do$dbg$resp(0's, _) :-
   bug_message(`Skip can only be selected at CALL or REDO`), !.
do$dbg$resp(0'f, _) :- !,
   fail.
do$dbg$resp(0'b, pig(P, I, G, N)) :- !,
   set_mode(trace, off),
   bug_message(`\n[Type 'quit.' to return]`),
   (amzi_listener:'{sys}command' -> top$loop($\n???- $) ;  true),
   set_mode(trace, on), !,
   report$(P, I, G, N).
do$dbg$resp(0'd, pig(P, I, G, N)) :- !,
   report$(P, I, G, N).
do$dbg$resp(0'@, pig(P, I, G, N)) :-
   bug_message(`Goal? `),
   read(Y),
   (call(Y) ;  true), !,
   report$(P, I, G, N).
do$dbg$resp(0'?, pig(P, I, G, N)) :-
   bug_message(`EXECUTION CONTROL:`),
   bug_message(`   c  or`),
   bug_message(`   [Enter]  creep to next leashed port`),
   bug_message(`   f        fail this predicate`),
   bug_message(`   l        leap to next spypoint`),
   bug_message(`   n        turn off trace (runs the rest of program)`),
   (
      (P = 'CALL' ;  P = 'REDO') ->
      bug_message(`   s        skip until this predicate exits or fails`) ;

      true
   ),
   bug_message(`SPYPOINTS & LEASHED PORTS:`),
   bug_message(`   @leash.           list leashed ports`),
   bug_message(`   @leash([ports]).  set leashing on ports (port or list)`),
   bug_message(`   @spy.             list spypoints`),
   bug_message(`   @spy(N/A).        add spypoint (name, name/arity or list)`),
   bug_message(`   @nospy(N/A).      remove spypoint (name, name/arity or list)`),
   bug_message(`   @nospyall.        remove all spypoints`),
   bug_message(`OTHER:`),
   bug_message(`   a,q       abort to debug listener`),
   bug_message(`   b         break to a new listener`),
   bug_message(`   d         display the current goal with bindings`),
   bug_message(`   h         halt Prolog and return to DOS`),
   bug_message(`   @<goal>   calls specified goal`),
   bug_message(`   [^break]  resumes trace mode at current predicate\n`),
   report$(P, I, G, N).
do$dbg$resp(S, pig(P, I, G, N)) :-
   string(S),
   string_term(S, T),
   call$i(T),
   report$(P, I, G, N).
do$dbg$resp(_, pig(P, I, G, N)) :-
   bug_message(`Type '?' for help\n`),
   report$(P, I, G, N).

%-------------------------------------------------------------------
% Set creep mode
%

set_creep(X) :-
   (
     dbg_retract('{sys}creep'(_))
     ;
     true), 
   !,
   dbg_asserta('{sys}creep'(X)),
   (X = on ->
      set_skip(off)
      ;
      true).

get_creep(X) :-
   dbg_clause('{sys}creep'(X)).

/* Setting skip on and off is a bit of a rat's nest.  This code should
   all be reworked some time.  In any case, set_skip(off) is now
   set up to be called whenever creep is turned on, but if there is
   no skipping to be turned off then don't turn tracing on.  This
   is the case when top$loop calls set_creep to initialize it.
   */

set_skip(off) :-
   dbg_retract('{sys}skip'),
   set_mode(trace, on), !.
set_skip(_).

% Versions of metapredicates that avoid use of metapredicates.
% This is because metapredicates are forced through the interpreter
% for metapredicate processing, which means the debugger winds
% up debugging itself.

dbg_asserta(X) :- asserta(amzi_debugger, X).  % amzi_system:assert$(amzi_debugger, a, X).

dbg_assertz(X) :- assertz(amzi_debugger, X).  % amzi_system:assert$(amzi_debugger, z, X).

dbg_retract(X) :- retract(amzi_debugger, X).
%   !,
%   repeat,
%   amzi_system:retract$db(amzi_debugger, X, X).

dbg_retractall(X) :- retractall(amzi_debugger, X).
%   dbg_retract(X),
%   fail.
%dbg_retractall(_).

dbg_clause(X) :- clause(amzi_debugger, X, _,_,_).  % amzi_system:clause$(amzi_debugger, X, _, _).

dbg_clause(M,X) :- clause(M, X, _,_,_).  % amzi_system:clause$(M, X, _, _).

dbg_not(X) :- clause(amzi_debugger, X, _,_,_), !, fail.
% amzi_system:clause$(amzi_debugger, X, _, _), !, fail.
dbg_not(_).

dbg_not(M,X) :- clause(M, X, _,_,_), !, fail.
% amzi_system:clause$(M, X, _, _), !, fail.
dbg_not(_,_).


%-------------------------------------------------------------------
% Booleans for testing state
%

b_creeping :-
   get_creep(on), !.

%b_skipping :-
%   dbg_clause('{sys}skip'), !.

b_spypoint(M:G) :-
   functor(G, P, A),
   spypoint(M:P/A).

:- end_body(amzi_debugger).