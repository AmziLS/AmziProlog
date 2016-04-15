%----------------------------------------------------------------------
%
% debug64.pro -- the compiler shell
%
% Copyright (c) 1992-2002 Amzi! inc. All Rights Reserved.
%
%----------------------------------------------------------------------

% Debug decorated files call debug$call and debug$info,
% which are implemented here.
%

:- body(amzi_system).

:- dynamic(debug$state/1).
:- dynamic(debug$64/0).

%internal_testing :- !.   % comment this out for production
internal_testing :-
   err$exec(execE, 
      `Attempt to run debug version of program in non-debugging environment `,
      debug64/1, debug64/1).

% New Debugger
debug64_loaded.

/*
debug64(on) :-
   (debug$64 ->
      true
      ;
      assert(debug$64) ).
debug64(off) :-
   retractall(debug$64).
*/

% These are called from alib predicates of the same name, but
% without the 64.  If this module isn't loaded, those predicates
% throw an error.

debug64_init :-
%buginit('c:/temp/bug64log.txt'),
   sys$retractall('{sys}debug$dynamic'(_)),
   set_debug$state(step_into),
   set_leash(call),
   set_leash(exit),
   set_leash(redo),
   set_leash(fail),
   set_leash(info),
   assert(debug64$initialized).
debug64_init :-
   err$exec(execE, 
      `Error initializing debugger; invalid license `,
      debug64_init/0, debug64_init/0).

% NOTE - leave the last debug64info clause in because it
% causes debug64$info to remain on the call stack, so the
% call stack report can find it.  In other words,
% it is a hack to get reporting right.  See
% see ControlStack::get_debug_stack() in engine.

debug64$info(Head, File, Line, 'INFO', Vars, I) :-
%   (debug64$initialized -> true; debug64_init),
   cntr_get(99, I),
   debug$report('INFO', Head, File, Line).
debug64$info(Head, File, Line, 'REDO', Vars, I) :-
   cntr_get(99, I),
   debug$report('REDO', Head, File, Line),
   !, fail.
debug64$info(_, _, _, _, _, _).

% NOTE - same note as above, keep last line in for
% call stack reporting.

debug64$call(DGoal, File, Line, Pre, Post, I1, I2) :-
   (DGoal = M:dcg$(G) ->
      Goal = M:G
      ;
      Goal = DGoal),
   pre_debug$call(Goal, File, Line, Pre, I1),
   (Goal == '!' ->
     true
     ;
     catch( Goal,error(Err, Attrs),debug64$error(error(Err, Attrs),Goal,File,Line)) ),
%     call(Goal) ),
   post_debug$call(Goal, File, Line, Post, I2).
debug64$call(_, _, _, _, _, _, _) :-
   fail.

pre_debug$call(Goal, File, Line, 'CALL', I) :-
   cntr_inc(99, I),
   debug$report('CALL', Goal, File, Line).
pre_debug$call(Goal, File, Line, 'FAIL', I) :-
   cntr_dec(99,_),
   cntr_get(99,I),
   debug$report('FAIL', Goal, File, Line),
   fail.

post_debug$call(Goal, File, Line, 'EXIT', I) :-
   cntr_dec(99,_),
   cntr_get(99,I),
   debug$report('EXIT', Goal, File, Line).
post_debug$call(Goal, File, Line, 'REDO', I) :-
   cntr_inc(99, I),
   fail.

% need to add a rethrow that throws the error so programs
% that are catching system errors can be debugged. the
% simple fix below (commented out) gets the debugger all
% confused for some reason.
debug64$error(error(Err,Attrs), Goal,File,Line) :-
   get_debug_stack(Stack),
   debug_stack('CALL', Goal, File, Line, Stack),
   debug_error([error = Err,goal=Goal,file=File,line=Line|Attrs]),
   get$action('CALL', File, Line),
   true.
%mm   throw(debug_abort).
%   throw(error(Err,Attrs)).

   
%--------------------------------------------------------------
% default output predicates, overridden by user predicates
% for use during internal testing
%


debug_clause(H,F,L,V,S) :-
   user_debug_clause(H,F,L,V,S),
   !.
debug_clause(Head, File, Line, Vars, Stack) :-
   internal_testing,
   write('INFO: '), write(Head), tab(2), write(File:Line), nl,
   tab(2), write(Vars), nl,
   output_stack(Stack), nl.

debug_variable(V,X) :-
   write(V=X), nl.

%debug_stack(_,_, listener, _,_) :-
%   !.
debug_stack(P,G,F,L,S) :-
%   (internal_testing -> debug_debug_stack(P,G,F,L,S) ; true),
   user_debug_stack(P,G,F,L,S),
   !.

debug_debug_stack(Port, Goal, File, Line, Stack) :-
%   internal_testing,
   write(Port), tab(1), write(Goal), tab(2), write(File:Line), nl,
   nl, write('Stack'), nl,
   output_stack(Stack),
   nl.

debug_action(A) :-
   user_debug_action(A),
   !.
debug_action(A) :-
   internal_testing,
   write('Action (step(s)/jump(j)/set_break(File,Line))? '),
   read_string(S),
   map_action(S,A).

debug_error(E) :-
   user_debug_error(E),
   !.
debug_error(E) :-
   internal_testing,
   write('*** Error ***'), nl,
   write(E), nl.

%output_stack(S) :-
%   write(S), nl, !.
   
output_stack([]).
output_stack( [goal(Goal,File,Line,Port,I) | Z] ) :-
   nl, tab(2), write(Port:Goal),
   tab(2), write(File:Line),
   !, output_stack(Z).
output_stack( [clause(Head,File,Line,Port,Vars,I,Cut) | Z] ) :-
   nl, tab(2), (Cut == 1 -> write('!'); true),
   write(Head),
   tab(2), write(File:Line),
   nl, tab(4), write(Vars),
   !, output_stack(Z).
output_stack( [X|Z] ) :-
   write(X), nl,
   !, output_stack(Z).

map_action(`s`,step_into).
map_action(`j`,jump).
map_action(S,A) :-
   string_term(S,A).

% debug reporting

%debug$report(_, _, listener, _, _) :-
%   !.
debug$report(Port, Goal, File, Line, Stack) :-
   debug$stop(Port, File, Line),
   debug_stack(Port, Goal, File, Line, Stack),
   get$action(Port, File, Line),
   !.
debug$report(_, _, _, _, _).

%debug$report(_, _, listener, _) :-
%   !.
debug$report(Port, Goal, File, Line) :-
   debug$stop(Port, File, Line),
   get_debug_stack(Stack),
   debug_stack(Port, Goal, File, Line, Stack),
   get$action(Port, File, Line),
   !.
debug$report(_, _, _, _).

% called by debug$report to see if we need to
% stop and report anything or not.

debug$stop(_, _, _) :-
   debug$state(listen_again),
   !,
   fail.
debug$stop(_, _, _) :-
   amzi_system:debug$pause,
   !.
debug$stop(Port, _, _) :-
   debug$state(step_into),
   debug$leash(Port),
   !.
debug$stop(Port, File, Line) :-
   debug$temp$break(File,Line),
   clear_temp_break,
   set_debug$state(step_into),
   debug$leash(Port),
   !.
debug$stop(Port, File, Line) :-
   debug$break(File,Line),
   debug$leash(Port),
   !.

get$action(Port, File, Line) :-
   debug_action(A),
   take$action(A, Port, File, Line),
   !.

% step_over - don't descend
% step_into - into next predicate
% step_return - back to calling predicate
% jump - jump to next breakpoint
% stop - abort
% clear_break - remove breakpoint
% set_break - set a breakpoint

take$action(S,P,F,L) :-
   string(S),
   !,
   string_term(S,T),
   take$action(T,P,F,L).
take$action(stop,_,_,_) :- !, abort(1).
%take$action(stop,_,_,_) :- !, throw(debug_abort).
take$action(step_into,_,_,_) :- !, set_debug$state(step_into).
take$action(step_over,'CALL',F,L) :-
   !,
   set_temp_break(F,L),
   set_debug$state(jump),
   set_debug$state(jump).
take$action(step_over,_,_,_) :-
   !,
   set_debug$state(step_into).
take$action(jump,_,_,_) :- !, set_debug$state(jump).
take$action(set_break(File,Line),P,F,L) :-
   !,
   set_break(File,Line),
   !, get$action(P,F,L).
take$action(clear_break(File,Line),P,F,L) :-
   clear_break(File,Line),
   !,
   get$action(P,F,L).
take$action(set_leash(Port),P,F,L) :-
   set_leash(Port),
   !,
   get$action(P,F,L).
take$action(clear_leash(Port),P,F,L) :-
   clear_leash(Port),
   !,
   get$action(P,F,L).
take$action(cut_display(OnOff),P,F,L) :-
   set_prolog_flag(debug64_cut, OnOff),
   !,
   get$action(P,F,L).
take$action(dcg_display(DD),P,F,L) :-
   set_dcg_display(DD),
   !,
   get$action(P,F,L).
take$action(open_listener,P,F,L) :-
   debug$state(S),
   set_debug$state(listen_again),
   nl,
   write('New listener, type quit to end'),
   nl,
   amzi_listener:listen_again,
   set_debug$state(S),
   !,
   get$action(P,F,L).

set_break(File, Line) :-
   debug$break(File,Line),
   !.
set_break(File, Line) :-
   assert( debug$break(File,Line) ).

clear_break(File, Line) :-
   retractall( debug$break(File,Line) ).

set_leash(P) :-
   port_name(P, Port), 
   ( debug$leash(Port) ->
       true
       ;
       assert( debug$leash(Port) ) ).

clear_leash(P) :-
   port_name(P, Port),
   retractall( debug$leash(Port) ).

port_name(call, 'CALL').
port_name(exit, 'EXIT').
port_name(redo, 'REDO').
port_name(fail, 'FAIL').
port_name(info, 'INFO').

% temporary breaks are used for step_over to mark
% the spot to stop at again.

set_temp_break(File, Line) :-
   clear_temp_break,
   assert( debug$temp$break(File,Line) ).

clear_temp_break :-
   retractall( debug$temp$break(_,_) ).

set_debug$state(X) :-
   debug$state(X), !.
set_debug$state(X) :-
   (retract(debug$state(_)); true),
   !,
   assert(debug$state(X)).

% controls how variables are displayed in DCG
%  all, none, first_last

set_dcg_display(DD) :-
   retractall( dcg$display(_) ),
   assert( dcg$display(DD) ).

%----------------------------------------------------------
% called from bagof at run time to extract the
% non-program variables from debug$call goals.

bind_debug_vars((A,B), V) :- !, combine_debug_vars(A,B,V).
bind_debug_vars((A;B), V) :- !, combine_debug_vars(A,B,V).
bind_debug_vars((A -> B), V) :- !, combine_debug_vars(A,B,V).
bind_debug_vars(call(A), V) :- !, bind_debug_vars(A).
bind_debug_vars(not(A), V) :- !, bind_debug_vars(A).
bind_debug_vars(\+(A), V) :- !, bind_debug_vars(A).
bind_debug_vars(once(A), V) :- !, bind_debug_vars(A).

bind_debug_vars(debug$call(G,X1,X2,X3,X4,X5,X6), [X1,X2,X3,X4,X5,X6]) :- !.
bind_debug_vars(X, []).

combine_debug_vars(A,B,V) :-
   bind_debug_vars(A,Va),
   bind_debug_vars(B,Vb),
   append$(Va,Vb,V),
   !.

debug_varsof(G, V) :-
   remove_debug_calls(G, GC),
   !,
   varsof(GC, V).

remove_debug_calls(debug$call(G,_,_,_,_,_,_), G).
remove_debug_calls(M:debug$call(G,_,_,_,_,_,_), M:G).
remove_debug_calls( (A1,B1), (A2,B2) ) :-
   remove_debug_calls(A1, A2),
   remove_debug_calls(B1, B2).
remove_debug_calls( (A1;B1), (A2;B2) ) :-
   remove_debug_calls(A1, A2),
   remove_debug_calls(B1, B2).
remove_debug_calls( (A1 -> B1; C1), (A2 -> B2; C2) ) :-
   remove_debug_calls(A1, A2),
   remove_debug_calls(B1, B2),
   remove_debug_calls(C1, C2).
remove_debug_calls(A, A).


%--------------------------------------------------------
% Get the stack frame and format nicely
%

get_debug_stack(S) :-
   debug$stack(Raw),
%   output_stack(Raw),
   filter_stack(Raw, S).

filter_stack([G|Gs], S) :-
% debugbugstack('Raw', [G|Gs]),
   get_indent(G, I),
   trim_wiggles([G|Gs], I, S2),
% debugbugstack('Trimmed Wiggles', S2),
   reverse$(S2, [], S3),
   trim_exits(S3, S4),
% debugbugstack('Trimmed Exits', S4),
   trim_goals(S4, S5),
% debugbugstack('Trimmed Goals', S5),
   clean_goals(S5, S6),
% debugbugstack('Cleaned Goals', S),
   fix_dcg(S6, S),
   true.

% When the indent level goes up and back down on the
% stack, that indicates choice points that might be
% reactivated again, but aren't on the current chain
% of execution, so we clear them out.
trim_wiggles([], _, []).
trim_wiggles([X|Z1], I, [X|Z2]) :-
   get_indent(X, II),
   II =< I,
   !, trim_wiggles(Z1, II, Z2).
trim_wiggles([X|Z1], I, Z2) :-
   !, trim_wiggles(Z1, I, Z2).

% When we get to an exit that is not followed by goals
% at its own indent level, then we're at the end of the
% interesting stack.  Huh?
trim_exits([], []).
trim_exits([goal(G,F,L,'EXIT',I), G2|Gs], [goal(G,F,L,'EXIT',I)]) :-
   get_indent(G2, II),
   II > I,
   !.
trim_exits([X|Z1], [X|Z2]) :-
   !, trim_exits(Z1, Z2).

% A clause entry is from debug$info, meaning we are at
% the clause level, not a goal.  The interesting stack
% is a collection of these clauses.  So we clear out
% the goals that have already been satisfied, and move
% the file pointer to the current goal.
%
% So this means if we have:  a :- b, c, d.  and are
% executing c, the stack will show up with a in it, but
% the line pointed to will be where c is.
trim_goals([], []).

/*
trim_goals([clause(_,listener,_,_,_,_,_)|X],
           Z)  :-
   trim_goals(X, Z),
   !.
trim_goals([goal(_,listener,_,_,_)|X],
           Z)  :-
   trim_goals(X, Z),
   !.
*/
   
%trim_goals([clause(H,'bin/listen_debug.txt',L1,Pi,V,I,C), goal(G,'bin/listen_debug.txt',L2,P,I2)|X],
%           [clause(H,'bin/listen_debug.txt',L1,Pi,V,I,C), goal(G,'bin/listen_debug.txt',L2,P,I2)|Z])  :-
%   !, trim_goals(X, Z).
trim_goals([clause(H,F,L,Pi,V,I,C),
            goal(_,_,_,_,_),
            goal(G,F2,L2,P,I2)|X
            ], Z) :-
   !, trim_goals([clause(H,F,L,Pi,V,I,C), goal(G,F2,L2,P,I2)|X], Z).
trim_goals([clause(H,F1,L1,Pi,V,I,C), goal(G,F2,L2,P,I2)],
           [clause(H,F2,L2,Pi,V,I,C), goal(G,F2,L2,P,I2)])  :-
   !.
/*
trim_goals([clause(_,listener,_,_,_,_,_)|X],
           Z)  :-
   trim_goals(X, Z),
   !.
trim_goals([goal(_,listener,_,_,_)|X],
           Z)  :-
   trim_goals(X, Z),
   !.
*/
trim_goals([clause(H,F1,L1,Pi,V,I,C), goal(_,F2,L2,_,_)|X],
           [clause(H,F2,L2,Pi,V,I,C)|Z])  :-
   !, trim_goals(X, Z).
trim_goals([G|X], [G|Z]) :-
   !, trim_goals(X, Z).

% The special goals, like once, not, call, have as arguments
% other debug$call terms, they should just display the actual
% goal being called.
clean_goals([], []).
clean_goals([G1|G1s], [G2|G2s]) :-
   clean_goal(G1, G2),
   !, clean_goals(G1s, G2s).

clean_goal( clause(H,F,L,P,V,I,C), clause(H,F,L,P,V,I,C) ) :- !.
clean_goal( goal(G1,F,L,P,I), goal(G2,F,L,P,I ) ) :- !, clean_goal(G1, G2).
clean_goal( M:G1, M:G2 ) :- !, clean_goal(G1, G2).
clean_goal( call(G1), call(G2) ) :- !, clean_goal(G1, G2).
clean_goal( not(G1), not(G2) ) :- !, clean_goal(G1, G2).
clean_goal( \+(G1), \+(G2) ) :- !, clean_goal(G1, G2).
clean_goal( once(G1), once(G2) ) :- !, clean_goal(G1, G2).
clean_goal( findall(X,G1,Y), findall(X,G2,Y) ) :- !, clean_goal(G1, G2).
clean_goal( bagof(X,G1,Y), bagof(X,G2,Y) ) :- !, clean_goal(G1, G2).
clean_goal( setof(X,G1,Y), setof(X,G2,Y) ) :- !, clean_goal(G1, G2).
clean_goal( catch(G1,X,E1), catch(G2,X,E2) ) :-
   !, clean_goal(G1, G2), clean_goal(E1, E2).
clean_goal( debug$call(G,_,_,_,_,_,_), G ) :- !.
clean_goal( G, G ).

get_indent(goal(_,_,_,_,I), I).
get_indent(clause(_,_,_,_,_,I,_), I).

fix_dcg([], []) :-
   !.
fix_dcg([clause(H,F,L,P,V,I,C)|X],[clause(H2,F,L,P,V2,I,C)|Y]) :-
   member$('$VAR'('__DCG_0') = _, V),
   strip_dcg_args(H, H2),
   (dcg$display(all) ->
      V2 = V
      ;
      fix_dcg_vars(V,V2) ),
   !,
   fix_dcg(X,Y).
fix_dcg([goal(M:dcg$(G),F,L,P,I)|X], [goal(M:G2,F,L,P,I)|Y]) :-
   strip_dcg_args(G,G2),
   !,
   fix_dcg(X,Y).
fix_dcg([A|X], [A|Y]) :-
   fix_dcg(X,Y).

strip_dcg_args(P, P2) :-
   P =.. [Func|ArgsD],
   reverse$(ArgsD, [_,_|Sgra]),
   reverse$(Sgra, Args),
   P2 =.. [Func|Args].

fix_dcg_vars([], []).
fix_dcg_vars(['$VAR'('__DCG_0') = V|X], ['$VAR'('__DCG_0') = V|Y]) :-
   dcg$display(first_last),
   clear_intermediate_vars(X, Y),
   !.
fix_dcg_vars(['$VAR'('__DCG_0') = V|X], Y) :-
   dcg$display(none),
   clear_intermediate_vars(X, Y),
   !.
fix_dcg_vars([V|X], [V|Y]) :-
   fix_dcg_vars(X,Y).

clear_intermediate_vars(['$VAR'('__DCG_N') = V|Z], ['$VAR'('__DCG_N') = V|Z]) :-
   dcg$display(first_last),
   !.
clear_intermediate_vars(['$VAR'('__DCG_N') = V|Z], Z) :-
   dcg$display(none),
   !.
clear_intermediate_vars([_|X], Y) :-
   clear_intermediate_vars(X,Y).

debugbugstack(Note, S) :-
   nl, write(Note), nl,
   dump_stack(S).

dump_stack([]) :- nl.
dump_stack([F|Fs]) :-
   tab(2), write(F), nl,
   !, dump_stack(Fs).

%---------------------------------------------------------------
% Used by listing to display decorated goals and clauses
%

% Uncomment this to see full listings of decorated clauses when debugging reader.
% debug$pp(_) :-
%    !, fail.

debug$pp(M:X) :-
   write('\n'),
   writeq(M), write(':'),
   debug_display(X),
   !.
debug$pp(X) :-
   write('\n'),
   debug_display(X),
   !.
debug$pp(X) :-
   pp$(X).

debug_display( (H :- debug$info(Head,File,Line,_,VarList,_)) ) :-
   unify_vars(VarList),
   writeq(H), write('.'),
   write('   % '), write(File), write(':'), write(Line), write('\n').
debug_display( (H :- debug$info(Head,File,Line,_,VarList,_), Goals) ) :-
   unify_vars(VarList),
   writeq(H), write(' :-'),
   write('   % '), write(File), write(':'), write(Line), write('\n'),
   display_goals(Goals),
   write('.\n').

display_goals( (A,B) ) :-
   display_goal(A),
   write(',\n'),
   !,
   display_goals(B).
display_goals( (A;B) ) :-
   display_goals(A),
   write(';\n'),
   !, display_goals(B).
display_goals( A ) :-
   display_goal(A).

/*
call_nometa(debug$call(!, xyz.pro, 20, H352, H353, H354, H355)) , debug64_cut,
*/

%display_goal(!) :- !.  % diplay the phoney ! in a debug$call
display_goal( (!, _) ) :-
   write('   !'),
   !.
display_goal(call_nometa(X)) :-
   !,
   display_goal(X).
display_goal(debug$call(G,File,Line,_,_,_,_)) :-
   !, display_goal(G).
display_goal(M:debug$call(G,File,Line,_,_,_,_)) :-
   !, display_goal(M:G).
display_goal(M:FindAll) :-
   FindAll =.. [F, Arg1, Arg2, Arg3],
   is_member(F, [findall, setof, bagof]),
   write('   '), writeq(M:F), write('( '),
   writeq(Arg1), write(', '), write('( '),
   display_goals(Arg2), write('), '),
   write(Arg3), write(')').
display_goal(M:Call) :-
   Call =.. [F, Arg],
   is_member(F, [call, not, once, (\+)]),
   write('   '), write(M:F), write('( ( '),
   display_goals(Arg), write(') )').
display_goal(M:catch(Arg1,Arg2,Arg3)) :-
   write('   '), write(M:catch), write('( ( '),
   display_goals(Arg1), write('), '),
   write(Arg2), write(', ( '),
   display_goals(Arg3), write(') )').
display_goal(catch(Arg1,Arg2,Arg3)) :-
   write('   '), write(catch), write('( ( '),
   display_goals(Arg1), write('), '),
   write(Arg2), write(', ( '),
   display_goals(Arg3), write(') )').
display_goal( (_,debug64_cut) ) :-
   write('   '), write('!').
display_goal(G) :-
   write('   '), write(G).


:- end_body(amzi_system).

