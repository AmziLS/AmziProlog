%-*-Prolog-*-  
% listen indented on 3/5/2000 by 'JOLI' 1.0.

%-------------------------------------------------------------------

% listen.pro - the main listener
%
% Copyright (c) 1992-2012 Amzi! inc. All rights reserved
%
%--------------------------------------------------------------------

:- module(amzi_listener).
:- import(amzi_debugger).
:- import(amzi_register).
:- end_module(amzi_listener).

:- body(amzi_listener).
:- export([
      do$/1,
      exp$listen$term/2,
      except$/1,
      go_listen/1,
      eclipse_listen/1,
      leash/0,
      listen_again/1,
      spy/0,
      bug_message/1,
      bug_goal_report/4,
      bug_action/1,
      bug_noclause/3 ]).

%-------------------------------------------------------
% Main listener loop
%

go_listen :-
   greetings,
   write(`\nType 'quit.' to exit\n`),
   top$loop(`\n?- `),
   !.

eclipse_listen :-
%   top$loop$no$catch(`\n?- `).
   top$loop(`\n?- `).

listen_again :-
   top$loop(`\n??- `).
      
% Make sure listener continues to work from current_user
% rather than current_stream, which might be reset by
% a see or tell.

top$loop$no$catch(Prompt) :-
   repeat,
   current_user(UserIn, UserOut, _),
   fflush(UserIn),
   fflush(UserOut),
   set_creep(on),
   write(UserOut, Prompt), 
   fflush(UserOut),
   read(UserIn, X),
   listen$(X).

top$loop(Prompt) :-
   repeat,
   amzi_system:set_debug$state(step_into),
   current_user(UserIn, UserOut, _),
   fflush(UserIn),
   fflush(UserOut),
   set_creep(on),
   write(UserOut, Prompt), 
   fflush(UserOut),
   catch( (
     read(UserIn, X),
     %fflush(UserIn),
     listen$(X) ),
     E, except$(E)).

%except$(E) :-
%   amzi_system:debug64$initialized,
%   !,
%   amzi_system:debug64$error(E).
% Called from take$action in debug64.pro
%except$(debug_abort) :-
   % 1=clean exit (0=reset & continue, 3=break)
%   abort(1).
except$(error(Err, Attrs)) :-
   write(Err), nl,
   member(rc = RC, Attrs),
   member(type = TYPE, Attrs),
   member(message = MSG, Attrs),
   write(RC), nl, write(MSG), nl, 
   member(predicate = PRED, Attrs),
   write($While executing: $), write(PRED), nl, 
   (
      TYPE == read ->
      member(read_buffer = RB, Attrs),
      write($Read buffer: $), write(RB), nl, 
      member(read_file = RF, Attrs),
      (
         RF == $$ ->
         true ;

         write($File: $), write(RF), 
         member(line_number = LN, Attrs),
         write($ Line $), write(LN), nl
      ) ;

      true
   ),
   (exception$debug$stack(RawStack) ->
      amzi_system:filter_stack(RawStack, Stack),
      amzi_system:debug64(on),
      amzi_system:debug$report('FAIL', exception, exception, -1, Stack)
      ;
      true),
   member(callstack = CALLSTACK, Attrs),
   write($Call stack:\n$), write(CALLSTACK), nl, !, 
   what$next(TYPE).
except$(E) :-
   write($Unexpected Catch: $), writeq(E), nl, 
   fail.                        % fail continues the repeat/fail listener loop.

% what$next - disposition after exception, success ends the listener
%  repeat/fail loop, failure continues it.

what$next(abort) :- !,
   write($Aborting listener\n$).
what$next(internal) :- !,
   write($Internal error, aborting listener, contact Amzi! technical support\n$).
what$next(fatal) :- !,
   write($Prolog resource exhausted, stacks and heaps reinitialized.\n$), 
   fail.
what$next(_) :-
   fail.


%-------------------------------------------------------------------
% The main listener entry point
%

listen$(X) :-
   tag(debug_abort),
   exp$listen$term(X, XE),
   do$(XE).

greetings :-
   write(`\n  ****************************************************\n`), 
   write(`\n  Amzi! Prolog Listener `),
   write(`\n  Version: `),
   version(Ver), write(Ver), nl,
   write(`  Copyright (c) 1992-2016 Amzi! inc.\n`),
   write(`  Open Source version licensed under the M.I.T. License.\n`),
   write(`  ****************************************************\n`).

%-------------------------------------------------------------------
% Things to do
%
% these all appear as built-ins to listener, but aren't.
%

do$(X) :-                                  % Integers and variables are no good
   (integer(X) ;  var(X)),
   current_user(_, OH, _),
   write(OH, `\nBAD GOAL : `), write(OH, X), !, 
   fail.
do$(end_of_file) :- !.                       % Ctrl-Z (EOF) or quit gets out
do$(quit) :- !.
do$((A :- B)) :-    % Many ways to add your clauses
   assertz(user:(A :- B)),
   current_user(_, OH, _),
   write(OH, `Term asserted`), !, 
   fail.
do$([H|T]) :- !,
   do$(consult([H|T])).
do$(add) :-
   current_user(_, OH, _),
   write(OH, `Adding typed clauses to dynamic predicates\n`), 
   write(OH, `Type 'quit.' to end\n`), !, 
   do$(consult(user)).
do$(replace) :-
   current_user(_, OH, _),
   write(OH, `Replacing dynamic predicates with those typed\n`), 
   write(OH, `Type 'quit.' to end\n`), !, 
   do$(reconsult(user)).
do$(edit(FileName)) :-                         % Edit and reload
   get_editor(Editor),
   file$name(FileName, pro, FileNameA),
   atom_codes(FileNameA, File),
   append(Editor, " ", Comm),
   append(Comm, File, Command),
   comm(Command),
   reconsult(FileName),
   current_user(_, OH, _),
   write(OH, `Reconsulted `), write(OH, FileName), nl(OH), !, 
   fail.

% Various debugging options

do$(debug) :-                                  % Debug
   set_debug(on), !,
   fail.
do$(nodebug) :-
   set_debug(off), !,
   fail.
do$(trace) :-
   set_mode(trace, on), !,
   fail.
do$(notrace) :-
   set_mode(trace, off), !,
   fail.
do$(spy) :-
   spy, !,
   fail.
do$(spy(S)) :-
   spy(S), !,
   fail.
do$(leash) :-
   leash, !,
   fail.
do$(leash(X)) :-
   amzi_debugger:leash(X), !,
   fail.
do$(debug64(X)) :-
   debug64(X), !,
   fail.

% If none of the above, a normal call

do$(X) :-                               % Prove normal goals
   varlist(VNL), 
   varsof(X, VL),
   current_user(_,OH,_),
%	fugit(X),
   (X = M:G -> true; M = user, G = X),
   (amzi_system:debug64$initialized -> listen$call(M:G); call(M:G)),
   write_vars(VNL, VL, 0, Nvars), 
   (Nvars == 0 ->
      MORE = no,
      nl(OH)
      ;
      tab(OH,1),
      respkey(Ans),
      more$(Ans, MORE) ),
   %   (Ans == 0'; -> MORE = yes; MORE = no) ),
   % (user_more -> MORE = yes; MORE = no) ),
   % (Ans == 0'. ; Ans == 13 ; Ans == 10),
   MORE == no,
   write(OH, `yes`),  % yes we're done, no more
   !, 
   fail.
do$(X) :-
   current_user(_, OH, _),
   write(OH, `no`), !, 
   fail.

more$(0';, yes).
more$(0'., no).
more$(13, no).
more$(10, no).
more$(_, yes).

% so we can call debug$call with the real goals
% added the open file for the dummy listener file to make the eclipse
% debugger happy and have a file to display when it's looking at the
% beginning stack frames with this call.
listen$call(X) :-
   varlist(VNL), 
   varsof(X, VL),
   get$varlist(VNL, VL, Vars),
%   amzi_system:debug_init,
%   open('bin/listen_debug.txt', write, H),
%   write(H, '?- '),
%   write(H, X),
%   write(H, '.\n'),
%   close(H),
%   debug$info(X, 'bin/listen_debug.txt', 1, _, Vars, _),
   debug$info(X, listener, 1, _, Vars, _),
   listen$call2(X).

get$varlist([], [], []).
get$varlist([CharList|CharLists], [Var|Vars], [Name=Var|Pairs]) :-
   atom_codes(Name, CharList),
   !, get$varlist(CharLists, Vars, Pairs).

listen$call2(M:(A,B)) :-
   !,
   listen$call2(M:A),
   listen$call2(M:B).
listen$call2(M:(A;B)) :-
   !,
   (listen$call2(M:A) ;
    listen$call2(M:B)).
listen$call2(M:(A->B;C)) :-
   !,
   (listen$call2(A) ->
      listen$call2(B) ;
      listen$call2(C) ).
%listen$call2(M:G) :-
%   debug$call(M:G, 'bin/listen_debug.txt', 1, _, _, _, _).
listen$call2(M:G) :-
   debug$call(M:G, listener, 1, _, _, _, _).

fugit(X):-                                      % tempus fugit while calling X
	TimeA is cpuclock,                           % an integer
	call(X),
	TimeB is cpuclock, 
	Interval is TimeB - TimeA, 
	write(Interval), write(` msecs`), nl.

%-------------------------------------------------------------------
% Debugging predicates, hooks into debug.pro
%

spy :-
   current_user(_, OH, _),
   write(OH, `\nSpying:`), 
   spypoint(S),
   nl(OH), write(OH, `   `), write(OH, S), 
   fail.
spy.

leash :-
   current_user(_, OH, _),
   write(OH, `\nLeashing: `), 
   leashport(X),
   write(OH, X), write(OH, ` `), 
   fail.
leash :-
   nl.

%-------------------------------------------------------------------
% Debugger and Listener User I/O
%

bug_message(M) :-
	bug_message_wide(M),
	!.
bug_message(M) :-
   write(M), nl.

bug_goal_report(Port, Invoke, Goal, Num) :-
	bug_goal_report_wide(Port, Invoke, Goal, Num),
	!.
bug_goal_report(Port, Invoke, Goal, Num) :-
   Ndent is Invoke mod 10,
   Tens is 10*(Invoke//10),
   (Tens > 0 -> (write(`[`), write(Tens), write(`] `)) ;  write(`[] `)),
   tab(Ndent), write(Port), 
   (Num > 0 -> (write(` (`), write(Num), write(`)`)) ;  tab(1)),
   bug$write(Goal).

bug$write(X) :-
   writeq(X).

bug_action(X) :-
	bug_action_wide(X),
	!.
bug_action(X) :-
   var(X), !,
   write(` ? `), 
   flush_out,
   respkey(X).
bug_action(nl) :- !,
   nl.

bug_noclause(M,F,A) :-
	bug_noclause_wide(M,F,A),
	!.
bug_noclause(M,F,A) :-
   write(`Warning: No clauses for `), write(M:F/A), nl.


%-------------------------------------------------------------------
% Miscellaneous predicates
%

append([], X, X).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).

member(A, [A|_]).
member(A, [_|Z]) :- member(A,Z).

write_vars([], [], N, N) :-
%telling(CurOut), fflush(CurOut), !.
   current_user(_, OH, _),
   fflush(OH), !.
write_vars([[0'*|_]|TN], [HV|TV], Sub, Tot) :- !,
   write_vars(TN, TV, Sub, Tot).
write_vars([HN|TN], [HV|TV], Sub, Tot) :-
   string_list(SHN, HN),
   current_user(_, OH, _),
   nl(OH), write(OH, SHN), write(OH, ` = `), writeq(OH, HV),
   Sub2 is Sub + 1,
   write_vars(TN, TV, Sub2, Tot).
% Force call$i of expand_term so debugger won't pick it up

exp$listen$term(X, XE) :- call(expand_term(X, XE)), !.
exp$listen$term(X, X).

get_editor(ProgL) :-
   amzi_system:'{sys}editor$'(Edit),
   (atom(Edit) -> atom_codes(Edit, EditL) ;  true),
   (list(Edit) -> EditL = Edit ;  true),
   (string(Edit) -> string_list(Edit, EditL) ;  true),
   append(EditL, " ", ProgL).

% Add a default extension to a filename, if necessary

file$name(File, _, File) :-
   file_exists(File), !.
file$name(File, Ext, FileName) :-
   atom(Ext),
   f$name(File, Ext, FileName).
file$name(File, [Ext|_], FileName) :-
   f$name(File, Ext, FileName), !.
file$name(File, [_|Rest], FileName) :-
   file$name(File, Rest, FileName).
file$name(File, _, File).

f$name(FileA, ExtA, FileNameA) :-
   atom_codes(FileA, FileL),
   not(member(0'., FileL)),
   atom_codes(ExtA, ExtL),
   append(FileL, ".", _L),
   append(_L, ExtL, FileNameL),
   atom_codes(FileNameA, FileNameL),
   file_exists(FileNameA), !.

:- end_body(amzi_listener).



