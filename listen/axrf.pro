% <PRE>
% Amzi! cross reference utility and
% lint checker.

% The cross reference utility can be used to find
% a number of common Prolog programming errors.
% It can check for correct references of predicates
% across modules, and take into account extended
% predicates (LSXs) and compiled libraries.
% 
% It provides a full cross reference of predicates,
% indicating for each predicate both predicates
% called by the given predicate, and predicates
% that call the given predicate. The defining
% modules for the cross-referenced predicates are
% given as well, with notes for built-in and
% extended predicates.
% 
% The cross reference utility checks for:
% 
%   - common Prolog punctuation errors
%   - undeclared discontiguous clauses
%   - orphaned predicates (never used)
%   - undefined predicates (used but not defined)
%   - module imports and exports
%   - extended (LSX) and library predicates

% To use the software from the listener,
% first load axrf.plm and then import it
% so you can access it from the listener.
%
% Call axrf:xref/2
%   arg1 - a list of files to be cross referenced
%   arg2 - the file to contin the xref data
%
% For example, to cross reference amzi_air.pro and
% amzi_air_ui.pro:
%
% ?- load(axrf).
% yes
% ?- import(axrf).
% yes
% ?- axrf:xref(['amzi_air.pro', 'amzi_air_ui.pro'], 'output.txt').
% yes
% 
% Then open output.txt in the editor to see the results.
%
% For complex projects that depend on other modules
% you can set up the correct environment for the
% program and then run axrf.  It is probably easiest
% to create a small test program that gets consulted
% and run in the listener.
%
% For example, here's a test file for a project with
% operator definitions that uses a library and an LSX.
%
% file runaxrf.pro
%
% :- load(axrf).
% :- consult('..\\jigs\\kw_ops.pro').
% :- load(list).
% :- loadlsx('aosutils.lsx').
% :- import(axrf).
% 
% go :- axrf:xref([
%        'author.pro',
%        'convert.pro',
%        'error_check.pro',
%        'find_replace.pro',
%        'utilities.pro',
%        'webls324.pro',
%        'xref.pro'],
%    'author.xrf').
%
% To run it open the listener and
%
% ?- consult(runaxrf).
% ?- go.
%

% To allow running from command line compiled code
% version.

main :-
   command_line(ARGS),
   get_files(ARGS, FILES),
   axrf:xref(FILES, 'xref.txt').

get_files([], []).
get_files([ARG|ARGS], [FILE|FILES]) :-
   atom_codes(FILE, ARG),
   get_files(ARGS, FILES).
   

:- module(axrf).
:- export([xref/1, xref/2, xref/3]).

xref(FILES) :-
   xinit,
   xread(FILES),
   xreport.

xref(FILES, OUTPUT) :-
   xinit,
   xread(FILES),
   tell(OUTPUT),
   xreport,
   told.

xref(FILES, WARNINGS, USES) :-
   xinit,
   xread(FILES),
   get_xlists(WARNINGS, USES).

xinit :-
   abolish(uses_temp/2),
   abolish(uses/2),
   abolish(import_mod/2),
   abolish(import_pred/2),
   abolish(export_pred/2),
   abolish(dynamic_pred/1),
   abolish(used_by/2),
   abolish(open_module/1),
   abolish(warning/1),
   abolish(discontig_pred/1),
   abolish(last_clause/1),
   abolish(file/2),
   abolish(pred_loc/4),
   assert(open_module(user)).

%-----------------------------------
% Read the files
%

xread([]).
xread([FILE|FILES]) :-
   file(FILE, _),
   !,
   xread(FILES).
xread([FILE|FILES]) :-
   xread(FILE),
   !,
   xread(FILES).
xread(FILE) :-
   fopen(H, FILE, r),
   asserta(file(FILE, H)),
   repeat,
   stream_property(H, line_number(LINE)),
   read(H,X),
   stream_property(H, line_number(LINE2)),
   (bad_term(H, X) ->
      true
      ;
      process(FILE, LINE, LINE2, X) ),
   X == end_of_file,
   !,
   fclose(H).

process(FILE, LINE, LINE2,  end_of_file ) :- !.
process(FILE, LINE, LINE2,  (A --> B) ) :-
   expand_term( (A-->B), AB ),
   !,
   process(FILE, LINE, LINE2, AB).
process(FILE, LINE, LINE2,  (:- module(M)) ) :-
   !, set_open_module(M).
process(FILE, LINE, LINE2,  (:- body(M)) ) :-
   !, set_open_module(M).
process(FILE, LINE, LINE2,  (:- end_module(_)) ) :-
   !, set_open_module(user).
process(FILE, LINE, LINE2,  (:- end_body(_)) ) :-
   !, set_open_module(user).
process(FILE, LINE, LINE2,  (:- import(IM)) ) :-
   !,
   open_module(M),
   add_import(M, IM).
process(FILE, LINE, LINE2,  (:- export(IM)) ) :-
   !,
   open_module(M),
   add_export(M, IM).
process(FILE, LINE, LINE2,  (:- dynamic(D)) ) :-
   !,
   open_module(M),
   add_dynamic(M, D).
process(FILE, LINE, LINE2,  (:- discontiguous(D)) ) :-
   !,
   open_module(M),
   add_discontiguous(M, D).
process(FILE, LINE, LINE2,  (:- multifile(D)) ) :-
   !,
   open_module(M),
   add_discontiguous(M, D).
process(FILE, LINE, LINE2,  (:- op(P, A, O)) ) :-
   !,
   call(op(P, A, O)).
process(FILE, LINE, LINE2,  (:- include(F)) ) :-
   !,
   (file(F, _) -> true
   ; xread(F) ).
process(FILE, LINE, LINE2,  (H :- B) ) :-
   !,
   functor(H, F, A),
   open_module(M),
   (pred_loc(M:F/A, _, _, _) -> 
      true 
   ;
      assertz(pred_loc(M:F/A, FILE, LINE, LINE2))
   ),
   get_uses(M:F/A, B),
   set_last_clause(M:F/A).
process(FILE, LINE, LINE2,  H ) :-
   !,
   functor(H, F, A),
   open_module(M),
   (pred_loc(M:F/A, _, _, _) -> 
      true 
   ;
      assertz(pred_loc(M:F/A, FILE, LINE, LINE2))
   ),
   get_uses(M:F/A),
   set_last_clause(M:F/A).

bad_term(H, (A,B) ) :-
   stream_property(H, line_number(N)),
   stream_property(H, file_name(FILE)),
   assert(warning(error, ['File: ', FILE, ' Line: ', N, 'Probable missplaced period before: ', (A,B)])).
bad_term(H, (A;B) ) :-
   stream_property(H, line_number(N)),
   stream_property(H, file_name(FILE)),
   assert(warning(error, ['File: ', FILE, ' Line: ', N, 'Probable missplaced period before: ', (A;B)])).
bad_term(H, ((A,B) :- _) ) :-
   stream_property(H, line_number(N)),
   stream_property(H, file_name(FILE)),
   assert(warning(error, ['File: ', FILE, ' Line: ', N, 'Probable missplaced period before: ', (A,B)])).
bad_term(H, ((A;B) :- _) ) :-
   stream_property(H, line_number(N)),
   stream_property(H, file_name(FILE)),
   assert(warning(error, ['File: ', FILE, ' Line: ', N, 'Probable missplaced period before: ', (A;B)])).
bad_term(H, ((A:-B) :- _) ) :-
   stream_property(H, line_number(N)),
   stream_property(H, file_name(FILE)),
   assert(warning(error, ['File: ', FILE, ' Line: ', N, 'Probable missplaced period around: ', (A:-B)])).
bad_term(H, (A:-B) ) :-
   bad_body(B, BADB),
   stream_property(H, line_number(N)),
   stream_property(H, file_name(FILE)),
   assert(warning(error, ['File: ', FILE, ' Line: ', N, 'Probable missing period before: ', BADB])).

bad_body( (C:-D), (C:-D) ) :- !.
bad_body( ((C:-D) , _), (C:-D) ) :- !.
bad_body( (_ , REST), X) :-
   !, bad_body(REST, X).
   

%-----------------------------------
% Keep track of imports and exports
%

add_import(M, []) :- !.
add_import(M, [IM:F/A|IMs]) :-
   !,
   asserta(import_pred(M, IM:F/A)),
   add_import(M, IMs).
add_import(M, [IM|IMs]) :-
   !,
   asserta(import_mod(M, IM)),
   add_import(M, IMs).
add_import(M, IM:F/A) :-
   !,
   asserta(import_pred(M, IM:F/A)).
add_import(M, IM) :-
   asserta(import_mod(M, IM)).

add_export(M, []) :- !.
add_export(M, [F/A|IMs]) :-
   !,
   asserta(export_pred(M, F/A)),
   add_export(M, IMs).
add_export(M, F/A) :-
   !,
   asserta(export_pred(M, F/A)).

add_dynamic(_, []) :- !.
add_dynamic(M, [P|Z]) :-
   add_dynamic(M, P),
   !, add_dynamic(M, Z).
add_dynamic(M, F/A) :-
   !,
   assert_dynamic(M:F/A).
add_dynamic(M, X) :-
   mod_functor(X, MM, F, A),
   (MM == de$fault -> M3 = M; M3 = MM),
   assert_dynamic(M3:F/A).

assert_dynamic(M:F/A) :-
   dynamic_pred(M:F/A),
   !.
assert_dynamic(M:F/A) :-
   asserta(dynamic_pred(M:F/A)).

add_discontiguous(_, []) :- !.
add_discontiguous(M, [P|Z]) :-
   add_discontiguous(M, P),
   !, add_discontiguous(M, Z).
add_discontiguous(M, F/A) :-
   !,
   assert(discontig_pred(M:F/A)).

%----------------------------------------
% Create uses/2 structures from
% initially read terms
%

get_uses(M:F/A) :-
   discontig_check(M:F/A),
   (uses_temp(M:F/A,_) ->
      true
      ;
      asserta(uses_temp(M:F/A, [])) ).

get_uses(M:F/A, B) :-
   discontig_check(M:F/A),
   (retract(uses_temp(M:F/A, L)) ->
      true
      ;
      L = []),
   add_use(B, L, L2),
   asserta(uses_temp(M:F/A, L2)).

% check if a discontiguous clause.  OK
% if declared to be such, or if last
% uses is the same (asserta).  Warning
% if already read, but not latest.

discontig_check(MFA) :-
   discontig_pred(MFA),
   !.
discontig_check(MFA) :-
   last_clause(MFA),
   !.
discontig_check(MFA) :-
   uses_temp(MFA,_),
   !,
   asserta(warning(error, 
      ['Discontiguous definition of: ', MFA]
      )).
discontig_check(_).

set_last_clause(MFA) :-
   (retract(last_clause(_)); true),
   !,
   assert(last_clause(MFA)).

add_use(X, L, L) :-
   var(X), !.
add_use( (G,Gs), L, L3 ) :-
   add_use(G, L, L2),
   !, add_use(Gs, L2, L3).
add_use( (G;Gs), L, L3 ) :-
   add_use(G, L, L2),
   !, add_use(Gs, L2, L3).
add_use( (G->Gs), L, L3 ) :-
   add_use(G, L, L2),
   !,
   add_use(Gs, L2, L3).
add_use( not(G), L, L2 ) :-
   !, add_use(G, L, L2).
add_use( call(G), L, L2 ) :-
   !, add_use(G, L, L2).
add_use( once(G), L, L2 ) :-
   !, add_use(G, L, L2).
add_use( catch(G,_,R), L, L3) :-
   add_use(G, L, L2),
   add_use(R, L2, L3),
   !.
add_use( ASSERT, L, L3 ) :-
   is_assert(ASSERT, G),
   !,
   (nonvar(G) ->
      mod_functor(G, MG, FG, AG),
      (MG == de$fault -> open_module(MMG); MMG = MG),
      add_dynamic(MMG, G),
      insert(MMG:FG/AG, L, L2)
      ;
      L2 = L ),
   mod_functor(ASSERT, M, F, A),
   insert(M:F/A, L2, L3),
   !.
add_use(retract(G), L, L3) :-
   (nonvar(G) ->
      mod_functor(G, MG, FG, AG),
      insert(MG:FG/AG, L, L2)
      ;
      L2 = L ),
   mod_functor(retract(G), M, F, A),
   insert(M:F/A, L2, L3),
   !.
add_use( IG, L, L ) :-
   is_member(IG, [!, true, fail]),
   !.
add_use( !, L, L ) :- !.
add_use( G, L, L2 ) :-
   mod_functor(G, M, F, A),
   insert(M:F/A, L, L2).
   
is_assert(assert(G), G).
is_assert(asserta(G), G).
is_assert(assertz(G), G).

%------------------------------------
% Resolve modules for used goals
%

resolve_uses :-
   resolve_dynamics,
   uses_temp(M:F/A, L),
   resolve_uses(L, M, L2),
   assertz(uses(M:F/A, L2)),
   fail.
resolve_uses.

resolve_dynamics :-
   dynamic_pred(M:F/A),
   (uses_temp(M:F/A, _) ->
      assert(warning(warning,
         ['Dynamic and static definitions for: ', M:F/A]))
      ;
      asserta(uses_temp(M:F/A, [])) ),
   fail.
resolve_dynamics.

resolve_uses([], _, []).
resolve_uses([de$fault:F/A|Z1], M, [M2:F/A|Z2]) :-
   find_mod(F/A, M, M2),
   !,
   resolve_uses(Z1, M, Z2).
resolve_uses([M1:F/A|Z1], M, [M1:F/A|Z2]) :-
   resolve_uses(Z1, M, Z2).

find_mod(F/A, M, M) :-
   uses_temp(M:F/A,_),
   !.
find_mod(F/A, _, builtin) :-
   current_predicate(amzi_system:F/A),
   !.
find_mod(F/A, _, extended) :-
   current_predicate(user:F/A),
   predicate_property(user:F/A, extended),
   !.
find_mod(F/A, M, M2) :-
   import_mod(M, M2),
   visible(M2:F/A),
   !.
find_mod(F/A, M, M2) :-
   import_pred(M, M2:F/A),
   visible(M2:F/A),
   !.
find_mod(F/A, M, user) :-
   uses_temp(user:F/A, _),
   !.
find_mod(F/A, _, undefined).

visible(M:F/A) :-
   uses_temp(M:F/A,_),
   export_pred(M, F/A),
   !.
visible(M:F/A) :-
   current_predicate(M:F/A),
   predicate_property(M:F/A, exported).
  
%-------------------------------------
% Generate used_by cross reference
%

get_used_by :-
   uses(M:F/A, _),
   findall(Mx:Fx/Ax,
      (uses(Mx:Fx/Ax,L), is_member(M:F/A,L)),
      LL),
   insert_list(LL, [], LLL),
   asserta(used_by(M:F/A, LL)),
   fail.
get_used_by.
   

%------------------------------------
% Analyze and Report
%

get_xlists(WARNINGS, USES) :-
   resolve_uses,
   get_used_by,
   warn_unused,
   warn_undefined,
   findall(warning(L,W), warning(L,W), Ws),
   sort(Ws, WARNINGS),
   convert_uses,
   convert_used_by,
   findall(ref(M:F/A, L1, L2, FILE, LINE, LINE2), (pred_loc(M:F/A, FILE, LINE, LINE2), uses2(M:F/A, L1), used_by2(M:F/A, L2)), L),
   sort(L, USES).

get_pred_loc(WHAT, FILE, LINE, LINE2) :-
   pred_loc(WHAT, FILE, LINE, LINE2), !.
get_pred_loc(_, '', -1, -1).

convert_uses :-
   retract(uses(M:F/A, L1)),
   convert_mfa_list(L1, L2),
   assertz(uses2(M:F/A, L2)),
   fail.
convert_uses.

convert_used_by :-
   retract(used_by(M:F/A, L1)),
   convert_mfa_list(L1, L2),
   assertz(used_by2(M:F/A, L2)),
   fail.
convert_used_by.

convert_mfa_list([], []) :-
   !.
convert_mfa_list([M:F/A | T], [loc(M:F/A, FILE, LINE, LINE2) | T2]) :-
   get_pred_loc(M:F/A, FILE, LINE, LINE2),
   !,
   convert_mfa_list(T, T2).

xreport :-
   resolve_uses,
   get_used_by,
   warn_unused,
   warn_undefined,
   warning_report,
   uses_report.

uses_report :-
   nl, write('----- Predicate Use -----'), nl,
   findall(M:F/A, uses(M:F/A,_), L),
   sort(L, SL),
   uses_report(SL).

   uses_report([]).
   uses_report([M:F/A|Z]) :-
      write(M:F/A), nl,
      uses(M:F/A, L),
      tab(2),
      (L \= [] ->
         write('subgoals:'),
         nl, tab(4),
         write_list(L, '\n    ')
         ;
         write('no subgoals') ),
      nl,
      used_by(M:F/A, L2),
      tab(2),
      (L2 \= [] ->
         write('called from:  '),
         nl, tab(4),
         write_list(L2, '\n    ')
         ;
         write('no callers') ),
      nl,
      uses_report(Z).

warning_report :-
   nl, write('----- Warnings -----'), nl,
   findall(W, warning(_, W), Ws),
   sort(Ws, SWs),
   write_warnings(SWs).
   
write_warnings([]).
write_warnings([W|Ws]) :-
   write_list(W, ''), nl,
   write_warnings(Ws).

%-----------------------------------
% Generate warnings
%

warn_unused :-
   used_by(MFA, []),
   asserta(warning(info, ['Unused predicate: ', MFA])),
   fail.
warn_unused.

warn_undefined :-
   uses(MFA, L),
   warn_undef(L, MFA),
   fail.
warn_undefined.

   warn_undef([],_) :- !.
   warn_undef([undefined:F1/A1|Z], M:F/A) :-
      asserta(warning(warning, 
         ['Undefined: ', F1/A1, ' in module ', M, ' called from ', F/A]
         )),
      !,
      warn_undef(Z, M:F/A).
   warn_undef([_|Z], MFA) :-
      warn_undef(Z, MFA).


%----------------------------------
% Utility predicates
%

set_open_module(X) :-
   retract(open_module(_)),
   assert(open_module(X)).

% used to pick of modname from goals,
% specify as default if none given

mod_functor(M:X, M, F, A) :-
   !, functor(X, F, A).
mod_functor(X, de$fault, F, A) :-
   functor(X, F, A).

insert_list([], L, L).
insert_list([A|X], L, LL) :-
   insert(A, L, L2),
   !, insert_list(X, L2, LL).

insert(A, [A|Z], [A|Z]) :-
   !.
insert(A, [], [A]) :-
   !.
insert(A, [B|Z], [A,B|Z]) :-
   A @< B,
   !.
insert(B, [A|Y], [A|Z]) :-
   insert(B,Y,Z).

write_list([], _).
write_list([X], _) :-
  !, write(X).
write_list([X|Y], Separator) :-
  write(X),
  write(Separator),
  write_list(Y, Separator).

:- end_module(axrf).