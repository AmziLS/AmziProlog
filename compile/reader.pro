%------------------------------------------------------------------------
%
% reader.pro -- clause reader, for compiler and consult
%
% Copyright (c) 1992-2009 Amzi! inc. All Rights Reserved.
%
%------------------------------------------------------------------------


% Now set on by default in the engine
%:- set_prolog_flag(preprocessor, on).

%#define M$DEBUGI
%#define M$DEBUG0
%#define M$DEBUG1
%#define M$DEBUG2

#ifdef M$DEBUGI
#define M$DEBUG_INIT internal$debug$init
#define TR(X) c$f(__LINE__, X), X, e$r(__LINE__, X)
#else
#define M$DEBUG_INIT true
#define TR(X) X
#endif

#ifdef M$DEBUG0
#define M$TR0(X) c$f(__LINE__, X), X, e$r(__LINE__, X)
#else
#define M$TR0(X) X
#endif

% Detailed debugging

#ifdef M$DEBUG2
#define M$TR2(X) c$f(__LINE__, X), X, e$r(__LINE__, X)
#else
#define M$TR2(X) X
#endif

% Higher level debugging

#ifdef M$DEBUG1
#define M$TR1(X) c$f(__LINE__, X), X, e$r(__LINE__, X)
#else
#define M$TR1(X) X
#endif

%:- sorted( oitem/4 ).

% Prolog reader in Prolog

:- body(amzi_system).

% used by alib to test if this code is present,
% which is necessary for bagof and free variables

%-------------------------------------------------
% Consult a file, decorating goals as
% debug$call providing file/line information
% and decorating clauses with a debug$info
% goal with file/line and variable information.
%

debug_consult([]) :- !.
debug_consult([File|Files]) :-
   debug_consult(File),
   !, debug_consult(Files).
debug_consult(File) :-
   file$name(File, pro, FName),
   read_file(FName, Lines),
   parse_file(debug_consult, FName, Lines).

debug_compile_read(File) :-
   file$name(File, pro, FName),
   read_file(FName, Lines),
   parse_file(debug_compile, FName, Lines),
   amzi_compiler:add_clause(end_of_file).

ot(File) :-
   catch( 
      (
      file$name(File, pro, FName),
      read_file(FName, Lines),
      parse_file(outline, FName, Lines)
      ),
      error(Error, Attrs), 
      (chdir(CurDir),
       throw(ide_error(Error, Attrs)) )
      ),
   write('******** outline *********'), nl,
   listing(user:item),
   nl,
   write('******** errors *********'), nl,
   process_errors(_),
   nl,
   write('******** done **********'), nl,
   findall(item(L,F,A,S), user:item(L,F,A,S), List),
   write(List), nl,
   write('******** really done *********'), nl.
   

outline_consult(Dir, File, List) :-
   % We have to set the current directory
   curdir(CurDir),
   asserta(outline_current_directory(CurDir)),
   chdir(Dir),
   
   catch( 
      (
      file$name(File, pro, FName),
      read_file(FName, Lines),
      parse_file(outline, FName, Lines)
      ),
      error(Error, Attrs), 
      (chdir(CurDir),
       throw(ide_error(Error, Attrs)) )
      ),
      
   ( setof(item(L, F, A, S), user:item(L, F, A, S), List);
     List = [] ),

%    findall(item(L,F,A,S), user:item(L,F,A,S), List),
%    findall(item(L,F,A,S), oitem(F,A,L,S), List),
     !,
   
   % Put the system back, in case Java/Eclipse cares
   chdir(CurDir).
outline_consult(_, _, _) :-
   outline_current_directory(CurDir),
   chdir(CurDir).

%----------------------------------
% output and debugging tools
%

internal$debug$init :-
   cntr_set(1,0),
   nl,
   write('------------------ Next Clause ------------------------------'),
   nl.

c$f(LINE, X, file(F,T,L), file(F,T,L)) :-
   cntr_inc(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*CALL*':X),
   nl,
   (list(T) ->
      current_line_number(N, file(F,T,L), file(F,T,L)),
      string_list(S,T),
      write(N:S),
      next_line(NL, file(F,T,L)),
      write(NL)
      ;
      next_line(NL, file(F,T,L)),
      write(NL),
      next_next_line(NNL, file(F,T,L)),
      write(NNL) ).
c$f(LINE, X, file(F,T,L), file(F,T,L)) :-
   cntr_dec(1,_),
   cntr_get(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*FAIL*':X), nl,
   !, fail.

c$f(LINE, X) :-
   cntr_inc(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*CALL*':X), nl.
c$f(LINE, X) :-
   cntr_dec(1,_),
   cntr_get(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*FAIL*':X), nl,
   !, fail.

e$r(LINE, X, file(F,T,L), file(F,T,L)) :-
   cntr_dec(1,_),
   cntr_get(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*EXIT*':X),
   nl,
   (list(T) ->
      current_line_number(N, file(F,T,L), file(F,T,L)),
      string_list(S,T),
      write(N:S),
      next_line(NL, file(F,T,L)),
      write(NL)
      ;
      next_line(NL, file(F,T,L)),
      write(NL),
      next_next_line(NNL, file(F,T,L)),
      write(NNL) ).
e$r(LINE, X, file(F,T,L), file(F,T,L)) :-
   cntr_inc(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*REDO*':X), nl,
   !, fail.
   
e$r(LINE, X) :-
   cntr_dec(1,_),
   cntr_get(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*EXIT*':X), nl.
e$r(LINE, X) :-
   cntr_inc(1,Dent),
   nl, tab(Dent), tab(Dent),
   writeq(LINE:'*REDO*':X), nl,
   !, fail.

show_stream(ID, file(F,T,L), file(F,T,L)) :-
   nl,
   write('ID' = ID), nl,
   write('F' = F), nl,
   write('T' = T), nl,
   ( L = [L1,L2,L3 | _], L123 = [L1,L2,L3,'...'] ; L123 = L ),
   !,
   ( write('L123' = L123), nl;
     write(ss_fail(ID)), nl, fail ).
%------------------------------------------------
% Read the file in to a list of strings
%

read_file(File, Lines) :-
   (retract(file$nlines(File, _)); true),
   open(File, read, _, [alias(in)]),
%   asserta(current_file(File)),
   cntr_set(23, 0),
   prepro_read_string(Line),
   read_lines(line(1,Line), Lines, NLines),
   assert(file$nlines(File, NLines)),
   close(in), 
   ( once(prepro_ifdef(_,_)) -> throw(`Mismatched preprocessor if/end directives`)
   ; true),
   true,
   !.

% add newline characters to end of lines, so white space
% exists between lines, and an ending line, so we can get
% the last line number.

read_lines(line(N,end_of_file), [line(N,`\n`)], NLines) :-
   cntr_get(23, CN),
   NLines is CN - 1,
%   NLines is N - 1,
   !.
read_lines(line(N,L), [line(N,LN)|Lines], NLines) :-
   strcat(L, `\n`, LN),
   prepro_read_string(NextLine),
   cntr_get(23, NN),
%   NN is N + 1,
   !, read_lines(line(NN,NextLine), Lines, NLines).

%----------------------------------------------------
% DCG utilities
%

current_file(F, file(F,L,Ls), file(F,L,Ls)).

current_line_number(N, file(F,[],[line(N,L)|Lines]), file(F,[],[line(N,L)|Lines])) :-
   !.
current_line_number(NN, file(F,Chars,[line(N,L)|Lines]), file(F,Chars,[line(N,L)|Lines])) :-
    NN is N - 1,
    !.
current_line_number(0, X, X).   % for testing with normal dcg$terminal

next_line('end of file\n', file(_,_,[]) ).
next_line(N:L, file(_,_,[line(N,L)|_]) ).

next_next_line('', file(_,_,[]) ).
next_next_line('end of file\n', file(_,_,[_])).
next_next_line(N:L, file(_,_,[_,line(N,L)|_])).

finished(file(F, [], []), file(F, [], [])).

remaining_text(TrimChars, file(F,Chars,L), file(F,Chars,L) ) :-
   trim_both(Chars, TrimChars).

trim_both(Chars, TrimChars) :-
   reverse$(Chars, RChars),
   trim(RChars, TRChars),
   reverse$(TRChars, RTRChars),
   trim(RTRChars, TrimChars).

trim([W|Cs], TCs) :-
   is_white(W),
   !, trim(Cs, TCs).
trim(Cs, Cs).

%----------------------------------------------------
% the top of the parser
% use of file/2 causes alib to use special
% dcg$terminal predicate.  See alib for details.
%

parse_file(BuildType, FName, Lines) :-
%   clear_errors,
   cntr_set(0,0),   % clause counter
   clauses(BuildType, file(FName,[],Lines), file(FName,[],[])),
   !.
parse_file(_, FName, _) :-
   process_errors(FName).


%---------------------------------------------------------
% Once a clause is found, different consumers want
% it packaged differently.
%

% Outliner
save_clause(outline, latent$expression, _, Line, include(F), _, not_dcg) :-
   !,
   consult(F),
   string_termq(TermStr, include(F)),
   functor(include(F), Functor, Arity),
   assert(user:item(Line, Functor, Arity, TermStr)).
save_clause(outline, latent$expression, _, Line, Goals, _, not_dcg) :-
   !,
   string_termq(TermStr, Goals),
   functor(Goals, Functor, Arity),
   (
      do_directive(Goals),
      assert(user:item(Line, Functor, Arity, TermStr))
      ;
      assert(user:item(Line, ( :- ), 0, TermStr))
   ),
   !.
save_clause(outline, Head, _, Line, Goals, VarsR, _) :-
   unify_vars(VarsR),
   string_termq(TermStr, Head),
   functor(Head, Functor, Arity),
   assert(user:item(Line, Functor, Arity, TermStr)),
   !.
   
%save_clause(_, Head, _, Line, _, _, _) :-
%   write(Line:Head), nl, !.
   
save_clause(debug_consult, latent$expression, _, _, include(F), _, not_dcg) :-
   !,
   debug_consult(F).
save_clause(debug_consult, latent$expression, _, _, Goals, _, not_dcg) :-
   !,
   (do_directive(Goals)
    ;
    loading_module(M),
    call(M : Goals) ),
   !.
save_clause(debug_consult, Head, File, Line, Goals, VarsR, not_dcg) :-
   reverse$(VarsR, [], VarsA),
   clear_anonymous_vars(VarsA, Vars),
   make_debug_clause(Head,File,Line,Goals,Vars,Clause),
   loading_module(M),
   consult$assertz(M, Clause),
   cntr_inc(0,_),
   !.
save_clause(debug_consult, Head, File, Line, Goals, VarsRA, dcg) :-
   add_difference_lists(Goals, DCGGoals, X0, Xn, NestedDCGVarsWithDups),
   flatten$(NestedDCGVarsWithDups, DCGVarsWithDups),
   (DCGVarsWithDups = [] ->
      DCGVarsWithDups2 = [X0,Xn]
      ;
      DCGVarsWithDups2 = DCGVarsWithDups ),
   remove_embedded_var_dups(DCGVarsWithDups2,DCGVars),
   label_dcg_vars(DCGVars, 0, LabeledDCGVars),
   Head =.. [HeadFunc|HeadArgs],
   append$(HeadArgs, [X0,Xn], DCGHeadArgs),
   DCGHead =.. [HeadFunc|DCGHeadArgs],
   clear_anonymous_vars(VarsRA, VarsR),
   reverse$(VarsR, Vars),
   append$(Vars, LabeledDCGVars, AllVars),
   make_debug_clause(DCGHead,File,Line,DCGGoals,AllVars,Clause),
   loading_module(M),
   consult$assertz(M, Clause),
   cntr_inc(0,_),
   !.

save_clause(debug_compile, latent$expression, _, _, include(F), _, not_dcg) :-
   !,
   debug_compile_read(F).
save_clause(debug_compile, Head, File, Line, Goals, VarsRA, not_dcg) :-
   clear_anonymous_vars(VarsRA, VarsR),
   reverse$(VarsR, [], Vars),
   make_debug_clause(Head,File,Line,Goals,Vars,Clause),
   amzi_compiler:add_clause(Clause),
   cntr_inc(0,_),
   !.
save_clause(debug_compile, Head, File, Line, Goals, VarsRA, dcg) :-
   add_difference_lists(Goals, DCGGoals, X0, Xn, NestedDCGVarsWithDups),
   flatten$(NestedDCGVarsWithDups, DCGVarsWithDups),
   (DCGVarsWithDups = [] ->
      DCGVarsWithDups2 = [X0,Xn]
      ;
      DCGVarsWithDups2 = DCGVarsWithDups ),
   label_dcg_vars(DCGVars, 0, LabeledDCGVars),
   Head =.. [HeadFunc|HeadArgs],
   append$(HeadArgs, [X0,Xn], DCGHeadArgs),
   DCGHead =.. [HeadFunc|DCGHeadArgs],
   clear_anonymous_vars(VarsRA, VarsR),
   reverse$(VarsR, Vars),
   append$(Vars, LabeledDCGVars, AllVars),
   make_debug_clause(DCGHead,File,Line,DCGGoals,AllVars,Clause),
   amzi_compiler:add_clause(Clause),
   cntr_inc(0,_),
   !.
   
save_clause(consult, Head, _, _, Goals, _, not_dcg) :-
   make_clause(Head, Goals, Clause),
   assert(user:Clause),
   !.
save_clause(consult, Head, _, _, Goals, _, dcg) :-
   !.
   
save_clause(compile, Head, _, _, Goals, _, not_dcg) :-
   make_clause(Head, Goals, Clause),
   amzi_compiler:compile_clause(Clause),
   !.
save_clause(compile, Head, _, _, Goals, _, dcg) :-
   !.

% add_difference_lists(Goals, DCGGoals, X0, Xn, DCGVars)

add_difference_lists( (Goal1 -> Goal2), (DCGGoal1 -> DCGGoal2), X0, Xn, DCGVars) :-
   !,
   add_difference_lists(Goal1, DCGGoal1, X0, X, Vars1),
   add_difference_lists(Goal2, DCGGoal2, X, Xn, Vars2),
   append$(Vars1, Vars2, DCGVars).
add_difference_lists( (Goal1, Goal2), (DCGGoal1, DCGGoal2), X0, Xn, DCGVars) :-
   !,
   add_difference_lists(Goal1, DCGGoal1, X0, X, Vars1),
   add_difference_lists(Goal2, DCGGoal2, X, Xn, Vars2),
   append$(Vars1, Vars2, DCGVars).

add_difference_lists( (Goal1; Goal2), (DCGGoal1; DCGGoal2), X0, Xn, DCGVars) :-
   !,
   add_difference_lists(Goal1, DCGGoal1a, X0, X1, Vars1),
   ( X0 == X1 ->
        DCGGoal1 = ( X1a = X0, DCGGoal1a )
        ;
        X1a = X1,
        DCGGoal1 = DCGGoal1a ),
   add_difference_lists(Goal2, DCGGoal2a, X0, X2, Vars2),
   ( X0 == X2 ->
        X2a = X1,
        DCGGoal2 = ( X2a = X0, DCGGoal2a )
        ;
        X2a = X2,
        DCGGoal2 = DCGGoal2a ),
   X1a = X2a,
   Xn = X1a,
   append$(Vars1, Vars2, DCGVars).

add_difference_lists(dcg$call(Goal,F,L,W,X,Y,Z), debug$call(dcg$(DCGGoal),F,L,W,X,Y,Z), X0, Xn, [X0,Xn]) :-
   !,
   Goal =.. [Func|Args],
   append$(Args, [X0,Xn], DCGArgs),
   DCGGoal =.. [Func|DCGArgs].
add_difference_lists(dcg$call(Goal), DCGGoal, X0, Xn, [X0,Xn]) :-
   !,
   Goal =.. [Func|Args],
   append$(Args, [X0,Xn], DCGArgs),
   DCGGoal =.. [Func|DCGArgs].
%add_difference_lists(OrdinaryGoal, OrdinaryGoal, X, X, [X,X]).
add_difference_lists(OrdinaryGoal, OrdinaryGoal, X, X, []).

% Some goals repeat vars, for example when the input and
% output lists are the same.  We don't need repeats in the
% list, except for maybe first and last.

remove_embedded_var_dups([], [X,X]) :-
   !.
remove_embedded_var_dups(In, Out) :-
   reverse$(In, [LastVar|InR]),
   remove_var_dups(InR, OutR),
   ( (is_member(LastVar, OutR)) ->
      remove_var(OutR, LastVar, OutRR)
      ;
      OutRR = OutR
      ),
   reverse$([LastVar|OutRR], OutT),
   (OutT = [V] ->
      Out = [V,V]
      ;
      Out = OutT).

% remove with strong unify so it works for vars
remove_var([], _, []) :-
   !.
remove_var([V1|VIns], V, VOuts) :-
   V1 == V,
   !,
   remove_var(VIns, V, VOuts).
remove_var([V1|VIns], V, [V1|VOuts]) :-
   !,
   remove_var(VIns, V, VOuts).

remove_var_dups([], []) :-
   !.
remove_var_dups([A|Xs],Ys) :-
   is_member(A, Xs),   % uses strong unify, so works for vars
   !,
   remove_var_dups(Xs, Ys).
remove_var_dups([A|Xs], [A|Ys]) :-
   remove_var_dups(Xs, Ys).
  
% Create names for the DCG variables
label_dcg_vars([DCG_N], _, ['$VAR'('__DCG_N') = DCG_N]) :-
   !.
label_dcg_vars([DCG_X|Vars], N, ['$VAR'(VName) = DCG_X | LabeledVars]) :-
   string_integer(S, N),
   string_atom(S, AN),
   atom_concat('__DCG_', AN, VName),
   NN is N + 1,
   !,
   label_dcg_vars(Vars, NN, LabeledVars).


% Bind the variables with the special names that the
% internal writer uses to make them look like vars that
% can be read in again by the reader.
bind_vars([]).
bind_vars([Name=Var|Vs]) :-
   Var = '$VAR'(Name),
   bind_vars(Vs).

clear_anonymous_vars([], []).
clear_anonymous_vars(['$VAR'('_')=_|VAs], Vs) :-
   !,
   clear_anonymous_vars(VAs, Vs).
clear_anonymous_vars([V|VAs], [V|Vs]) :-
   clear_anonymous_vars(VAs, Vs).

% defined in alib now
% unify_vars([]).
% unify_vars([V=V|Vs]) :-
%    unify_vars(Vs).

make_debug_clause(latent$expression, _, _, Goals, _,
      ( :- Goals ) ).
% Don't decorate dynamic clauses, because they can't
% be retracted.
make_debug_clause(Head, File, Line, [], VarList, Head) :-
   functor(Head, F, A),
   loading_module(M),
   sys$clause('{sys}debug$dynamic'(M:F/A)),
   !.
make_debug_clause(Head, File, Line, [], VarList,
      (Head :- debug$info(Head,File,Line,_,VarList,_)) ).
make_debug_clause(Head, File, Line, Goals, VarList,
      (Head :- debug$info(Head,File,Line,_,VarList,_), Goals) ).

make_clause(Head, [], Head).
make_clause(Head, Goals, (Head :- Goals) ).

%--------------------------------------------------
% error handling
%

clear_errors :-
   abolish(error_found/4),
   abolish(parse_message/5).
   
process_errors(F) :-
   write('Error report for file '), write(F), nl,
   cntr_get(0,NClauses),
   write(NClauses), write(' clauses read'), nl,
   cntr_set(0,0),
   parse_message(Type, File, Line, Msg, Text),
   cntr_inc(0,_),
   write('Parse '), write(Type), tab(1),
   write(File:Line), tab(2), write(Msg), nl,
   tab(2), write(Text), nl,
   fail.
process_errors(_) :-
   cntr_get(0,N),
   write(N), write(' parse messages'), nl.

/*
exec_warning(MsgList) :-
   list_to_string(MsgList, Msg),
   asserta( parse_message(warning, '', '', Msg, []) ),
   !.
*/
   
parse_error(MsgList, F1, F2) :-
   list(MsgList),
   !,
   list_to_string(MsgList, Msg),
   parse_error(Msg, F1, F2).
parse_error(Msg, file(F, [], []), file(F, [], [])) :-
   !,
   file$nlines(F, N),
%   asserta(error_found(F, N, Msg, [])),
   augment_message(Msg, end_of_file, MsgA),
   asserta( parse_message(error, F, N, MsgA, []) ),
   throw(parse_error).   
parse_error(Msg, file(F, [], [line(N,S2)|Z]), file(F, [], [line(N,S2)|Z])) :-
   !,
%   asserta(error_found(F, N, Msg, S2)),
   augment_message(Msg, S2, MsgA),
   asserta( parse_message(error, F, N, MsgA, []) ),
   throw(parse_error).
parse_error(Msg, file(F, Line, [line(N,S2)|Z]), file(F, Line, [line(N,S2)|Z])) :-
   NN is N - 1,
   string_list(S1,Line),
%   asserta(error_found(F, NN, Msg, S1)),
   augment_message(Msg, S1, MsgA),
   asserta( parse_message(error, F, NN, MsgA, []) ),
   throw(parse_error).

warning(MsgList, F1, F2) :-
   list(MsgList),
   !,
   list_to_string(MsgList, Msg),
   warning(Msg, F1, F2).
warning(Msg, file(F, [], []), file(F, [], [])) :-
   !,
   file$nlines(F, N),
   augment_message(Msg, end_of_file, MsgA),
   asserta( parse_message(warning, F, N, Msg, []) ). 
warning(Msg, file(F, [], [line(N,S2)|Z]), file(F, [], [line(N,S2)|Z])) :-
   !,
   augment_message(Msg, S2, MsgA),
   asserta( parse_message(warning, F, N, MsgA, S2) ).
warning(Msg, file(F, Line, [line(N,S2)|Z]), file(F, Line, [line(N,S2)|Z])) :-
   NN is N - 1,
   string_list(S1,Line),
   augment_message(Msg, S1, MsgA),
   asserta( parse_message(warning, F, NN, MsgA, S1) ).

list_to_string(L, S) :-
   stringize(L, LS),
   stringlist_concat(LS, S).

% make all list elements strings, for concatenation
stringize([], []).
stringize([A|X], [A|Y]) :-
   string(A),
   !, stringize(X,Y).
stringize([A|X], [B|Y]) :-
   string_term(B,A),
   !, stringize(X,Y).

% add the first few characters of where we were trying
% to parse to the message.

augment_message(Msg, end_of_file, MsgA) :-
   strcat(Msg, ` at end of file  `, MsgA),
   !.
augment_message(Msg, Line, MsgA) :-
   string_length(Line, N),
   N =< 25,
   stringlist_concat([Msg, ` at: `, Line, `  `], MsgA),
   !.
augment_message(Msg, Line, MsgA) :-
   sub_string(Line, 1, 25, SubLine),
   stringlist_concat([Msg, ` at: `, SubLine, ` ...  `], MsgA).


% if there was an error, then go to next .white and
% continue.

clear_to_dot --> ".", [C], { is_white(C) }, !.
clear_to_dot --> clear_comment, !, clear_to_dot.
clear_to_dot --> clear_quotes, !, clear_to_dot.
clear_to_dot --> [X], !, clear_to_dot.
clear_to_dot --> finished, !.
%clear_to_dot --> parse_error(`Unable to recover from previous error`).

clear_comment --> "%", clear_rest_of_line.
clear_comment --> "/*", clear_end_of_comment.

clear_rest_of_line --> [10], !.
clear_rest_of_line --> [13], !.
clear_rest_of_line --> "\n", !.
clear_rest_of_line --> [_], !, clear_rest_of_line.

clear_end_of_comment --> "*/", !.
clear_end_of_comment --> [X], !, clear_end_of_comment.
clear_end_of_comment --> finished.

clear_quotes --> clear_quoted_chars(0'').
clear_quotes --> clear_quoted_chars(0'`).   % `
clear_quotes --> clear_quoted_chars(0'").   % "
clear_quotes --> [C], { not(is_alpha_numeric(C)) }, clear_quoted_chars(0'$).   % $

is_alpha_numeric(C) :- ( is_lower(C); is_upper(C); is_digit(C); C == 0'_ ).


clear_quoted_chars(Q) --> [Q], clear_non_quotes(Q).

clear_non_quotes(Q) --> [Q,Q], !, clear_non_quotes(Q).
clear_non_quotes(Q) --> [Q], !.
clear_non_quotes(Q) --> [_], !, clear_non_quotes(Q).
clear_non_quotes(Q) --> finished.


error(file(_,[],[])) :-
   !.
error(file(_,X,[Line|_])) :-
   writeq(error(X:Line)), write('\n').
   

%--------------------------------------------------------
% clauses
% we need to catch the ending condition to see if
% there was an error or not.  Checking twice for
% finished, the second being for the odd case where
% we had an unexpected end-of-file, say with unbalanced
% comment delimiters.
%

clauses(_) --> wh, finished, !.
clauses(BT) -->
   get_clause(BT),
   !,
   clauses(BT).
clauses(BT) --> clear_to_dot, !, clauses(BT).

get_clause(BT, In, Out) :-
   M$DEBUG_INIT,
   catch(
       aclause(BT, In, Out),
       parse_error,
       get_clause_error ).

get_clause_error :-
   M$DEBUG_INIT,
   fail.

%----------------------------------------
% Preprocessor Stuff
%

prepro_read_string(Line) :-
   read_string(in, S),
   cntr_inc(23, NN),
   ( S == end_of_file -> Line = end_of_file ; prepro_input(S, Line) ).

prepro_input(S, Line) :-
   sub_string(S, 1, 1, `#`),
   prepro_action(S),
%   prepro_read_string(Line),
   Line = `\n`,
   !.
prepro_input(S, Line) :-
   prepro_ifdef(_, false),
%   prepro_read_string(Line),
   Line = `\n`,
   !.
prepro_input(S, Line) :-
   prepro_replace(S, Line).
   
prepro_action(S) :-
   string_tokens(S, T),
   prepro_parse(PreProCommand, T, _),
   prepro_do(PreProCommand).

quotedValue(Q, [X,Q], Ts, V) :-
   !,
   reverse$([Q,X|Ts], V).
quotedValue(Q, [Q], Ts, V) :-
   !,
   reverse$([Q|Ts], V).
%   stringlist_concat(Vs, ` `, V).
quotedValue(Q, [X|Z], Ts, V) :-
   quotedValue(Q, Z, [` `, X|Ts], V).


prepro_parse(define(M,V)) -->
   ['#', define, M, Q | Vs],
   { quotedValue(Q, Vs, [Q], V) },
   !.
prepro_parse(define(M,V)) -->
   ['#', define, M, V],
   !.
prepro_parse(define(M,'')) -->
   ['#', define, M],
   !.
prepro_parse(undef(M)) -->
   ['#', undef, M],
   !.
prepro_parse(ifdef(M)) -->
   ['#', ifdef, M],
   !.
prepro_parse(ifndef(M)) -->
   ['#', ifndef, M],
   !.
prepro_parse(else) -->
   ['#', else],
   !.
prepro_parse(endif) -->
   ['#', endif],
   !.
prepro_parse(error(E)) -->
   ['#', error, '`', E, '`'],
   !.
% Mary not able to test this.
%prepro_parse(UNK) -->
%   ['#', UNK],
%   parse_error([`Bad preprocessor directive `, UNK]),
%   !.



prepro_do(define(M,V)) :-
   ( prepro_ifdef(_,false) -> true ; sys$assert(prepro_macro(M,V)) ),
   !.
prepro_do(undef(M)) :-
   ( prepro_ifdef(_,false) -> true ; sys$retract(prepro_macro(M,_)) ),
   !.
prepro_do(ifdef(M)) :-
   prepro_macro(M, _),
   sys$assert(prepro_ifdef(M,true)),
   !.
prepro_do(ifdef(M)) :-
   sys$assert(prepro_ifdef(M,false)),
   !.
prepro_do(ifndef(M)) :-
   prepro_macro(M, _),
   sys$assert(prepro_ifdef(M,false)),
   !.
prepro_do(ifndef(M)) :-
   sys$assert(prepro_ifdef(M,true)),
   !.
prepro_do(else) :-
   sys$retract(prepro_ifdef(M, Old)),
   (Old == true -> New = false ; New = true),
   sys$assert(prepro_ifdef(M, New)),
   !.
prepro_do(endif) :-
   sys$retract(prepro_ifdef(_, _)),
   !.
prepro_do(error(E)) :-
   throw(E),
   !.
prepro_replace(Line, Line) :-
   not prepro_macro(_, _),
   !.
prepro_replace(S, Line) :-
   string_tokens(S, T, ` \n\t(),.{};:][|&*+-./<=>@\\^~`),
   replace_macros(T, TM),
   stringlist_concat(TM, Line).

replace_macros([], []).
replace_macros(['__DATE__'|Xs], [Date|Ys]) :-
   date(MON, DAY, YEAR),
   string_number(SMON, MON),
   string_number(SDAY, DAY),
   string_number(SYEAR, YEAR),
   stringlist_concat([`\``, SMON, `/`, SDAY, `/`, SYEAR, `\``], Date),
   !, replace_macros(Xs, Ys).
replace_macros(['__TIME__'|Xs], [Time|Ys]) :-
   time(HOUR, MIN, SEC),
   string_number(SHOUR, HOUR),
   string_number(SMIN, MIN),
   string_number(SSEC, SEC),
   stringlist_concat([`\``, SHOUR, `:`, SMIN, `:`, SSEC, `\``], Time),
   !, replace_macros(Xs, Ys).
replace_macros(['__LINE__'|Xs], [Line|Ys]) :-
   cntr_get(23, N),
   string_number(Line, N),
   !, replace_macros(Xs, Ys).
replace_macros([M|Xs], [M|Ys]) :-
   prepro_macro(M, ''),
   !, replace_macros(Xs, Ys).
replace_macros([M|Xs], Zs) :-
   prepro_macro(M, V),
   list(V),
   append$(V, Ys, Zs),
   !, replace_macros(Xs, Ys).
replace_macros([M|Xs], [V|Ys]) :-
   prepro_macro(M, V),
   !, replace_macros(Xs, Ys).
replace_macros([X|Xs], [X|Ys]) :-
   replace_macros(Xs, Ys).

%----------------------------------------
% a clauses
%

aclause(BT) -->
   wh,
   adirective(Goals, Line),
   wh,
   { save_clause(BT,latent$expression,_,Line,Goals,_,not_dcg) },
   !.
aclause(BT) -->
   current_file(F),
   wh,
   aheadterm(BT, Head, L, [], VarsHead),
   wh,
   abody(BT,Goals,VarsHead,VarListR,DCG),
   wh,
   { save_clause(BT,Head,F,L,Goals,VarListR,DCG) },
   !.
aclause(BT) -->
   parse_error(`Bad clause head`).

abody(BT,Goals,Vin,Vout,not_dcg) -->
   ":-",
   non_graphic,
   wh,
   !,
   goals(BT,Goals,Vin,Vout),
   wh,
   ".".  % take this out when we figure out parens in goallists
abody(BT,Goals,Vin,Vout,dcg) -->
   "-->",
   non_graphic,
   wh,
   !,
   dcg_goals(BT,Goals,Vin,Vout),
   wh,
   ".".  % and this
abody(BT,[],V,V,not_dcg) -->
   ".".
abody(BT,_,_,_,_) -->
   ":-",
   non_graphic,
   parse_error(`Bad clause body after neck ( :- )`).
abody(BT,_,_,_,_) -->
   "-->",
   non_graphic,
   parse_error(`Bad DCG clause body after DCG operator ( --> )`).
abody(BT,_,_,_,_) -->
   parse_error(`Clause missing ending period`).

% Always use consult form of make_goal, so directive is
% not decorated.  Someday we might want to change this,
% so the debugger can be started from directives.
adirective(Goals, Line) -->
   ":-",
   non_graphic,
   current_line_number(Line),
   wh,
   !,
   goals(consult,Goals,_,_),
   wh,
   ".".  % and this
adirective(_, _) -->
   ":-",
   non_graphic,
   parse_error(`Bad directive`).
   

%-----------------------------------
% dcg goals
%

dcg_goals(BT,Gs,Vin,Vout) -->
   dcg_goal_list(BT,GL,Vin,Vout),
   { exp(GL,Gs) }.
dcg_goals(BT,Goals,Vin,Vout) -->
   parse_error(`Unsupported DCG option for source code debugging`).

dcg_goal_list(BT,[G|Gs],Vin,Vout) -->
   dcg_goal(BT,G,Vin,Vx),
   dcg_more_goals(BT,Gs,Vx,Vout).

% put first and last back in, when we sort () issue out
%dcg_more_goals(_,[],V,V,S,[S]) -->
%   ".", !, wh.
dcg_more_goals(BT,[op$(',',1000,xfy),G|Gs],Vin,Vout) -->
   ",", !, wh,
   dcg_goal(BT,G,Vin,Vx), wh,
   dcg_more_goals(BT,Gs,Vx,Vout).
dcg_more_goals(BT,[op$(';',1100,xfy),G|Gs],Vin,Vout) -->
   ";",
   !, wh,
   dcg_goal(BT,G,Vin,Vx), wh,
   dcg_more_goals(BT,Gs,Vx,Vout).
dcg_more_goals(BT,[op$('->',1050,xfy),G|Gs],Vin,Vout) -->
   "->",
   non_graphic,
   !, wh,
   dcg_goal(BT,G,Vin,Vx), wh,
   dcg_more_goals(BT,Gs,Vx,Vout).
dcg_more_goals(_,[],V,V) -->
   [], wh.
%dcg_more_goals(_,_,_,_,_,_) -->
%   parse_error(`confusion parsing goals in DCG predicate`).

dcg_goal(BT,Goal,Vin,Vout) -->
	[X],
	var(X),
   !,
   current_file(F),
   current_line_number(L),
   { make_dcg_goal(BT,phrase(X),F,L,Goal) }.
dcg_goal(BT,pa$ren(G),Vin,Vout) -->
   "(", wh,
   dcg_goal_list(BT, GL,Vin,Vout),
   wh,
   ")", wh,
   peek_exp_end(goal),
   !,
   { exp(GL, G) }.
dcg_goal(BT,Goal,V,V) -->
   dcg_asis_goal(G),
   !,
   current_file(F),
   current_line_number(L),
   { make_goal(BT,G,F,L, Goal) }.
dcg_goal(BT,pa$ren(G),Vin,Vout) -->
   "{", wh,
   agoallist(BT, GL,Vin,Vout),
   wh,
   "}", wh,
   !,
   {  exp(GL, G) }.
dcg_goal(BT,Goal,Vin,Vout) -->
   alist(List,Vin,Vout),
   !,
   current_file(F),
   current_line_number(L),
   { make_dcg_goal(BT,dcg$terminal(List),F,L,Goal) }.
dcg_goal(BT,call(Goal),Vin,Vout) -->
   "call(", wh,
   dcg_goal(BT,Goal,Vin,Vout),
   wh, ")".
dcg_goal(BT,bug(Goal),Vin,Vout) -->
   "bug(", wh,
   dcg_goal(BT,Goal,Vin,Vout),
   wh, ")".
dcg_goal(BT,bug(Goal),Vin,Vout) -->
   "? ", wh,
   dcg_goal(BT,Goal,Vin,Vout).
dcg_goal(BT,Goal,Vin,Vout) -->
   aatom(arg,Func),
   "(", wh,
   args(Args,Vin,Vout), wh,
   !,
   current_file(F),
   current_line_number(L),
   { G =.. [Func|Args],
     make_dcg_goal(BT,G,F,L,Goal) }.
dcg_goal(BT,Goal,V,V) -->
   aatom(arg,Func),
   !,
   current_file(F),
   current_line_number(L),
   { make_dcg_goal(BT,Func,F,L,Goal) }.

   
dcg_asis_goal(!) --> "!", wh.
dcg_asis_goal(fail) --> "fail", wh.
dcg_asis_goal(true) --> "true", wh.

%----------------------------------
% goals
%

goals(BT,Gs,Vin,Vout) -->
   agoallist(BT, GL,Vin,Vout),
   { exp(GL,Gs) }.

agoallist(BT,[A|Z],Vin,Vout) -->
   agoal(BT,A,Vin,Vx),
   wh,
   !,
   more_goals(BT,Z,Vx,Vout).

% put this first and last goals back in when we figure
% out how to deal with abiguity of terms and parens.
%more_goals(BT,EndChar,[],V,V) -->
%   [EndChar], !, wh.
more_goals(BT,[op$(',',1000,xfy),A|Z],Vin,Vout) -->
   ",", !, wh,
   agoal(BT,A,Vin,Vx), wh,
%   !,
   more_goals(BT,Z,Vx,Vout).
more_goals(BT,[op$(';',1100,xfy),A|Z],Vin,Vout) -->
   ";", !, wh,
   agoal(BT,A,Vin,Vx), wh,
%   !,
   more_goals(BT,Z,Vx,Vout).
more_goals(BT,[op$('->',1050,xfy),A|Z],Vin,Vout) -->
   "->",
   non_graphic,
   !, wh,
   agoal(BT,A,Vin,Vx), wh,
%   !,
   more_goals(BT,Z,Vx,Vout).
more_goals(BT,[],V,V) -->
   [], wh.
%more_goals(_,EndChar,_,_,_) -->
%   { string_list(S, [EndChar]) },
%   parse_error([`Confusion looking for closing "`, S, `" punctuation.`]).

% tricky bit here, the parens could be part of an expression,
% rather than a goal list.

agoal(BT,pa$ren(G),Vin,Vout) -->
   "(", wh,
   agoallist(BT, GL,Vin,Vout), wh,
   ")", wh,
   peek_exp_end(goal),
   !,
   { exp(GL, G) }.
agoal(BT,Goal,Vin,Vout) -->
   current_file(F),
   agoalterm(BT,G,L,Vin,Vout),
   { make_goal(BT,G,F,L, Goal) }.
agoal(BT,_,_,_) -->
   "(",
   parse_error(`unbalenced parentheses in goals`).
agoal(_,_,_,_) -->
   parse_error(`bad goal`).
   
aheadterm(BT,G,L,Vin,Vout) -->
   once_aexpression(goal,G,L,Vin,Vout),
%   { not_redefined_system_predicate(G) },
   !.
aheadterm(_,_,_,_,_) --> parse_error(`Attempt to redefine system predicate`), !.

not_redefined_system_predicate(_) :-
   loading_module(amzi_system).
not_redefined_system_predicate(G) :-
   functor(G, Functor, Arity),
   predicate_property(Functor/Arity, defined_in(amzi_system)),
   predicate_property(Functor/Arity, exported),
   !,
   fail.
not_redefined_system_predicate(_).


agoalterm(BT,G,L,Vin,Vout) --> special_goal(BT,G,L,Vin,Vout), !.
agoalterm(BT,!,L,V,V) --> "!", current_line_number(L), !.
agoalterm(BT,G,L,Vin,Vout) --> aexpression(goal,G,L,Vin,Vout), !.
agoalterm(BT,X,L,Vin,Vout) --> avar(X,V1,V2), current_line_number(L), !.
agoalterm(BT,_,_,_,_) -->
   parse_error(`Bad goal term`).

special_goal(BT,G,L,Vin,Vout) -->
   findall_functor(FF),
   !,
   "(", wh,
   current_line_number(L),
   aterm(arg,Arg1,Vin,Vx), wh,
   ",", wh,
   afree_vars(FF, FreeVars, Vx,Vy), wh,
   agoal(BT,G2,Vy,Vz), wh,
   { append$(FreeVars, [G2], GL),
     exp(GL,Arg2) },
   ",", wh,
   aterm(arg,Arg3,Vz,Vout), wh,
   ")",
   { G =.. [FF,Arg1,Arg2,Arg3] }.
special_goal(BT,G,L,Vin,Vout) -->
   call_functor(CF),
   !,
   "(", wh,
   current_line_number(L),
   agoal(BT,G2,Vin,Vout), wh,
   ")", 
   { exp([G2],Arg1),
     G =.. [CF,Arg1] }.
special_goal(BT,G,L,Vin,Vout) -->
   "catch",
   !,
   "(", wh,
   current_line_number(L),
   agoal(BT,G1,Vin,Vx), wh,
   ",", wh,
   { exp([G1],Arg1) },
   aterm(arg,Arg2,Vx,Vy), wh,
   ",", wh,
   agoal(BT,G3,Vy,Vout), wh,
   ")", wh,
   { exp([G3],Arg3),
     G =.. [catch,Arg1,Arg2,Arg3] }.
special_goal(BT,_,_,_,_) -->
   findall_functor(FF),
   parse_error([`Bad goals for `, FF]).
special_goal(BT,_,_,_,_) -->
   call_functor(FF),
   parse_error([`Bad goals for `, FF]).
special_goal(BT,_,_,_,_) -->
   "catch",
   parse_error(`Bad goals for catch`).

findall_functor(findall) --> "findall".
findall_functor(setof) --> "setof".
findall_functor(bagof) --> "bagof".

call_functor(call) --> "call".
call_functor(not) --> "not".
call_functor(not) --> [0'\, 0'+].  % watch out for escape!
call_functor(once) --> "once".

% used in special case of goals in setof and bagof

afree_vars(findall, [], V, V) --> [].
afree_vars(_, [Var, op$('^',200,xfy)|FVs], Vin, Vout) -->
   avar(Var, Vin, Vx), wh,
   "^", wh,
   afree_vars(FVs, Vx, Vout).
afree_vars(_, [], V, V) --> [].

hats_off_debug$call(V^G, Hats, F, L, RespectCall) :-
   !, hats_off_debug$call(G, V^Hats, F, L, RespectCall).
hats_off_debug$call(G, Hats, F, L, RespectCall ) :-
   hats_on(Hats, debug$call(G,F,L,_,_,_,_), RespectCall),
   !.

hats_on(Y, RC, Y^RC) :-
   var(Y),
   !.
hats_on(X^Y, RC, RespectCall) :-
   !, hats_on(Y, X^RC, RespectCall).

make_goal(debug_consult, G,F,L, debug$call(G,F,L,_,_,_,_) ) :- var(G), !.
make_goal(debug_consult, !,F,L, (debug$call('!',F,L,_,_,_,_), debug64_cut) ) :-
   !.
make_goal(debug_consult, V^G,F,L, RespectCall ) :-
   hats_off_debug$call(G, V, F, L, RespectCall),
   !.
make_goal(debug_consult, G,F,L, debug$call(G,F,L,_,_,_,_) ).

make_goal(debug_compile, G,F,L, debug$call(G,F,L,_,_,_,_) ) :- var(G), !.
make_goal(debug_compile, !,F,L, (debug$call('!',F,L,_,_,_,_), debug64_cut ) ) :-
   !.
make_goal(debug_compile, V^G,F,L, RespectCall ) :-
   hats_off_debug$call(G, V, F, L, RespectCall),
   !.
make_goal(debug_compile, G,F,L, debug$call(G,F,L,_,_,_,_) ).

make_goal(outline, G,_,_,G).

make_goal(consult, G,_,_,G).

make_goal(compile, G,_,_,G).

% Flag goals for DCG that need extra arguments added in
% in a final pass.

make_dcg_goal(debug_consult, G,F,L, dcg$call(G,F,L,_,_,_,_) ).

make_dcg_goal(debug_compile, G,F,L, dcg$call(G,F,L,_,_,_,_) ).

make_dcg_goal(outline, G,_,_,G).

make_dcg_goal(consult, G,_,_,dcg$call(G)).

make_dcg_goal(compile, G,_,_,dcg$call(G)).

%-----------------------------------------
% directive processing
%

do_directive(quit) :- !.
do_directive(([H|T])) :- !,
   consult([H|T]), !.
do_directive((module(M))) :-
   module$(M), !.
do_directive((end_module(M))) :-
   end_module$(M), !.
do_directive((body(M))) :-
   module$(M), !.
do_directive((end_body(M))) :-
   end_module$(M), !.
do_directive((import(M))) :-
   import(M), !.
do_directive((import(M,L))) :-
   import(M,L), !.
do_directive((export(L))) :-
   export(L), !.
do_directive((metapredicate(MP))) :-
   meta$assert(MP), !.
do_directive((sorted(P))) :-
   set$sorted(P),
   add_debug_dynamic(P), !.
do_directive((indexed(P))) :-
   set$$indexed(P),
   functor(P,F,A),
   add_debug_dynamic(F/A), !.
%do_directive((include(File))) :-  % keep commented out, each case is different see save_clause
%   consult(File), !.
do_directive((dynamic(DList))) :-
   add_debug_dynamic(DList), !.
do_directive((noNonTerminals)) :-
   sys$assertz('{sys}no$nt'), !.
do_directive((nonTerminal(F1/A))) :-
   sys$assertz('{sys}is$nt'(F1, A)), !.
do_directive((nonTerminal((F1/A, F2)))) :-         % can be many
   sys$assertz('{sys}is$nt'(F1, A)),
   do_directive((nonTerminal(F2))), !.
do_directive((dcg_terminal(F))) :-                 % usr-supplied terminal/3
   sys$assertz('{sys}is$t'(F)), !.                                        % can only be one

do_directive((set_prolog_flag(F,V))) :-
   set_prolog_flag(F,V), !.
do_directive((op(P,A,O))) :-
   op(P,A,O),
   !.
%do_directive(X) :-
%   loading_module(M),
%   call(M : X), !.
   

add_debug_dynamic(N/A) :-
  atom(N),
  integer(A), !,
  loading_module(M),
  sys$assert('{sys}debug$dynamic'(M:N/A)).
add_debug_dynamic((A, B)) :-
  add_debug_dynamic(A),
  add_debug_dynamic(B).
add_debug_dynamic(P) :-
  functor(P, N, A), !,
  loading_module(M),
  sys$assert('{sys}debug$dynamic'(M:N/A)).

%-------------------------------
% general terms
%

aterm(Context,T,Vin,Vout) --> 
  aexpression(Context,T,_,Vin,Vout).
aterm(_,_,_,_) -->
  parse_error(`Bad term`).

%--------------------------
% structures
%

% picking up curly braces and treating contents as a term
% for now, which is OK, but when we get to DCG they are
% real args if the {} are goals, so beware.
astructure('{}'(A),Line,Vin,Vout) -->
   "{",wh,
   current_line_number(Line),
   aterm(term,A,Vin,Vout), wh,
   "}", wh.
astructure(T,Line,Vin,Vout) -->
   aatom(arg,F),
   current_line_number(Line),
   "(", wh,
   args(As,Vin,Vout), wh,
   { T =.. [F|As] }.
astructure(_,_,_,_) -->
   "{",
   parse_error(`unbalanced curly braces`).
astructure(_,_,V,V) -->
   aatom(arg,F),
   "(",
   parse_error([`bad structure `, F]).

args([A|As],Vin,Vout) -->
   aterm(arg, A,Vin,Vx), wh,
   more_args(As,Vx,Vout).

more_args([],V,V) -->
   ")",
   !.
more_args([A|As],Vin,Vout) -->
   ",", !, wh,
   aterm(arg, A,Vin,Vx), wh,
   more_args(As,Vx,Vout).
more_args(_,_,_) -->
   parse_error(`incomplete structure`).

%------------------------------
% Operator Expressions
%

% First tokenize the input stream and then evaluate
% the list as an expression.

once_aexpression(Context, E, Line, Vin, Vout) -->
   aexpression(Context, E, Line, Vin, Vout),
   !.

% just so we can parse the reader itself
aexpression(_, T, L, Vin, Vout) -->
   "op$(",
   !,
   current_line_number(L),
   args(As,Vin,Vout), wh,
%   ")",
   { T =.. [op$|As] }.
aexpression(Context, E, Line, Vin, Vout) -->
   aexplist(Context, ExpList, Line, Vin, Vout),
   { exp(ExpList, E) }.  % not primitive
aexpression(_,_,_,_,_) -->
   parse_error(`bad expression, possible operator errors`).

aexplist(Context, ExpList, Line, Vin, Vout) -->
   aphrase(Context,ExpList1,Vin,Vx),
   wh,
   current_line_number(Line),
%   !,
   more_expression(Context, ExpList2,Vx,Vout),
   { append$(ExpList1, ExpList2, ExpList) }.

% put the phrase in as a list, so it is expanded
% by itself first as its operators, if any, can't
% override the infix operator

more_expression(Context, [], V, V) -->
   peek_exp_end(Context),
   !.
more_expression(Context, ExpList, Vin, Vout) -->
   ainfix(Op,P,A),
   wh,
%   { not(goal_op(Context,Op)) },
   aphrase(Context,ExpList1, Vin, Vx),
   wh,
   { append$([op$(Op,P,A)|ExpList1], ExpList2, ExpList) },
   !,
   more_expression(Context, ExpList2, Vx, Vout).
%more_expression(_, _, _, _) -->
  % parse_error(`possible operator errors parsing expression`).


% Check for structure first in case its the odd case
% of something like +(a,b) which should be a structure,
% rather than +/1 with the arg ','(a,b), also have to
% check for numbers first too, so -1 is -1 not -(1).
aphrase(Context,[X|Z],Vin,Vout) -->
   astructure(X,_,Vin,Vout),
   wh,
   !,
   postfixes(Z).
aphrase(Context,[X|Z],V,V) -->
   anumber(X), wh,
   !,
   postfixes(Z).
aphrase(Context,[op$(Op,P,A)|L],Vin,Vout) -->
   aprefix(Op,P,A), wh,
   aphrase(Context,L,Vin,Vout).
aphrase(Context,[X|Z],Vin,Vout) -->
   aexpterm(Context,X,Vin,Vout), wh,
   !,
   postfixes(Z).
%aphrase(_,_,_,_) -->
%   parse_error(`bad phrase in expression`).

% Can't cut on the search for postfixes, the postfix op
% might actually be an infix op.
postfixes([op$(Op,P,A)|Z]) -->
   apostfix(Op,P,A),
   wh,
   postfixes(Z).   
postfixes([]) --> [].  

aexpterm(Context,pa$ren(EL),Vin,Vout) -->
   "(", wh,
   aterm(term, EL, Vin, Vout), wh,
   ")",
   !.
%aexpterm(_,T,Vin,Vout) --> astructure(T,_,Vin,Vout), !.
aexpterm(_,T,Vin,Vout) --> alist(T, Vin, Vout), !.
aexpterm(_,T,Vin,Vout) --> avar(T,Vin,Vout), !.
aexpterm(Context,T,V,V) --> aprimitive(Context,T), !.
%aexpterm(_,!,V,V) --> "!".

exp(EL, E) :-
   reduce(EL, op$(x,0,x), 0, [], E),
   !.
exp(EL, E) :-
   write('******** Bad Expression ****************'), nl,
   write(EL), nl, nl,
   fail.

% reduce(ExpressionList, LastOperator, MaxP, EvalStack, Expression)
% LastOperator is the last operator pushed on the 
% EvalStack.  But prefix ops get pushed on anyway, so MaxP
% keeps track of the maximum precedence pushed.

%reduce(EL, LastOp, MaxP, Out, E) :-
%   write('Input' = EL), nl,
%   write('LastOp' = LastOp), nl,
%   write('MaxP' = MaxP), nl,
%   write('Output' = Out), nl, nl,
%   fail.
reduce([], _, _, EvalSt, E) :-
   compress_stack(EvalSt, 1201, [E]),
   !.
reduce([V|Z], LastOp, MaxP, EvalSt, E) :-
   var(V),
   !,
   reduce(Z, LastOp, MaxP, [V|EvalSt], E).
reduce([op$(Op,P,A)|_], op$(Op,P,A), _, _, _) :-
   (A == xfx; A == xf; A == fx),
   !,
   fail.
reduce([op$(Op1,P1,A1)|Z], op$(Op2,P2,A2), MaxP, EvalSt, E) :-
   ( pop_first(op$(Op1,P1,A1), op$(Op2,P2,A2), MaxP) ->
       compress_stack(EvalSt, P1, EvalSt2),
       MaxP2 = P1
       ;
       EvalSt2 = EvalSt,
      ( P1 > MaxP -> MaxP2 = P1; MaxP2 = MaxP) ),
   !,
   reduce(Z, op$(Op1,P1,A1), MaxP2, [op$(Op1,P1,A1)|EvalSt2], E).
reduce([pa$ren(EL)|Z], LastOp, MaxP, EvalSt, E) :-
   !,
   reduce([EL|Z], LastOp, MaxP, EvalSt, E).  % EL already reduced
reduce([A|Z], LastOp, MaxP, EvalSt, E) :-
   !,
   reduce(Z, LastOp, MaxP, [A|EvalSt], E).

% pop the stack first if the new operator
% is a higher precedence infix, or if its
% a higher precedence postfix, or if its
% an equal precedence yfx.  prefix ops
% just put on no matter what precedence.

pop_first(op$(_,P1,A1), op$(_,P2,A2), MaxP) :-
   is_infix(A1),
   P1 > P2,
   !.
pop_first(op$(_,P1,A1), op$(_,P2,A2), MaxP) :-
   is_infix(A1),
   P1 < P2,
   !, fail.
pop_first(op$(_,P1,yfx), _, _) :-
   !.
pop_first(op$(_,P1,A1), _, MaxP) :-
   is_postfix(A1),
   P1 > MaxP,
   !.

% compress the stack down to MaxP, that is, we've had an operator
% that will swallow up everything on the stack up until an
% equal or higher operator.
compress_stack([], _, []) :-  % might happen the first time around
   !.
compress_stack([A], _, [A]) :-  % normal end condition
   !.
compress_stack([X,op$(Op,P,A),Y|Z], MaxP, SS) :-
   nonvar(P),   % op$ structure not unified with a var
   P =< MaxP,
   oparg(X),
   oparg(Y),
   is_infix(A),
   E =.. [Op,Y,X],
   !,
   compress_stack([E|Z], MaxP, SS).
compress_stack([X,op$(Op,P,A)|Z], MaxP, SS) :-
   nonvar(P),   % op$ structure not unified with a var
   P < MaxP,
   oparg(X),
   is_prefix(A),
   E =.. [Op,X],
   !,
   compress_stack([E|Z], MaxP, SS).
compress_stack([OP|Z], MaxP, SS) :-
   is_op(OP),
   extract_postfixes([OP|Z], MaxP, E, Z1),
   !,
   compress_stack([E|Z1], MaxP, SS).
compress_stack(S, _, S).

extract_postfixes([X|Z], _, X, Z) :-
   oparg(X),
   !.
extract_postfixes([op$(Op,P,A)|Z], MaxP, E, Z2) :-
   P >= MaxP,
   !,
   fail.
extract_postfixes([op$(Op,P,A)|Z], MaxP, E, Z2) :-
   is_postfix(A),
   E =.. [Op,E2],
   !,
   extract_postfixes(Z, MaxP, E2, Z2).

%is_op(X) :- nonvar(X), X = op$(_,_,_).
is_op(OP) :- functor(OP, op$, 3).

oparg(A) :- var(A), !.
oparg(op$(_,_,_)) :- !, fail.
oparg(_).

is_infix(xfx).
is_infix(xfy).
is_infix(yfx).

is_postfix(xf).
is_postfix(yf).

is_prefix(fy).
is_prefix(fx).

% Operators

ainfix(Op,P,A) -->
   aatom(term,Op),
   { current_op(P,A,Op),
      is_infix(A) }.

aprefix(Op,P,A) --> aatom(term,Op), { current_op(P,A,Op), is_prefix(A) }.

apostfix(Op,P,A) --> aatom(term,Op), { current_op(P,A,Op), is_postfix(A) }.

peek_exp_end(Type, DCG, DCG) :-
   exp_end(Type, DCG, _),
   !.

exp_end(goal) --> ".".
exp_end(goal) --> ",".
exp_end(goal) --> ";".
exp_end(goal) --> ":-", non_graphic.
exp_end(goal) --> "->", non_graphic.
exp_end(goal) --> "-->", non_graphic.
exp_end(goal) --> ")".
exp_end(goal) --> "}".
exp_end(goal) --> "]".

%exp_end(arg) --> ",", [C], { not(is_graphic(C)) }.
exp_end(arg) --> ",".
exp_end(arg) --> ")".

exp_end(term) --> ")".
exp_end(term) --> "}".

exp_end(list_elem) --> ",".
exp_end(list_elem) --> "|".
exp_end(list_elem) --> "]".

% Lists

alist(L, Vin, Vout) -->
   "[", wh,
   aelems(L, Vin, Vout),
   !,
   wh.
alist(Qs, V, V) -->
   { current_prolog_flag(double_quote_strings, off) },
   quoted_chars(0'", Qs).   % "

aelems([], V, V) -->
   "]",
   !.
aelems([E|T], Vin, Vout) -->
   aterm(list_elem, E, Vin, Vx),
   !,
   atail(T, Vx, Vout).
aelems(_, _, _) -->
   parse_error(`Bad list`).

atail([], V, V) -->
   "]", wh,
   !.
atail(T, Vin, Vout) -->
   "|", wh,
   !,
   atailterm(T,Vin,Vout),
   wh,
   "]", wh,
   !.
atail(T, Vin, Vout) -->
   ",", wh,
   !,
   aelems(T, Vin, Vout), wh.
atail(_, _, _) -->
   parse_error(`Bad tail of list`).

atailterm(T, Vin, Vout) -->
   avar(T, Vin, Vout),
   !.
atailterm(T, Vin, Vout) -->
   alist(T, Vin, Vout),
   !.
atailterm(T, Vin, Vout) -->
%   warning(`list tail not a list`),
%   wh,
   aterm(list_elem, T, Vin, Vout),
   !.
atailterm(_, _, _) -->
   parse_error(`Bad list tail`).


% Variables

avar(V,Vin,Vout) --> var_chars(L), { get_var(L, V, Vin, Vout) }.

var_chars([C|Cs]) --> var_init(C), var_rest(Cs).

var_init(C) --> [C], { is_var_init(C) }.

var_rest([C|Cs]) --> [C], { is_var_char(C) }, !, var_rest(Cs).
var_rest([]) --> [].

is_var_init(C) :- C == 0'_, !.
is_var_init(C) :-
   current_prolog_flag( upper_case_atoms, off ),
   is_upper(C),
   !.
is_var_init(C) :-
   current_prolog_flag( vba, on ),
   C == 0'?.

is_var_char(C) :- ( is_lower(C); is_upper(C); is_digit(C); is_ok_text(C) ).

get_var(L, V, VarListIn, VarListOut) :-
   string_list(S, L),
   string_term(S, Vx),
   atom_codes(VName, L),
   var(Vx),
   update_varlist(VName, Vx, V, VarListIn, VarListOut),
   !.
   

update_varlist('_', V, V, VarList, ['$VAR'('_') = V|VarList]).
update_varlist(VName, _, V, VarList, VarList) :-
   member$('$VAR'(VName) = V, VarList).
update_varlist(VName, V, V, VarList, ['$VAR'(VName) = V|VarList]).

% primitives

aprimitive(Context,T) --> aatom(Context,T), !.
aprimitive(_,T) --> anumber(T), !.
aprimitive(_,T) --> astring(T), !.
aprimitive(_,T) --> apointer(T), !.

% strings

astring(S) --> string_chars(Cs), { string_list(S,Cs) }.

string_chars(Cs) --> quoted_chars(0'`, Cs), !.    % `
string_chars(Cs) --> quoted_chars(0'$, Cs), !.    % $
string_chars(Cs) -->
   { current_prolog_flag(double_quote_strings, on) },
   quoted_chars(0'", Cs),
   !.

% Numbers

anumber(N) -->
   asign(C),
   !,
   anumber_chars(Cs),
   { string_list(S,[C|Cs]), string_term(S,N) },
   next_non_number.
anumber(N) -->
   anumber_chars(Cs),
   { string_list(S,Cs), string_term(S,N) },
   next_non_number.

next_non_number -->
   [C],
   { (is_digit(C); is_letter(C)) },
   parse_error(`bad number`).
next_non_number --> [].

asign(0'-) --> "-", !.
asign(0'+) --> "+".

anumber_chars([0'0, C|Cs]) -->
   [0'0, C],
   { is_base(C) },
   !,
   based_chars(C,Cs).
anumber_chars([C|Cs]) -->
   [C],
   { is_digit(C) },
   dec_digits(Ds),
   more_number_chars(MDs),
   { append$(Ds, MDs, Cs) }.

based_chars(0'', [C]) --> [C], !.
based_chars(0'x, Cs) --> !, hex_digits(Cs).
based_chars(0'X, Cs) --> !, hex_digits(Cs).
based_chars(0'w, Cs) --> !, hex_digits(Cs).
based_chars(0'W, Cs) --> !, hex_digits(Cs).
based_chars(0'b, Cs) --> !, bin_digits(Cs).
based_chars(0'B, Cs) --> !, bin_digits(Cs).
based_chars(0'o, Cs) --> !, oct_digits(Cs).
based_chars(0'O, Cs) --> !, oct_digits(Cs).

hex_digits([C|Cs]) --> [C], { is_hex(C) }, !, hex_digits(Cs).
hex_digits([]) --> [].

bin_digits([C|Cs]) --> [C], { is_bin(C) }, !, bin_digits(Cs).
bin_digits([]) --> [].

oct_digits([C|Cs]) --> [C], { is_oct(C) }, !, oct_digits(Cs).
oct_digits([]) --> [].
   
dec_digits([C|Cs]) --> [C], { is_digit(C) }, !, dec_digits(Cs).
dec_digits([]) --> [].

more_number_chars([0'., C|Cs]) -->
   ".",
   [C],
   { is_digit(C) },
   !,
   dec_digits(Ds),
   exponent_chars(MDs),
   { append$(Ds, MDs, Cs) }.
more_number_chars(Cs) -->
   exponent_chars(Cs).

exponent_chars([E,S,D|Cs]) -->
   [E], { (E == 0'e; E == 0'E) },
   asign(S),
   [D], { is_digit(D) },
   !,
   dec_digits(Cs).
exponent_chars([E,S,D|Cs]) -->
   [E], { (E == 0'd; E == 0's) },
   asign(S),
   [D], { is_digit(D) },
   !,
   dec_digits(Cs).
exponent_chars([E|Cs]) -->
   [E], { (E == 0'e; E == 0'E) },
   !,
   dec_digits(Cs).
exponent_chars([C]) -->
   [C], { (C == 0'r; C == 0'R) },
   !.
exponent_chars([C]) -->
   [C], { (C == 0'g; C == 0'G; C == 0'f) },
   !.
exponent_chars([]) --> [].


% Atoms

aatom(term,',') --> ",", !.
aatom(term,';') --> ";", !.
aatom(_, !) --> "!", !.
aatom(Context,A) --> aatom_chars(Context,L), { atom_codes(A,L) }.

aatom_chars(_,[C|Cs]) --> aatom_init(C), aatom_rest(Cs), !.
aatom_chars(_,Cs) --> quoted_chars(0w0027, Cs), !.
aatom_chars(Context,Cs) --> op_chars(Context,Cs).

aatom_init(C) --> [C], { is_atom_init(C) }.

aatom_rest([C|Cs]) --> [C], { is_atom_char(C) }, !, aatom_rest(Cs).
aatom_rest([]) --> [].

op_chars(Context,[C|Cs]) --> [C], { is_graphic(C) }, !, more_op_chars(Context,Cs).

more_op_chars(Context,[C|Cs]) --> [C], { is_graphic(C) }, !, more_op_chars(Context,Cs).
more_op_chars(_,[]) --> [].

is_atom_init(C) :- is_lower(C), !.
is_atom_init(C) :-
   current_prolog_flag(upper_case_atoms, on),
   is_upper(C).

is_atom_char(C) :- ( is_lower(C); is_upper(C); is_digit(C); is_ok_text(C) ).

% Quoted chars, reads from input quote character, Q,
% to next instance of Q.

quoted_chars(Q, Cs) --> [Q], non_quotes(Q, Cs).

non_quotes(Q, [Q|Cs]) --> [Q,Q], !, non_quotes(Q, Cs).
non_quotes(Q, []) --> [Q], !.
non_quotes(Q, [C|Cs]) --> is_escape(Q, C), non_quotes(Q, Cs).
non_quotes(Q, [C|Cs]) --> [C], non_quotes(Q, Cs).
non_quotes(Q, []) --> finished, parse_error([`Unbalanced delimiter: `, Q]).

% Character classification

is_escape(_, _) -->
   { current_prolog_flag(string_esc, off), !, fail }.
is_escape(Q, C) -->
   [0'\],
   escape_code(Q, C),
   !.

escape_code(_, C) --> [0'n], { atom_codes('\n', [C]) }.
escape_code(_, C) --> [0'a], { atom_codes('\a', [C]) }.
escape_code(_, C) --> [0'b], { atom_codes('\b', [C]) }.
escape_code(_, C) --> [0'f], { atom_codes('\f', [C]) }.
escape_code(_, C) --> [0'r], { atom_codes('\r', [C]) }.
escape_code(_, C) --> [0't], { atom_codes('\t', [C]) }.
escape_code(_, C) --> [0'v], { atom_codes('\v', [C]) }.
escape_code(_, C) --> [0'\], { atom_codes('\\', [C]) }.
escape_code(Q, Q) --> [Q].

escape_code(_, C) -->
   oct_digits(Cs),
   [0'\],
   { string_list(S, [0'0, 0'o|Cs]),
     string_term(S, C),
     C =< 0xffff }.
escape_code(_, C) -->
   [0'x],
   hex_digits(Cs),
   [0'\],
   { string_list(S, [0'0, 0'x|Cs]),
     string_term(S, C),
     C =< 0xffff }.

escape_code(Q, C) --> [0'n], { atom_codes('\n', [C]) }.

is_letter(C) :- (is_lower(C); is_upper(C)).

is_lower(C) :-
   C >= 0w0061, C =< 0w007a,
   !.
is_lower(C) :-
   C < 0w00c0,
   !, fail.
is_lower(C) :-
   (C >= 0w00c0, C =< 0w1fff
    ;
    C >  0w3040, C =< 0wd7ff
    ;
    C >= 0we000, C =< 0wffef).
   
is_upper(C) :- C >= 0w0041, C =< 0w005a.

is_digit(C) :- C >= 0w0030, C =< 0w0039.

is_base(C) :-
   is_member(C, "xXwWoObB'").

is_hex(C) :-
   ( C >= 0'0, C =< 0'9
     ;
     C >= 0'a, C =< 0'f
     ;
     C >= 0'A, C =< 0'F ).

is_oct(C) :- C >= 0'0, C =< 0'7.

is_bin(C) :- (C == 0'0; C == 0'1).

is_ok_text(C) :- (C == 0'_; C == 0'$).

% fail if its a graphic, otherwise don't eat the character,
% needed after looking for "->" and the like to make sure it
% isn't a user operator, like "->>"
non_graphic --> [C], { is_graphic(C) }, !, fail.
non_graphic --> [].

is_graphic(C) :-
   is_member(C, "&*+-./:<=>@\\^~"),
   !.
is_graphic(0'#) :-
   current_prolog_flag(preprocessor, off),
   !.
is_graphic(0'?) :-
   current_prolog_flag(vba, off),
   !.
is_graphic(C) :-
   C < 0w00a1,
   !, fail.
is_graphic(C) :-
   (C >= 0w00a1, C =< 0w00bf
    ;
    C >= 0w2010, C =< 0x303f).

% White space

wh --> [C], { is_white(C) }, !, wh.
wh --> is_comment, !, wh.
wh --> [].

is_white(W) :-
   W =< 0w0020,
   !.
is_white(W) :-
   W =< 0w007f,
   !, fail.
is_white(W) :-
   W < 0w00a0,
   !.
is_white(W) :-
   (W >= 0w2000, W =< 0w200f
    ;
    W >= 0wfff0, W =< 0wfffe
    ;
    W == 0wfeff).

% Comments

is_comment --> "%", rest_of_line.
is_comment --> "/*", end_of_comment.

rest_of_line --> [10], !.
rest_of_line --> [13], !.
rest_of_line --> "\n", !.
rest_of_line --> [_], !, rest_of_line.

end_of_comment --> "*/", !.
end_of_comment --> finished, parse_error(`Missing end of comment`).
end_of_comment --> [X], !, end_of_comment.

:- end_body(amzi_system).
