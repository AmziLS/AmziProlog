%----------------------------------------------------------
% traffic3.pro - a small sample of a customized rule-based
%   system shell that illustrates the use of alternate
%   Prolog syntax to make more readable rules.
%

% These operator definitions allow the rules to be
% written without the strict functor(arg1, arg2)
% syntax normally used.

:- op(790, fx, if).
:- op(780, xfx, then).
:- op(770, xfy, and).
:- op(730, xfx, else).
:- op(700, xfx, is).  % what it was
:- op(700, xfx, oneof).
:- op(690, fx, not).  % less than what it was

% These are the rules of the system.  Typically these rules
% would be in a separate file that is consulted after the 
% inference engine, below, is loaded.  The file with the 
% inference engine would also contain the operator definitions
% so they don't have to be included with the rules.
%
% The rules are goal-driven rules using simple attribute
% value pairs for representing information.  The rule base
% is queried from Prolog with the goal ?- prove(action is X).
%
% upper_case_atoms & double_quote_strings
%
% These mode settings tell the Prolog reader to use
% an alternate syntax reading terms.  _ only for variables
% (upper case does NOT signify a variable), 
% " for string delimiters, and no case restriction on atoms.
%
% By setting these in main/0, the reader uses that syntax
% for reading the knowledge base, and for reading user input
% at the command line.
%
% In this example, it means Boston can be entered without
% quotes, and is an atom.  In the knowledge base, the ""
% indicates strings for answers.
%
% This version of traffic also supports logical variables
% in the rules, and they are entered in the rules using
% the _varname syntax for variables.
%
% It also supports if-then-else rules.

% The main loop for the program.

main :-
  set_mode(upper_case_atoms, on),
  set_mode(double_quote_strings, on),
  loop,
  % turn them off in case returning to
  % listener environment
  set_mode(upper_case_atoms, off),
  set_mode(double_quote_strings, off).

loop :-
  do(help),
  repeat,
  write('command> '),
  read_string(S),
  string_term(S,COMMAND),
  do(COMMAND),
  COMMAND == quit.

do(help) :-
  write('Commands: help, quit, load, go.'), nl,
  write('First load the knowledge base, then go.'), nl,
  !.
do(quit) :- !.
do(load) :-
  reconsult('traffic3.kb'),
  !.
do(go) :-
  retractall(known(_,_)),
  prove(action is X),
  write(X), nl,
  !.
do(X) :-
  write('Bad command':X), nl.

% The rest of the code is the custom inference engine.  Given
% the goal of 'proving' an attribute value pair, the system
% looks for rules with a 'then' side that provides a value for
% the attribute.  If it finds one, then it attempts to prove
% the subgoals on the 'if' side of the rule.
%
% If there are no rules for finding the value of an attribute,
% then the user is asked for a value.

% prove/1 is the guts of the inference engine.  It recognizes these
% cases.
% 1) There is a list of subgoals to prove, separated by 'and's.
%    In that case, call getav to see if the first is true, and,
%    if so prove the rest.
% 2) There is a single goal, call getav to see if its true.
% 3) A oneof operator that calls member.  It's easy to add
%    other features to the rules like this.

prove( Attr is Value and Rest ) :-
   getav(Attr, Value),
   prove(Rest).
prove( Attr is Value ) :-
   getav(Attr, Value).
prove( Value oneof List ) :-
   member(Value, List).

% getav/2 gets the values of attributes in two different ways.
% 1) If there is a rule defining this attribute, only use the
%    the rule to get its value.
% 2) If there is no rule, then its fair to ask the user for
%    the value.

% Prolog note: the line 'if Conditions ...' is a Prolog statement
% simply looking for that pattern.  If it finds one, the variables
% are unified appropriately.  If the subsequent prove( Conditions )
% fails, then Prolog backtracking will return to the 'if ...'
% goal and find the next rule that matches and try again.

% In this version of traffic, we added support for else
% after a then.  So we have two rule patterns to look for
% before deciding to ask the user.

% This version of traffic remembers the results of all
% inferences, so it asserts known/2 facts for each attribute
% it either deduces or learns from asking the user.

getav( Attr, Value ) :-
   known(Attr, X),
   !,
   X = Value.
getav( Attr, Value ) :-
   if Conditions then Attr is X,
   trace(trying,failing,if Conditions then Attr is X),
   prove( Conditions ),
   trace(proved,abandoned,Conditions),
   assert( known(Attr,X) ),
   !,
   X = Value.
getav( Attr, Value ) :-
   if Conditions then Attr is X else Attr is Y,
   trace(trying,failing,if Conditions then Attr is X else Attr is Y),
   ( prove( Conditions ) ->
       trace(proved,Conditions),
       assert( known(Attr,X) )
       ;
       trace(disproved,Conditions),
       assert( known(Attr,Y) ) ),
   !,
   known(Attr,Value).
getav( Attr, Value) :-
   ask( Attr, X ),
   assert( known(Attr,X) ),
   !,
   Value = X.

trace(T,_,X) :-
   write('TRACE '),
   writeq(T:X), nl.
trace(_,F,X) :-
   write('TRACE '),
   writeq(F:X), nl, fail.

trace(T,X) :-
   write('TRACE '),
   writeq(T:X), nl.

% Prolog note: ask/2 can be used in two different ways.
% 1) When Val is a variable, it means 'what is the value of
%    this attribute?'
% 2) When Val is bound to some value, it means 'is this the
%    value of the attribute?'

ask(Attr, Val) :-
  write('What is the value of '),
  write(Attr), write('? '),       % ask the user
  read_string(S),                 % get the answer
  string_term(S,Val).

member(A, [A|_]).
member(A, [_|Z]) :- member(A,Z).
 