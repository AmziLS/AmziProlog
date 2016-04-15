%----------------------------------------------------------
% traffic1.pro - a small sample of a customized rule-based
%               system shell
%
% Based on the ideas in Building Expert Systems in Prolog
%

% These operator definitions allow the rules to be
% written without the strict functor(arg1, arg2)
% syntax normally used.
% For example, the first traffic_light rule would have
% to be written as:
%   if(then(is(traffic_light,green),is(action,proceed))).
% without the operator definitions.  Both forms are
% equivalent as far as Prolog is concerned.

:- op(790, fx, if).     % prefix operator
:- op(780, xfx, then).  % infix operator
:- op(770, xfy, and).   % infix that can be linked
:- op(700, xfx, is).    % what it was
:- op(690, fx, not).    % less than what it was

% These are the rules of the system.  Typically these rules
% would be in a separate file that is consulted after the 
% inference engine, below, is loaded.  The file with the 
% inference engine would also contain the operator definitions
% so they don't have to be included with the rules.
%
% The rules are goal-driven rules using simple attribute
% value pairs for representing information.  The rule base
% is queried from Prolog with the goal ?- prove(action is X).

if traffic_light is green
then action is proceed.

if traffic_light is red
then action is stop.

if traffic_light is yellow and
   driving is crazy
then action is accelerate.

if traffic_light is yellow and
   driving is sane
then action is stop.

if city is 'Boston'
then driving is crazy.

if city is not 'Boston'
then driving is sane.

% A place to start for testing.  Simply a convenience to see
% if the system works.

main :-
  retractall(known(_,_)),
  prove(action is X),
  write(action is X), nl.

% The rest of the code is the custom inference engine.  Given
% the goal of 'proving' an attribute value pair, the system
% looks for rules with a 'then' side that provides a value for
% the attribute.  If it finds one, then it attempts to prove
% the subgoals on the 'if' side of the rule.
%
% If there are no rules for finding the value of an attribute,
% then the user is asked for a value.

% prove/1 is the guts of the inference engine.  It recognizes three
% cases.
% 1) There is a list of subgoals to prove, separated by 'and's.
%    In that case, call getav to see if the first is true, and,
%    if so prove the rest.
% 2) There is a single goal with a negation.  This will only
%    happen if the Value is bound to a value, in which case
%    simply ask to see if the Value is correct, and negate.
% 3) There is a single goal, call getav to see if its true.

prove( Attr is Value and Rest ) :-
   getav(Attr, Value),
   prove(Rest).
prove( Attr is not Value) :-
   atomic(Value),
   not(ask(Attr, Value)).
prove( Attr is Value ) :-
   getav(Attr, Value).

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

getav( Attr, Value ) :-
   if Conditions then Attr is X,
   Value = X,
   prove( Conditions ).
getav( Attr, Value) :-
   not(if _ then Attr is _),
   ask( Attr, Value ).

% Prolog note: ask/2 can be used in two different ways.
% 1) When Val is a variable, it means 'what is the value of
%    this attribute?'
% 2) When Val is bound to some value, it means 'is this the
%    value of the attribute?'

ask(Attr, Val) :-
  known(Attr, X),   % is there a known value for this attribute?
  !,                % if so don’t ask again
  X = Val.          % succeed or fail based on the expected value.
ask(Attr, Val) :-
  write('What is the value of '),
  write(Attr), write('? '),       % ask the user
  read(X),                        % get the answer
  assert( known( Attr, X ) ),     % remember it
  X = Val.                        % succeed or fail based on the value

