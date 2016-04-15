%----------------------------------------------------------
% traffic2.pro - a small sample of a customized rule-based
%                system shell
%
% This system uses a data-driven or forward-chaining engine.
% The goal-driven engine in traffic1.pro is probably more
% natural for this particular problem, but this code illustrates
% how its just as easy to build a simple data-driven engine, that,
% in this case, uses the same rule format.
%
% To start the program:  ?- main.
%
% Based on the ideas in Building Expert Systems in Prolog
%

% These operator definitions allow the rules to be
% written without the strict functor(arg1, arg2)
% syntax normally used.
%
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
% These are the same rules in traffic1.pro, using simple
% attribute-value pairs for representing data.  The rules
% are interpreted in a data-driven manner in this program.
% This means the values for traffic_light and city are
% asked for first, and then that data is used to eventually
% generate the resulting action.

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

% The place to start.  Since this is a forward-chaining data-driven
% system, we start by gathering all the data.

main :-
  retractall(known(_,_)),
  ask(traffic_light, _),
  ask(city, _),
  go,
  known(action,X),
  write(action is X), nl.

% The rest of the code is the custom inference engine.  The data-
% driven engine is often called a rewrite engine, or production
% engine.  The engine looks for a rule whose 'if' side matches
% some of the known facts.  It then 'rewrites' the data, erasing
% the known facts that caused the rule to fire, and writes the
% facts stated in the 'then' side of the rule.
%
% The cycle then repeats, and, since the known facts have changed,
% a new rule will fire next time.  When no more rules fire, the
% system is done, and whatever is in the known facts is the answer.

% Prolog note: the line 'if Conditions ...' is a Prolog statement
% simply looking for that pattern.  If it finds one, the variables
% are unified appropriately.  If the subsequent matchall(Conditions)
% fails, then Prolog backtracking will return to the 'if ...'
% goal and find the next rule and try again.  When no more rules
% fire, the cycle is finished.

go :-
  if Conditions then Actions,
  matchall(Conditions),
  take(Actions),
  clearall(Conditions),
  go.
go.

% Note that the handling of 'not' is a bit of a problem.  The precise
% definition of 'not' for this system is that there is a value for
% the given attribute, but that value is not the value specified.
% This definition makes it possible to retract the value of known
% for that attribute so the rule won't file again.  (Otherwise the
% last rule, with just a not in the 'if' side, will fire forever.)

matchall(Condition and Rest) :-
  !,
  match(Condition),
  matchall(Rest).
matchall(Condition) :-
  match(Condition).

  match(Attr is not Val) :-
    !,
    known(Attr,X),
    not(Val=X).
  match(Attr is Val) :-
    known(Attr,Val).

take(Attr is Val and Rest) :-
  !,
  assert(known(Attr,Val)),
  take(Test).
take(Attr is Val) :-
  assert(known(Attr,Val)).

clearall(Condition and Rest) :-
  !,
  clear(Condition),
  clearall(Rest).
clearall(Condition) :-
  clear(Condition).

  clear(Attr is not Val) :-
    !,
    retract(known(Attr,_)).
  clear(Attr is Val) :-
    retract(known(Attr,Val)).

% Prolog note: ask/2 can be used in two different ways.
% 1) When Val is a variable, it means 'what is the value of
%    this attribute?'
% 2) When Val is bound to some value, it means 'is this the
%    value of the attribute?'

ask(Attr, Val) :-
  known(Attr, X),   % is there a known value for this attribute?
  !,                % if so don't ask again
  X = Val.          % succeed or fail based on the expected value.
ask(Attr, Val) :-
  write('What is the value of '),
  write(Attr), write('? '),       % ask the user
  read(X),                        % get the answer
  assert( known( Attr, X ) ),     % remember it
  X = Val.                        % succeed or fail based on the value

