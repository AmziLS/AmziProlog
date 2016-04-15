% test.pro
%
% This file contains various predicates used by the API test
% files.

hello(X, prolog).

% main/0 asserts some dynamic predicates.
% parent(X,Y) means X is the parent of Y.

main :-
  assert(parent(elaine, mary)),
  assert(parent(elaine, kathy)),
  assert(parent(elaine, ricky)),
  assert(parent(elaine, jenny)),
  assert(female(elaine)),
  assert(female(mary)),
  assert(female(kathy)),
  assert(female(jenny)),
  assert(male(ricky)).

sibling(X, Y) :-
  parent(M, X),
  parent(M, Y),
  X \= Y.

% sister(X,Y) means X is the sister of Y.
% brother(X,Y) means X is the brother of Y.

sister(X, Y) :-
  sibling(X, Y),
  female(X).

brother(X, Y) :-
  sibling(X, Y),
  male(X).

% The following two predicates test custom extended
% predicates that are implemented in the Delphi sample
% application.  They both provide simple Windows
% services, being a yes/no message box and an input
% box, directly to the Prolog program.

% yesno/1 tests the extended predicate tfmessagebox/1,
%   which succeeds or fails based on user reaction.

yesno($You selected yes$) :-
  tfmessagebox($Succeed or Fail$).
yesno($You selected no$).

% yourname/1 tests the extended predicate inputbox/2,
%   which puts up an edit box for the user to enter
%   a value, which is then unified with the second
%   argument.

yourname(X) :-
  inputbox('Enter your name', X).