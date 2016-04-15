%-------------------------------------------------------------
% CATCH.PRO
%
% This sample code illustrates the use of catch/throw in
% error handling.
%
% Two predicates, catch/3 and throw/1, are a general purpose
% mechanism for handling user-defined exceptions in Prolog
% code.  The two key predicates' arguments are:
%
% catch(Goal, Catcher, Recover) -
%    Goal    - is a normal Prolog goal to be proved
%    Catcher - is a term used as a pattern to be checked
%              against possible thrown exceptions
%    Recover - is a goal to be proved if a thrown exception
%              is caught.
%
% throw(Term) -
%    Term - a term that is used in a search for a matching
%           catch term
%
% catch(G,C,R) is fully equivalent to 'call(G)' for the normal
% case where no exception is thrown during the execution of
% goal G.  If a 'throw(C)' is encountered, then the catch/3
% goal is replaced with the goal 'call(R)' instead.
%
% The catch/throw pair allow the flow of control to skip over
% all of the intermediate predicates, which is especially
% useful for dealing with error exceptions.
%
% Throw will only succeed if the term thrown matches an
% active catch term.  After the goal of the catch is satisfied
% the catch term can no longer be thrown to.  An attempt to
% throw an uncaught term results in a system error.
%
% ISO conformance: Amzi! catch/throw is ISO conformant.
%

main :-
  % Catch and process all throws not handled by subsequent
  % catches, including throw(quit) used to end the program.
  catch(doit, X, error_handler(X)).

error_handler(quit) :-
  write($Quitting\n$).
error_handler(X) :-
  write($Unknown Error$ : X), nl.

doit :-
  repeat,
  write($Enter one or done or something else: $),
  read_string(S),
  string_atom(S, A),
  catch(do(A), badcommand(X), (write($Bad Command$:X),nl)),
  fail.

do(one) :-
  catch(do_one, except(X), except(X)), !.
do(done) :-
  throw(quit).
do(X) :-
  throw(badcommand(X)).

except(notinteger:X) :-
  write(X), write($ not an integer\n$).
except(toosmall:X) :-
  write(X), write($ is too small\n$).
except(toobig:X) :-
  write(X), write($ is too big\n$).

do_one :-
  repeat,
  write($Enter a number between 10 and 20,\n$),
  write($'q' to quit, or anything else to see an error:\n$),
  read_string(S),
  string_term(S,T),
  check(T),
  fail.

check(q) :- throw(quit).
check(X) :-
  not(integer(X)),
  throw(except(notinteger:X)).
check(X) :-
  X > 10,
  X < 20,
  !, write($Very good\n$).
check(X) :-
  X =< 10,
  throw(except(toosmall:X)).
check(X) :-
  X >= 20,
  throw(except(toobig:X)).
check(X) :-
  throw(badcheck(X)).
