% Events.pro - illustrate date and time fields
% with ODBC

% Load the LSX that provides the extended predicates
% for the ODBC interface.  This could also be done
% from amzi.cfg, or from a Logic Server host language
% program, or by including in the project properties
% of an Eclipse project.

:- loadlsx('aodbc.lsx').

% Make user the Prolog portion of the ODBC library,
% which provides a Prolog-like interface to the
% database, is loaded, and that its predicates are
% imported. This can also be done from the project
% properties of an Eclipse project.

:- ensure_loaded(aodbc).
:- import(aodbc).

main :-
  db_open(events),
  db_query(games,
    [home=H, away=A, day=D, time=T]),
  write(H:A), tab(1),
  write(D), tab(1), write(T), nl,
  fail.
main :-
  db_close.

% defined the fields in the 'games' table, so that
% the Prolog ODBC predicates can generate the
% appropriate SQL.

db_table(games, home, a20).
db_table(games, away, a20).
db_table(games, day, d).
db_table(games, time, t).

