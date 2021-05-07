/* LSODBC.PRO - Prolog covers for the ODBC
   extended predicates.

   db_open/1,2,3
   db_close/0
   db_execquery/5
   db_getnext/2
   db_getonce/2
   db_query/2
   db_queryonce/2
   */

:- module(aodbc).

:- export([
   db_open/1,
   db_open/2,
   db_open/3,
   db_close/0,
   db_execquery/5,
   db_getnext/2,
   db_getonce/2,
   db_query/2,
   db_query/4,
   db_queryonce/2,
   db_queryonce/4 ]).



/* The open and close predicates */

db_open(DBSource) :-
  db$open(DBSource, '', '').
db_open(DBSource, UserID) :-
  db$open(DBSource, UserID, '').
db_open(DBSource, UserID, Password) :-
  db$open(DBSource, UserID, Password).

db$open(DBSource, UserID, Password) :-
  db_init,
  retractall(db$hconnect(_)),
  db_connect(H, DBSource, UserID, Password),
  assert(db$hconnect(H)).

db_close :-
  retract(db$hconnect(H)),
  db_freeall,
  db_disconnect(H),
  db_free.

% db_query versions - for pre-built SQL queries.
%    SQL - the SQL query
%    Input - a list of input values to variables in query
%    Format - the formats of output, choices are `sN`, `i`, `d`, or `f`.
%    Output - a list of variables to be bound with answers. 
%
% ?- db_query(person, [pid=P, surname='Bear', name=N]).
% P = 5
% N = `Phillip` ;
% 
% P = 13
% N = `Edward` 
% 
% ?- db_query(`select pid, name from person where surname = ?`, ['Bear'], [i, s40], [PID, NAME]).
% 
% PID = 5
% NAME = `Phillip` ;
% 
% PID = 13
% NAME = `Edward` ;
% 

db_queryonce(SQL, Input, Format, Output) :-
  db$hconnect(HC),
  db_query(HC, Q, SQL, Input, Format),
  db_fetch(Q, X),
  db$next(Q, X, Output),
  !,
  db_freeq(Q).
  
db_query(SQL, Input, Format, Output) :-
  db$hconnect(HC),
  db_query(HC, Q, SQL, Input, Format),
  db_fetch(Q, X),
  db$next(Q, X, Output).


/* Build an SQL query from a Prolog query using field=val
   pairs.  For example, to get the date of birth and birthplace
   based on someone's first and last name from the sample
   dbgene database, the following query would be used

   ?- db_query(person,
        [dob=BDay, birthplace=BP, name='John', surname='Doe']).

   db_query/2 will backtrack over all solutions.
   */

db_query(Table, QueryList) :-
  db$build_query(Table, QueryList, SQL, Input, Format, Output),
  db$hconnect(HC),
  db_query(HC, Q, SQL, Input, Format),
  db_fetch(Q, X),
  db$next(Q, X, Output).

db$next(Q,Output, Output).
db$next(Q, _, Output) :-
  db_fetch(Q, X),
  db$next(Q, X, Output).

db_queryonce(Table, QueryList) :-
  db$build_query(Table, QueryList, SQL, Input, Format, Output),
  db$hconnect(HC),
  db_query(HC, Q, SQL, Input, Format),
  db_fetch(Q, X),
  db$next(Q, X, Output),
  !,
  db_freeq(Q).

/* Build an SQL query from the Prolog attr=val list form of query */

db$build_query(T, QL, SQL, In, Form, Out) :-
  db$bq(T, QL, SelList, WhList, In, Form, Out),
  fix$where(WhList, WhClause),
  fix$select(T, SelList, SelClause),
  append(SelClause, WhClause, SQLList),
  string$words(SQL, SQLList).

/* Use either '=' or ':' operator to separate fields and
   values.  ':' will be preferred in future.
   */

db$bq(T, [], [], [], [], [], []).
db$bq(T, [Field:Val|Rest],
    [Field|SelList], WhList, In, [F|Form], [Val|Out]) :-
  var(Val),
  db_table(T, Field, F),
  db$bq(T, Rest, SelList, WhList, In, Form, Out).
db$bq(T, [Field:Val|Rest],
    SelList, [Field,' = ','?' |WhList], [Val|In], Form, Out) :-
  nonvar(Val),
  db$bq(T, Rest, SelList, WhList, In, Form, Out).
db$bq(T, [Field=Val|Rest],
    [Field|SelList], WhList, In, [F|Form], [Val|Out]) :-
  var(Val),
  db_table(T, Field, F),
  db$bq(T, Rest, SelList, WhList, In, Form, Out).
db$bq(T, [Field=Val|Rest],
    SelList, [Field,' = ','?' |WhList], [Val|In], Form, Out) :-
  nonvar(Val),
  db$bq(T, Rest, SelList, WhList, In, Form, Out).

fix$select(T, [], ['select null from ', T]).
fix$select(T, Sel, ['select '|FixSel]) :-
  fix$sel(T, Sel, FixSel).

fix$sel(T, [F], [F, ' from ', T]) :- !.
fix$sel(T, [F|Rest], [F, ', '|FRest]) :-
  fix$sel(T, Rest, FRest).

fix$where([], []).
fix$where(Wh, [' where '|FixWh]) :-
  fix$wh(Wh, FixWh).

fix$wh([F,' = ','?'], [F,' = ','?']) :- !.
fix$wh([F,' = ','?' |Rest], [F, ' = ', '?', ' and '|FRest]) :-
  fix$wh(Rest, FRest).
 
string$words(S, Ws) :-
  atomlist_concat(Ws, BigAtom),
  string_atom(S, BigAtom).

append([], X, X).
append([H|X], Y, [H|Z]) :- append(X,Y,Z).

:- end_module(aodbc).


