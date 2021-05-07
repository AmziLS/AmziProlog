:- loadlsx('amysql/amysql.lsx').


main :-
   test_mysql.

% Here's how to start and end a session.
test_mysql :-
   mysql_init(`root`, `password`),
   mysql_connect(`ddi`),
   do_tests,
   mysql_close.

% These are tests without a list of types, all values
% come back as strings.
do_tests :-
   test_sql(SQL),
   nl, nl, write(SQL), nl, nl,
   mysql_query(SQL, ROWS),
   write(yes-ROWS), nl,
   answer_report(ROWS),
   fail.
% These are tests with a type list specified, so that
% the values are converted into the appropriate Prolog types.
do_tests :-
   test_sql(SQL, TYPES),
   nl, nl, write(SQL), nl, write(TYPES), nl, nl,
   mysql_query(SQL, ROWS, TYPES),
   answer_report(ROWS),
   fail.
do_tests.

% Use build_sql to create a query with variable test
% criteria.  Create a list of strings for the query and
% insert value(X) wherever the value of a Prolog variable
% needs to be inserted.

test_sql(`select count(*) from primaryname`).
test_sql(`select name, genericID from allname where name = 'metoprolol'`).
test_sql(Q) :-
   NAME = 'Lotrimin',
   build_sql([`select name, genericID from allName `,
              `where name = `, value(NAME)], Q).

test_sql(`select count(*) from primaryname`, [number]).
test_sql(`select name, genericID from allname where name = 'metoprolol'`, [atom, number]).
test_sql(Q, [atom, number]) :-
   NAME = 'Lotrimin',
   build_sql([`select name, genericID from allName `,
              `where name = `, value(NAME)], Q).
% The following test finds the ID of Lotrimin and uses that to find
% other drugs with the same ID.  This could be done in SQL directly,
% but this shows how to use one query to generate input for another.
test_sql(Q, [number, atom]) :-
   NAME = 'Lotrimin',
   build_sql([`select name, genericID from allName `,
              `where name = `, value(NAME)], Q1),
   mysql_query(Q1, Rows1, [atom, number]),
   % Rows1 comes back as [ [`name`, `genericID`], ['Lotrimin', 207] ].
   % We just want the ID for the next query.
   Rows1 = [ _, [ _, ID ] | _ ],
   build_sql([`select genericID, name from allName `,
              `where genericID = `, value(ID)], Q).

test_sql(`delete from route where routeID = 111`).
test_sql(`delete from route where routeID = 112`).
test_sql(`INSERT INTO route (routeID,route,catagory) VALUE (111,'Dennis','den')`).
test_sql(`INSERT INTO route (routeID,route,catagory) VALUE (112,'Mary','mar')`).
test_sql(`select route from route where routeID = 111`).

%--------------------------------------------------------
% These are the support predicates needed to make
% mysql_query work.
%

mysql_query(SQL, ROWS) :-
   mysql_query(SQL, ROWS, []).
   
mysql_query(SQL, ROWS, TYPES) :-
  mysql_reverse_query(SQL, RevROWS),
  reverse_rows(RevROWS, TYPES, [], ROWS).

reverse_rows([], _, _, []) :-
  !.
reverse_rows([HEADERS], _, ROWS, [HEADERS|ROWS]) :-
  !.
reverse_rows([RR|RRs], TYPES, SoFar, ROWS) :-
  type_conversions(RR, TYPES, R),
  !,
  reverse_rows(RRs, TYPES, [R|SoFar], ROWS).
  
type_conversions(ROW, [], ROW) :-
  !.
type_conversions([S|Ss], [Type|Types], [T|Ts]) :-
  type_convert(Type, S, T),
  !,
  type_conversions(Ss, Types, Ts).

type_convert(atom, S, T) :- string_atom(S, T), !.
type_convert(string, S, S) :- !.
type_convert(number, S, T) :- string_number(S, T), !.
type_convert(a, S, T) :- string_atom(S, T), !.
type_convert(s, S, S) :- !.
type_convert(n, S, T) :- string_number(S, T), !.
type_convert(_, S, S).

% The answer is a list of lists, with the first list
% the headers and the other lists the rows.  Write them
% on successive lines.

answer_report([]) :-
  nl, nl, !.
answer_report([X|Z]) :-
  writeq(X),
  nl,
  answer_report(Z).

% Take an input list and create an SQL text string.

build_sql(List, Text) :-
   sql_stringize(List, StringList),
   stringlist_concat(StringList, Text).

% stringize/2 converts a list of terms to a list of
% strings/atoms that can be concatenated using
% stringlist_concat.  Note the addition of the `'`
% for text as the result is intended for use in SQL.

sql_stringize([], []) :-
   !.
sql_stringize([value(X)|Xs], [`'`, X, `'`|Ss]) :-  % quote text
   (atom(X); string(X)),
   !,
   sql_stringize(Xs, Ss).
sql_stringize([value(X)|Xs], [S|Ss]) :-   % numbers as is
   number(X),
   string_term(S, X),
   !,
   sql_stringize(Xs, Ss).
sql_stringize([value(X)|Xs], [`'`, S, `'`|Ss]) :-  % quote whatever else
   string_term(S, X),
   !,
   sql_stringize(Xs, Ss).
sql_stringize([X|Xs], [X|Ss]) :-
   (string(X); atom(X)),
   !,
   sql_stringize(Xs, Ss).
sql_stringize([X|Xs], [S|Ss]) :-
   string_term(S, X),
   !,
   sql_stringize(Xs, Ss).
