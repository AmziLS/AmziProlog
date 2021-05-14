%----------------------------------------------------------------------
% DBGENE.PRO - genealogical relationships
%
% A version of the genealogical application that uses a database to
% store the basic data about people.
%
%   
% To use it, you must first open a connection and a database.  The sample
% gene.mdb is a Microsoft Access database with some people in it.  You
% can create your own with any database you can access through ODBC.
%


%----------------------------------------------------------------------
% Prolog front end
%

:- import(aodbc).

main :-
  write($Enter Database Name: $),
  read_string(DB),
  db_open(DB),
  command_loop.

command_loop :-
  display_help,
  repeat,
  write($> $),
  read(T),
  once( do(T) ),
  T == quit.

display_help :-
  write($Enter Prolog queries (ending with .): $), nl,
  write($  people - get a list of people$), nl,
  write($  relationships - get a list of relationships$), nl,
  write($  R(P1,P2) - a Prolog query of this form$), nl,
  write($      Each person can be represented as:$), nl,
  write($        ID - the integer ID number for the person$), nl,
  write($        First or surname$), nl,
  write($        [Surname, Firstname]$), nl,
  write($      Instantiated the second argument first.$), nl,
  write($      Examples:$), nl,
  write($        sister(X, 'Jane').$), nl,
  write($        ancestor(X, ['Doe', 'Jane']).$), nl,
  write($        father(4, 5).$), nl,
  write($  relation(R,P1,P2) - find relationship between$), nl,
  write($      two people. (Might take a while) Ex:$), nl,
  write($        relation(R, 'Jane', 'Harvey').$), nl,
  nl.

do(relationships) :-
  relations(Rs),
  write_list(Rs).
do(people) :-
  list_people.
do(help) :-
  display_help.
do( relation(R,P1,P2) ) :-
  get_pid(P1, Pid1),
  get_pid(P2, Pid2),
  Q2 =.. [relation,R,Pid1,Pid2],
  call(Q2),
  fullname(Pid1, Sur1, Mid1, Nm1),
  fullname(Pid2, Sur2, Mid2, Nm2),
  write_line(
    [Nm1, $ $, Mid1, $ $, Sur1,
     $ is $, R, $ of $,
     Nm2, $ $, Mid2, $ $, Sur2]), 
  nl,
  fail.
do(Q) :-
  Q =.. [R, P1, P2],
  get_pid(P1, Pid1),
  get_pid(P2, Pid2),
  Q2 =.. [R, Pid1, Pid2],
  call(Q2),
  fullname(Pid1, Sur1, Mid1, Nm1),
  fullname(Pid2, Sur2, Mid2, Nm2),
  write_line(
    [Nm1, $ $, Mid1, $ $, Sur1,
     $ is $, R, $ of $,
     Nm2, $ $, Mid2, $ $, Sur2]),
  nl,
  fail.
do(quit).

write_line([]).
write_line([H|T]) :-
  write(H),
  write_line(T).

write_list([]).
write_list([H|T]) :-
  write(H), nl,
  write_list(T).

query(Q, AnsS) :-
  Q =.. [R, P1, P2],
  get_pid(P1, Pid1),
  get_pid(P2, Pid2),
  Q2 =.. [R, Pid1, Pid2],
  call(Q2),
  fullname(Pid1, Sur1, Mid1, Nm1),
  fullname(Pid2, Sur2, Mid2, Nm2),
  Ans =.. [R, (Nm1, Mid1, Sur1), (Nm2, Mid2, Sur2)],
  string_term(AnsS, Ans).

query(Q, Pid1, Sur1, Mid1, Nm1) :-
  Q =.. [R, P1, P2],
  get_pid(P1, Pid1),
  get_pid(P2, Pid2),
  Q2 =.. [R, Pid1, Pid2],
  call(Q2),
  fullname(Pid1, Sur1, Mid1, Nm1).

list_people :-
  fullname(Pid, Surname, Middle, Name),
  write(Pid:[Surname, Middle, Name]), nl,
  fail.
list_people.

find_family(Surname) :-
  fullname(Pid, Surname, Middle, Name),
  write(Pid:[Surname, Middle, Name]), nl,
  fail.
find_family(_).

show_relatives(P, Relationship) :-
  get_pid(P, Pid),
  Q =.. [Relationship, Rid, Pid],
  call(Q),
  fullname(Rid, RSurname, RMiddle, RName),
  write(Rid:[RSurname, RMiddle, RName]), nl,
  fail.
show_relatives(_, _).

get_pid(Pid1, Pid2) :-
  var(Pid1),
  var(Pid2),
  !.
get_pid(0, _) :-
  !,
  fail.
get_pid(Pid1, Pid2) :-
  integer(Pid1),
  !,
  Pid1 = Pid2.
get_pid(Pid1:_, Pid2) :-
  integer(Pid),
  !,
  Pid1 = Pid2.
get_pid([Sur,Mid,Nm], Pid) :-
  var(Pid),
  fullname(Pid, Sur, Mid, Nm).
get_pid(Nm, Pid) :-
  var(Pid),
  fullname(Pid, Sur, Mid, Nm).
get_pid(Sur, Pid) :-
  var(Pid),
  fullname(Pid, Sur, Mid, Nm).
  

%----------------------------------------------------------------------
% person object
%
% These predicates define the interface to a person.  All of the
% genealogical rules are based on these predicates, which are
% based on the basic representation of a person.  These are the
% only rules which need to be changed if the representation of
% a person is changed.
%

% Database design
%
% person
%   pid        number (key)
%   surname    text[40] (indexed)
%   midname    text[40]
%   name       text[40]
%   gender     text[1]  m/f
%   mother     number (indexed) (a pid)
%   father     number (indexed) (a pid)
%   dob        text[10] (date yyyy-mm-dd)
%   dod        text[10] (date yyyy-mm-dd)
%   birthplace text[50]
%   nation     text[40]
%   note       text[255]
%
% marriage
%   husband    number (indexed) (a pid)
%   wife       number (indexed) (a pid)
%   married    text[10] (date yyyy-mm-dd)
%   divorced   text[10] (date yyyy-mm-dd)
%   note       text[255]

db_table(person, pid, i).
db_table(person, surname, s40).
db_table(person, midname, s40).
db_table(person, name, s40).
db_table(person, gender, a1).
db_table(person, mother, i).
db_table(person, father, i).
db_table(person, dob, s10).
db_table(person, dod, s10).
db_table(person, birthplace, s50).
db_table(person, nation, s40).
db_table(person, note, s255).

db_table(marriage, husband, i).
db_table(marriage, wife, i).
db_table(marriage, married, s10).
db_table(marriage, divorced, s10).
db_table(marriage, note, s255).

fullname(P, S, M, N) :-
  pid_ok(P),
  db_query(person, [pid=P, surname=S, midname=M, name=N]).

male(P) :-
  pid_ok(P),
  db_query(person, [pid=P, gender=m]).

female(P) :-
  pid_ok(P),
  db_query(person, [pid=P, gender=f]).

firstlast(F, L) :-
  db_query(person, [name=F, surname=L]).

mother(M, C) :-
  pid_ok(M),
  pid_ok(C),
  db_query(person, [pid=C, mother=M]).

father(F, C) :-
  pid_ok(F),
  pid_ok(C),
  db_query(person, [pid=C, father=F]).

husbandwife(H, W) :-
  pid_ok(H),
  pid_ok(W),
  db_query(marriage, [husband=H, wife=W]).

/* Don't bother checking a PID that is 0, which
   signifies an unknown person.
   */

pid_ok(P) :-
  var(P), !.
pid_ok(P) :-
  integer(P),
  P > 0.

%----------------------------------------------------------------------
% relationships
%

spouse(P1,P2) :- husbandwife(P1,P2).
spouse(P1,P2) :- husbandwife(P2,P1).

parent(P,C) :-
 (mother(P,C) ; father(P,C)).

child(C,P) :- parent(P,C).

son(C,P) :- parent(P,C), male(C).

daughter(C,P) :- parent(P,C), female(C).

wife(W,P) :-
  spouse(W,P),
  female(W).

husband(H,P) :-
  spouse(H,P),
  male(H).

ancestor(A,P) :-
  parent(A,P).
ancestor(A,P) :-
  parent(X,P),
  ancestor(A,X).

descendent(D,P) :-
  parent(P,D).
descendent(D,P) :-
  parent(P,X),
  descendent(D,X).

full_sibling(S1, S2) :-
  mother(M,S2),
  mother(M,S1),
  S1 \= S2,
  father(F,S1),
  father(F,S2).

half_sibling(S1, S2) :-
  mother(M,S2),
  mother(M,S1),
  S1 \= S2,
  father(F1,S1),
  father(F2,S2),
  F1 \= F2.
half_sibling(S1, S2) :-
  father(F,S2),
  father(F,S1),
  S1 \= S2,
  mother(M1,S1),
  mother(M2,S2),
  M1 \= M2.

sibling(S1, S2) :-
  full_sibling(S1,S2).
sibling(S1, S2) :-
  half_sibling(S1,S2).

sister(S,P) :-
  sibling(S,P),
  female(S).

brother(B,P) :-
  sibling(B,P),
  male(B).

step_sibling(S1, S2) :-
  parent(P2, S2),
  spouse(M2, P2),
  parent(M2, S1),
  not(parent(M2,S2)),
  not(half_sibling(S1,S2)).
  
uncle(U,X) :-
  parent(P,X),
  brother(U,P).

aunt(A,X) :-
  parent(P,X),
  sister(A,P).

step_parent(P2,C) :-
  parent(P,C),
  spouse(P2,P),
  not(parent(P2,C)).

step_mother(M,C) :- step_parent(M,C), female(M).

step_father(F,C) :- step_parent(F,C), male(F).

step_child(C2,P) :- step_parent(P,C2).

step_daughter(D,P) :- step_child(D,P), female(D).

step_son(S,P) :- step_child(S,P), male(S).

nephew(N,X) :-
  sibling(S,X),
  parent(S,N),
  male(N).

niece(N,X) :-
  sibling(S,X),
  parent(S,N),
  female(N).

cousin(X,Y) :-
  parent(P,Y),
  sibling(S,P),
  parent(S,X).

grandmother(GM,X) :-
  parent(P,X),
  mother(GM,P).

grandfather(GF,X) :-
  parent(P,X),
  father(GF,P).

grandparent(GP,X) :-
  parent(P,X),
  parent(GP,P).

grandson(GS,X) :-
  grandchild(GS,X),
  male(GS).

granddaughter(GD,X) :-
  grandchild(GD,X),
  female(GD).

grandchild(GC,X) :-
  parent(X,C),
  parent(C,GC).

%----------------------------------------------------------------------
% relation/3 - used to find relationships between individuals
%

relations([parent, wife, husband, ancestor, descendent, full_sibling,
    half_sibling, sibling, sister, brother, step_sibling, uncle,
    aunt, mother, father, child, son, daughter, step_parent,
    step_child, step_mother, step_father, step_son, step_daughter,
    nephew, niece, cousin, grandmother, grandfather, grandparent,
    grandson, granddaughter, grandchild]).

relation(R, X, Y) :-
  relations(Rs),
  member(R,Rs),
% write(trying:R),nl,
  Q =.. [R,X,Y],
  call(Q).


%----------------------------------------------------------------------
% Semantic Integrity Checks on Update
%

add_person(Name,Gender,Mother,Father,Spouse) :-
  retractall(message(_)),
  dup_check(Name),
  add(Name,Gender,Mother,Father,Spouse),
  ancestor_check(Name),
  mother_check(Name, Gender, Mother),
  father_check(Name, Gender, Father),
  spouse_check(Name, Spouse).

dup_check(Name) :-
  person(Name),
  assert(message($Person is already in database$)),
  !, fail.
dup_check(_).
  
ancestor_check(Name) :-
  ancestor(Name,Name),
  assert(message($Person is their own ancestor/descendent$)),
  !, fail.
ancestor_check(_).

mother_check(_, _, Mother) :- not(person(Mother)), !.
mother_check(_, _, Mother) :-
  male(Mother),
  assert(message($Person's mother is a man$)),
  !, fail.
mother_check(Name, male, _) :-
  mother(Name, X),
  assert(message($Person, a male, is someone's mother$)),
  !, fail.
mother_check(_,_,_).

father_check(_, _, Father) :- not(person(Father)), !.
father_check(_, _, Father) :-
  female(Father),
  assert(message($Person's father is a man$)),
  !, fail.
father_check(Name, female, _) :-
  father(Name, X),
  assert(message($Person, a female, is someone's father$)),
  !, fail.
father_check(_,_,_).

spouse_check(Name, Spouse) :-
  spouse(Name, X),
  X \= Spouse,
  assert(message($Person is already someone else's spouse$)),
  !, fail.
spouse_check(Name, Spouse) :-
  blood_relative(Name, Spouse),
  assert(message($Person is a blood relative of spouse$)),
  !, fail.
spouse_check(_,_).
  
blood_relative(X,Y) :- (ancestor(X,Y); ancestor(Y,X)).
blood_relative(X,Y) :- sibling(X,Y).
blood_relative(X,Y) :- cousin(X,Y).
blood_relative(X,Y) :- (uncle(X,Y); uncle(Y,X)).
blood_relative(X,Y) :- (aunt(X,Y); aunt(Y,X)).

%----------------------------------------------------------------------
% utility predicates
%

message(X) :-
  messagebox(X).

tfmessage(X) :-
  tfmessagebox(X).

