%----------------------------------------------------------------------
% WGENE.PRO - genealogical relationships
% Copyright (c) 1994 Amzi! inc.  All Rights Reserved.
%
% A Prolog database of relations derived from basic information about
% individuals.  The relations ships can all be read as 'relationship
% of', so for example, parent(P,C) means P is parent of C.
%
% When there is a performance trade-of in the implementation of a rule,
% it is assumed that in general the second argument of a relation will
% most likely be bound.  See for example full_sibling/2, which will
% have a smaller search for full_sibling(X,joe), than full_sibling(joe,X).
%

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
  Q =.. [R,X,Y],
  call(Q).


%----------------------------------------------------------------------
% person object
%
% These predicates define the interface to a person.  All of the
% genealogical rules are based on these predicates, which are
% based on the basic representation of a person.  These are the
% only rules which need to be changed if the representation of
% a person is changed.
%
% The current representation is flat database relations of the form:
%   person(Name, Gender, Mother, Father, Spouse).
%

add(Name,Gender,Mother,Father,Spouse) :-
  assert(person(Name,Gender,Mother,Father,Spouse)).
add(Name,_,_,_,_) :-
  delete(Name),
  fail.

close :-
  retractall(person(_,_,_,_,_)).

save(FileName) :-
  tell(FileName),
  listing(person),
  told.

delete(X) :-
  retract(person(X,_,_,_,_)).

person(X) :-
  person(X,_,_,_,_).

male(X) :-
  person(X,male,_,_,_).

female(Y) :-
  person(Y,female,_,_,_).

mother(M,C) :-
  person(C,_,M,_,_).

father(F,C) :-
  person(C,_,_,F,_).

spouse(S,P) :-
  person(P,_,_,_,S),
  S \= single.

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

member(X, [X|_]).
member(X, [_|Y]) :- member(X,Y).

