/* pets.pro

   A trivial expert system used in various samples to
   illustrate how to connect host language programs
   to a Prolog advisor.
   */

pet(dog) :- sound(woof).
pet(cat) :- sound(meow).
pet(duck) :- sound(quack).

sound(X) :-
  ask(sound, X,
      $What sound does the pet make [woof, meow, quack]? $).

/* This version uses an ask/3 predicate that remembers
   answers to questions by storing them in the dynamic
   database in the structure

     known(Attribute, Value)

   If a value for the attribute is already known, then
   that value is used to unify with the asked for attribute.

   If a value is not already known, then the user is prompted
   for an answer, and the answer remembered in known/2.

   A mechanism like this is necessary to prevent the system
   from re-asking the user the sound for each rule of the
   pet advisor.

   prompt/2 can be implemented in Prolog, but can also be
   implemented as an extended predicate, as it is in the
   sample programs.
   */

ask(Attr, Val, _) :-
  known(Attr, V),
  !,
  V = Val.
ask(Attr, Val, Prompt) :-
  prompt(Prompt, V),
  assert(known(Attr, V)),
  !,
  V = Val.

/* Call the clear predicate if you want to rerun
   the pet advisor.
   */

clear :- retractall(known(_,_)).
  