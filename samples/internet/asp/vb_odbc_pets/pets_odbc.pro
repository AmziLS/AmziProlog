:-import(aodbc).

pet(P) :-
  db_open(pets),
  sound(S),
  db_query(pets, [pet=P, sound=S]).

main :-
  asserta(sound(woof)),
  pet(P),
  write(P), nl.

db_table(pets, pet, a50).
db_table(pets, sound, a50).
