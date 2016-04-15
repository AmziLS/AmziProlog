main :-
   write('What sound does the pet make?'),
   nl,
   write('Enter woof or meow followed by a period'),
   nl,
   read(Sound),
   pet(Pet, Sound),
   write('The pet is a: '),
   write(Pet),
   nl.
   
pet(dog, woof).
pet(cat, meow).
pet(unknown, _).