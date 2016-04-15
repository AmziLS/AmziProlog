%-*-Prolog-*-  
% testmm indented on 6/10/1999 by 'JOLI' 1.0.

% * test MASTER MIND by Ray Reeves
% * All rights reserved.

testmm:-
   T1 is cputime,
   test1,
   T2 is cputime,
   T12 is T2 - T1,
   write(`time: `), write(T12), write(` seconds`), nl, write(`Finished`), nl.

test1 :-
   cntr_set(1, 1),
   cntr_set(2, 0),
   abolish(sum/1),
   assert((sum(0))),
   member3(G1, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], R2),
   member3(G2, R2, R3),
   member3(G3, R3, R4),
   member3(G4, R4, _),
   test2(G1, G2, G3, G4),
   fail.
test1.

test2(G1, G2, G3, G4) :-
   cntr_inc(1, N),                             % test #
   write(N), 
   spaces(5),
   write(G1), 
   spaces(1),
   write(G2), 
   spaces(1),
   write(G3), 
   spaces(1),
   write(G4), nl, 
   mm(G1, G2, G3, G4),
   cntr_get(20, M),                            % guesses
   cntr_inc(M, _),                             % histogram
   spaces(5),
   write(M), 
   retract((sum(OldTot))),
   NewTot is M + OldTot,
   assert((sum(NewTot))),
   Av is (NewTot)/N,
   spaces(5),
   write(`Average: `), write(Av), nl, nl, !.

member3(X, [X|Y], Y).
member3(X, [Y|Z], [Y|Rest]) :-
   member3(X, Z, Rest).

spaces(0) :- !.
spaces(1) :-
   put(0' ), !.
spaces(5) :-
   write(`     `).



