%-*-Prolog-*-  
% zebra indented on 8/31/1999 by 'JOLI' 1.0.

% /* A PROLOG program to solve the "5 houses" problem, by Lewis Baxter.
% * Solution by Ray Reeves 
% * 
% 1 The Englishman lives in the red house.
% 2 The Spaniard owns the dog.
% 3 The Norwegian lives in the first house on the left.
% 4 Kools are smoked in the yellow house.
% 5 The man who smokes Chesterfields lives next to the man with the fox.
% 6 The Norwegian lives next to the blue house.
% 7 The Winston smoker owns snails.
% 8 The lucky strike smoker drinks orange juice.
% 9 The Ukrainian drinks tea.
% 10 The Japanese smokes parliaments.
% 11 Kools are smoked in the house next to where the horse is kept.
% 12 Coffee is drunk in the green house.
% 13 The green house is immediately to the right of the ivory house.
% 14 Milk is drunk in the middle house.
%*/
zebra :-
   fugit(zebra1).

zebra1 :-
   Houses = [ff(1, _, _, norwegian, _, _, _),  %3
             ff(2, blue, _, _, _, _, _),       %6
             ff(3, _, milk, _, _, _, _),       %14
             ff(4, _, _, _, _, _, _),          %
             ff(5, _, _, _, _, _, _)],
   Colors = [ff(_, red, _, englishman, _, _, _), %1
             [ff(I3, yellow, _, _, kools, _, _), ff(I4, _, _, _, _, horse, 
                                                    _)], %4,11
             [ff(_, ivory, _, _, _, _, _), ff(_, green, coffee, _, _, _, _)]
                                               %12,13
            ],
   Drinks = [ff(_, _, water, _, _, _, _), ff(_, _, orange_juice, _, 
                                             lucky_strike, _, _), %8
             ff(_, _, tea, ukrainian, _, _, _)], %9
   Nationalities = [ff(_, _, _, spaniard, _, dog, _), %2
                    ff(_, _, _, japanese, parliaments, _, _) %10
                   ],
   Smokes = [[ff(I1, _, _, _, chesterfields, _, _), ff(I2, _, _, _, _, fox, 
                                                       _)], %5
             ff(_, _, _, _, winston, snails, _)], %7
   Pets = [[ff(I2, _, _, _, _, fox, _), ff(I1, _, _, _, chesterfields, _, 
                                           _)], %5
           [ff(I4, _, _, _, _, horse, _), ff(I3, _, _, _, kools, _, _)], %11
           ff(_, _, _, _, _, zebra, _)],
   Dummy = [D1, D2, D3, D4, D5],
   solve(problem(Houses, perm(Colors), perm(Drinks), perm(Nationalities), 
                 perm(Smokes), perm(Pets), Dummy), Solutions),
   displaySolutions(Solutions).

displaySolution(ff(House, Color, Drink, Nationality, Smoke, Pet, _)) :-
   write(House), tab(1), write(Color), tab(1), write(Drink), tab(1), 
   write(Nationality), tab(1), write(Smoke), tab(1), write(Pet), nl.
                                            % There are no negative constraints

1 - P - _.
2 - P - _.
3 - P - _.
4 - P - _.
5 - P - _.
6 - P - _.
7 - P - _.
                                               % Utility Predicates 

writelabel(zebra) :- !,
   wca(5, 32, 112),
   write(zebra).
writelabel(water) :- !,
   wca(5, 32, 112),
   write(water).
writelabel(X) :-
   write(X).



