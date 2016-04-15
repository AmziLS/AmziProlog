%-*-Prolog-*-  
% mm indented on 3/20/2002 by 'JOLI' 1.0.

/*
 * MASTER MIND by Ray Reeves
 * All rights reserved.
 * 
 * Guess which four decimal digits I am thinking of.
 * ask makes up to five standard guesses, breaking off when 4 cattle seen.
 * then fit will continue with guesses consistent with previous ones, and
 * digits not known to be dead.
 */
main :-
  repeat,
  write(`\nType mm(N1, N2, N3, N4) or all or quit\n`),
  read(X),
  (X == quit ; (X == all -> testmm ; call(X), fail)).

mm(C1, C2, C3, C4) :-
  cntr_set(20, 0),
  retractall(code(_, _, _, _)),
  assert(code(C1, C2, C3, C4)),               % player's secret code
  guess(0, [0, 1, 2, 3], [], []), !.          % make first guess

guess(4, _, PrevGuesses, L0) :- !,
  fit(PrevGuesses, L0).                       % got all bulls already
guess(BullCount, Guess, PrevGuesses, L0) :-
  Guess = [A, B, C, D],
  askUser(Guess, Cattle1, Bulls1, _, L0, L1),
  BullCount1 is BullCount + Bulls1,
  Guesses = [Guess|PrevGuesses],
  (
     Bulls1 == 4 ->
     done ;

     (
        Cattle1 == 0 ->                       % all dead
        E is (4 + A) mod 10,
        F is (4 + B) mod 10,
        G is (4 + C) mod 10,
        H is (4 + D) mod 10,
        (
           E == 0,
           F == 1 ->                          % been all around
           fit(Guesses, L1) ;

           guess(BullCount1, [E, F, G, H], Guesses, L1)
        ) ;

        (
           Cattle1 == 4 ->                    % full house
           fit(Guesses, L1) ;

           (
              Guess = [2, 3, 4, 5],
              member(score(S, _, _, [0, 1, 2, 3]), L0),
              SoFar is Cattle1 + S,
              SoFar >= 6 ->
              fit(Guesses, L1) ;

              (
                 Guess = [4, 5, 6, 7],
                 member(score(S, _, _, [0, 1, 2, 3]), L0),
                 SoFar is Cattle1 + S,
                 SoFar >= 4 ->
                 fit(Guesses, L1) ;

                 (
                    Guess = [6, 7, 8, 9],
                    member(score(S, _, _, [2, 3, 4, 5]), L0),
                    SoFar is Cattle1 + S,
                    SoFar >= 4 ->
                    fit(Guesses, L1) ;

                    (
                       C == 0,
                       D == 1 ->              % been all around
                       fit(Guesses, L1) ;

                       E is (4 + A) mod 10,
                       F is (4 + B) mod 10,

                                              % basic left shift
                       guess(BullCount1, [C, D, E, F], Guesses, L1)
                    )
                 )
              )
           )
        )
     )
  ).

                            % test for done or refit with augmented guess list

fit(Guesses, InitialScores) :-                % find digit domain
  setof(X, memberGuesses(X, Guesses), Digits1),
  pickDead(InitialScores, Digits1, Digits2),
  fit(4, Digits2, Digits2, InitialScores, InitialScores, []).

                                              % got enough 'bulls' for a guess

fit(0, InitialDigits, Digits, InitialScores, Scores, BullsFound) :- !,
  (member(S, Scores), S = score(C, _, _, _), C \= 0 -> fail ;  true),
  askUser(BullsFound, Cattle, Bulls, Cows, Scores, _),
  (
     Bulls == 4 ->
     done ;

     Scores1 = [score(Cattle, Bulls, Cows, BullsFound)|InitialScores],
     fit(4, InitialDigits, InitialDigits, Scores1, Scores1, [])
  ).

                                           % make a guess that fits the scores
fit(Bix, InitialDigits, Digits, InitialScores, Scores, BullsFound) :-
  pick(Bull, Digits, Rest),                   % choose putative bull
  consistentList(Bull, Bix, Scores, Scores1),
  BullsFound1 = [Bull|BullsFound],
  Bix1 is Bix - 1,                            % find next bull
  fit(Bix1, InitialDigits, Rest, InitialScores, Scores1, BullsFound1).

consistentList(_, _, [], []) :- !.            % check digit against all scores
consistentList(X, I, [HeadIn|Tail], [HeadOut|Rest]) :-
  consistent(X, I, HeadIn, HeadOut),
  consistentList(X, I, Tail, Rest).

% check digit X index I against this score

consistent(X, _, Score, Score) :-
  Score = score(_, _, _, Guess),
  index(X, Index, Guess),
  Index == 0, !.                              % X not in guess
consistent(X, I, score(Cattle, Bulls, Cows, Guess), ScoreOut) :-
  Bulls > 0,                                  % guess has bulls
  index(X, I, Guess), !,                      % and this could be
  BullsOut is Bulls - 1,
  CattleOut is Cattle - 1,
  ScoreOut = score(CattleOut, BullsOut, Cows, Guess).
consistent(X, I, score(Cattle, Bulls, Cows, Guess), ScoreOut) :-
  Cows > 0,                                   % guess has cows
  index(X, J, Guess),
  J \= I,                                     % and this could be
  CowsOut is Cows - 1,
  CattleOut is Cattle - 1,
  ScoreOut = score(CattleOut, Bulls, CowsOut, Guess).

                                              % get the score

askUser(Guess, Cattle, Bulls, Cows, ScoreIn, ScoreOut) :-
  getScore(Guess, 4, 0, 0, Bulls, Cows),
  Cattle is Bulls + Cows,
  ScoreOut = [score(Cattle, Bulls, Cows, Guess)|ScoreIn],
  cntr_inc(20, _),                            % guess count
  cntr_get(20, N),
  write(N), tab(5), write(Guess), write('   Bulls: '), write(Bulls), 
  write('   Cows: '), write(Cows), nl, !.

getScore(_, 0, B, C, B, C) :- !.
getScore(Guess, I, Bin, Cin, Bulls, Cows) :-  % mark each digit
  code(C1, C2, C3, C4),
  arg(I, code(C1, C2, C3, C4), CI),
  index(CI, Index, Guess),
  (
     Index == 0 ->
     Bout is Bin,
     Cout is Cin ;

     (
        Index == I ->
        Bout is Bin + 1,
        Cout is Cin ;

        Bout is Bin,
        Cout is Cin + 1
     )
  ),
  I1 is I - 1, !,
  getScore(Guess, I1, Bout, Cout, Bulls, Cows).

memberGuesses(X, [[A, B, C, D]|_]) :-
  member(X, [A, B, C, D]).
memberGuesses(X, [_|Rest]) :-
  memberGuesses(X, Rest).

pickList([], D, D).                           % pick elements out of a list
pickList([X|Y], D0, Dn) :-
  pick(X, D0, D1),
  pickList(Y, D1, Dn).

pick(X, [X|Rest], Rest).                      % pick one element from a list
pick(X, [Y|Z], [Y|Rest]) :-
  pick(X, Z, Rest).

index(X, 1, [X, _, _, _]) :- !.
index(X, 2, [_, X, _, _]) :- !.
index(X, 3, [_, _, X, _]) :- !.
index(X, 4, [_, _, _, X]) :- !.
index(_, 0, _).

done :-
  write('Done'), nl.

pickDead(Scores, D0, Dn) :-

% if two exclusive scores total four cattle, other two digits are dead 
  (
     member(score(C1, _, _, [2, 3, 4, 5]), Scores),
     member(score(C2, _, _, [6, 7, 8, 9]), Scores),
     C12 is C1 + C2,
     C12 == 4 ->
     pickList([0, 1], D0, D1) ;

     D1 = D0
  ),
  (
     member(score(C0, _, _, [0, 1, 2, 3]), Scores),
     member(score(C2, _, _, [6, 7, 8, 9]), Scores),
     C02 is C0 + C2,
     C02 == 4 ->
     pickList([4, 5], D1, D2) ;

     D2 = D1
  ),
  (
     member(score(C3, _, _, [4, 5, 6, 7]), Scores),
     member(score(C4, _, _, [8, 9, 0, 1]), Scores),
     C34 is C3 + C4,
     C34 == 4 ->
     pickList([2, 3], D2, D3) ;

     D3 = D2
  ),
  (
     member(score(C1, _, _, [2, 3, 4, 5]), Scores),
     member(score(C4, _, _, [8, 9, 0, 1]), Scores),
     C14 is C1 + C4,
     C14 == 4 ->
     pickList([6, 7], D3, D4) ;

     D4 = D3
  ),

% if one score is dead, all four digits are dead 
  (member(score(0, _, _, X), Scores), pickList(X, D4, Dn) ;  Dn = D4).

/*	needed by UNSW Prolog
setof(X, P, L1) :-
	bagof(X, P, L),
	sort(L, L1).

sort([A|B], Y) :- !, sort(B, X), insert(A, X, Y).
sort([], []).
*/

insert(X, [], [X]) :- !.
insert(X, [X|Y], [X|Y]) :- !.
insert(X, [A|B], [A|B1]) :-
  X > A, !,
  insert(X, B, B1).
insert(X, L, [X|L]).

member(X, [X|_]).
member(X, [_|Y]) :-
  member(X, Y).
