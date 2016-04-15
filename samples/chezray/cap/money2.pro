%% Code generated by crypto on 3/26/2002.

:- dcg_terminal(draw).
:- noNonTerminals.

money2 :-
    solution(money2,[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], X).

solution(money2) -->   [M], M > 0, M < 5,
   [E], E > 0, 
   [N], 
   [D], 
   evaluate(0 + D + N + E , SCarry4, Y), 
   [Y], 
   [R], 
   evaluate(SCarry4 + N + E + R , SCarry3, E), 
   [O], 
   [V], 
   evaluate(SCarry3 + E + V + O , SCarry2, N), 
   [S], S > 0, 
   evaluate(SCarry2 + S + E + M , SCarry1, O), 
   evaluate(SCarry1 , 0, M),    M > 0, 
   displayGram(5, 5, 
               ['S', 'V', 'O', 'R', 'Y', 'D', 'N', 'E', 'M'], 
                      [S, V, O, R, Y, D, N, E, M]	).

pos('S', 2, 1, 0).
pos('E', 3, 1, 0).
pos('N', 4, 1, 0).
pos('D', 5, 1, 0).
pos('E', 2, 2, 0).
pos('V', 3, 2, 0).
pos('E', 4, 2, 0).
pos('N', 5, 2, 0).
pos('M', 2, 3, 0).
pos('O', 3, 3, 0).
pos('R', 4, 3, 0).
pos('E', 5, 3, 0).
pos('M', 1, 5, 0).
pos('O', 2, 5, 0).
pos('N', 3, 5, 0).
pos('E', 4, 5, 0).
pos('Y', 5, 5, 0).
barLine(1, 4, real).
