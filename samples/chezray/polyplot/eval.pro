%-*-Prolog-*-  
% eval indented on 7/8/1999 by 'JOLI' 1.0.


    /**************************************************************\
    *                                                              *
    * eval.pro -  General expression evaluator.                    *
    *                                                              *
    * Copyright (c) 1998 by Ray Reeves.  All Rights Reserved.      *
    *                                                              *
    \**************************************************************/


/* Designed for polynomial expressions but probably more generally useful.
 * If binary functor then resolve args and call eval0, else no change.
 */
eval([], []) :- !.                             %1 short cuts
eval([0], 0) :- !.
eval([0.0], 0) :- !.
eval([] * _, []) :- !.                         %2
eval(_ * [], []) :- !.                         %3
eval(0 * _, 0) :- !.                           %4
eval(0/_, 0) :- !.                             %5
eval(_ * 0, 0) :- !.                           %6
eval([F|Fs], [F|Fs]) :- !.                     %7 lists unchanged
eval(T, E) :-                                  %8 binary functors, not lists
   functor(T, F, 2),
   functor(T0, F, 2),                          % copy functor
   arg(1, T, A1),
   arg(2, T, A2),
   eval(A1, A10),                              % recurse left
   eval(A2, A20),                              % recurse right
   arg(1, T0, A10),                            % build new args
   arg(2, T0, A20),
   eval0(T0, E).
eval(F, F).                                    %9 else unchanged

