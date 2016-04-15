                      /* COMPILER-WORLD */
            /* HOW TO SOLVE IT, Problem 75, p. 102 */


add(add(R,V1+V2), holds(acc,V1+V2)).
add(sub(R,V1-V2), holds(acc,V1-V2)).
add(load(R,V),    holds(acc,V)).
add(store(R,V),   holds(reg,R,V)).

del(Action, holds(acc,_)) :-
    add(Action, holds(acc,_)).
del(Action, holds(reg,R,_)) :-
    add(Action, holds(reg,R,_)).

precond(load(R,V),    [holds(reg,R,V)]).
precond(store(R,V),   [holds(acc,V)]).
precond(add(R,V1+V2), [holds(acc,V1),holds(reg,R,V2)]).
precond(sub(R,V1-V2), [holds(acc,V1),holds(reg,R,V2)]).

given_list([ holds(reg,1,c1),
             holds(reg,2,c2),
             holds(reg,3,c3),
             holds(reg,4,c4) ]).

run1 :-
    given_list(Start),
    warplan(Start,[ holds(reg,1,c1+(c2-c3)),
                    holds(reg,2,c2-c3),
                    holds(reg,3,c4+c4) ]).

run2 :-                                  /* Does this really work? */
    given_list(Start),
    warplan(Start,[holds(acc, (c1-c2) + (c3-c4))]).

run3 :-                                  /* Loops */
    given_list(Start),
    warplan(Start,[ holds(reg,1,c1+(c2-c3)),
            holds(reg,2,c2-c3),
            holds(acc,c1) ]).

recompile :-
    reconsult(compile).




