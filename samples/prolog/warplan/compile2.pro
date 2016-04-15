                      /* COMPILER-WORLD */
            /* HOW TO SOLVE IT, Problem 75, p. 102 */


?- op(250, yfx, #).
?- op(250, yfx, holds).
?- op(150, xfy, +).
?- op(150, xfy, -).
?- op(150, fx,  load).
?- op(250, fx,  add).
?- op(150, fx,  sub).
?- op(150, fx,  store).
?- op(150, fx,  reg).


add( add   R # V1+V2, acc   holds V1+V2).
add( sub   R # V1-V2, acc   holds V1-V2).
add( load  R # V,     acc   holds V).
add( store R # V,     reg R holds V).

del(Action, acc holds Z) :-
    add(Action, acc holds V).
del(Action, reg R holds Z) :-
    add(Action, reg R holds V).

precond(load  R # V,     [reg R holds V]).
precond(store R # V,     [acc   holds V]).
precond(add   R # V1+V2, [reg R holds V2, acc holds V1]).
precond(sub   R # V1-V2, [reg R holds V2, acc holds V1]).

given(reg 1 holds c1).
given(reg 2 holds c2).
given(reg 3 holds c3).
given(reg 4 holds c4).

run1 :-
    plans([acc holds c1]).

run2 :-
    plans([acc holds c1+c2]).

run3 :-
    plans([acc holds c1-c2]).

run4 :-
    plans([acc holds (c1-c2) + c3]).

recompile :-
    reconsult(compile2).




