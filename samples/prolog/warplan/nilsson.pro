/* STRIPS-style planner for the blocks-world -- from Nilsson,
 * PRINCIPLES OF ARTIFICIAL INTELLIGENCE, p. 281.
 */

run1 :-                                                  /* LOOPS */
    warplan( [ on(b,a), ontable(a), ontable(c), armempty,
               ontable(d), clear(b), clear(c), clear(d) ],
             [ on(b,d), on(c,a) ]).

run2 :-                                                  /* LOOPS */
    warplan( [ on(c,a), ontable(a), ontable(b), armempty,
               clear(c), clear(b) ],
             [ on(b,c), on(a,b) ] ).



precond(stack(X,Y),   [ clear(Y),
                        holding(X) ]).
precond(unstack(X,Y), [ on(X,Y),
                        clear(X),
                        armempty ]).
precond(pickup(X),    [ clear(X),
                        ontable(X),
                        armempty ]).
precond(putdown(X),   [ holding(X) ]).

add(stack(X,Y),   armempty).
add(stack(X,Y),   on(X,Y)).
add(stack(X,Y),   clear(X)).
add(unstack(X,Y), holding(X)).
add(unstack(X,Y), clear(Y)).
add(pickup(X),    holding(X)).
add(putdown(X),   ontable(X)).
add(putdown(X),   armempty).
add(putdown(X),   clear(X)).

del(stack(X,Y),   holding(X)).
del(stack(X,Y),   clear(Y)).
del(unstack(X,Y), armempty).
del(unstack(X,Y), on(X,Y)).
del(unstack(X,Y), clear(X)).
del(pickup(X),    armempty).
del(pickup(X),    ontable(X)).
del(pickup(X),    clear(X)).
del(putdown(X),   holding(X)).

imposs([holding(_), armempty]).
imposs([on(X,_),    ontable(X)]).
imposs([clear(X),   on(_,X)]).

