                    /* FIVE-BLOCK WORLD */
            /* HOW TO SOLVE IT, Problem 73, p. 99 */

add(move(U,V,W), on(U,W)).
add(move(U,V,W), clear(V)).

del(move(U,V,W), on(U,Z)).
del(move(U,V,W), clear(W)).

precond(move(U,V,table),[on(U,V),not_equal(V,table),clear(U)]).
precond(move(U,V,W),    [clear(W),on(U,V),not_equal(U,W),clear(U)]).

imposs([on(X,Y),clear(Y)]).
imposs([on(X,Y),on(X,Z),not_equal(Y,Z)]).
imposs([on(X,X)]).


run1 :-
    warplan([ on(a,table),
           on(b,d),
           on(c,e),
           on(d,table),
           on(e,table),
           clear(a),
           clear(b),
           clear(c) ],
         [on(a,b),on(b,c),on(c,d),on(d,e)] ).

