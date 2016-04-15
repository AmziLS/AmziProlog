/*                      SUSSMAN'S ANOMALY                          */
/*                   WARPLAN PROBLEM-MODULE                        */

add(move(U,V,W), on(U,W)).
add(move(U,V,W), clear(V)).

del(move(U,V,W), on(U,V)).
del(move(U,V,W), clear(W)).

precond(move(U,V,table),[on(U,V),not_equal(V,table),clear(U)]).
precond(move(U,V,W),    [clear(W),on(U,V),not_equal(U,W),clear(U)]).

imposs([on(X,Y),clear(Y)]).
imposs([on(X,Y),on(X,Z),not_equal(Y,Z)]).
imposs([on(X,X)]).
imposs([clear(table)]).

run1 :-
    warplan([ on(a,table),
              on(b,table),
              on(c,a),
              clear(b),
              clear(c) ],
            [ on(b,c)]).

run2 :-
    warplan([ on(a,table),
              on(b,table),
              on(c,a),
              clear(b),
              clear(c) ],
            [ on(a,b),
              on(b,c)]).
 
