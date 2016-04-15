/* From Rich & Knight, ARTIFICIAL INTELLIGENCE (Second Edition),
 * p. 358.
 */


run1 :-
    warplan( [dirty(sink), have(garbage), clean(oven),
              dirty(floor) ],
             [clean(sink), clean(floor) ] ).

run2 :-
    warplan( [dirty(sink), dirty(floor), have(garbage), dirty(oven),
              dirty(refrigerator), dirty(counters) ],
             [clean(sink), clean(floor), clean(oven),
              clean(refrigerator), clean(counters) ] ).


run3 :-
    warplan( [ dirty(oven), dirty(refrigerator) ],
             [ clean(oven), clean(floor),
               clean(sink), clean(refrigerator) ] ).


precond(Act,Conds) :-
    can(Act,Conds).


can(apply(oven_cleaner),  [ dirty(oven) ]).
add(apply(oven_cleaner),  have(oven_cleaner)).
del(apply(oven_cleaner),  dont_have(oven_cleaner)).

can(remove(oven_cleaner), [ dirty(oven), have(oven_cleaner) ]).
add(remove(oven_cleaner), dirty(floor)).
add(remove(oven_cleaner), clean(oven)).
add(remove(oven_cleaner), dont_have(oven_cleaner)).
del(remove(oven_cleaner), dirty(oven)).
del(remove(oven_cleaner), clean(floor)).
del(remove(oven_cleaner), have(swept_floor)).
del(remove(oven_cleaner), have(oven_cleaner)).


can(sweep(floor),         [ dirty(floor), dont_have(garbage) ]).
add(sweep(floor),         have(swept_floor)).
del(sweep(floor),         dont_have(swept_floor)).


can(take_out(garbage),    [ have(garbage) ]).
add(take_out(garbage),    dont_have(garbage)).
del(take_out(garbage),    have(garbage)).


can(wash(floor),           [ have(swept_floor) ]).
can(wash(X),               [ dirty(X) ]) :-
    X \== floor,
    X \== oven.
add(wash(X),              clean(X)).
add(wash(refrigerator),   dirty(floor)).
add(wash(refrigerator),   have(garbage)).
add(wash(refrigerator),   dirty(counters)).
add(wash(refrigerator),   dont_have(swept_floor)).
add(wash(counters),       dirty(sink)).
add(wash(floor),          dirty(sink)).
del(wash(X),              dirty(X)).
del(wash(floor),          have(swept_floor)).
del(wash(refrigerator),   clean(floor)).
del(wash(refrigerator),   clean(counters)).
del(wash(refrigerator),   have(swept_floor)).
del(wash(refrigerator),   dont_have(garbage)).
del(wash(counters),       clean(sink)).
del(wash(floor),          clean(sink)).


imposs([clean(X), dirty(X)]).
imposs([have(X),  dont_have(X)]).


rich2 :-
    reconsult(rich2).

