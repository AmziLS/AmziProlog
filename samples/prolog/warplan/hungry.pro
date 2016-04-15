/*                         HUNGRY.PRO                          */

run1 :-
    warplan([ available(secretary) ],
            [ not_hungry ]).

precond(visit(cafeteria),       [ can_locate(cafeteria)]).
precond(visit(vending_machine), [ can_locate(vending_machine),
                                  have(change)]).
precond(visit(office),          []).
precond(visit(secretary),       [ can_locate(secretary),
                                  available(secretary)]).
precond(visit(colleague),       []).

imposs([hungry,not_hungry]).

del(visit(cafeteria),       hungry).
del(visit(vending_machine), hungry).

add(visit(cafeteria),       not_hungry).
add(visit(colleague),       can_locate(_)).
add(visit(office),          can_locate(_)).
add(visit(office),          have(change)).
add(visit(secretary),       can_locate(_)).
add(visit(secretary),       have(change)).
add(visit(vending_machine), not_hungry).

