
/***************************************************************/
/*                         WEDDING.PRO                         */
/***************************************************************/

run1 :-
    warplan([ has_car(best_man),lives_close_to_church(bride),
              not_at_church(groom),not_at_church(bride),
              not_at_church(pastor),not_arranged(flowers),
              not_available(church),not_available(reception_hall),
              available(florist),affordable(florist),
              affordable(reception_hall),no_other_weddings(church),
              no_services(church) ],
            [ at_church(groom), at_church(bride), at_church(pastor),
              arranged(flowers), available(church),
              available(reception_hall) ]).

imposs([at_church(X),not_at_church(X)]).
imposs([available(X),not_available(X)]).
imposs([arranged(X), not_arranged(X)]).

/**************************************************************/
/*  For an operator to be used, the appropriate precondition  */
/*  must be given somewhere.  If at least one precondition    */
/*  doesn't exist to satisfy each unique goal in a recommended*/
/*  clause, Prolog will return no (There is no solution).     */
/**************************************************************/

precond(walk(X),                  [lives_close_to_church(X)]).
precond(best_man_drive(X),        [has_car(best_man)]).
precond(drive(X),                 [has_car(X)]).
precond(schedule(church),         [no_other_weddings(church),
                                   no_services(church)]).
precond(schedule(florist),        [available(florist),
                                   affordable(florist)]).
precond(schedule(reception_hall), [affordable(reception_hall)]).

/*****************************************************************/
/* Notice that all given preconditions aren't deleted.  The fact */
/* that the groom is at the church wouldn't change the fact that */
/* the best man or groom had cars or the groom lived close to    */
/* the church. As we discussed in class, the Goal State is a     */
/* of the means end analysis Final State.                        */
/*****************************************************************/

del(best_man_drive(X), not_at_church(X)).
del(drive(X),          not_at_church(X)).
del(walk(X),           not_at_church(X)).
del(schedule(X),       not_available(X)).
del(schedule(florist), not_arranged(flowers)).

/***************************************************************/
/* All expected facts must be added or you may never reach the */
/* Final State you are looking for.                            */
/***************************************************************/

add(best_man_drive(X), at_church(X)).
add(drive(X),          at_church(X)).
add(walk(X),           at_church(X)).
add(schedule(florist), arranged(flowers)).
add(schedule(X),       available(X)).







