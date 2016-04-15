/*************************************************************************/
/*                                                                       */
/*    EENG-749      Artificial  Intelligence                             */
/*                                                                       */
/*    Assignment #1 - lab1.pro                                           */
/*                                                                       */
/*    By:  FLTLT Ralph W. Dries                                          */
/*    Due: 14 April 1990                                                 */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                              LAB1.PRO                                 */
/*                                                                       */
/*   This problem module applies means-ends planning to a motor-vehicle  */
/*   oil change.                                                         */
/*                                                                       */
/*************************************************************************/


run1 :-
    warplan([ bad(oil),
              closed(sump),
              closed(filler),
              have(oil),
              cold(engine) ],
            [ good(oil),
              closed(sump),
              closed(filler) ]).


imposs([good(X),bad(X)]).
imposs([open(X),closed(X)]).
imposs([have(X),have_no(X)]).
imposs([cold(X),hot(X)]).

precond(pour_oil,           [have(oil),closed(sump),open(filler),
                             removed(oil)]).
precond(drain_oil,          [open(sump)]).
precond(buy_oil,            [visited(store)]).
precond(visit_store,        [bad(oil),have_no(oil)]).
precond(run_engine,         [cold(engine),bad(oil),closed(sump),
                             closed(filler)]).
precond(remove_sump_plug,   [closed(sump),have(oil),hot(engine)]).
precond(remove_filler_cap,  [closed(filler),removed(oil),closed(sump)]).
precond(replace_sump_plug,  [removed(oil),open(sump)]).
precond(replace_filler_cap, [open(filler)]).

add(pour_oil,           good(oil)).
add(pour_oil,           have_no(oil)).
add(drain_oil,          removed(oil)).
add(buy_oil,            have(oil)).
add(visit_store,        visited(store)).
add(run_engine,         hot(engine)).
add(remove_sump_plug,   open(sump)).
add(remove_filler_cap,  open(filler)).
add(replace_sump_plug,  closed(sump)).
add(replace_filler_cap, closed(filler)).

del(pour_oil,           have(oil)).
del(pour_oil,           removed(oil)).
del(buy_oil,            have_no(oil)).
del(run_engine,         cold(engine)).
del(remove_sump_plug,   closed(sump)).
del(remove_filler_cap,  closed(filler)).
del(drain_oil,          bad(oil)).
del(replace_sump_plug,  open(sump)).
del(replace_filler_cap, open(filler)).

