/***************************************************************/
/*                                                             */
/*       MODULE:  LAB1.PRO                                     */
/*                                                             */
/*       Arthur Lee Sumner      EENG 749       28 April 1990   */
/*                                                             */
/*       A problem-module to apply means-ends planning to      */
/*       changing oil in an automobile.  This module requires  */
/*       the search-module MEANS.PRO to run.  With both        */
/*       modules loaded,type 'run.' to begin the oil change.  */
/*                                                             */
/***************************************************************/

run1 :-
  warplan([ closed(hood),
            closed(crankcase),
            closed(oil_pan),
            bad(oil),
            old(oil_filter),
            in(dip_stick),
            unk(oil_level) ],
          [ good(oil),
            closed(crankcase),
            full(oil_level),
            in(dip_stick),
            closed(hood) ]).


precond(add_new_oil,          [ drain(oil),closed(oil_pan),
                                new(oil_filter)]).
precond(drain_old_oil,        [ open(oil_pan)]).
precond(insert_new_oil_filter,[ no(oil_filter)]).
precond(remove_old_oil_filter,[ drain(oil)]).
precond(remove_crankcase_cap, [ open(hood),closed(crankcase)]).
precond(replace_crankcase_cap,[ open(hood),open(crankcase)]).
precond(remove_oil_pan_plug,  [ open(crankcase)]).
precond(replace_oil_pan_plug, [ drain(oil),open(oil_pan)]).
precond(check_oil_level,      [ out(dip_stick),good(oil)]).
precond(remove_dip_stick,     [ open(hood),in(dip_stick)]).
precond(replace_dip_stick,    [ open(hood),out(dip_stick)]).
precond(raise_hood,           [ closed(hood)]).
precond(lower_hood,           [ open(hood),closed(crankcase),
                                in(dip_stick)]).

del(add_new_oil,            drain(oil)).
del(drain_old_oil,          bad(oil)).
del(insert_new_oil_filter,  no(oil_filter)).
del(remove_old_oil_filter,  old(oil_filter)).
del(remove_crankcase_cap,   closed(crankcase)).
del(replace_crankcase_cap,  open(crankcase)).
del(remove_oil_pan_plug,    closed(oil_pan)).
del(replace_oil_pan_plug,   open(oil_pan)).
del(check_oil_level,        unk(oil_level)).
del(remove_dip_stick,       in(dip_stick)).
del(replace_dip_stick,      out(dip_stick)).
del(raise_hood,             closed(hood)).
del(lower_hood,             open(hood)).

add(add_new_oil,            good(oil)).
add(drain_old_oil,          drain(oil)).
add(insert_new_oil_filter,  new(oil_filter)).
add(remove_old_oil_filter,  no(oil_filter)).
add(remove_crankcase_cap,   open(crankcase)).
add(replace_crankcase_cap,  closed(crankcase)).
add(remove_oil_pan_plug,    open(oil_pan)).
add(replace_oil_pan_plug,   closed(oil_pan)).
add(check_oil_level,        full(oil_level)).
add(remove_dip_stick,       out(dip_stick)).
add(replace_dip_stick,      in(dip_stick)).
add(raise_hood,             open(hood)).
add(lower_hood,             closed(hood)).
