/*                         FLASH.PRO                           */

/*  A problem-module to apply means-ends planning to flash-    */
/*  light-repair. This module, which is discussed in Sections  */
/*  11.5 and 11.6 of the text ARTIFICIAL INTELLIGENCE THROUGH  */
/*  PROLOG, by N.C. Rowe, should accompany the search-module   */
/*  MEANS.PRO. With both modules loaded, type 'run1' or 'run2'.*/

run1 :-
    warplan([ closed(case),
              closed(top),
              inside(batteries),
              defective(batteries),
              ok(light),
              unbroken(case) ],
            [ ok(batteries),
               closed(case),
               closed(top) ]).

run2 :-
    warplan([ closed(case),
              closed(top),
              inside(batteries),
              defective(batteries),
              defective(light),
              unbroken(case) ],
            [ ok(batteries),
              closed(case),
              closed(top),
              ok(light) ]).

precond(replace_batteries, [open(case),outside(batteries),
                            unbroken(case) ]).
precond(replace_light,     [open(top)]).
precond(disassemble_case,  [closed(case)]).
precond(assemble_case,     [open(case),closed(top),unbroken(case)]).
precond(disassemble_top,   [open(case),closed(top)]).
precond(assemble_top,      [open(top)]).
precond(turn_over_case,    [open(case)]).
precond(smash_case,[]).

del(replace_batteries, outside(batteries)).
del(replace_batteries, defective(batteries)).
del(replace_light,     defective(light)).
del(disassemble_case,  closed(case)).
del(assemble_case,     open(case)).
del(disassemble_top,   closed(top)).
del(assemble_top,      open(top)).
del(turn_over_case,    inside(batteries)).
del(smash_case,        unbroken(case)).
del(smash_case,        closed(case)).
del(smash_case,        closed(top)).
del(smash_case,        inside(batteries)).

add(replace_batteries, inside(batteries)).
add(replace_batteries, ok(batteries)).
add(replace_light,     ok(light)).
add(disassemble_case,  open(case)).
add(assemble_case,     closed(case)).
add(disassemble_top,   open(top)).
add(assemble_top,      closed(top)).
add(turn_over_case,    outside(batteries)).
add(smash_case,        broken(case)).
add(smash_case,        open(case)).
add(smash_case,        open(top)).
add(smash_case,        outside(batteries)).

imposs([inside(batteries), outside(batteries)]).
imposs([open(case),        closed(case)]).
imposs([unbroken(case),    broken(case)]).
imposs([ok(light),         defective(light)]).
imposs([open(top),         closed(top)]).














