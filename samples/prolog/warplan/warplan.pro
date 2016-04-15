/*************************  WARPLAN.PRO  ***************************

    Based on: Warren, D.H.D., "WARPLAN: A System for Generating
    Plans," DCL Memo 76, Department of Artificial Intelligence,
    University of Edinburgh, 1974. Much of the commentary is adapted
    from the treatment in PROLOG FOR PROGRAMMERS, by Kluzniak &
    Szpakowicz, Section 8.1, "Planning."

    We consider planning with respect to a finite, usually small,
    set of OBJECTS to which simple ACTIONS are applicable. Objects
    consitute a closed "world," whose state is defined by a list of
    FACTS describing relationships among its objects.

    An ACTION transforms one state into another by ADDING or DELETING
    facts. A fact established by an action is called a GOAL ACHIEVED
    by the action. PLANNING consists in finding a sequence of actions
    that lead from a given INITIAL STATE to a given FINAL STATE.

    Certain relations are to be defined in the problem-module, or
    "world-description":

    precond(ACTION,PRECONDITION) :-
        PRECONDITION is a list of facts that must hold for ACTION to
        be applicable.
    add(FACT,ACTION) :-
        FACT is added by ACTION.
    del(FACT,ACTION) :-
        FACT is deleted by ACTION.
    imposs(FACTS) :-
        FACTS is a list representing an impossible combination of
        facts.
    always(FACT) :-
        FACT holds in the initial state,
        and is unaffected by any action.
    given(INITIALSTATE) :-
        INITIALSTATE is a list of facts holding in the initial
        state.

    The facts making up the 'given' relation are asserted by the
    planning module when the procedure 'warplan' is executed. In
    Warren's implementation, the 'given' facts are an explicit part
    of the problem-module.

    We can use variables to express relations among objects. Thus
    the statement "a clear block U is sitting on a block V" might be
    expressed by the list
            [on(U,V), not_equal(V,table), clear(U)]

    A state derived by actions A1,...,An is denoted by the list
    [An,...,A1]. Example:
     [ move(c,table,a), move(a,table,b), move(c,a,table) ]

    The program begins with a list of goals (the description of the
    desired final state) and an empty plan. At each step, the goal-
    list shrinks or the plan grows; successive intermediate states
    approximate the final state.

    Roughly speaking, the plan is constructed backwards: we look for
    preconditions of actions that achieve the final state, then for
    preconditions of actions that achieve those preconditions, etc.

    If a goal in the original list does not hold in an
    intermediate state, the program
       ** chooses an action that adds this fact,
       ** inserts the action into the current partial plan,
       ** removes the fact from the current list, and
       ** adds the fact to the action's preconditions.

    A partial plan usually contains variables. For example, to
    achieve 'a on b' in the blocks-world, we use the action
    'move(a,V,b)', whose precondition includes the fact 'a on V' for
    an unknown V. Such variables require some care: the fact 'U on c'
    may in general differ from 'a on V', even though the two are
    unifiable. We can either use the built-in comparison-operator
    '==' to compare facts, or instantiate their variables (by use of
    the procedure make_ground/1) prior to the comparison. The latter
    procedure is based on numbervars/3, which is built into some
    Prologs, but not into Prolog-1.

******************************************************************/


/*-------------------- ENTRY TO PROBLEM-SOLVER ------------------*/
/*                                                               */
/*                 Generation and output of a plan.              */
/*                                                               */
/*---------------------------------------------------------------*/

warplan(Start,Goals) :-
    inconsistent(Goals,[]),
    !,
    write('Impossible.'),
    nl.
warplan(Start,Goals) :-
    retractall(given(_)),
    assert_given_facts(Start),
    plan(Goals,[],[],Plan),
    !,
    nl,
    print_plan(Plan),
    nl.

assert_given_facts([]).
assert_given_facts([Fact|Facts]) :-
    assertz(given(Fact)),
    assert_given_facts(Facts).

/*----------------- ENTRY TO MAIN RECURSIVE LOOP ------------------*
 *
 * plan(RemGoals, SGoals, PPlan, Plan) :-
 *      Plan is a plan that
 *         contains PPlan as a subplan,
 *         preserves the goals already solved in SGoals, and
 *         solves the goals in RemGoals.
 *
 * Goals consist of facts; plans consist of actions.
 *
 *----------------------------------------------------------------*/

plan([],_,Plan,Plan).
plan([G|Gs], Solved, PP, Plan) :-
    write('.'),                             /* The marching dots. */
    solve(G,Solved,PP,NewSolved,NewPP),
    plan(Gs,NewSolved,NewPP,Plan).

/*-------------------  SOLVING A GOAL  ---------------------------*
 *
 *  solve(G,Solved,PP,NewSolved,NewPP) :-
 *      G         is a single goal,
 *      Solved    is a list of goals solved by PP,
 *      PP        is a partial plan,
 *      NewSolved is a list comprising G and Solved,
 *                in which G is not repeated, and
 *      NewPP     is a plan, containing PP as a subplan,
 *                which solves NewSolved.
 *
 *----------------------------------------------------------------*/

solve(G,Solved,PP,Solved,PP) :-      /* Case 1: G is always true. */
    always(G).

solve(G,Solved,PP,Solved,PP) :-     /* Case 2: G states something */
    call(G).                        /* that is true outside the   */
                                    /* planning "world." Example: */
                                    /* equality of objects.       */

solve(G,Solved,PP,NewSolved,PP) :-     /* Case 3: Goal G already  */
    holds(G,PP),                       /* holds in the state      */
    attach_if_new(G,Solved,NewSolved). /* produced by the current */
                                       /* partial plan.           */

solve(G,Solved,PP,[G|Solved],NewPP) :- /* Case 4: Look for an     */
    add(A,G),                          /* action A which adds G,  */
    achieve(A,G,Solved,PP,NewPP).      /* then solve G by         */
                                       /* achieving A.            */

/*--------------------  ACHIEVING AN ACTION  ---------------------*
 *
 *  achieve(A,G,Solved,PP,NewPP) :-
 *      G is a given goal to be solved by a given action A,
 *      Solved is a list of goals solved by PP,
 *      PP is a partial plan,
 *      NewPP is a new partial plan incorporating A into PP.
 *
 *  The procedure achieve/5 tries to apply a given action, A, i.e.,
 *  to insert it someplace into the current plan. Action A is
 *  applicable if
 *     (a) it does not delete any of the solved goals, Solved,
 *     (b) its precondition is consistent with those goals, and
 *     (c) a plan for achieving this precondition can be
 *         constructed.
 *
 *  Procedure achieve/5 has two clauses, corresponding to two ways
 *  of achieving an action:
 *
 *  Extension:
 *       1. Check that the action A preserves (i.e., does not
 *          delete any of) the solved goals, Solved.
 *       2. Look up the pre-conditions, Conds, in the database and
 *          check that Conds is consistent with Solved.
 *       3. All being well, call 'plan' recursively to change the
 *          current plan PP to a new plan NewPP, which produces a
 *          state in which Conds and Solved are attained. A can then
 *          be applied in NewPP, corresponding to the plan resulting
 *          from this call of 'achieve'.
 *       4. Finally, the check again that A preserves Solved; this
 *          repetition is necessary because A and Solved may not have
 *          been instantiated to ground terms at the time of the
 *          original check.
 *
 *  Insertion:
 *          If the last action A1 in the current partial plan does
 *          not delete the current goal G, we try to insert the
 *          action A somewhere before A1. To do so, we must retrace
 *          the set of solved goals to the point before A1.
 *
 *  Note that possible additions to Solved made by the recursive call
 *  to 'plan' are invisible to 'achieve'; they are only needed
 *  locally, during the construction of the intermediate plan,
 *  NewPP. The additional call to 'doesnt_delete_any' is necessary
 *  because of variables in the in the plan. For example, the action
 *  'move(b,a,W)' need not delete the fact 'clear(c)', so
 *  'doesnt_delete_any' lets it through; however, 'plan' may
 *  instantiate W as c, and this ought to cause a failure.
 *
 *  If, for any of these reasons, the action A cannot be added at
 *  the end of the plan, 'achieve' will try to undo the last action,
 *  A1, and insert A earlier into the plan. This is only possible
 *  if A1 does not delete the fact to be added by A. The procedure
 *  'retrace' removes from Solved all goals (facts) that may be
 *  established by A1 but are different from A1's preconditions.
 *  Specifically, it removes the goals added by A1 and the goals
 *  that constitute the preconditions of A1; the latter goals will
 *  be re-inserted by 'append'.
 *
 *----------------------------------------------------------------*/

achieve(A,_,Solved,PP,[A|NewPP]) :-                  /* Extension */
    doesnt_delete_any(A,Solved),
    precond(A,Conds),
    not inconsistent(Conds,Solved),
    plan(Conds,Solved,PP,NewPP),
    doesnt_delete_any(A,Solved).

achieve(A,G,Solved,[A1|PP],[A1|NewPP]) :-            /* Insertion */
    doesnt_delete(A1,G),
    retrace(A1,Solved,NewSolved),
    achieve(A,G,NewSolved,PP,NewPP),
    doesnt_delete(A1,G).


/*---------  CHECKING IF A GOAL (FACT) HOLDS IN A STATE  ---------*
 *
 *  holds(Goal,Plan) :-
 *      Goal holds after executing Plan.
 *
 *  A goal holds after executing a given plan if it is given or if
 *  it is added by one of the actions in the plan and preserved by
 *  all subsequent actions, if any.
 *
 *  Everything that holds in a state of the world can be determined
 *  from the plan which produces that state of the world. The system
 *  chains backwards through the sequence of actions, so long as
 *  none of those actions deletes the sought-for facts, until the
 *  fact is found in the add-set of an action or was given in the
 *  initial state.
 *
 *----------------------------------------------------------------*/

holds(G,[A|_]) :-
    add(A,G).
holds(G,[A|PP]) :-
    !,
    doesnt_delete(A,G),
    holds(G,PP),
    doesnt_delete(A,G).
holds(G,_) :-
    given(G).


/*-------  PROVING THAT AN ACTION PRESERVES A SET OF GOALS  ------*
 *
 *  doesnt_delete_any(Action,Goals) :-
 *      Action does not delete any of the goals.
 *
 *----------------------------------------------------------------*/

doesnt_delete_any(A,[G|Gs]) :-
    doesnt_delete(A,G),
    doesnt_delete_any(A,Gs).
doesnt_delete_any(_,[]).

doesnt_delete(A,G) :-
    make_ground(A,G),
    del(A,G),
    !,
    fail.
doesnt_delete(_,_).


/*------------  RETRACING AN ACTION ALREADY ACHIEVED  ------------*
 *
 *  retrace(A,Solved,NewSolved) :-
 *      NewSolved is formed from Solved by deleting the goals that
 *          are added by action A, but
 *          are not among A's preconditions.
 *
 *  In other words, remove only the new goals added by A.
 *  'retrace' first removes both the goals added by A and the
 *  the preconditions of A. The final 'append' re-installs the
 *  preconditions.
 *
 *----------------------------------------------------------------*/

retrace(A,Solved,NewSolved) :-
    precond(A,Conds),
    retrace_aux(A,Solved,Conds,AuxSolved),
    append(Conds,AuxSolved,NewSolved).

retrace_aux(A,[G|Gs],Conds,NewGs) :-       /* G is added by A:    */
    add(A,G1),                             /* omit G.             */
    G == G1, !,
    retrace_aux(A,Gs,Conds,NewGs).
retrace_aux(A,[G|Gs],Conds,NewGs) :-       /* G is one of A's     */
    member(G1,Conds),                      /* preconditions:      */
    G == G1, !,                            /* omit G.             */
    retrace_aux(A,Gs,Conds,NewGs).
retrace_aux(A,[G|Gs],Conds,[G|NewGs]) :-   /* Otherwise, keep G.  */
    retrace_aux(A,Gs,Conds,NewGs).
retrace_aux(_,[],_,[]).                    /* We're done.         */

/*-------  CHECKING A SET OF CONDITIONS FOR INCONSISTENCY  -------*
 *
 *  inconsistent(Conds,SolvedGoals) :-
 *      There is an 'impossible' set of Goals that
 *      (a) shares a goal with Conds and
 *      (b) is a subset of the union of Conds and SolvedGoals.
 *
 *----------------------------------------------------------------*/

inconsistent(Conds,Solved) :-
    make_ground(Conds,Solved),
    imposs(S),
    check(intersect(Conds,S)),
    append(Conds,Solved,L),
    subset(S,L), !.


/*-------  CONVERTING THE VARIABLES IN A TERM TO CONSTANTS --------*
 *
 * numbervars(Term,N1,N2) :-
 *    The distinct variables in Term are instantiated successively
 *    as constants of the form $VAR(N), where N ranges from N1 to
 *    N2 - 1.
 *
 * Example:
 *                  ?- numbervars(f(X,a,g(b,X,Y),Y,Z),3,N).
 *                  X = $VAR(3)
 *                  Y = $VAR(4)
 *                  Z = $VAR(5)
 *                  N = 6
 *
 *----------------------------------------------------------------*/


/*---------------------- UTILITY PROCEDURES -------------------------*/

print_plan([X|Rest]) :-
    print_plan(Rest),
    write(X),nl.
print_plan([]).

attach_if_new(X,P,P) :-
    member(Y,P),
    X == Y,                         /* HOW TO SOLVE IT:  equiv(X,Y), */
    !.
attach_if_new(X,P,[X|P]).

append([],L,L).
append([X|L],M,[X|N]) :-
    append(L,M,N).

member(X,[X|_]).
member(X,[_|L]) :-
    member(X,L).

intersect(S1,S2) :-
    member(X,S1),
    member(X,S2).

subset([],_).
subset([X|L],M) :-
    member(X,M),
    subset(L,M).

not_equal(X,Y) :-               /* May be used by problem-module. */
    not X = Y,
    not X = '$VAR'(_),
    not Y = '$VAR'(_).

equiv(X,Y) :-
    not not_equiv(X,Y).

not_equiv(X,Y) :-
    make_ground(X,Y),
    X = Y,
    !,
    fail.
not_equiv(X,Y).

make_ground(X,Y) :-
    numbervars(foo(X,Y),0,_).

/* Like a call, but doesn't instantiate variables */
check(Call) :-
    not(not(call(Call))).


again :-
    notrace,
    reconsult(warplan).

