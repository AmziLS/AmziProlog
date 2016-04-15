                       /* STRIPS-WORLD */
            /* HOW TO SOLVE IT, Problem 76, p. 104 */

add(climboff(B),     onfloor).
add(climbon(B),      on(robot,B)).
add(gothru(D,R1,R2), inroom(robot,R2)).
add(goto1(P,R),      at(robot,P)).
add(goto2(X,R),      nextto(robot,X)).
add(pushto(X,Y,R),   nextto(X,Y)).
add(pushto(X,Y,R),   nextto(Y,X)).
add(turnon(S),       status(S,on)).

del(Action, at(X,Z)) :-
    moved(X,Action).
del(Action, nextto(Z,robot)) :-
    !,
    del(Action, nextto(robot,Z)).
del(pushto(X,Y,R), nextto(robot,X)) :-
    !,
    fail.
del(climbon(B),nextto(robot,B)) :-
    !,
    fail.
del(climboff(B),nextto(robot,B)) :-
    !,
    fail.
del(Action, nextto(X,Z)) :-
    moved(X, Action).
del(Action, nextto(Z,X)) :-
    moved(X, Action).
del(Action, on(X,Z)) :-
    moved(X, Action).
del(climbon(B),onfloor).
del(gothru(B,R1,R2),inroom(robot,Z)).
del(turnon(S),status(S,Z)).

moved(robot,goto1(P,R)).
moved(robot,goto2(X,R)).
moved(robot,pushto(X,Y,R)).
moved(X,    pushto(X,Y,R)).
moved(robot,climbon(B)).
moved(robot,climboff(B)).
moved(robot,gothru(D,R1,R2)).

precond(goto1(P,R),       [ locinroom(P,R),
                            inroom(robot,R),
                            onfloor ]).

precond(goto2(X,R),       [ inroom(X,R),
                            inroom(robot,R),
                            onfloor ]).

precond(pushto(X,Y,R),    [ pushable(X),
                            inroom(Y,R),
                            inroom(X,R),
                            nextto(robot,X),
                            onfloor ]).

precond(turnon(lightswitch(S)),
                          [ on(robot,box(1)),
                            nextto(box(1),
                            lightswitch(S)) ]).

precond(climbon(box(B)),  [ nextto(robot,box(B)),
                            onfloor ]).

precond(climboff(box(B)), [ on(robot,box(B)) ]).

precond(gothru(D,R1,R2),  [ connects(D,R1,R2),
                            inroom(robot,R1),
                            nextto(robot,D),
                            onfloor ]).

always(connects(D,R1,R2)) :-
    connects1(D,R1,R2).
always(connects(D,R2,R1)) :-
    connects1(D,R1,R2).
/*                                       How to Solve It:
always(inroom(D,R1)) :-
    always(connects1(D,R0,R1)).
*/
always(inroom(D,R1)) :-                  /* Polish */
    always(connects(D,R1,R2)).
always(pushable(box(N))).
always(locinroom(point(N),room(1))) :-   /* Polish */
    range(N,1,5).
always(locinroom(point(6),room(4))).
always(inroom(lightswitch(1),room(1))).
always(at(lightswitch(1),point(4))).

connects1(door(N),room(N),room(5)) :-
    range(N,1,4).

range(M,M,_).
range(M,L,N) :-
    L < N,             /* HOW TO SOLVE IT: not_equal(L,N) */
    L1 is L + 1,
    range(M,L1,N).



given_list([ at(box(1),point(1)),
             at(box(2),point(2)),
             at(box(3),point(3)),
             at(robot, point(5)),
             inroom(box(1),room(1)),
             inroom(box(2),room(1)),
             inroom(box(3),room(1)),
             inroom(robot,room(1)),
             onfloor,
             status(lightswitch(1),off) ]).


run1 :-
    given_list(Start),
    warplan(Start,[status(lightswitch(1),on)]).

run2 :-
    given_list(Start),
    warplan(Start,[ nextto(box(1),box(2)),
            nextto(box(2),box(3)) ]).

run3 :-
    given_list(Start),
    warplan(Start,[at(robot,point(6))]).

run4 :-
    given_list(Start),
    warplan(Start,[ nextto(box(2),box(3)),
            nextto(box(3),door(1)),
            status(lightswitch(1),on),
            nextto(box(1),box(2)),
            inroom(robot,room(2)) ]).

restrips :-
    reconsult(strips).








