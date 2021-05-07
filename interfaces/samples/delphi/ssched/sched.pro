/* Amzi! Sports Scheduling Demo
   Copyright (c) 1996-98 Amzi! inc. All Rights Reserved.
*/
  
/* ------------------------------------------------------
   The main schedule interface.  These are the predicates
   that provide services to the user interface portion of
   the program, whether it is the Prolog section in 
   SSMAIN.PRO or written in a host language such as Delphi 
   in the demo.

   To use with the Delphi front-end, compile and link this
   program into an XPL file.

   The calling program must first clear the schedule, then
   assert schedule information about teams (team/2) and
   rounds (round/2).  Then it calls schedule/1 and retrieves
   the scheduled rounds (round/3).

   clear_schedule - clear dynamic database for new
      schedule

   init_teams - called to initialize the team list

   schedule(N) - run the main scheduler, N is 1 or 2
      for number of cycles

   saveas(F) - save text version of schedule in file F

   Dynamic database predicates used by schedule:

   team(N, Name) - team number and name for each team,
      asserted by calling program

   round(N, Name) - round number and name for each round,
      asserted by calling program

   Coding convention - It is quite common in a recursive
   Prolog predicate to have two arguments used in the
   building of a list.  One argument holds the accumulated
   results of the list from level to level.  The other
   argument will hold the completed list, and is unified
   with the accumulated result when the recursion reaches
   the boundary condition.

   This construct is represented by the + operator in this
   code.  So, if you see two variables separated by a +
   operator, it means the first is an accumulator and the
   second is the final result.
*/

/* Clear the dynamic database of assertions */

clear_schedule :-
  retractall(team(_,_)),
  retractall(teams(_,_)),
  retractall(round(_,_)),
  retractall(round(_,_,_)),
  retractall(final_schedule(_)),
  retractall(fault_count(_)),
  assert(fault_count(0)).

/* Initialize the list of teams */

init_teams :-
  findall(N, team(N, _), LN),
  length(LN, L),
  (L mod 2 =:= 1 ->
    assert(team(0, bye)), TL = [0|LN], LL is L + 1
    ; TL = LN, LL is L),
  assert(teams(LL, TL)).

/* Run the main scheduler */

schedule(N) :-
  catch( sched(N), Exception, process_ex(Exception) ).

process_ex(give_up) :- fail.
process_ex(_) :- fail.

sched(1) :-
  games(G),
  !,
  first_half(1-I2, G, []+Sr, []+_),
  reverse(Sr, S),
  assert_sched(S).
sched(2) :-
  games(G),
  !,
  first_half(1-I2, G, []+S1, []+G2),
  once( reverse(G2, G2r) ),
  second_half(I2, G2r, []+S2),
  append(S2, S1, R),
  reverse(R, S),
  assert_sched(S).

/* Save the schedule output to a text file */

saveas(F) :-
  string_atom(F, FA),
  final_schedule(S),
  tell(FA),
  disp_sched(S), nl,
  disp_team_scheds(S), nl,
  told.
saveas(F) :- told.


/* The predicates that do the work of scheduling
   --------------------------------------------- */

/* first_half schedules the first half of a full
   2-cycle round robin, second half the second.
   Most all of the predicates have slightly
   different forms for each half.  In particular
   the first half predicates keep a list of the
   rematch games for feeding to the second
   half predicates, which don't need to carry that
   extra argument */
    
first_half(I2-I2, [], S+S, G2+G2).
first_half(I-I2, G, Sa+S, G2a+G2) :-
  sched1_round(I, G, Round, Gx, G2x),
  once( append(G2x, G2a, G2n) ),
  status(I),
  II is I + 1,
  first_half(II-I2, Gx, [Round|Sa]+S, G2n+G2).

second_half(I, [], S+S).
second_half(I, G, Sa+S) :-
  sched2_round(I, G, Round, Gx),
  status(I),
  II is I + 1,
  second_half(II, Gx, [Round|Sa]+S).

status(I) :-
  report(finished(I), Continue),
  (Continue=stop -> throw(give_up); true).
status(I) :-
  retract(fault_count(N)),
  NN is N + 1,
  assert(fault_count(NN)),
  report(fault(NN), Continue),
  (Continue=stop -> throw(give_up); true),
  fail.

/* Initialize a round and get the games for
   that round */

sched1_round(I, G, round(I, Games), Gz, G2) :-
  init1_round(I, Games, G2),
  get1_games(I, G, Games),
  once( left1_to_sched(G, Games, Gz) ).

sched2_round(I, G, round(I, Games), Gz) :-
  init2_round(I, Games),
  get2_games(I, G, Games),
  once( left2_to_sched(G, Games, Gz) ).

/* Each round is initialized by a template.  In
   this case, each is the same general pattern,
   but this mechanism allows specific games to
   be placed on specific rounds. */

init1_round(N, Games1, Games2) :-
  teams(NT, _),
  template1(NT, []+Games1, []+Games2R),
  reverse(Games2R, Games2).

init2_round(N, Games) :-
  teams(NT, _),
  template2(NT, []+Games).

/* The template for a round is a list of games, in
   this case set to a number of variable pairs.  For
   the first half, the template includes two lists, one
   with a game, and the other with the rematch of the
   game.  For the second half, the template is just
   the games. */

template1(0, G1+G1, G2+G2).
template1(NT, G1a+G1, G2a+G2) :-
  NT > 0,
  NT2 is NT-2,
  template1(NT2, [Away-Home|G1a]+G1, [Home-Away|G2a]+G2).

template2(0, G+G).
template2(NT, Ga+G) :-
  NT > 0,
  NT2 is NT - 2,
  template2(NT2, [T1-T2|Ga]+G).

/* After the games have been scheduled for a day, take
   them out of the list of games left to be scheduled. */

left1_to_sched(G, [], G).
left1_to_sched(G, [Away-Home|X], Gl) :-
  delete(Away-Home, G, Gx),
  delete(Home-Away, Gx, Gy),
  left1_to_sched(Gy, X, Gl).

left2_to_sched(G, [], G).
left2_to_sched(G, [Game|X], Gl) :-
  delete(Game, G, Gx),
  left2_to_sched(Gx, X, Gl).

/* Generate games for a given round by walking the pairs
   in the template for the round, unifying them with a game
   from the list of games left to play.  After picking a
   game for the round, clean the list so that no other games
   involving either of those teams will be scheduled that
   round. */

get1_games(_, [], []).
get1_games(I, G, [Away-Home|Gs]) :-
  deal(Away-Home, G, Gx),
  once( clean(Gx, Home, Away, Gn) ),
  get1_games(I, Gn, Gs).

get2_games(_, [], []).
get2_games(I, G, [Away-Home|Gs]) :-
  deal(Away-Home, G, Gx),
  once( clean(Gx, Home, Away, Gn) ),
  get2_games(I, Gn, Gs).


/* Textual display of full schedule
   -------------------------------- */

disp_sched([]).
disp_sched([round(N, Games)|T]) :-
  round(N, Date),
  write(Date), nl,
  disp_games(Games), nl,
  disp_sched(T).

  disp_games([]).
  disp_games([Bye|X]) :-
    (Bye = 0-T; Bye = T-0),
    !,
    team(T, Name),
    tab(2), write(Name), tab(1),
    write(bye), nl,
    disp_games(X).
  disp_games([H-A|X]) :-
    team(H, Home),
    team(A, Away),
    tab(2), write(Home), tab(1), write('@'),
    tab(1), write(Away), nl,
    disp_games(X).

/* Display the schedule for each team */

disp_team_scheds(S) :-
  teams(_, T),
  team_scheds(T, S).

  team_scheds([], _).
  team_scheds([0|X], S) :-
    team_scheds(X, S).
  team_scheds([T|X], S) :-
    team(T, Name),
    write($\nSchedule for : $), write(Name), nl,
    team_sched(T, S),
    team_scheds(X, S).

  team_sched(_, []).
  team_sched(T, [round(N, Games)|X]) :-
    round(N, Date),
    disp_round(T, round(Date, Games)),
    team_sched(T, X).

  disp_round(T, round(N, Games)) :-
    member(T-O, Games), !,
    team(O, Name),
    write(N),
    (O > 0 -> write($: Away $); write($: $)),
    write(Name), nl.
  disp_round(T, round(N, Games)) :-
    member(O-T, Games), !,
    team(O, Name),
    write(N),
    (O > 0 -> write($: Home $); write($: $)),
    write(Name), nl.


/* Utility Predicates
   ------------------ */

/* Generate a list of games from the list of teams.  Home
   and away games are alternated to provide better balance
   in the schedule.  The game list is the primary input to
   the scheduler, which picks games from the list to build
   each round's schedule.  The game list is of the form:

   [2-1, 1-3, 4-1, 1-5, ...... 1-2, 3-1, 1-4...] */

games(G) :-
  teams(_, T),
  g_list(T, T, []+G).

  g_list([_], [], G+G).
  g_list([_|X], [], A+G) :-
    teams(_, T),
    g_list(X, T, A+G).
  g_list([T|X1], [T|X2], A+G) :-
    g_list([T|X1], X2, A+G).
  g_list([T1|X1], [T2|X2], A+G) :-
    (T1+T2) mod 2 =:= 1, !,
    g_list([T1|X1], X2, [T2-T1|A]+G).
  g_list([T1|X1], [T2|X2], A+G) :-
    g_list([T1|X1], X2, [T1-T2|A]+G).

/* reverse a list */

reverse(F, R) :-
  rev(F, [], R).

  rev([], R, R).
  rev([H|T], A, R) :-
    rev(T, [H|A], R).

/* append two lists together */

append([], Z, Z).
append([H|X], Y, [H|Z]) :-
  append(X, Y, Z).

/* find members of a list */

member(X, [X|_]).
member(X, [_|Y]) :-
  member(X, Y).

/* get the length of a list */

length(L, N) :-
  len(L, 0, N).

  len([], N, N).
  len([_|X], A, N) :-
    AA is A + 1,
    len(X, AA, N).

/* clean removes all the games that no longer make
   sense for a given round from the list of remaining
   games.  If, for example, we've selected game 3-8 for
   the round we're working on, then we don't need to look
   at any other games involving teams 3 or 8. */

clean(Gx, Home, Away, Gn) :-
  remove(Home, Gx, Gy),
  remove(Away, Gy, Gn).

clean(Gx, T, Gn) :-
  remove(T, Gx, Gn).

  remove(T, Gx, Gz) :-
    delete(T-_, Gx, Gy),
    remove(T, Gy, Gz).
  remove(T, Gx, Gz) :-
    delete(_-T, Gx, Gy),
    remove(T, Gy, Gz).
  remove(T, Gx, Gz) :-
    delete(T=T, Gx, Gy),
    remove(T, Gy, Gz).
  remove(_, Gn, Gn).

/* set an element of a list to a certain value */

set_elem(1, X, [_|Z], [X|Z]) :- !.
set_elem(N, X, [A|Z1], [A|Z2]) :-
  NN is N - 1,
  set_elem(NN, X, Z1, Z2).

/* delete an element from a list and return the
   remaining list.  When the first argument is
   a variable, delete/3 can be used in a back-tracking
   loop to select elements from a list. */

delete(A,[A|X],X).
delete(A,[B|X],[B|Y]) :- delete(A,X,Y).

/* deal/3 (like deal cards from a deck) is often identical
   to delete/3.  In this application, deal/3 is used to
   select games to schedule on a given round, and the list of
   games for the round can be in any order.  So, deal/3 is
   modified so that it only returns the remainder of a list,
   thereby eliminating permutations from the search. */

deal(A-H, X, Y) :-
  var(A), var(H),
  !, pick(A-H, X, Y).
deal(A-H, X, Y) :-
  delete(A-H, X, Y).

pick(A,[A|X],X).
pick(A,[B|X],Y) :- pick(A,X,Y).

/* delete all the elements in the first list from the
   second, returning the remainder in the third. */

delete_list([], A, A).
delete_list([G|X], A, Z) :-
  delete(G, A, A2),
  delete_list(X, A2, Z).

/* same as delete_list, except some of the elements in the
   first list might not be in the second.  The third argument
   contains the list of elements that were actually deleted. */

delete2_list([], Del, Del, Gz, Gz).
delete2_list([G|X], Ad, Del, Ag, Gz) :-
  once( delete(G, Ag, AGx) ), !,
  delete2_list(X, [G|Ad], Del, AGx, Gz).
delete2_list([G|X], Ad, Del, Ag, Gz) :-
  delete2_list(X, Ad, Del, Ag, Gz).

/* delete a pattern, find all the elements that match it.
   /3 version just deletes, /5 version returns list of
   deleted elements */

delete_pat(_, [], []).
delete_pat(P, [H|X], X2) :-
  not(not(P=H)), !,
  delete_pat(P, X, X2).
delete_pat(P, [H|X], [H|X2]) :-
  delete_pat(P, X, X2).

delete_pat(_, [], [], Z, Z).
delete_pat(P, [H|X], X2, A, Z) :-
  not(not(P=H)), !,
  delete_pat(P, X, X2, [H|A], Z).
delete_pat(P, [H|X], [H|X2], A, Z) :-
  delete_pat(P, X, X2, A, Z).

/* move all elements of a list that match a pattern to
   the end of the list */

move_to_back(P, G, G2) :-
  delete_pat(P, G, GLeft, [], GDel),
  append(GLeft, GDel, G2).

/* move all elements of a list that match a pattern to
   the front of the list */

move_to_front(P, G, G2) :-
  delete_pat(P, G, GLeft, [], GDel),
  append(GDel, GLeft, G2).
  
/* scramble a list into a random order, can be used
   to scramble starting team or game list so that
   each run produces a random variation of a valid
   schedule */

shuffle(Tin, Tout) :-
  time(_, Min, Sec, _),
  N is Min + Sec,
  set_random(N),
  shuffle1(Tin, [], Tout).

  set_random(N) :-
    for(I, 1, N, 1),
    X is random,
    fail.
  set_random(_).

  shuffle1([], A, A).
  shuffle1(Tin, A, Tout) :-
    length(Tin, L),
    N is 1 + integer( random * L ),
    deleteN(N, Elem, Tin, Tx),
    shuffle1(Tx, [Elem|A], Tout).
    
  deleteN(1, H, [H|Z], Z).
  deleteN(_, _, [], []) :- !, fail.
  deleteN(N, H, [X|Z], [X|Z2]) :-
    NN is N - 1,
    deleteN(NN, H, Z, Z2).

/* translate schedule list into separate assertions
   that are easier to pick up by a host language
   program */

assert_sched(S) :-
  retractall(round(_,_,_)),
  assert(final_schedule(S)),
  asrt_sched(S).

  asrt_sched([]).
  asrt_sched([round(N, Games)|X]) :-
    as_sch(N, Games),
    asrt_sched(X).

    as_sch(_, []).
    as_sch(N, [A-H|X]) :-
      team(A, Away),
      team(H, Home),
      round(N, Round),
      assert(round(Round, Away, Home)),
      as_sch(N, X).
