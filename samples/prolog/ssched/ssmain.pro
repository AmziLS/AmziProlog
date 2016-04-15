/* Amzi! Sports Scheduling Demo

   Amzi! inc.
   e-mail info@amzi.com
   http://www.amzi.com
   */

/* --------------------------------------
   The Prolog file SSCHED.PRO is designed to provide
   scheduling services to another module that provides
   the user interface.  The primary user interface is
   written in Delphi, but any tool can be used.  This
   Prolog file is a pure Prolog interface, designed to
   be used from a Prolog listener.  */

main :-
  clear_schedule,
  read_teams,
  read_cycles(NCycles),
  read_rounds(NCycles),
  !,
  schedule(NCycles),
  final_schedule(S),
  nl,nl,
  disp_sched(S),
  disp_team_scheds(S),
  nl,nl,
  save_option,
  nl.

/* Get the names of the teams */

read_teams :-
  retractall(team(_,_)),
  write($Enter Teams/Players, CR to end\n\n$),
  cntr_set(3, 1),
  repeat,
  write('> '),
  read_string(T),
  rd_team(T).

  rd_team($$) :-
    init_teams.
  rd_team(T) :-
    cntr_inc(3, N),
    assert(team(N, T)),
    fail.

/* Get the number of cycles */

read_cycles(NCycles) :-
  retractall(ncycles(_)),
  write($How many matches does each pair play (1 or 2)? $),
  read_string(Str),
  string_integer(Str, NCycles),
  (NCycles = 1; NCycles = 2),
  !.
read_cycles(NCycles) :-
  write($Cycles must be 1 or 2, try again\n$),
  read_cycles(NCycles).

/* and the number of rounds */

read_rounds(_) :-
  write($Customize round names? (y/n) $),
  read_string(Ans),
  Ans \= $y$,
  !,
  assert(round(N, round(N))).
read_rounds(NCycles) :-
  teams(NTeams, _),
  NRounds is NCycles * (NTeams - 1),
  for(I, 1, NRounds, 1),
    write($Round $), write(I), write($: $),
    read_string(RName),
    assert(round(I, RName)),
    fail.
read_rounds(_).

/* let the user save the results */
  
save_option :-
  write($Enter file name to save schedule, or Enter to skip: $),
  read_string(SSaveFile),
  SSaveFile \= $$,
  !,
  saveas(SSaveFile).
save_option.

/* a predicate called by the scheduler to report on its
   progress */

report(X, Continue) :-
  write(X), 
%  write($ continue(y/n): $),
%  read_string(Ans),
  Ans = y,
  (Ans = y -> Continue = go; Continue = stop).