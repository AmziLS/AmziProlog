% RUBDISP - Copyright (C) 1993, Amziod
% This file contains the display predicates.  

:-export cube_print/1.
:-export wrfield/2, rdfield/2, rdchar/2.
:-export writec/2.
:-export color/1,color/2,color/3.
:-export m_disp/1,m_erase/1,m_choose/2.

:-import error/1.               % rubik
:-import get_flag/2.            % rubik
:-import sidecolor/1.          % dynamic database

:- op(500,xfy,:).

% cube_print - displays the full color cube. Both variables and
%              blanks appear as spaces.  unification is again used
%              to map the input cube to the individual displays

cube_print(cube(F, R, U, B, L, D,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, 
  V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, 
  V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, 
  V41, V42, V43, V44, V45, V46, V47, V48, V49, 
  V50, V51, V52, V53, V54)) :-
  nl,
  tab(6), pc([V28, V45, V22]),
  tab(6), pc([V53, B, V51]),
  tab(6), pc([V25, V43, V19]),
  pc([V29, V54, V26, V27, V44, V21, V20, V52, V23]),
  pc([V37, L, V35, V36, U, V32, V31, R, V33]),
  pc([V17, V50, V14, V15, V40, V9, V8, V48, V11]),
  tab(6), pc([V13, V39, V7]),
  tab(6), pc([V49, F, V47]),
  tab(6), pc([V16, V41, V10]),
  tab(6), pc([V18, V42, V12]),
  tab(6), pc([V38, D, V34]),
  tab(6), pc([V30, V46, V24]),
  check_step,
  !.

check_step :-
  get_flag(stepmode, on),
  write($Hit Enter to continue$),
  get0(_).
check_step.
  

pc([]):- nl.
pc([V1| V2]):-
  sidecolor(V1 - C),
  write(C), tab(1),
%  write(V1), tab(1),
  pc(V2).

% wrfield & rdfield - allow input and output to a named field

wrfield(F,X):-
  field(F,P),
  write(P),
  write(X),
  nl.

rdfield(F,X):-
  field(F,P),
  write(P),
  read(X).

rdchar(F,X):-
  field(F,P),
  write(P),
  get(X).


% field - these are the field definitions for the cube program

field(prob, $Problem: $).
field(stage, $\nStage:   $).
field(target, $Target:  $).
field(rot, $Rotation: $).
field(try, $Trying: $).
field(prompt, $>$).
field(error, $Error: $).
field(done, $Done: $).
field(continue, $Hit Enter to continue.$).
field(stepmode, $Stepmode? (y/n): $).
field(history, $History? (y/n): $).
field(move, $Enter move\n(end with period, ex. u., -l., ct1., -tc3.) : $).
field(moves, $Moves: $).
field(rotations, $Rotations: $).
field(sequences, $Sequences: $).
field(end_disp, $Enter q. to end$).
field(msg20, $ $).
field(msg21, $ $).

m_disp(Menu):-
  menu(Menu, Choices),
  m_dis(1, Choices), !.

m_dis(_, []) :- nl.
m_dis(N, [H|T]) :-
  write($[$),write(N),write($]$),
  write(H), tab(1),
  NN is N + 1,
  m_dis(NN, T).

m_choose(Menu,Choice):-
  write($Choice: $),
  get(Nascii),
  N is Nascii - 0'0,
  menu(Menu, Choices),
  m_ch(N, Choices, Choice).

m_ch(N, [], _) :- write($Bad menu choice, try again$), nl, fail.
m_ch(1, [X|_], X) :- !.
m_ch(N, [H|T], X) :-
  NN is N - 1,
  m_ch(NN, T, X).

menu(main, [solve, manual, help, exit]).
menu(solve, [random, manual, edit]).

