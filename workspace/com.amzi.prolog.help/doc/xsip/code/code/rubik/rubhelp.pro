% RUBHELP - Copyright (C) 1994, Amzi! inc.
% This the the help you get when you ask for help.

:- export rub_help/0.

rub_help:-
  helpscreen(_),
  nl,write($[more - hit any key to continue]$),
  get1(_),
  fail.
rub_help.

helpscreen(intro):-
  write($INTRODUCTION$),nl,nl,
  write($The cube solver will generate a sequence of moves that will$),nl,
  write($solve any given cube (if solvable).  See rubdoc1.txt for$),nl,
  write($notes on the method.$),nl,nl.
helpscreen('menu options'):-
  write($MAIN MENU OPTIONS$),nl,nl,
  write($Solve - solves three types of cubes (from submenu)$),nl,nl,
  write($      random - generate a random cube to solve$),nl,
  write($      manual - allows you to scramble your own$),nl,
  write($      edit   - allows you to describe a real cube$),nl,nl,
  write($        with the option (prompts)$),nl,nl,
  write($      stepmode - stops after each sequence (useful if$),nl,
  write($                 solving a real cube)$),nl,
  write($Manual - allows manipulation of cube (useful to see the$),nl,
  write($         effects of all the legal moves)$),nl,nl,
  write($Help   - this stuff$),nl,nl,
  write($Exit   - return to dos$),nl.
helpscreen(notation):-
  write($NOTES ON NOTATION$),nl,nl,
  write($The cube is unfolded so all six sides are visible.  All moves$),nl,
  write($are labeled by the side they affect.  The letters used are:$),nl,nl,
  write($                 B - back$),nl,
  write($       L - left  U - up    R - right$),nl,
  write($                 F - front$),nl,
  write($                 D - down$),nl,nl,
  write($Directions - + clockwise, - counterclockwise$),nl,nl,
  write($Pieces are referred to by color.  The colors are:$),nl,nl,
  write($         W - white, G - green, B - blue, Y - yellow,$),nl,
  write($         R - red (PC magenta), O - orange (PC red) $),nl,nl,
  write($Moves - three types$),nl,nl,
  write($     Side moves - represented by single side letter, ex +r$),nl,
  write($     Rotations - rotate entire cube, preface side with r$),nl,
  write($                 ex. -ru, +rr (used to exploit symmetry)$),nl,
  write($     Sequences - sequence of moves by name ex. +ct1$),nl.
helpscreen('solve display'):-
  write($SOLVE DISPLAY FIELDS$),nl,nl,
  write($Stage - the current stage (see rubdoc1.txt)$),nl,nl,
  write($Target - the piece being solved for$),nl,nl,
  write($Trying - the n-1 nodes of the breadth first search$),nl,nl,
  write($Rotation - the chosen sequence of moves for the current goal$),nl,nl,
  write($Hit any key to end$).

