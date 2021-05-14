% RUBEDIT - Copyright (C) 1994, Amzi! inc.
%           This module allows the user to easily enter a scrambled
%           cube position.  the cube is displayed in goal form.
%           the cursor keys move from tile to tile, and the f1 key
%           selects the color for the tile.  repeated hits of f1
%           changes the color.  f1 was chosen since that allows a
%           machine with a pcmouse to do cube editing with the mouse and
%           and the left button (f1) with no special changes.

:-export redit/1.
:-export set_tcolor/1.
:-export get_color/1.
:-export rewrite/2.

:-import cube_print/1.        % rubdisp
:-import error/1.             % rubik
:-import ghoul/1.             % rubdata
:-import wrfield/2.           % rubdisp
:-import sidecolor/1.			% dynamic database

redit(Y):-
  ghoul(G),
  cube_print(G),
  write($Enter single letters separated by spaces in the pattern$),nl,
  write($of the display.  The letters should represent the colors$),nl,
  write($on your cube.  Exact spacing isn't critical.$),nl,
  read_cube(X),            % read it off the screen
  trans_cube(X,Y),         % change colors to side notation
  cube_print(Y).
redit(_):-
  error('failing edit'),halt.

% read_cube - reads the edited cube directly from the screen, there was
% no need to save information about colors during the cursor movement
% stage ("edi").  it was for this reason that "change_color" writes the
% letter of the color in the tile.

% read_cube looks exactly like print_cube, only in reverse

read_cube(cube(F, R, U, B, L, D,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, 
  V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, 
  V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, 
  V41, V42, V43, V44, V45, V46, V47, V48, V49, 
  V50, V51, V52, V53, V54)):-
  rc([V28, V45, V22]),
  rc([V53, B, V51]),
  rc([V25, V43, V19]),
  rc([V29, V54, V26, V27, V44, V21, V20, V52, V23]),
  rc([V37, L, V35, V36, U, V32, V31, R, V33]),
  rc([V17, V50, V14, V15, V40, V9, V8, V48, V11]),
  rc([V13, V39, V7]),
  rc([V49, F, V47]),
  rc([V16, V41, V10]),
  rc([V18, V42, V12]),
  rc([V38, D, V34]),
  rc([V30, V46, V24]),
  !.

rc([]):- !.
rc([V1| V2]):-
  get(X),
  name(V1,[X]),
  !,rc(V2).

trans_cube(X,Y):-
  get_color(X),        % establish new side colors
  rewrite(X,Y).        % translate color notation to side notation

get_color(X):-
  X=..[cube,F,R,U,B,L,D|_],         % the sides in color notation
  set_tcolor(['F'-F,'R'-R,'U'-U,'B'-B,'L'-L,'D'-D]).

rewrite(C,S):-
  var(S),                  % this one if color input and side output
  C=..[cube|Clist],
  rewrit(Clist,Slist),
  S=..[cube|Slist],!.
rewrite(C,S):-
  var(C),              % this one if side input, and color out.  it
  S=..[cube|Slist],    % is called by the manual routine when building
  rewrit(Clist,Slist), % a cube to solve.  rotate moves might have been used
  C=..[cube|Clist],!.  % which changed the side colors

rewrit([],[]):- !.
rewrit([X|Ctail],[Y|Stail]):-
  var(X), var(Y),
  !, rewrit(Ctail,Stail).
rewrit([Color|Ctail],[Side|Stail]):-
  sidecolor(Side-Color),
  !,rewrit(Ctail,Stail).

set_tcolor([]):- !.
set_tcolor([S-C|Tail]):-
  retract(sidecolor(S-_)),
  assert(sidecolor(S-C)),
  !,set_tcolor(Tail).

