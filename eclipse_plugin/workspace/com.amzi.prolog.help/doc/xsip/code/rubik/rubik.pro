% CUBE SOLVER II
%   A Rubik's Cube Solver
%   written by Dennis Merritt
%   as described in Building Expert Systems in Prolog (Springer-Verlag)
%   available from:
%     Amzi! inc.
%     40 Samuel Prescott Dr.
%     Stow, MA 01775 USA
%     Tel 508/897-7332, FAX 508/897-2784
%     e-mail amzi@world.std.com
%
%  This program may be copied, modified and redistributed although proper
%  acknowledgement is appreciated.
%
%  This implementation was done with Cogent Prolog, also available
%  from Amzi! inc.
%
%  This is the main module which contains the predicates for 
%         the main control loop,
%         manual mode,
%         solve mode, and
%         utility functions.
%
% Note - The Cogent/Prolog compiler supports modules.  The export declarations
%        are for predicates defined in the current module which may be used
%        by other modules.  The import declarations are for predicates 
%        defined in other modules.

:-export main/0.
:-export append/3.
:-export get_flag/2.
:-export set_flag/2.
:-export error/1.
:-export reverse/2.

:-import add_history/1.     % rubhist
:-import cnd/2.             % rubdata
:-import cube_print/1.      % rubdisp
:-import get_color/1.       % rubedit
:-import pristine/1.        % rubdata
:-import rub_help/0.        % rubhelp
:-import m_disp/1.          % rubdisp
:-import m_choose/2.        % rubdisp
:-import move/3.            % rubmov
:-import orientation/2.     % rubdata
:-import pln/2.             % rubdata
:-import rdfield/2.         % rubdisp
:-import rdchar/2.          % rubdisp
:-import redit/1.           % rubedit
:-import rewrite/2.         % rubedit
:-import rot/3.             % rubmov
:-import seq/2.             % rubdata
:-import side_color/1.      % rubdata
:-import s_r/2.             % rubdata
:-import vw/2.              % rubdata 
:-import wrfield/2.         % rubdisp
:-import writec/2.          % rubdisp

:-import logfile/1.			% dynamic db
:-import impplan/1.        % dynamic db
:-import state/1.          % dynamic db
:-import crit/1.           % dynamic db
:-import ghoul/1.          % dynamic db
:-import sidecolor/1.      % dynamic db
:-import flag/2.				% dynamic db
:-import cand/1.				% dynamic db
:-import candmove/1.			% dynamic db

:-op(500,xfy,:).

main :- banner, go.  % The start up entry point

go:-                       % The main control loop 
  repeat,
  init_color,
  m_disp(main),            % The main menu
  m_choose(main,X),        % Select an item
  do(X),                   % Execute it
  X == exit.               % Go back to the repeat or end

% These are the predicates which are called for the various
% main menu choices.  The cut after each ensures they wont be
% backtracked into when the main loop fails.

do(solve):-solve,!.        % in this module
do(manual):-manual,!.      % in this module
do(help):-rub_help,!.      % in rubhelp
do(exit).                  % built-in predicate to exit

banner:-
  nl,nl,
  write($Cube Solver II$),nl,
  write($An illustrative Prolog program from$),nl,
  write($Building Expert Systems in Prolog (Springer-Verlag) by Dennis Merritt$),nl,
  write($implemented in Cogent Prolog$),nl,nl,
  write($For more information contact:$),nl,
  write($Amzi! inc.$),nl,
  write($40 Samuel Prescott Dr.$),nl,
  write($Stow, MA 01775 USA$),nl,
  write($Tel 508/897-7332, FAX 508/897-2784$),nl,
  write($e-mail amzi@world.std.com$),nl,nl.

% These predicates initialize the state to the goal state (ghoul),
% and allow you to enter single moves.  They are intended to demonstrate the
% effects of the various sequences used by the solve routines.

% They are also called by the solve routine if manual scrambling 
% is requested

manual:-
  pristine(G),                            % Start with the goal state
  retractif(state(_)),
  assert(state(G)),
  cube_print(G),                       % Display it
  disp_moves,                          % List the possible moves
  repeat,                              % Start repeat-fail loop
  rdfield(move,M),                     % Get a move
  (M==q, nl, !                         % If '', clear and end
   ;
   state(S),
   man_move(M,S,S2),                   % Apply move to it
   retract(state(_)),
   assert(state(S2)),
   cube_print(S2),fail).               % Print it and fail back

man_move(M,S,S2):-
  movel(M,S,S2),!.
man_move(M,S,S2):-               % Pop a + in front of an unsigned move
  movel(+M,S,S2),!.
man_move(M,_,_):-
  error('Unknown move'-M),!,fail. 

disp_moves:-                  % List the three types of moves
  wrfield(moves,''),          % Heading
  move(X,_,_),                % Will backtrack through all moves
  write(X),tab(1),            % Write move
  fail.                       % Go back for the next one
disp_moves:-                  
  nl,
  wrfield(rotations,''),      % No more moves, do the same for rots
  rot(X,_,_),
  write(X),tab(1),
  fail.
disp_moves:-                  % And again for seqs
  nl,
  wrfield(sequences,''),
  seq(X,_),
  write(X),tab(1),
  fail.
disp_moves:-                 % Got em all, end
  nl,
  wrfield(end_disp,'').

% This is the main body of the program which actually solves the cube.
% See rubdoc1 and rubdoc2 for the big picture

solve:-
  m_disp(solve),              % solve submenu
  m_choose(solve,X),
  rdchar(stepmode,SM),
  (SM==0'y , set_flag(stepmode,on)   % check for a y (scan code 21)
   ;
   set_flag(stepmode,off)),
  solve(X).                   % call solve w/ arity one with menu choice

solve(X):-
  init_solve(X),              % initialize all the stuff
  T1 is cputime,
  stages,
  T is cputime - T1,
  state(S),
  cube_print(S),
  write($Done  time = $),
  write(T), nl, nl.
solve(X):-
  error('failing to solve'),
  halt.             % something wrong, back to main

init_solve(X):-
  wrfield(prob,X),
  initialize(X),               % getting closer to the real work
  !.

initialize(X):-
  pristine(G),
  retractall(ghoul(_)),
  assert(ghoul(G)),
  init_crit(Crit),           % set up the initial criteria (all variables
  retractall(crit(_)),
  assert(crit(Crit)),
  retractall(stage(_)),
  assert(stage(1)),           % the first stage will call the others
  !,initial(X).              % get specific start state in the database

initial(random):-                 % create a new random cube
  random_cube(Cube),
  retractall(state(_)),
  assert(state(Cube)), !.
initial(edit):-                   % edit your own
  redit(Cube),
  retractall(state(_)),
  assert(state(Cube)),
  new_colors(Cube), !.
initial(manual):-                 % scramble your own
  manual,
  state(Cube),
  new_colors(Cube),!.

stages:-
  repeat,
  retract(stage(N)),
  init_stage(N,Plan),        % Set the stage, get the plan
  state(S),
  cube_print(S),
  build_plan(Plan),
  improve(N,Plan),                % Put the pieces in the plan in place
  vw(N,V),                   % undo the stage view (done by init_stage)
  undo_view(V),
  N2 is N + 1,               % next stage
  assert(stage(N2)),
  N2 >= 7.

build_plan([]) :- !.
build_plan([H|T]) :-
  assert(impplan(H)),
  build_plan(T).

% init_stage goes to rubdata to get the table entries which define
% the heuristics for the stage

init_stage(N,Plan):-         % return list of target pieces for this stage
  wrfield(stage,N),
  cnd(N,Cands),              % set up candidate moves used by search
  build_cand(Cands),
  vw(N,V),                   % set up preferred view for stage
  set_view(V),
  pln(N,Plan),!.             % get list of target pieces

% improve - works through the list of target pieces for the stage.
%           it first checks to see if its already in place

improve(Stage,[]) :- !.
improve(Stage,[Piece|Rest]) :-
  impro(Stage,Piece),
  !, improve(Stage,Rest).

improve(Stage):-
  impplan(Piece),
  impro(Stage,Piece).

impro(Stage,Piece) :-
  add_criteria(Piece,Crit),                % Add new piece to criteria
  target_loc(Piece,Pos,Orient),            % Where is it
  impr(Orient,Stage,Pos,Piece),
  !.

impr(0,_,_,_) :- !.                        % In place and oriented
impr(_,Stage,Pos,Piece) :- imp(Stage,Pos,Piece).

% imp - getting into the real work

imp(Stage,Pos,Piece):-
  color_piece(PieceC,Piece),         % translate side notation to
  wrfield(target,PieceC),            %   color notation for display
  heuristics(Stage,Pos),             % See if special help is needed.
  orientation(Piece, View),          % Preferred view for this piece.
  set_view(View),
  crit(Crit),
  state(State),
  cntr_set(4,0),						% to limit wild searches
%  gc(7),
  rotate(Moves,State,Crit),      % Search for moves which transform
  retract(state(_)),
  assert(state(Crit)),
  wrfield(rot,Moves),
  add_history(Moves),
  undo_view(View),!.

heuristics(Stage,Pos):-
  (shift_right_1(Stage,Pos);
   shift_right_2(Stage,Pos)),!.
heuristics(_,_):-true.

% The shift_right heuristics are used to avoid the situations where
% the piece is in one of the target positions for the stage, but the
% wrong one, or mis-oriented.  By blindly moving it to the right the
% search is reduced since it doesn't have to search to move it both 
% out of a critical target position and back into the correct one.

shift_right_1(1,Pos):-
  smember('L',Pos),            % Is the target piece already on the left?
  s_r(Pos,Moves),              % If so get the canned moves to move it 
  change(Moves),               % right for easy search.
  !.

shift_right_2(Stage,Pos):-
  Stage < 4,                   % If the target piece is not on the right
  notsmember('R',Pos),         % side, get the canned moves to put it
  s_r(Pos,Moves),              % there to allow easier search
  change(Moves),
  !.

% rotate - the real guts of the solution, all the rest of the code provides
%          support for these six lines.

% These lines illustrate the power and obscurity of Prolog.
% Prolog can be very expressive when the main information is carried
% in the predicate.  However, sometimes the work is being done by
% unification, and it is not at all apparent by reading the code.
% Furthermore, since Prolog predicates often work backwards and
% forwards, it is not clear in a given case what is intended to be
% be input, and what is the output, and, as in this case, what might
% be an in-out.

% The input and output states of rotate are:

% Input: Moves - unbound
%        State - bound to the cube structure for the current state
%        Crit  - partially bound cube structure.  the bound portions
%                represent the pieces in place + the current goal piece

% Output: Moves - a list of moves
%         State - same as input
%         Crit  - fully bound to the new state

% rotate does a breadth first search by recursively calling itself
% before it calls get_move which trys new moves.  it does not save the
% search trees as most breadth first algorithms do, but rather recalculates
% the moves since they can be executed so fast.

% get_move fails when called with the partially bound Crit, unless
% it is a move which reaches the desired state.  The failure causes
% backtracking.  However when rotate calls itself, it gives it a
% fully unbound variable NextState.  This call to rotate succeeds and
% keeps adding new moves generated by get_move on backtracking.

% eventually get_move finds a match and rotate succeeds.

rotate([], State, State).            % start with a no move
rotate(Moves, State, Crit):-         % nothing didnt work, get serious
  rotate(PriorMoves, State, NextState), % get something to build on
%  cntr_inc(4,N4),
%  check_prog(N4),
  get_move(ThisMove, NextState, Crit),  % generate possible moves
  append(PriorMoves, [ThisMove], Moves).   % build up the list

check_prog(N) :- N < 250, !.
check_prog(_) :-
  error('not converging'),
  halt.


% The following predicates all perform various useful services
% for the main predicates above.  Some are declared export as well
% and are used by other modules

% add_criteria puts a new piece on the criteria structure.  it works
% by creating two piece format lists, one of the goal state, and the
% other of the current criteria.  It then walks through the two lists
% simultaneously looking for the target piece in the goal state.
% when it finds it it adds it to the criteria.  Crit is unbound on entry

add_criteria(Piece,Crit):-
  crit(OldCrit),
  pieces(OldCrit, OldCritP),
  ghoul(Ghoul),
  pieces(Ghoul, GhoulP),
  add_crit(OldCritP, GhoulP, NewCritP, Piece),
  pieces(Crit, NewCritP),
  retract(crit(_)),
  assert(crit(Crit)), !.

add_crit([V1|V2], [V3|V4], [V3|V2], V5):-
  matches(V3, V5),!.
add_crit([V1|V2], [V3|V4], [V1|V5], V6):-
  !,add_crit(V2, V4, V5, V6).
add_crit(V1, V2, V3, V4):-
  error('something wrong with add_crit'),!.

% The center tiles dont move on the cube.  Sooo if someone enters a cube
% with different color sides then we must find the new center tiles
% and map the new colors to the sides accordingly

new_colors(Cube):- 
  rewrite(ColorCube,Cube),
  get_color(ColorCube),
  rewrite(ColorCube,NewCube),
  retract(state(_)),
  assert(state(NewCube)).

% Set up the initial mapping of sides to colors

init_color:-
  side_color(SC),
  retractall(sidecolor(_)),
  ini_col(SC).

ini_col([]):- !.
ini_col([S-C|T]):-
  assert(sidecolor(S-C)),
  ini_col(T).

% translate a piece in piece notation to color notation

color_piece(PieceC,Piece):-
  Piece=..[p|Args],
  col_p(ArgsC,Args),
  PieceC=..[p|ArgsC].

col_p([],[]):- !.
col_p([PC|RestC],[P|Rest]):-
  sidecolor(P-PC),
  col_p(RestC,Rest).

% execute about 50 or 60 random rotations to the goal cube.  due to the
% random function, the random cubes will be the same from run to
% run.  It always starts from the same seed.

random_cube(Cube):-
  ghoul(Start),
  rand_cub(Start,Cube,50).

rand_cub(Cube,Cube,0).
rand_cub(Now,Cube,N):-
  repeat,
  rand_move(M,RN),
  movel(M,Now,Next),
  NN is N - 1,
  !,rand_cub(Next,Cube,NN).

rand_move(M,RN):-
  RN is integer(random*12),
  arg(RN,m(+f,+b,+r,+l,+u,+d,-f,-b,-r,-l,-u,-d),M).

% the classic

member(V1, [V1|V2]):- !.
member(V1, [V2|V3]):-
  member(V1, V3).

% display a list of terms without the list notation

write_list([]):-true.
write_list([H|T]):-
  write(H),tab(1),
  write_list(T).

% target_loc finds the location of a given piece on the cube.  it can
% also be used to find the piece at a given location.  it returns the
% orientation as well, which is 0 if in place, or 1 if in place but
% twisted

target_loc(Piece, Pos, Orient):-
  ghoul(Gt),
  pieces(Gt, G),
  state(St),
  pieces(St, S),
  find_piece(G, S, Pos, Piece, Orient),!.
target_loc(Piece, _,_):-
  error('Failing to find piece'-Piece),
  fail.

% find_piece does the work for target_loc, walking two lists simultaneously
% looking for either the piece or the position, whichever is bound.

find_piece([Gh|Gt], [Sh|St], Pos, Piece, Orient):-
  matches(Pos, Gh),
  matches(Piece, Sh),
  comp(Gh,Sh,Orient),!.
find_piece([V1|V2], [V3|V4], V5, V6, Orient):-
  !,find_piece(V2, V4, V5, V6, Orient).

matches(V1, V2):-
  comp(V1, V2, V3),
  V3 < 2,!.

% comp returns 0 if direct hit, 1 if in place but twisted, and
% 2 if no match

comp(p(V1), p(V1), 0):- !.
comp(p(V1, V2), p(V1, V2), 0):- !.
comp(p(V1, V2), p(V2, V1), 1):- !.
comp(p(V1, V2, V3), p(V1, V2, V3), 0):- !.
comp(p(V1, V2, V3), p(V1, V3, V2), 1):- !.
comp(p(V1, V2, V3), p(V2, V1, V3), 1):- !.
comp(p(V1, V2, V3), p(V2, V3, V1), 1):- !.
comp(p(V1, V2, V3), p(V3, V1, V2), 1):- !.
comp(p(V1, V2, V3), p(V3, V2, V1), 1):- !.
comp(V1, V2, 2).

% allows easy handling of database entries used as flags

set_flag(Flag,Val):-
  retract(flag(Flag,_)),
  assert(flag(Flag,Val)), !.
set_flag(Flag,Val):-
  assert(flag(Flag,Val)).

get_flag(Flag,Val):-
  flag(Flag,Val).

% get_move is used by rotate to generate moves.  the possible moves
% are stored in the database under the key cand.  backtracking causes
% successive moves to be tried

get_move(+V1, V2, V3):-
  cand(V1),
  movep(V1, V2, V3).
get_move(-V1, V2, V3):-
  cand(V1),
  movep(V1, V3, V2).

% build_cand creates the database of possible moves for a given stage.
% this is one of the important heuristics for limiting the search

build_cand(V1):-
  retractall(cand(_)),
  retractall(candmove(_)),
  build_cands(V1),!.

build_cands([]):- !.
build_cands([V1|V2]):-
  can_seq(V1),
  assertz(cand(V1)),
  !,build_cands(V2).

can_seq(M):-                      % if the search move is a sequence
  seq(M,S),                       % precompute it, so it isn't constantly
  variable(X),                    % redone during search.
  move_list(S,X,Y),
  assertz(candmove(m(M,X,Y))), !.
can_seq(_).

% another classic

append([], V1, V1).
append([V1|V2], V3, [V1|V4]):-
  append(V2, V3, V4).

% apply a list of moves to a state

move_list([], V1, V1):- !.
move_list([Move|V3], V4, V5):-
  movel(Move, V4, V6),
  !,move_list(V3, V6, V5).

% movel is the basic move predicate called from everywhere

movel(+M, V2, V3):-        % distinguish between clockwise
  movep(M, V2, V3),!.
movel(-M, V2, V3):-        % and counter clockwise moves
  movep(M, V3, V2),!.

% find the move, be it a simple move, a rotation, or a sequence.
% if its a sequence break it into its simple componenents

movep(M, X, Y):- move(M, X, V3), !, Y = V3.
movep(M, X, Y):- rot(M, X, V3), !, Y = V3.
movep(M, X, Y):- candmove(m(M,X,V3)), !, Y = V3.
movep(V1, V2, V3):-
  seq(V1, V4), !,
  move_list(V4, V2, V3),!.
movep([V1|V2], V3, V4):- move_list([V1|V2], V3, V4),!.

% same as move_list, only print new state when done

move_listp(V1, V2, V3):-
  move_list(V1, V2, V3),
  wrfield(rot,V1).

% change is move_list for keeps.
% it takes the old value changes it, updates it,
% and records the history.  it is called by the heuristic routines

change(ML):-
  retract(state(Old)),
  move_listp(ML,Old,New),
  add_history(ML),
  assert(state(New)), !.

% establish a new view.  this means not just rotating the cube, but also
% rotating the criteria and the goal structures.  this is necessary so
% any predicates working with any of the three winds up comparing
% apples and apples.

set_view([]):- !.
set_view(V):-
  retract(state(S1)),
  move_list(V, S1, S2),
  assert(state(S2)),
  retract(ghoul(G1)),
  move_list(V, G1, G2),
  assert(ghoul(G2)),
  retract(crit(C1)),
  move_list(V, C1, C2),
  assert(crit(C2)),
  wrfield(rot,V),
  add_history(V),!.

undo_view([]):- !.
undo_view(RV):-
  reverse(RV,V),
  set_view(V),!.


% convert a cube structure to a list of pieces and visa versa

pieces(cube(X1, X2, X3, X4, X5, X6,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, 
  V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, 
  V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, 
  V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
    [p(X1), p(X2), p(X3), p(X4), p(X5), p(X6),
  p(V7, V8, V9), p(V10, V11, V12), p(V13, V14, V15), p(V16, V17, V18), 
  p(V19, V20, V21), p(V22, V23, V24), p(V25, V26, V27), p(V28, V29, V30), 
  p(V31, V32), p(V33, V34), p(V35, V36), p(V37, V38),
  p(V39, V40), p(V41, V42), 
  p(V43, V44), p(V45, V46), p(V47, V48),
  p(V49, V50), p(V51, V52), p(V53, V54)]).

% get an unbound cube

variable(cube(X1, X2, X3, X4, X5, X6,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, 
  V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, 
  V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, 
  V48, V49, V50, V51, V52, V53, V54)).

% the initial criteria, unbound except for the six center tiles

init_crit(cube('F', 'R', 'U', 'B', 'L', 'D',
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, 
  V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, 
  V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, 
  V48, V49, V50, V51, V52, V53, V54)).

notsmember(X,Y):-smember(X,Y),!,fail.
notsmember(X,Y):-true.

% like the classic, but works on a structure instead

smember(X,Y):-
  Y=..[Fun|Args],
  member(X,Args).

% display errors

error(X):-
  wrfield(error,X), nl,
  get1(_).

% reverse a list of moves, and flip the signs along the way

reverse(L, R) :- rever(L, [], R).

rever([], Z, Z).
rever([H|T], X, Z) :-
  flip_sign(H, FH),
  rever(T, [FH|X], Z).

flip_sign(+ X, - X):- !.
flip_sign(- X, + X):- !.

retractif(X) :-
  retract(X),
  !.
retractif(_).


