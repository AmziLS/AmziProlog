% RUBDATA - Copyright (C) 1994, Amzi! inc.

%           This file contains all the data needed to drive
%           the main cube solving predicates.

:-export seq/2, s_r/2, orientation/2.
:-export cnd/2, pln/2, vw/2.
:-export pristine/1.
:-export side_color/1.

% the sequences of moves used to perform special transformations
% such as twisting the corners without moving anything else
		  
seq(s, [+rr, -r, +l]).
seq(tc1, [-l, +u, +r, -u, +l, +u, -r, -u]).
seq(tc1u2, [+ru, +ru, +tc1, -ru, -ru]).
seq(tc3, [+r, -u, -l, +u, -r, -u, +l, +u]).
seq(ct1, [-r, +d, +r, +f, +d, -f, -u, +f,
          -d, -f, -r, -d, +r, +u]).
seq(ct3, [-r, +d, +r, +f, +d, -f, +u, +u,
          +f, -d, -f, -r, -d, +r, +u, +u]).
seq(ef1, [-u, +f, -r, +u, -f, -s, +f, -u,
          +r, -f, +u, +s]).
seq(ef2, [+l, +f, -u, +f, -r, +u, -f, -s,
          +f, -u, +r, -f, +u, +s, -f, -l]).
seq(et1, [+f, +f, +r, +r, +f, +f, +r, +r,
          +f, +f, +r, +r]).
seq(h, [+l, +f, +u, -f, -u, -l]).
seq(g, [-r, -f, -u, +f, +u, +r]).
seq(pt, [+ru, +ru]).
seq(mr2a, [+r, +f, -r, -f]).
seq(mr2b, [-r, -u, +r, +u]).
seq(mr3a, [-u, +r, +u]).
seq(mr3b, [+f, -r, -f]).

% cnd defines the moves which will be used in a given stage for search

cnd(1, [r, u, f]).
cnd(2, [r, mr2a, mr2b]).
cnd(3, [r, mr3a, mr3b]).
cnd(4, [r, tc1u2, ct1]).
cnd(5, [u, h, g, ef1, ef2]).
cnd(6, [u, tc1, tc3, ct1, ct3]).

% s_r is used by the shift_right heuristics.  it lists the move sequence
% needed to move a piece which is not on the right, to the right.  the
% first arguement is the position the piece is at

s_r(p('F','L','U'), [-mr2a]).
s_r(p('F','L','D'), [+rr, -mr2a, -rr]).
s_r(p('B','L','U'), [-rr, -mr2a, +rr]).
s_r(p('B','L','D'), [+rr, +rr, -mr2a, -rr, -rr]).
s_r(p('F','U'), [-mr3a]).
s_r(p('F','D'), [+s, -mr3a, -s]).
s_r(p('B','U'), [-s, -mr3a, +s]).
s_r(p('B','D'), [+s, +s, -mr3a, -s, -s]).
s_r(p('L','U'), [+u, +u]).
s_r(p('F','L'), [+f, +f]).
s_r(p('L','D'), [+d, +d]).
s_r(p('B','L'), [+b, +b]).

% orientation defines the rotation moves necessary to position the
% cube to take advantage of symmetry for each piece

orientation(p('F','L','U'), []).
orientation(p('F','L','D'), [+rr]).
orientation(p('B','L','U'), [-rr]).
orientation(p('B','L','D'), [+rr, +rr]).
orientation(p('F','U'), []).
orientation(p('F','D'), [+s]).
orientation(p('B','U'), [-s]).
orientation(p('B','D'), [+s, +s]).
orientation(p('L','U'), []).
orientation(p('F','L'), [+rr]).
orientation(p('L','D'), [+rr, +rr]).
orientation(p('B','L'), [-rr]).
orientation(_, []).

% pln lists the target pieces for each stage

pln(1, [p('L','U'),p('F','L'),p('L','D'),p('B','L')]).
pln(2, [p('B','L','D'),p('F','L','D'),p('B','L','U')]).
pln(3, [p('F','U'),p('F','D'),p('B','U'),p('B','D')]).
pln(4, [p('F','L','U')]).
pln(5, [p('R','U'),p('F','R'),p('R','D'),p('B','R')]).
pln(6, [p('F','R','U'),p('B','R','U'),p('B','R','D'),p('F','R','D')]).

% vw defines the preferred orientation for a stage

vw(5, [-rf]).
vw(6, [-rf]).
vw(_, []).

% this is the pristine state

pristine(cube('F','R','U','B','L','D',
 'F','R','U','F','R','D','F','L','U','F','L','D','B','R','U','B','R','D',
 'B','L','U','B','L','D','R','U','R','D','L',
 'U','L','D','F','U','F','D','B','U',
 'B','D','F','R','F','L','B','R','B','L')).

% the initial mapping of sides and colors

side_color(['F'-'G', 'R'-'R', 'U'-'W', 'B'-'Y', 'L'-'O', 'D'-'B']).
