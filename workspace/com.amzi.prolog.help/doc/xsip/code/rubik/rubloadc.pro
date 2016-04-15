% RUBLOAD - Loads the necessary files for solving Rubik's Cube.

:-nl,write('loading rubik'),nl,
    consult('rubik.plm').
:-write('loading rubmov'),nl,
    consult('rubmov.plm').
:-write('loading rubdata'),nl,
    consult('rubdata.plm').
:-write('loading rubdisp'),nl,
    consult('rubdisp.plm').
:-write('loading rubhist'),nl,
    consult('rubhist.plm').
:-write('loading rubedit'),nl,
    consult('rubedit.plm').
:-write('loading rubhelp'),nl,
    consult('rubhelp.plm').
:-retract((restart:-X)).
:- write('rubik loaded'),nl.
