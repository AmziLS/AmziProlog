% RUBLOAD - Loads the necessary files for solving Rubik's Cube.

:-nl,write('loading rubik'),nl,
    consult('rubik.pro').
:-write('loading rubdata'),nl,
    consult('rubdata.pro').
:-write('loading rubdisp'),nl,
    consult('rubdisp.pro').
:-write('loading rubedit'),nl,
    consult('rubedit.pro').
:-write('loading rubhelp'),nl,
    consult('rubhelp.pro').
:-write('loading rubhist'),nl,
    consult('rubhist.pro').
:-write('loading rubmov'),nl,
    consult('rubmov.pro').
:-write('rubik loaded'),nl.
