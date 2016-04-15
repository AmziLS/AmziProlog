% Module definition for the 'data' module.  nextto/2
% is defined in the 'data' body, and doesn't change,
% so it can be compiled. But loc/2 is dynamically
% asserted/retracted from the other modules, so it is
% declared as dynamic.

:- module(data).
:- export([nextto/2, loc/2]).
:- dynamic(loc/2).

nextto(pen, yard).
nextto(yard, house).

loc(egg,pen).
loc(ducks,pen).
loc(you,house).

:- end_module(data).
