% RUBHIST - Copyright (C) 1994, Amzi! inc.

%  This module records history information so you can unscramble
%  a real cube by looking at the log file.

:-export add_history/1.

:-import append/3.                % rubik
:-import attr/2.                  % rubdisp
:-import clr_bottom/0.            % rubik
:-import error/1.                 % rubik
:-import bug/1.                   % rubik
:-import get_flag/2.              % rubik
:-import reverse/2.               % rubik
:-import move/3.                  % rubmove
:-import rot/3.                   % rubmove
:-import seq/2.                   % rubdata
:-import wrfield/2.               % rubdisp

% add_history takes a list of moves as input.  as output it sends
% the expanded version of the moves to the logfile.  That is, sequences
% are broken down into primitive moves before being written to the
% window

add_history(V1):-
  expand(V1, V2),          % expand the list
  de_list(V2,V3),          % remove inbedded lists (flatten the list)
  segment_list(V3,V4),     % break into pieces that fit in window
  write_hist(V4),
  !.
add_history(X):-
  error([add_history,X]).

write_hist([]).
write_hist([FirstLine|Rest]) :-
  write('  Moves: '),
  wr_hist(FirstLine),
  nl,
  write_hist(Rest).

wr_hist([]).
wr_hist([H|T]) :-
  tab(2),
  write(H),
  wr_hist(T).

% expand pushes its way through a list of moves and sequences, making
% sequences into other move lists. it takes care to preserve the
% meaning of a counterclockwise sequence by reversing the list defining
% the sequence.  this reverse also changes the sign of each term along
% the way.  the first argument is the imput list, the second is output

expand([], []) :- !.
expand([Term|V3], [Term|V4]):-
  moveterm(Term, X),            % strip the sign
  (move(X,_,_); rot(X,_,_)),    % its a primitive
  !, expand(V3, V4).
expand([Seq|V3], [Termlist|V5]):-
  moveterm(Seq,S),              % we can guess its a sequence
  seq(S, SL),
  (signterm(Seq,-), reverse(SL,Sterms);   % flip if necessary
   Sterms = SL),
  expand(Sterms,Termlist),      % double recursion, on this sequence
  !, expand(V3, V5).            % and the rest of the list
expand(X,_):-
  error(['expand fails on',X]).

% separate the move and sign of a term, first arg is input, second output

moveterm(+ X, X) :- !.
moveterm(- X, X) :- !.

signterm(+ X, +) :- !.
signterm(- X, -) :- !.

% "expand" left imbedded lists where sequences used to be, flatten them
% out since they arn't necessary

de_list([], []) :- !.
de_list(V1, [V1]):-
  (V1 = +X; V1 = -X).
de_list([V1|V2], V3):-
  de_list(V1, V4),          % double recursion on the head and tail
  de_list(V2, V5),
  append(V4, V5, V3).

% having flattened it, segment_list breaks a long list into smaller
% lists that will fit in the display window.  this is because the
% window routine is too lazy to deal with lines that are too long

segment_list([A,B,C,D,E|Tin],[[A,B,C,D,E]|Tout]):-
  segment_list(Tin,Tout).
segment_list([],[]) :- !.
segment_list(L,[L]) :- !.

