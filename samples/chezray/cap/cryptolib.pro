%-*-Prolog-*-  
% cryptolib indented on 11/29/2001 by 'JOLI' 1.0.

/**************************************************************************\
*		cryptolib.pro                                                        *
* Author:	Ray Reeves                                                     *
* Created:	January 1992                                                   *
\**************************************************************************/
:- op(500, fx, *).
:- op(500, fx, **).

*[X] :-
  draw([X], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], _).

**[X] :-
  domain(Domain),
  draw([X], Domain, _).

+ [X] :-
  draw([X], [1, 2, 3, 4, 5, 6, 7, 8, 9], _).

/* these enhancements to the draw routines greatly improve performance */

draw([X], + Y, + Y) :- !,                     % leave pack intact
  draw([X], Y, _).
draw([X], Y, Z) :-                       % the dealer. deal X from Y leaving Z
  (
     var(X), !,
     pick(X, Y, Z) ;                          % pick any card

     delete(X, Y, Z)                          % pick this card
  ).                

pick(X, [Y, Y|L], [Y|Rest]) :- !,
  pick(X, [Y|L], Rest).                       % skip dups
pick(X, [X|L], L).
pick(X, [Y|L], [Y|Rest]) :-
  pick(X, L, Rest).

delete(X, [X|L], L) :- !.
delete(X, [Y, Z|L], [Y|Rest]) :-              % del from ordered list
  X >= Z,                                     % still possible
  delete(X, [Z|L], Rest).

evaluate(Exp, Carry, Result) :-               % simple column evaluation 
  X is Exp,
  Carry is X//10,
  Result is X mod 10.

decname(0, [0]).                         % utility like name, but for numbers 
decname(Integer, List) :-
  decname(Integer, [], List).

decname(0, X, X).
decname(Integer, Acc, List) :-
  nonvar(Integer),
  X is Integer//10,
  Y is Integer mod 10,
  decname(X, [Y|Acc], List).
decname(Integer, Acc, List) :-
  decname(X, [Y|Acc], List),
  Integer is 10*X + Y.

displayGram(Cols, Lines, X, Y) :-        % display solutions to the cryptogram
  nl,                                      % X is var list, Y is solution list
  Cols1 is Cols - 1,
  displayLines(Cols, 1, Lines, X, Y), !.

displayLines(Cols, Line, Lines, X, Y) :-
  (
     Line > Lines ->
     true ;                                   % stop

     displayLine(Cols, Line, X, Y),           % display each line
     nl, 
     Line1 is Line + 1,
     displayLines(Cols, Line1, Lines, X, Y)
  ).

displayLine(Cols, Line, X, Y) :-
  (
     barLine(_, Line, _) ->                   % a bar line
     displayBars(Cols) ;

     (barLine(2, _, BarTag) -> true ;  BarTag = real),
     displayVars(1, Line, Cols, X, Y, BarTag)
  ).

displayBars(0) :- !.
displayBars(Cols) :-
  write('-'), 
  Cols1 is Cols - 1,
  displayBars(Cols1).

displayVars(Col, Line, Cols, X, Y, BarTag) :- % display each character
  (
     Col > Cols ->
     true ;

     findVar(Col, Line, X, Y, BarTag, Var),   % not a bar line
     write(Var), 
     Col1 is Col + 1,
     displayVars(Col1, Line, Cols, X, Y, BarTag)
  ).

findVar(Col, Line, X, Y, BarTag, Var) :-      % find symbol at this position
  (
     pos(Symbol, Col, Line, _) ->
     interpretSymbol(Symbol, X, Y, Var) ;     % found

     Var = ' '                                % not found
  ).                                          %

interpretSymbol(Symbol, [Symbol|X], [Var|Y], Var) :- !. % found, bind Var
interpretSymbol(Symbol, [_|X], [_|Y], Var) :- % keep looking
  interpretSymbol(Symbol, X, Y, Var).

