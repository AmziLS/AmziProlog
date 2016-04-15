%-*-Prolog-*-  
% crypto indented on 11/29/2001 by 'JOLI' 1.0.

/**************************************************************************\
* Author:	Ray Reeves                                                     *
* Created:	January 1992                                                   *
*                                                                          *
*
\**************************************************************************/
%:- module(crypto).

main :-
  write(`\nCRYPTO-ARITHMETIC PUZZLES\n`),
  open(puzzles, read, S1),
  read(S1, PuzzleList),
  close(S1), !,
  repeat,
  reset,
  writePuzzles(PuzzleList, []),
  write(`\nSelect puzzle name:\n\n`),
  read(X),
  (
     (X == q ;  X == quit) ;

     catch((crypto(X, PuzzleList), fail), grief(G), console(G))
  ).

console(setup) :-
  write(`error: Invalid specification in .cry file\n`).

crypto(Puzzle, PuzzleList) :-
  abolish(puzzle/2),
  (
     getDomain(Puzzle, PuzzleList, Domain), ! ;

     write(`Puzzle `), write(Puzzle), write(` not found in puzzles list.\n`),
     fail
  ),
  atomlist_concat([Puzzle, '.', cry], Cryptogram),
  write(`Problem: (`),  write(Puzzle), write(`.cry), `), write(`Domain: `), 
	write(Domain), write(` :\n\n`),
  printFile(Cryptogram),
  write(`Generating solution code (`), write(Puzzle), write(`.pro):\n`),
  T1 is cputime,
  (setup(Puzzle, Domain) -> true ;  throw(grief(setup))),
  T2 is cputime,
  T12 is T2 - T1,
  write(`time: `), write(T12), write(` seconds\n\n`), 
	write(`Executing solution code:\n`),
  abolish(domain/1),
  abolish(puzzle/0),
  consult(Puzzle),
  T3 is cputime,
  (call(Puzzle), fail ;  true),               % needs call/1 or crashes
  T4 is cputime,
  T43 is T4 - T3,
  write(`\nno more solutions\n`),
  write(`time for all solutions: `), write(T43), write(` seconds\n\n`), !.

assertPuzzles([], []) :- !.
assertPuzzles([P|File], [Name|Puzzles]) :-
  functor(P, Name, Arity),
  (
     Arity = 0 ->
     assert(puzzle(Name, `[0,1,2,3,4,5,6,7,8,9]`)) ;

     arg(1, P, D),
     assert(puzzle(Name, D))
  ),
  assertPuzzles(File, Puzzles).

getDomain(Puzzle, [Puzzle|_], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) :- !.
getDomain(Puzzle, [P|_], Domain) :-
  functor(P, Puzzle, 1), !,
  arg(1, P, Domain).
getDomain(Puzzle, [_|X], Domain) :-
  getDomain(Puzzle, X, Domain).

writePuzzles -->
  [Puzzle],
  {write(Puzzle), nl},
  writePuzzles.
writePuzzles --> [].

printFile(File) :-
  open(File, read, X),
  close(X),
  open(File, read, Stream),
  repeat,
  read_string(Stream, S),
  (S == end_of_file ;  write(S), fail),
  nl, 
  close(Stream), !.

