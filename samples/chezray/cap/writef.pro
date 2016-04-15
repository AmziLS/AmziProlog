%-*-Prolog-*-  
% writef indented on 11/23/1999 by 'JOLI' 1.0.


/*  WRITEF.PRO -- A formatted write() predicate based on the C "printf"
**
**  Copyright (c) 1992-2002, Amzi! inc.
**  All Rights Reserved.
**
**  writef(FormatList, ArgList) where ArgList is a list of
**  Prolog terms to be printed according to the FormatList
**
*/

%:- export 
% writef/2.

% Default is no args to stdout
writef(Format) :-
   writef(Format, [], -1).

writef(Format, Args) :-
   writef(Format, Args, -1).

writef([H|T], AL, W) :-
   not(not(writef_([H|T], AL, W))).            % Save stack space
writef([], _, _).

/*
**      writef parses Format and writes Args out accordingly - traversing
**      the Format list recursively
*/

writef_([0'\, Esc|Tail], AL, W) :-              % Print an escaped character
   print_escape(Esc, W), !,                    % i.e. \x
   writef_(Tail, AL, W).
writef_([0'%, 0'%|Tail], AL, W) :- !,          % "..%% " prints out as single %
   put(W, 37),
   writef_(Tail, AL, W).
writef_([0'%|Tail], [H|T], W) :- !,        % Otherwise we have a genuine format
   p_flags(Flag, Tail, Tail2),                 % Parse of flags
   p_u_int(Width, Tail2, Tail3),               % And width 
   p_precision(Precision, Tail3, Tail4),       % And precision 
   p_type(Type, Tail4, Tail5),                 % And type
   printf_(W, format(Flag, Width, Precision, Type), H), % Print it
   writef_(Tail5, T, W).                       % And continue with rest.. 
writef_([H|T], AL, W) :-                       % Otherwise elements of format 
   put(W, H),                                  % print as themselves
   writef_(T, AL, W).
writef_([], _, _).                             % No format left -- all done 


/*
**      DCGs to parse the format list
*/

p_flags(left) -->                              % Left justify flag
   [0'-], !.                                     % returns symbol left or none
p_flags(none) -->
   [0'+], !.
p_flags(none) --> [].

p_u_int(Val, [I|T], Out) :-                % Parse an int - return none is none
   is_int(I),                                  % Returns none if no int 
   p_u_int(Val, _, [I|T], Out).

p_u_int(none) --> [].

p_u_int(Val, Pow10) -->                     % Actually parses and evaluates int
   [I],                                        % Get the symbol
   {is_int(I)}, !,                             % An integer
   p_u_int(Val2, P10),                         % Parse rest of int
   {
      Val is ((I - 0'0)*P10) + Val2,           % And compute the int 
      Pow10 is P10*10
   }.
p_u_int(0, 1) --> [].

p_precision(P) -->                             % Precision is .number
   [0'.],
   p_u_int(P).
p_precision(none) --> [].                      % Returns none if no precision 

p_type(H) -->                                  % Parse type character
   [H],
   {member(H, "adicslt")}, !.                  % Must be one of these

/*
*       print out escaped (\Esc) character
*/ 

print_escape(0'n, W) :-
   nl(W).                                      % New line
print_escape(0'a, W) :-
   put(W, 7).                                  % Bell
print_escape(0't, W) :-
   tab(W, 8).                                  % Tab
print_escape(0'b, W) :-
   put(W, 8).                                  % Backspace
print_escape(C, W) :-
   put(0'\, W),
   put(C, W).                                  % Else print \ as well
                                               %
/*
 *       print out the argument with respect to the format structure
 *       printf(Window, format(Flag,Width,Precision,Type), Term)
 */ 

printf_(W, format(_, _, _, 0't), Term) :- !,   % t format just prints term
   write(W, Term).
printf_(W, format(Flag, Width, Prec, 0's), S) :- % Treat string
   string(S),                                  % By converting it to list
   string_list(S, L),
   printf_(W, format(Flag, Width, Prec, 0'l), L).
printf_(W, format(Flag, Width, Prec, 0'a), S) :- % Treat atom
   (atom(S) ; var(S)),                         % By converting it to list
   name(S, L),
   printf_(W, format(Flag, Width, Prec, 0'l), L).
printf_(W, format(Flag, Width, Prec, Type), Term) :- % General case
   term_size(Type, Term, Prec, Size, CTerm),   % Convert to list & size 
   (
      integer(Prec) ->
      maxof(Size1, Size, Prec) ;               % Include precision in
                                               % Padding calculation 

      Size1 = Size
   ),
   print_pad(Flag, none, W, Width, Size1),     % Possibly print lead spaces
   print_item(W, format(Prec, Type, Size, CTerm)), % Print out term
   print_pad(Flag, left, W, Width, Size1), !.  % Possibly print trail spaces


/*
**      print_item(W, format(Precsion,Type,Size,Term)) - does printing
*/ 

print_item(W, format(_, 0'c, _, C)) :-
   put(W, C).
print_item(W, format(_, 0'l, Size, CTerm)) :-   % A list
   putsn(W, Size, CTerm).                      % Easy
print_item(W, format(Prec, 0'i, Size, CTerm)) :- % 0'i is synonym
   print_item(W, format(Prec, 0'd, Size, CTerm)). % for 0'd
print_item(W, format(Prec, 0'd, Size, CTerm)) :- % 0'd
   (
      integer(Prec) ->
      (
         Prec > Size ->                        % Left pad with 0 if
         Pad is Prec - Size,                   % precision warrants
         putcn(W, Pad, 0'0) ;

         true
      ) ;

      true
   ),
   putsn(W, Size, CTerm).                      % Then print list


/*
**      term_size computes number of characters required to print the term
**      and also converts the term to its representation as a list
**      of Size characters. We pass precision in since size can be influenced 
**      by it (e.g. 0'l)
*/ 

term_size(0'c, C, _, 1, C).                     % Characters are trivial
term_size(0'd, I, _, L, Is) :-                  % Integers
   term_size(0'i, I, _, L, Is).
term_size(0'i, R, _, L, Is) :-
   number(R),
   I is integer(R),                            % Convert to int
   i_to_s(I, L, Is).                           % Convert to string
term_size(0'l, CharList, Prec, L, CharList) :-
   check_and_size_charlist(L1, CharList),
   (integer(Prec) -> minof(L, Prec, L1) ;  L = L1).

/*
**      i_to_s(Integer, Size, IntegerAsString)
*/ 

i_to_s(I, L, [0'-|S]) :-
   I < 0, !,
   IP is - I,
   i_to_s(IP, L1, S),
   L is L1 + 1.
i_to_s(0, 1, [0'0]) :- !.
i_to_s(I, L, S) :-
   I1 is I//10,
   Digit is (I - 10*I1) + 0'0,
   i_to_s1(I1, L, Digit, S1),
   reverse$(S1, S).

i_to_s1(I, L, Dig, [Dig|T]) :-
   I > 0,
   I1 is I//10,
   Digit is (I - 10*I1) + 0'0,
   i_to_s1(I1, L2, Digit, T),
   L is L2 + 1.
i_to_s1(0, 1, Dig, [Dig]).

/*
**      utility routines
*/

member(H, [H|_]).
member(H, [_|T]) :-
   member(H, T).

length(0, []).
length(M, [H|T]) :-
   length(N, T),
   M is N + 1.

/*
**      like length but also chescks to make sure the
**      list is a printable ascii list
*/ 

check_and_size_charlist(0, []).
check_and_size_charlist(M, [H|T]) :-
   check_and_size_charlist(N, T),
   integer(H),
   0 =< H,
   H =< 255,                                   % is it printable ??
   M is N + 1.

/*
**      write N copies of char
*/ 

putcn(W, I, C) :-
   for(J, 1, I, 1),
   put(W, C),
   fail.
putcn(_, _, _).

/*
**      write up to first N characters of list
*/

putsn(W, 0, L) :- !.
putsn(W, N, [H|T]) :-
   N > 0,
   N1 is N - 1,
   put(W, H),
   putsn(W, N1, T).

/*
**      simply print out chars of a char list
*/ 

puts(W, []) :- !.
puts(W, [H|T]) :-
   put(W, H),
   puts(W, T).

% maxof(Max, A, B)
maxof(A, A, B) :-
   A >= B, !.
maxof(B, A, B).

% minof(Min, A, B)
minof(A, A, B) :-
   A =< B, !.
minof(B, A, B).

% is C an ascii integer ?
is_int(C) :-
   0'0 =< C,
   C =< 0'9.

% B is list A reversed
reverse$(A, B) :-
   reverse$(A, [], B).

reverse$([X|Y], A, Z) :-
   reverse$(Y, [X|A], Z).
reverse$([], Z, Z).

% This routines figues out whether there is a need to pad the ouputed
% string with blanks. Padding make take place on the left (if Flag = Justify)
% or on the right. Printing takes place at window W.

print_pad(Flag, Justify, W, Width, Size1) :-
   (integer(Width), Width > Size1) ->          % Width calc
   (
      Flag = Justify ->
      (Spaces is Width - Size1, putcn(W, Spaces, 0' )) ;

      true
   ) ;

   true.

/*--------------------------------------------------------------------------*/ 
testWritef :-
   writef("Hello, Prolog World\n"),
   writef("Now is %d (seconds) for %cll %s %l", 
          [33, 0'a, `good men`, "to come to the aid of the party"]),
   writef("\n%-10sjustified%10ljustified\n", [`left`, "right"]),
   writef("\n%5.2i %.3a\n", [42, foobar]).

