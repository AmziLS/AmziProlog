%-*-Prolog-*-  
% poly indented on 7/8/1999 by 'JOLI' 1.0.
%line 3
%line 4
    /**************************************************************\
    *                                                              *
    * poly.pro Polynomial manipulation in Prolog                   *
    *                                                              *
    * Copyright (c) 1998 by Ray Reeves.  All Rights Reserved.      *
    *                                                              *
    \**************************************************************/
/*
* This relates to real functions of a complex variable: w = F(z).

* No complex variables are employed here; z is thought of as an implied 
* free variable to which F may be applied later. 
* The most general form of meromorphic function is a quotient of two
* polynomials, denoted here by pq(F, G). The practical significance of 
* this form is illustrated by transfer functions in electronic cicuit theory.
* A poly(nomial) is a list of real coefficents of z, from const to degree.
* A monic poly is one where the coefficient of the highest degree is 1.
* Any poly may be rendered monic by factoring out a real scale factor. 
* A rational quotient of polys is pq(numerator, denominator), and pq may
* be reduced to lower terms by cancelling common leading zeros, rendering 
* numerator and denominator monic, and taking the quotient of the scales. 
* No attempt is made to cancel common algebraic factors.

* The poly with roots at a +- b*i in the complex plane (where a and b are
* real and i = sqrt(-1)) is the quadratic with real coefficients:     
*  [a^2 + b^2, -2a, 1].
* Thus, any poly may be defined as a product of real linear and quadratic
* factors from the roots, if you know them.
* Cons-ing a zero onto a polynomial is the same as post-multiplying by z.

* Compound expressions on F involving arithmetic operators +, -, * may be 
* evaluated with eval. There are also clauses for differentiation and 
* integration of polys, which is a form of function composition. 

* Polys may be expressed in binary form as [F|Fs] = F+Fs*z,
* and this leads to recursive expressions for operations on polys.
* eg.: 
*  [F|Fs]*[G|Gs] = (F+Fs*z) * (G+Gs*z) = F*G + (G*Fs+F*Gs)*z + Fs*Gs*z*z 
*  Function composition of polynomials denoted in math notation by F(G) 
*  is denoted here by apply(F, G). Then we have:
*  apply([F|Fs], G) = F + G*apply(Fs, G).

* Halley and Newton are classical root-finding transformers, which when 
* composed with a poly and a root guess will usually, and in some sense, 
* return a better guess.

* Polynomials may be printed in arithmetic notation with portray(poly, F).
* Quotients may be printed in arithmetic notation with:
*              portray(pq, Scale, pq(F, G)).

* Contour and gradient maps of quotients may be displayed on the complex 
* plane with reliefpq(Scale, pq(F, G), Legend), which uses 
* comm('plotserver') to start the display co-processor. The window header 
* displays the Legend, and the status bar shows the result w (in polar 
* coordinates) of applying Scale * pq(F, G) once to the cartesian coordinates 
* z of the mouse position.

* The primitive distancepq(Scale, pq(F, G), Legend) is similar, but calls 
* comm('plotserver distance'), which colors the transformed function output 
* according to distance from a root of the original function.
* ----------------------------------------------------------------------
*/

polycsum(K, pq(GNum, GDenom), pq(ENum, GDenom)) :- % sum of const and exprssn
   eval(K * GDenom + GNum, ENum).
polycsum(K, G, E) :-
   eval(G, G1),
   polyc1sum(K, G1, E).

polyc1sum(F, [], F).
polyc1sum(K, [G|Gs], [E|Gs]) :-
   E is K + G.
                                           
polycdiff(K, pq(GNum, GDenom), pq(ENum, GDenom)) :- % diff of const and exprssn
   eval(K * GDenom - GNum, ENum).
polycdiff(K, G, E) :-
   eval(G, G1),
   polyc1diff(K, G1, E).

polyc1diff(F, [], F).
polyc1diff(F, [G], []) :-
   F =:= G.
polyc1diff(K, [G|Gs], [E|Gs]) :-
   E is K - G.

polyderiv(pq(F, G), E) :- !,
   polyderiv(F, F1),
   polyderiv(G, G1),
   eval(G * F1 - F * G1, Num),
   eval(G * G, Denom),
   eval(pq(Num, Denom), E).
polyderiv([F|Fs], G) :- !,                     % derivative
   polyderiv(Fs, 0, G).
polyderiv([], []).
polyderiv(F, E) :-
   eval(F, G), !,
   polyderiv(G, E).

polyderiv([F|Fs], N, [G|Gs]) :- 
   N1 is N + 1,
   G is N1 * F, !,
   polyderiv(Fs, N1, Gs). 
polyderiv([], _, []).

polyinteg(F, K, [K|G]) :-  !,                    % integral
   polyinteg1(F, 1, G).

polyinteg1([], _, []).
polyinteg1([F|Fs], N, [G|Gs]) :-
   N1 is N + 1,
   G is F/N, !,
   polyinteg1(Fs, N1, Gs).

eval0([0], 0).                                 %1 canonicalize zeros
eval0([0.0], 0).                               %2 canonicalize zeros
/* One level binary expressions with level zero arguments */
eval0([] * _, []) :- !.                        %3 short cuts
eval0(_ * [], []) :- !.                        %4
eval0(0 * _, 0) :- !.                          %5
eval0(_ * 0, 0) :- !.                          %6
eval0([] + G, G) :- !.                         %7
eval0(F + [], F) :- !.                         %8
eval0(0 + G, G) :- !.                          %9
eval0(F + 0, F) :- !.                          %10
eval0([] - G, E) :- !,
   eval0(-1 * G, E).                           %11
eval0(F - [], F) :- !.                         %12
eval0(F - 0, F) :- !.                          %13
eval0([F|Fs] + [G|Gs], E) :- !,                %14 sums
   FG is F + G,
   eval0(Fs + Gs, FsGs),
   (FsGs = [0] -> E1 = [FG];  E1 = [FG|FsGs]),
   (E1 = [E2], E2 =:= 0 -> E = [];  E = E1).
eval0(F + G, E) :-                             %15
   number(F), !,
   (number(G) -> E is F + G;  polycsum(F, G, E)).
eval0(F + G, E) :-                             %16
   number(G), !,
   polycsum(G, F, E).
eval0(pq(NumF, Denom) + pq(NumG, Denom), pq(NumE, Denom)) :- !, %17
   eval0(NumF + NumG, NumE).
eval0(pq(NumF, DenomF) + pq(NumG, DenomG), pq(NumE, DenomE)) :- !, %18
   eval0(DenomF * DenomG, DenomE),
   eval0(NumF * DenomG, W1),
   eval0(NumG * DenomF, W2),
   eval0(W1 + W2, NumE).
eval0(F + pq(Num, Denom), pq(FNum, Denom)) :- !, %19
   eval(F * Denom + Num, FNum).
eval0([F|Fs] - [G|Gs], E) :- !,                %20 differences
   FG is F - G, 
   eval0(Fs - Gs, FsGs),
   (FsGs = [0] -> E1 = [FG];  E1 = [FG|FsGs]),
   (E1 = [E2], E2 =:= 0 -> E = [];  E = E1).
eval0(F - G, E) :-                             %21
   number(F), !,
   (number(G) -> E is F - G;  polycdiff(F, G, E)).
eval0(F - G, E) :-                             %22
   number(G), !,
   G1 is - G, 
   polycsum(G1, F, E).
eval0(pq(NumF, Denom) - pq(NumG, Denom), pq(NumE, Denom)) :- !, %23
   eval(NumF - NumG, NumE).
eval0(pq(NumF, DenomF) - pq(NumG, DenomG), pq(NumE, DenomE)) :- !, %24
   eval0(DenomF * DenomG, DenomE),
   eval0(NumF * DenomG, W1),
   eval0(NumG * DenomF, W2),
   eval0(W1 - W2, NumE).
eval0(F - pq(A, B), pq(C, B)) :-               %25
   eval(F * B - A, C).
eval0(pq(A, B) * pq(C, D), pq(Num, Denom)) :- !, %26 products
   eval0(A * C, Num),
   eval0(B * D, Denom).
eval0(A * pq(B, C), pq(Num, C)) :- !,          %27
   eval0(A * B, Num).
eval0(pq(A, B) * C, pq(Num, B)) :- !,          %28
   eval0(A * C, Num).
eval0(F >> 1, E) :- !,                         %29 z multiplied
   eval0(F * z, E).
eval0(F >> N, [0|E]) :-
   N > 1, !,                                   %29 z multiplied
   N1 is N - 1,
   eval0(F >> N1, E).
eval0(F * z, E) :-                             %29 z multiplied
   (number(F) -> E = [0.0|[F]];  E = [0.0|F]).
eval0(F * G, E) :- !,                          %30
   polyprod(F, G, E).
eval0(pq(A, pq(B, C)), E) :-                   %31 quotient
   eval(A * pq(C, B), E).
eval0(apply([F], G), [F]).                     %32 composition
eval0(apply([F|Fs], G), E) :-                  %33
   eval0(F + G * apply(Fs, G), E).
eval0(F, F).                                   %34 atom or unrecognised

polyprod([F|Fs], [G|Gs], E) :- !, 
   (
      F =:= 0 ->
      (                                        % F is 0
         E0 = [0], G =:= 0 ->                  % G is 0
         E1 = [0];  eval0(G * Fs, E1)          % G is not 0
      );

      (                                        % F is not 0
         G =:= 0 ->                            % G is 0
         E0 = [0],
         eval0(F * Gs, E1);                    % F & G are not 0

         EF is F * G, 
         E0 = [EF],
         eval((F * Gs + G * Fs), E1)
      )
   ),
   (Fs = [] -> E2 = [0];  (Gs = [] -> E2 = [0];  eval0(Fs * Gs, E2))),
   eval(E0 + E1 * z + E2 * z * z, E).
polyprod(F, G, E) :-
   number(F), !,
   (number(G) -> E is F * G;  polycprod(F, G, E)).
polyprod(F, G, E) :-
   number(G), !,
   polycprod(G, F, E).

polycprod(_, [], []).                          %1
polycprod(K, [G|Gs], [E|Es]) :- !,             %2
   E is K * G, 
   polycprod(K, Gs, Es).
polycprod(K, pq(GNum, GDenom), pq(ENum, GDenom)) :- !, %3
   polycprod(K, GNum, ENum).
polycprod(K, G, E) :-                          %4
   eval(G, G1),
   eval(K * G1, E).

lowestpq(pq(F, G), Scale, pq(FM, GM)) :-       % pq(F, G) in lowest terms
   cancel(pq(F, G), pq(FC, GC)),
   monic(FC, SN, FM),
   monic(GC, SD, GM),
   Scale is float(SN)/SD.

cancel(pq([Z1|N], [Z2|D]), E) :-
   Z1 =:= 0,
   Z2 =:= 0,
   cancel(pq(N, D), E).
cancel(pq(N, D), pq(N, D)).

monic(F, MF) :-
   monic(F, _, MF).

monic(F, D, MF) :-
   last(F, D),
   (D =:= 1.0 -> MF = F;  scaledown(D, F, MF)).

last([F], F).
last([F|Fs], Last) :-
   last(Fs, Last).

scaledown(_, [], []).
scaledown(D, [X|Xs], [M|Ms]) :-
   M is float(X)/D, 
/*
	(
		 X =:= 0 -> 
		 M = 0 ; 

		 (
			  D =:= 1 -> 
			  M = X ; 
			  (
					X =:= D ->
					M = 1 ;

					M = X/D
			  )
		 )
	),
*/
   scaledown(D, Xs, Ms).

reverse(A, Z) :-
   reverse(A, Z, []).

reverse([H|T]) -->
   reverse(T),
   [H].
reverse([]) --> [].

% portray(pq, S, pq(F, G)) assumes F and G are monic. 
% Therefore [X] == [1] 
portray(pq, Scale, pq(1, G)) :-                %1
   portray(pq, Scale, pq([1], G)).
portray(pq, Scale, pq(F, [1])) :-              %2
   portray(pq, Scale, pq(F, [1])).
portray(pq, Scale, pq([1], G)) :-              %3
   write(Scale), write('/'), write('('), 
   portray(poly, G),
   write(')').
portray(pq, Scale, pq(F, [1])) :-              %4
   (
      Scale =:= 1 ->
      portray(poly, F);

      write(Scale), tab(1), write('*'), tab(1), write('('), 
      portray(poly, F),
      write(')')
   ).
portray(pq, Scale, pq(F, G)) :-                %5
   (Scale =:= 1 -> true;  write(Scale), write($ * $)),
                                              % Scale is now written (if not 1)
   write('('), 
   portray(poly, F),
   write(')'), tab(1), write('/'), tab(1), write('('), 
   portray(poly, G),
   write(')').

portray(poly, [F]) :-                          %6
   write(F).
portray(poly, [F|Fs]) :-                       %7
   putterm(1, Fs),                             % write tail first
   (F =:= 0 -> true;  write($ + $), write(F)).

putterm(_, [X]) :-
   X =:= 0.                              %1 empty term
putterm(Exponent, [F]) :-                      %2 non empty term
   (F =:= 1 -> true;  write(F), write('*')),
   write(z), write($**$), write(Exponent).
putterm(Exponent, [F|Fs]) :-                   %3      
   Exponent1 is Exponent + 1,
   putterm(Exponent1, Fs),                     % recurse to the end
   (
      F =:= 0;

      write($ + $), 
      (F =\= 1 -> write(F), write('*');  true),
      write(z), 
      (Exponent =:= 1;  write($**$), write(Exponent))
   ).

newton(F, S, N) :-                          % Newton's root finding transformer
   polyderiv(F, F1),
   eval([0, 1] - pq(F, F1), G),
   lowestpq(G, S, N).

halley(F, S, H) :-                          % Halley's root finding transformer
   polyderiv(F, FI), 
   polyderiv(FI, FII), 
   eval(2 * FI, FI2), 
   eval(pq(FII, FI2), FF), 
   eval(FF * F, D), 
   eval(FI - D, Denom), 
   eval(pq(F, Denom), T3), 
   eval([0.0, 1.0] - T3, G), 
   lowestpq(G, S, H), !.

flow(X) :-
  write('> '), write(X), nl.
flow(X) :-
  write('< '), write(X), nl, 
  fail.

