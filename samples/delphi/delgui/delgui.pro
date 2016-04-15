%------------------------------------------------------------
% DELGUI.PRO - The Prolog portion of the Amzi! Prolog
%              / Delphi GUI toolbox
%
%------------------------------------------------------------

% A predicate that can be called first just to make
% sure the Delphi front-end has successfully loaded the
% Logic Server, and that the extended DelGUI predicates
% have been successfully initialized.

hello :-
  w_message($Logic Server alive and well$).

%--------------------------------------------------------------
% Writes a string or a term to a message box.  This predicate
% illustrates how to use Prolog and Delphi together for creating
% extended predicates.  The showmessage predicate is simple to
% implement with just a string argument.  The logic for displaying
% either a term (which is converted to a string) or string is
% handled in Prolog.
%

w_message(S) :-
  string(S),
  !,
  showmessage(S).
w_message(T) :-
  string_term(S, T),
  showmessage(S).

%-------------------------------------------------
% Prolog code that generates a point list from a
% symbolic Prolog equation.  The points are then
% graphed on a scigraph component.
%


pg$getpointlist(L, YMin, YMax, Begin, End, Inc, F) :-
  pg$getpoints(Begin, End, Inc, F),
  findall(X:Y, pg$xy(X,Y), L),
  pg$ymin(YMin),
  pg$ymax(YMax).

pg$getpoints(Begin, End, Inc, f(N) = F) :-
  retractall(pg$x(_)),
  retractall(pg$xy(_,_)),
  assert(pg$x(Begin)),
  repeat,
  retract(pg$x(N)),
  Y is F,
  pg$chkmin(Y),
  pg$chkmax(Y),
  assert(pg$xy(N,Y)),
  NN is N + Inc,
  assert(pg$x(NN)),
  NN > End,
  !.

pg$chkmin(Y) :-
  pg$ymin(X),
  X < Y,
  !.
pg$chkmin(Y) :- 
  retractall(pg$ymin(_)),
  assert(pg$ymin(Y)).

pg$chkmax(Y) :-
  pg$ymax(X),
  X > Y,
  !.
pg$chkmax(Y) :-
  retractall(pg$ymax(_)),
  assert(pg$ymax(Y)).


%----------------------------------------------------------------
% Code that displays Prolog terms made out of mathematical
% operators as they might appear in a textbook.
%

% set_equation is called with a string containing a valid
% Prolog mathematical term, such as a + b, a/2 + 3 * (c - d), or
% 'X' = sqrt(a * b ** 3).  It converts the string to a Prolog
% term and saves it in the dynamic database.

set_equation(SEq) :-
  string(SEq),
  !,
  (string_term(SEq, Eq) ->
     true;
     w_message($Invalid Prolog Term$) ),
  set_equation(Eq).
set_equation(Eq) :-
  retractall(equation(_)),
  retractall(eq_commands(_)),
  assert(equation(Eq)).

% paint_equation is called from Delphi is response to an
% OnPaint message for the PaintBox control used to display
% the equation.  The argument C identifies the canvas to
% which all of the draw commands are targetted.  eq_commands
% contains a list of commands that can be used to paint the
% PaintBox.  If it doesn't exist, show_graphic_eq is called
% to generate such a list from the equation stored above.

paint_equation(C) :-
  eq_commands(Commands),
  !,
  do_graphics(Commands).
paint_equation(C) :-
  equation(Eq),
  show_graphic_eq(Eq, C).

% show_graphic_eq calls grapheq, which does all the work,
% and returns a list of Prolog statements that, when
% executed, will paint the PaintBox.  The commands are saved
% in the dynamic database for subsequent use, such as when the
% window is overlapped and requires repainting.

show_graphic_eq(Eq, C) :-
  grapheq(C, Eq, 0:0, _, 1200, Commands),
  assert(eq_commands(Commands)),
  do_graphics(Commands).

% do_graphics walks the list of commands, calling each one.
% These commands are either arithmetic equations or calls to
% the extended predicates for drawing, such as draw_lineto/3.

do_graphics([]).
do_graphics([H|T]) :-
  call(H),
  !, do_graphics(T).
do_graphics([H|T]) :-
  string_term(S, bad_draw_command(H)),
  w_message(S),
  fail.

% grapheq does all the work.  It takes advantage of the way
% Prolog uses mathematical operators to easily walk down the
% structure.  For example, the equation y = m * x + b is entered
% in Prolog as written, but is really stored as =(y,+(*(m,x),b)).
%
% This lends itself to a recursive approach.  Using the example,
% the first level of recursion would see A = B, where A & B are
% logical Prolog variables.  A is instantiated to y, and B is
% instantiated to +(*(m,x),b).
%
% grapheq at this top level recursively calls grapheq for both
% A and B, getting the list of commands for drawing each. It then
% creates its command list which is the commands for A, followed
% by the commands for placing the '=' sign, followed by the commands
% for displaying B.
%
% In this case, the command for displaying A is simple.  It is just a
% draw_textout(C, X, Y, $y$) command.  The command list returned for
% displaying B is more complex, and is made up of the appended results
% from various levels of recursion.  The call to display B calls
% the grapheq clause that handles A2 + B2, where A2 is *(m,x) and B2
% is b.  B2 winds up being a simple textout again, but A2 recursively
% calls a clause that can draw A3 * B3.  Finally, both sides are simple
% with A3 = m and B3 = x.
%
% In simplified form, the recursion looks like:
%
% grapheq( y = m*x+b )
%   grapheq( y )
%   grapheq( m*x + b )
%     grapheq( m * x )
%       grapheq( m )
%       grapheq( x )
%     grapheq( b )
%
% Each level of the recursion returns useful information to the
% level that called it.  This includes the command list needed to
% draw that term and the height and width of the rectangle needed
% to contain the displayed version of the term.  (Our simple example
% plays out linearly, but the process gets more complex with fractions
% that build on top of each other, exponents, and/or square roots.  But
% even the simple case requires knowledge of the width of the A side
% in order to know where to place the operator (=, +, etc.) symbol.)
%
% Getting the height and width of each term to be displayed is needed
% to determine the X,Y position for each of the drawing commands.  For
% example, fractions need to know where to draw the numerator, the
% denominator and the line, as well as how long the line is.  Similarly,
% exponents need to be placed and the size and scope of a square root
% symbol needs to be determined.
%
% This is all done with a bit of Prolog magic.  Consider the case of
% displaying 'A' = 1/100.  It should come out as
%
%        1
%  A = -----
%       100
%
% Notice that the numerator and denominator are centered on a line that
% extends one character width beyond the longer of the two.  We don't
% know where to draw the '1' until after the '100' has been figured out.
% Further, the A is vertically centered based on the fraction's height.
%
% This is why a command list is used.  The commands for drawing the '1',
% 'A', '=', '100' and the line all refer to various X and Y positions,
% but these are left as unbound Prolog variables.  They are all relative
% positions, based on the positions of the other elements.
%
% Only the first call to grapheq has a concrete initial position.  The
% initial positions of all subsequent calls are all relative computations.
% When the full command list is returned to the top level, then its
% initial position values are unified with the appropriate assignment
% statements in the command list.  And all of the relative assignements
% down through the command list are tied to that initial position.  This
% happens for free in Prolog.
%
% The result is, the final command list has all of the equations for
% computing where to display each item in the term as well as the
% draw commands for displaying the items.
%
% There is more complexity as you look at the code.  grapheq keeps track
% of operator precedences for deciding when to draw parenthesis, as well
% as dealing with square roots.
%
% There are clauses in grahpeq for dealing with
%   a prefix '-' operator
%   most binary infix operators, + - = *
%   fractions  /
%   exponents **
%   square roots  sqrt(A)
%   and a simple text string, the bottom of a recursion.
%

grapheq(_, X, _, _, _, _) :-
  var(X),
  !,
  w_message($Graph Error: Variable input to graph$),
  fail.

% Display a term with a unary - operator.

grapheq(C, - A, X:Y, H:W, TopPrec, Commands) :-
  !,
  draw_textwidth(C, $ - $, TW),
  unary_op(-, Prec),
  grapheq(C, A, Xb:Yb, Hb:Wb, Prec, BComm),
  char_height(C, CH),
  (optype(A,**) ->
     DeltaO is Hb - CH;
     DeltaO is integer((Hb-CH)/2) ),
  H = Hb,
  W is TW + Wb,
  Commands =
    [Xo is X,
     Yo is Y + DeltaO,
     draw_textout(C, Xo, Yo, $ - $),
     Xb is X + TW,
     Yb is Y
     | BComm].

% Display a binary operator that expands from left
% to right, such as + and *.  Check the precedence
% of the calling operator, and if this operator's
% precedence is higher, add parenthesis.  Adjust the
% vertical height using a different strategy for
% terms with exponents and those without.

grapheq(C, AOPB, X:Y, H:W, TopPrec, Commands) :-
  aopb(AOPB, OP, A, B, Prec),
  !,
  grapheq(C, A, Xa:Ya, Ha:Wa, Prec, AComm),
  draw_textwidth(C, OP, TW),
  grapheq(C, B, Xb:Yb, Hb:Wb, Prec, BComm),
  char_width(C, CW),
  (Prec > TopPrec -> ParenW = CW; ParenW = 0),
  maxi(Ha, Hb, H),
  W is Wa + TW + Wb + 2*ParenW,
  char_height(C, CH),
  (H = Ha -> TallArg = A; TallArg = B),
  (optype(TallArg,**) ->
     DeltaO is H - CH,
     DeltaA is H - Ha,
     DeltaB is H - Hb
     ;
     DeltaO is integer( (H-CH)/2 ),
     DeltaA is integer( (H-Ha)/2 ),
     DeltaB is integer( (H-Hb)/2 ) ),
  append(
    [( ParenW = 0 -> true;
       Xp1 is X,
       Yp is Y + DeltaO,
       draw_textout(C, Xp1, Yp, $($) ),
     Xa is X + ParenW,
     Ya is Y + DeltaA
     | AComm],
    [Xo is X + Wa + ParenW,
     Yo is Y + DeltaO,
     draw_textout(C, Xo, Yo, OP),
     Xb is Xo + TW,
     Yb is Y + DeltaB
    | BComm],
    CommX),
  append(CommX,
    [( ParenW = 0 -> true;
       Xp2 is Xb + Wb,
       draw_textout(C, Xp2, Yp, $)$) )],
    Commands).

% Display a fraction, reducing the font and putting
% the numerator and denominator over each other.
% Center both.

grapheq(C, A / B, X:Y, H:W, TopPrec, Commands) :-
  !,
  reduce_font(C, FSize, FSize2),
  grapheq(C, A, Xa:Ya, Ha:Wa, 1200, AComm),
  grapheq(C, B, Xb:Yb, Hb:Wb, 1200, BComm),
  restore_font(C, FSize),
  char_height(C, CH),
  char_width(C, CW),
  H is Ha + Hb + integer(1 + CH / 10),
  maxi(Wa, Wb, Wx),
  CenterA is integer( (Wx-Wa)/2 ),
  CenterB is integer( (Wx-Wb)/2 ),
  Extend is integer(1 + CW/2),
  W is Wx + 2 * Extend,
  append(
    [draw_font(C, _, _, FSize2),
     Xa is X + Extend + CenterA,
     Ya is Y
     | AComm],
    [Xo is X,
     Yo is Y + Ha,
     Xl is X + Wx + 2*Extend,
     draw_moveto(C,Xo,Yo),
     draw_lineto(C,Xl,Yo),
     Xb is X + Extend + CenterB,
     Yb is Y + Ha + integer(1 + CH / 10)
     | BComm],
    CommX),
  append(CommX,
    [draw_font(C, _, _, FSize)],
    Commands).

% Display an exponent, reducing the font as well.

grapheq(C, A ** B, X:Y, H:W, TopPrec, Commands) :-
  !,
  binary_op(**, Prec),
  grapheq(C, A, Xa:Ya, Ha:Wa, Prec, AComm),
  reduce_font(C, FSize, FSize2),
  grapheq(C, B, Xb:Yb, Hb:Wb, Prec, BComm),
  restore_font(C, FSize),
  ( Ha > Hb ->
      Elevate is Ha - integer(Hb/2);
      Elevate is integer(Ha/2) ),
  Lower is Elevate + Hb - Ha,
  H is Hb + Elevate,
  W is Wa + Wb,
  append(
    [Xa is X,
     Ya is Y + Lower
     | AComm],
    [draw_font(C, _, _, FSize2),
     Xb is X + Wa,
     Yb is Y
     | BComm],
    CommX),
  append(CommX,
    [draw_font(C, _, _, FSize)],
    Commands).

% Display a square root, drawing the symbol around
% the enclosed term.

grapheq(C, sqrt(A), X:Y, H:W, TopPrec, Commands) :-
  !,
  grapheq(C, A, Xa:Ya, Ha:Wa, 1200, AComm),
  char_width(C, CW),
  char_height(C, CH),
  Offset is integer(1 + CH/10),
  H is Ha + Offset,
  W is Wa + CW,
  Commands =
    [X1 is X,
     Y1 is Y + integer(2*Ha/3),
     draw_moveto(C, X1, Y1),
     X2 is X1 + integer(CW/2),
     Y2 is Y + Ha + Offset,
     draw_lineto(C, X2, Y2),
     X3 is X + CW,
     Y3 is Y,
     draw_lineto(C, X3, Y3),
     X4 is X3 + Wa + integer(CW/2),
     Y4 is Y3,
     draw_lineto(C, X4, Y4),
     Xa is X + CW,
     Ya is Y + Offset
     | AComm].

% Take whatever's left over and convert it to a string
% and simply display it.

grapheq(C, A, X:Y, H:W, _, [draw_textout(C, X, Y, S)]) :-
  string_term(S, A),
  draw_textwidth(C, S, W),
  draw_textheight(C, S, H).

% Various useful support predicates

aopb(A = B, $ = $, A, B, P) :- binary_op(=,P).
aopb(A is B, $ = $, A, B, P) :- binary_op(=,P).
aopb(A + B, $ + $, A, B, P) :- binary_op(+,P).
aopb(A - B, $ - $, A, B, P) :- binary_op(-,P).
aopb(A * B, $ $, A, B, P) :- binary_op(*,P).

optype(X, OP) :-
  X =.. [OP,_,_].
optype(X, OP) :-
  X =.. [OP,_].

char_height(C, CH) :-
  draw_textheight(C, $M$, CH).

char_width(C, CW) :-
  draw_textwidth(C, $M$, CW).

reduce_font(C, FSize, FSize2) :-
  draw_font(C, _, _, FSize),
  (FSize >= 10 ->
      FSize2 is FSize - 2;
      FSize2 = 8),
  draw_font(C, _, _, FSize2).

restore_font(C, FSize) :-
  draw_font(C, _, _, FSize).

unary_op(Op, Prec) :-
  cur$unop(Prec, _, Op).

binary_op(Op, Prec) :-
  cur$biop(Prec, _, Op).

%---------------------
% Utility predicates
%

maxi(A, B, A) :- A >= B, !.
maxi(A, B, B).

member(X, [X|_]).
member(X, [_|Z]) :- member(X, Z).

append([], X, X).
append([A|X], Y, [A|Z]) :-
  append(X, Y, Z).



