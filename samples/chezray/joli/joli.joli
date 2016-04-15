%-*-Prolog-*-  
% joli indented on 3/19/2002 by 'JOLI' 1.0.

% Copyright (c) 2002 by Ray Reeves. All Rights Reserved.
/*
There are six styles for laying out terms:

  directive: 
  long:      vertical layout for top level goals and long nests.

The following styles may handle long lines:
 
  short:     horizontal layout for short nests. One shot.
  expr:      horizontal wrappable. One shot.
  wrap:      horizontal wrappable. Persistent.
  write:     horizontal wrappable. Persistent.

writeHandler has the difficulty that writes are not nested, so 
a signal with a special index (0) is thrown when writes are over.
*/
main :-
  abolish(commentCol/1),
  abolish(tabSize/1),
  abolish(hIn/1),
  abolish(hOut/1),
  write(` What file? `), 
  read(Name),
  nl, 
  name(Name, SrcName),
  append(SrcName, ".joli", Out),
  name(OUT, Out),
  append(SrcName, ".pro", In1),
  name(IN1, In1),
  append(SrcName, ".pl", In2),
  name(IN2, In2),

% catch(open(IN1, read, H, [type(binary)]), error(system_error, _), 
% catch(open(IN2, read, H, [type(binary)]), error(system_error, _), 
  catch(open(IN1, read, H, []), error(system_error, _), 
       catch(open(IN2, read, H, []), error(system_error, _), 
            (
               write(`Can't find `), write(IN1), write(` or `), write(IN2), 
               nl, 
               fail
            ))),
  close(H),
  catch(open(IN1, read, Hin, []), error(system_error, _), 
       open(IN2, read, Hin, [])),
  open(OUT, write, Hout, []), !,
  assert(hIn(Hin)),
  clause(hIn(X), true),
  assert(hOut(Hout)),
  assert(commentCol(48)),
  assert(tabSize(3)),
  date(Month, Day, Year),
  write(Hout, `%-*-Prolog-*-  `), nl(Hout),   % line 1
  write(Hout, `% `), write(Hout, Name), write(Hout, ` indented on `), 
  write(Hout, Month), write(Hout, '/'), write(Hout, Day), write(Hout, '/'), 
  write(Hout, Year), write(Hout, ` by 'JOLI' 1.0.`), nl(Hout), nl(Hout), 
  cntr_set(1, 4),                             % lino
  cntr_set(2, 1),                             % output col
  cntr_set(3, 0),                             % depth
  cntr_set(4, 0),                             % indent
  cntr_set(5, 0),                             % input col
  cntr_set(6, 1),                             % handler id
  cntr_set(7, 2),                             % lfs. anticipate 2
  cntr_set(8, 0),                             % stars
  catch(startPrograms([10], _), err(E), eHandler(E)),
  abolish(commentCol/1),
  abolish(tabSize/1),
  close(Hin),
  close(Hout).

eHandler(eof) :-
  write(`** Unexpected !EOF **`), nl.
eHandler(unbal) :-
  lino(Lino),
  write(`** Unbalanced parens **`), write(` line `), write(Lino), nl.

startPrograms -->
  prime,
  programs.

programs -->
  check('end_of_file'), !.
programs -->
  lf0,                                        % blank line between programs
                                              % do not indent for new program
  program(principal(Name, Arity, _, _)),      % get Name and Arity
  {setIndent(0)},

% ({ wasCommentLine} -> [] ; lf0, lf), % between programs
  (
     {nonvar(Name)} ->
     {(Name = [_|_] -> name(N, Name) ;  N = Name)},
     {write(N), write('/'), write(Arity), nl} ;

     []
  ),
  programs.

program(ProgName) -->
  check(0':), !,
  directives(ProgName).
program(ProgName) -->
  clauses(ProgName).

directives(ProgName) -->
  pushState(State),
  directive(Principal), !,                    % get Principal
  (
     {ProgName = Principal} ->                % instantiate or check name
     (
        check('!EOF') ;                       % stop (:-)

        (check(0':) -> directives(ProgName) ;  []) % continue
     ) ;

     popState(State)                          % stop and back up
  ).

directive(Principal) -->
  check(0':),
  [0':],
  prime0,
  check(Dash),
  - [0':],
  {Dash == 0'-}, !,                           % no head
  fire,
  firePrime,
  fireBlank,                                  % after neck
  {setIndent(tab)},
  (
     opDecs ->                                % special case, with semantics
     {Principal = principal("op", 3, _, _)} ;

     dirTerms(Principal)                      % one or more terms
  ),
  check(X),
  [0'.],                                      % may be 2 in pipe
  {fire(0'.)},
  prime,                                      % read or not
  {setIndent(0)},
  lf.

dirTerms(Principal) -->
  term([directive(0)], 1, 0, High, P),
  (
     check(0',) ->
     firePrime,
     fireBlank,
     dirTerms(Principal) ;

     check(0'.),
     {Principal = P}
  ).

opDecs -->
  opDec,
  (check(0',) -> firePrime, fireBlank, opDecs ;  check(0'.)).

opDec -->
  check(0'(), !,
  firePrime,
  opDec,
  check(0')),
  firePrime.
opDec -->
  check(0'o),
  lexeme([opDec], 0, _, Functor, Op, _),
  {name(op, Op)},
  check(0'(),
  firePrime,
  lexeme([opDec], 0, _, nmbr, P, _),
  {int_list(Prec, P)},
  check(0',),
  firePrime,
  fireBlank,
  lexeme([opDec], 0, _, F, A, _),
  {name(Assoc, A)},
  check(0',),
  firePrime,
  fireBlank,
  opName(Prec, Assoc),
  check(0')),
  firePrime.

opName(Prec, Assoc) -->
  check(0'[), !,                              % list of operator names
  {col(Col)},
  firePrime,
  (
     check(0']) ->
     {Prec = 0},
     {Assoc = 0},
     firePrime ;                              % empty list

     {indent(Indent)},
     {setIndent(Col)},
     opNames(Prec, Assoc),
     {setIndent(Indent)},
     check(0']),
     firePrime
  ).
opName(Prec, Assoc) -->
  lexeme([opName], 0, _, F, N, Arg1),
  {name(Name, N)},
  {
     functor(Check, current_op, 3),
     arg(1, Check, Prec),
     arg(2, Check, Assoc),
     arg(3, Check, Name),
     functor(Term, op, 3),
     arg(1, Term, Prec),
     arg(2, Term, Assoc),
     arg(3, Term, Name),
     (call(Check), ! ;  call(Term))           % assert only of not there
  }.

opNames(Prec, Assoc) -->
  opName(Prec, Assoc),
  (
     check(0',) ->
     firePrime,
     {col(Col)},
     (
        {Col > 70} ->
        lf ;                                  % wrap

        fireBlank
     ),
     opNames(Prec, Assoc) ;

     (
        check(0':) ->                         % ops, conditional dec
        body(directive(1)) ;                  % ignore body semantics

        []
     )
  ).

int_list(Int, List) :-
  int_list(Int, 0, List).

int_list(Int, Int, []).
int_list(Int, Acc, [D|Rest]) :-
  Acc1 is 10*Acc + D - 0'0,
  int_list(Int, Acc1, Rest).

clauses(ProgName) -->
  pushState(State),
  claws(Principal),
  (                                           % clause has ended
     {ProgName = Principal} ->                % instantiate or check name
     firePrime,                               % same, so now fire period
     lf2,                                     % ensure at new line
     check(EOF),
     ({EOF = end_of_file} ;  clauses(ProgName)) ; % eof or continue

     popState(State)                          % new name, back up & stop
  ).

claws(Principal) -->

% {resetWasCommentLine},
  {setIndent(1)},
  head(Principal),
  {indent(Indent)},
  (
     [nonunit(Neck)] ->
     (
        format(neck, Neck, _),                % ray
        (
           check(0'.) ;                       % DCG unit after all

           (
              check(0'!) ->                   % look for cut
              {col(Col1)},
              ({Col1 > Indent} -> fireBlank ;  []), % blank if not at indent
              firePrime,                      % fire the cut
              (
                 check(0',) ->
                 firePrime,
                 {setIndent(tab)},
                 lf,
                 body([long(1)]) ;

                 []                           % nonunit, 0'!, 0'.
              ) ;

              {setIndent(tab)},               % nonunit
              lf,
              body([long(1)])
           ) ;

           []
        ) ;

        []
     ) ;

     []
  ),
  check(0'.).          % do not prime in case its eof, which locks the stream!

head(Principal) -->
  term([head], 1, 0, principal("head", 0, -1, _), Principal).

body(Style) -->
  terms(Style, _), !.

eoHead(Name) :-
  depth(0),
  isNeck(Name).

eoBody(0'.).

eoTerm(0';, P, Style) :-
  not isHead(Style),
  P < 1100.
eoTerm(0',, P, _) :-
  P < 1100.
eoTerm(0'., _, _).
eoTerm(0'), _, _).
eoTerm(0'}, _, _).
eoTerm(0'], _, _).
eoTerm(nonunit(_), _, _).

eoTerms(0')).
eoTerms(0'}).
eoTerms(0']).
eoTerms(0'.).
eoTerms(nonunit(_)).

nestPair(0'(, 0')).
nestPair(0'{, 0'}).
nestPair(0'[, 0']).

isShort([handler(_, short, _, _, _)|_]).
isShort([short(_)|_]).

isLong([long(_)|_]).
isLong([handler(_, long, _, _, _)|_]).
isLong([directive(_)|_]).

isHead([head|_]).

isNeck("::-").                                % for bin prolog
isNeck(":-").
isNeck("-->").

isWrite("write").
isWrite("nl").
isWrite("writeq").
isWrite("tab").
isWrite("put").
isWrite("writelog").
isWrite("nllog").

isWrap([handler(_, wrap, _, _, _)|_]).
isWrap([wrap(_)|_]).

isWrites([handler(_, write, _, _, _)|_]).

isDirective([directive(_)|_]).

writeHandler(Hid, Style, Arity) -->
  catch(terms(Style, Arity), tooLong(Hid, write, Ix, State), 
       (
          popState(State),                    % pop to signalled State
          lf,
          ({Ix == 0} -> [] ;  writeHandler(Hid, Style, Arity))
       )).

       % handler(Unique handler id, Kind, term indent index, arg index, State)

exprHandler(Hid, Style, Rand, Higher, Highest) -->
  pushState(State),
  {Style1 = [handler(Hid, expr, 1, 1, State)|Style]},
  catch(                                      % establish handler
term(Style1, 1, Rand, Higher, Highest), tooLong(Hid, expr, _, _), 
       (
          popState(State),
          lf,
          {fireBlank},
          term([expr(1)|Style], 1, Rand, Higher, Highest)
       )).

wrapHandler(Hid, Style, Arity) -->
  catch(terms(Style, Arity), tooLong(Hid, wrap, Ix, State), 
       (
          popState(State),
          lf,
          {updateIx(Ix, Style, Style1)},
          wrapHandler(Hid, Style1, Arity)
       )).

updateIx(Ix, [handler(H, wrap, 1, _, 0)|S], [handler(H, wrap, 1, Ix, 0)|S]).

shortHandler(Style, Arity) -->
  {newHandler(Hid)},
  pushState(State),
  catch(callShort(Hid, Style, Arity), tooLong(Hid, short, _, _), 
       (popState(State), callLong([long(1)|Style], Arity))).

callShort(Hid, Style, Arity) -->
  check(Open),
  {nestPair(Open, Close)},                    % short is only for nests
  {linoColIndent(Lino, _, Indent)},
  {depthInc},
  {setIndent(Indent)},                        % ? already set
  firePrime,                                  % Open (with comment?)
  {confirmShorts(Style, Lino)},
  (
     {existsHandler(Style, _)} ->             % handler exists,
     {Style1 = [short(1)|Style]} ;            % so no handler here

     {Style1 = [handler(Hid, short, 1, 1, 0)|Style]} % reveal handler above
  ),
  terms(Style1, Arity),
  check(Close),
  {checkCol(Style1, 80)},
  {depthDec},
  {setIndent(Indent)},
  {confirmShorts(Style1, Lino)},
  firePrime.

callLong(Style, Arity) -->
  {linoColIndent(Lino, _, Indent)},
  check(Open),
  {nestPair(Open, Close)},
  {depthInc},
  firePrime,
  {tabIndent},
  ({lino(Lino)} -> lf1 ;  {padToIndent}),
  terms([long(1)|Style], Arity),
  check(Close),
  {checkCol(Style, 80)},
  {depthDec},
  {setIndent(Indent)},
  lf,
  firePrime.

/*
'terms' is not strictly necessary, because a body or an arg list can be 
considered a single term with the comma as operator. However, a comma as 
principal functor is not interesting, and it is easier to calculate arity
if the sub-terms are considered as separate terms. But directives  have 
principal functors above the comma, and then it is best to consider the 
commas as operators. Thus term will end on a comma (or semicolon) if the
the principal functor is less than 1100.
*/

terms(Style, Arity) -->                       % only if write handler
  {Style = [handler(_, write, _, _, _)|Tail]},
  pushState(State),
  term(Style, 1, 0, 0, principal(Name, _, _, _)),
  (
     {not(isWrite(Name))} ->                  % end of writes
     popState(State),
     lf,
     terms(Tail, Arity) ;

     prime,                                   % still a write
     check(Stop1),
     (
        {eoTerms(Stop1)} ->
        {getArity(Style, Arity)} ;            % exit, set Arity

        (format(Stop1, Style), ! ;  []),
        check(Stop2),
        (
           {eoTerms(Stop2)} ->
           {getArity(Style, Arity)} ;         % exit found by format

           {blankety},
           pushState(State2),
           {bumpIndices(State2, Style, Style1)},
           terms(Style1, Arity)
        )
     )
  ).
terms(Style, Arity) -->
  {linoColIndent(Lino, Col1, _)},
  pushState(State),
  term(Style, 1, 0, 0, principal(Name, _, _, _)),
  (
     {isLong(Style)},
     {isWrite(Name)} ->                       % a write
     popState(State),
     {newHandler(Hid)},
     {Style1 = [handler(Hid, write, 1, 1, 0)|Style]},
     writeHandler(Hid, Style1, _) ;           % writeHandler calls terms

     prime,                                   % not a write
     {(isShort(Style) -> confirmShorts(Style, Lino) ;  true)},
     {checkCol(Style, 75)},
     check(Stop1),
     (
        {eoTerms(Stop1)} ->
        {getArity(Style, Arity)} ;            % exit, set Arity

        (format(Stop1, Style), ! ;  []),
        {(isShort(Style) -> confirmShorts(Style, Lino) ;  true)},
        check(Stop2),
        (
           {eoTerms(Stop2)} ->
           {getArity(Style, Arity)} ;         % exit found by format

           check(Stop2),
           {linoColIndent(Lino2, Col2, Indent)},
           (
              {isLong(Style)} ->
              lf ;                            % long

              ({isWrap(Style)} -> {pad(Indent)} ;  []),
              (
                 {Stop2 = 0'|} ->
                 [] ;

                 ({Col2 > Indent + 1} -> fireBlank ;  []) % not long
              )
           ),
           pushState(State2),
           {bumpIndices(State2, Style, Style1)},
           {checkCol(Style1, 75)},
           terms(Style1, Arity)
        )
     )
  ).

getArity([Style|_], Arity) :-
  (
     functor(Style, F, 5) ->                  % style F/5
     arg(4, Style, Arity) ;

     functor(Style, F, 1),                    % style F/1
     arg(1, Style, Arity)
  ).

bumpIndices(State, StyleIn, StyleOut) :-
  StyleIn = [Style|Rest],                     % get index of term
  (
     functor(Style, F, 5) ->                  % style F/5
     arg(1, Style, Hid),
     arg(2, Style, Kind),
     arg(3, Style, IndentIx),
     arg(4, Style, ArgIx),
     arg(5, Style, _),                        % forget current state
     IndentIx1 is IndentIx + 1,               % next term indent index
     functor(NewStyle, F, 5),
     arg(1, NewStyle, Hid),
     arg(2, NewStyle, Kind),
     arg(3, NewStyle, IndentIx1),
     arg(4, NewStyle, ArgIx1),
     arg(5, NewStyle, State) ;                % penultimate state

     functor(Style, F, 1),                    % style F/1
     arg(1, Style, ArgIx),
     functor(NewStyle, F, 1),
     arg(1, NewStyle, ArgIx1)
  ),
  ArgIx1 is ArgIx + 1,                        % next arg index
  StyleOut = [NewStyle|Rest].

confirmShorts(Style, Lino) :-                 % still same line
  (lino(Lino), ! ;  throwety(Style)).

lookForCut(Style) -->                         % fires 0'!
  ([10] -> {LF = 1}, prime1 ;  {LF = 0}, []), % remove lf, if any
  (
     [0'!] ->                                 % is cut, so fire it
     {fire(" !")},
     ({LF == 1} -> - [10] ;  prime) ;         % replace lf, if any

     ({LF == 0} -> - [10] ;  [])              % no cut, insert a lf
  ).

lookForNull -->
  prime1,                                     % [C], don't comment yet
  ([10] -> prime1 ;  []),
  (
     [0'[] ->
     prime1,
     (
        [0']] ->
        {fire(" []")},
        prime,
        (check(0',) -> firePrime ;  []) ;

        - [0'[]                               % list not null
     ) ;

     []
  ).

disjSpace(Style, Lino) -->
  (
     {isShort(Style)} ->
     {(lino(Lino) -> fireBlank ;  throwety(Style))} ;

     (
        {(isLong(Style) ;  isWrites(Style))} -> % some kind of long
        lf0,
        lf0,
        lf ;                                  % blank line and indent

        fireBlank                             % some other style
     )
  ).

/*
   LContext is true (1) if an operand has just been seen, and therefore
   an operator is expected. This resolves infix/prefix operator ambiguity.
*/

term(_, _, _, _, principal("!", 0, 0, cut)) -->
  check(0'!), !,                              % term is 0'!
  firePrime.
term(Style, TermIx, LContext, High, Highest) -->
  pushState(State),
  {lino(Lino)},
  check(Start),
  callTerm0(Start, Style, LContext, Rand, Term0),

                                              %
  {Term0 = principal(Name, Arity, Prec, Type)},
  {(isShort(Style) -> confirmShorts(Style, Lino) ;  true)},
  {col(Col)},
  (
     {Col > 75},                              % long line
     {Type \== operator},
     {not(member(handler(_, _, _, _, _), Style))},
     {TermIx > 1} ->
     popState(State),                         % long term, no handler
     lf,                                      % so backup and lf
     fireBlank,
     callTerm0(Start, Style, LContext, Rand, Term0) ;

     []                                       % regular line
  ),
  (
     ({Name = "/\\"}) ->                      % special case
     popState(State),                         % need a blank after all
     {blankety},
     callTerm0(Start, Style, LContext, Rand, Term0) ;

     []                                       % regular case
  ),
  {highestPrec(High, Term0, Higher, BreakPoint)}, % brpoint if this is higher
  {Higher = principal(N, A, P, T)},
  check(Stop),
  (
     (
        {eoHead(Name)},
        - [nonunit(Name)] ;

        {Name = "->"} ;

        {eoTerm(Stop, P, Style)}
     ),
     {Highest = Higher} ;                     % term has ended

     prime,                                   % now do active chars
     (specialTerm(Name, Style), ! ;  format(Stop, Style), ! ;  {blankety}),
     ({Stop == 0',} -> ({isLong(Style)} -> lf ;  fireBlank) ;  []),
     {Ix is TermIx + 1},
     (
        ({BreakPoint == 0} ;  {existsHandler(Style, _)}) ->
        term(Style, Ix, Rand, Higher, Highest) ; % recurse

        {newHandler(Hid)},                   % establish handler and call term
        exprHandler(Hid, Style, Rand, Higher, Highest)
     )
  ), !.

callTerm0(0'(, Style, _, 1, Term0) --> !,     % nested
  {Term0 = principal("nest", Arity, 0, nest)},
  shortHandler(Style, Arity).
callTerm0(0'{, Style, _, 1, Term0) --> !,     % nested
  {Term0 = principal("nest", Arity, 0, nest)},
  shortHandler(Style, Arity).
callTerm0(_, Style, LContext, Rand, Term0) --> % not nested
  term0(Style, LContext, Rand, Term0).

term0(Style, LContext, Rand, Term0) -->       % lexeme + args
  {lino(Lino)},
  lexeme(Style, LContext, Rand, Type, Name, Prec),
  {Term0 = principal(Name, Arity, Prec, Type)},
  (
     {Type == operator} ->
     {Arity = 2} ;                            % near enough!

     (                                        % not an operator
        {Type == fnctr} ->                    % not an atom, must have args
                          % Origin of 'wrap' style. Only other place is alist 
        wrapNest(Style, principal("nest", Arity, _, _)) ;

        {Arity = 0}                           % no args
     )
  ).

                                              % terms needing special format

specialTerm("|", _) --> [].
specialTerm("*", _) --> [].
specialTerm("**", _) --> [].
specialTerm("/", _) --> [].
specialTerm("//", _) --> [].
specialTerm("->", [handler(_, expr, _, _, _), long(_)|_]) -->
  lf.
specialTerm("->", [long(_)|_]) -->
  lf.

                                              % chars needing special format

format(0'|, _) --> [].
format(0'*, _) --> [].
format(0'/, _) --> [].
format(0'., _) -->
  {depth(Depth)},
  ({Depth > 0} -> {throw(err(unbal))} ;  []).
format(0',, Style) -->                        % comma
  check(0',),
  firePrime,
  (
     {isWrap(Style)} ->
     [] ;

     lookForCut(Style),
     prime,                                   % do comments
     {padToIndent},
     (
        check(0',) ->                         % comma after 0'!
        firePrime ;

        (check(0';) -> format(0';, Style) ;  [])
     )
  ).
format(0';, Style) -->
  {not(isHead(Style))},
  {lino(Lino)},
  fireBlank,                                  % in case of operators
  firePrime,
  disjSpace(Style, Lino).

format(neck, "::-", _) --> [].                % for BinProlog
format(neck, ":-", _) --> [].
format(neck, "-->", _) -->
  lookForNull.

wrapNest(Style, principal("nest", Arity, 0, nest)) -->
  check(Open),
  {nestPair(Open, Close)}, !,
  {depthInc},
  {linoColIndent(_, Col, Indent)},
  {setIndent(Col)},
  firePrime,                                  % Open
  (
     {existsHandler(Style, _)} ->             % handler exists
     {Style1 = [wrap(1)|Style]},              % so no handler here
     terms(Style1, Arity) ;

     {newHandler(Hid)},                       % no handler, so make one
     {Style1 = [handler(Hid, wrap, 1, 1, 0)|Style]},
     wrapHandler(Hid, Style1, Arity)
  ),
  check(Close),
  {depthDec},
  {padToIndent},
  {setIndent(Indent)},                        % Close
  firePrime.                                  % Must not prime before wrap

lexeme(Style, LContext, Rand, Type, Name, Prec) -->
  check(Initial),
  {prepareGoal(Initial, Style, Fnctr, TypeName, Goal)},
  Goal,
  prime0,
  check(Stop),
  {lextype(Stop, Fnctr, TypeName, LContext, Rand, Type1, Prec)},
  prime1,                                     % get black after Stop
  (
     {Type1 == negative},
     nmbr(Number, _) ->
     {Name = Number},                         % a negative number
     {Type = nmbr} ;

     {Name = TypeName},                       % not a negative number
     {Type = Type1}                           % may or may not be prefix
  ),
  {checkCol(Style, 75)},
  prime, !.

prepareGoal(Initial, Style, Fnctr, TypeName, Goal) :-
  class(Initial, Class),
  action(Class, Fnctr),                       % determine what sort
  functor(Goal, Fnctr, 2),
  arg(1, Goal, TypeName),
  arg(2, Goal, Style).

lextype(_, operator, "-", 0, 0, negative, 0) :- !. % must be prefix -
lextype(Stop, Fnctr, Name, LContext, Rand, Functor, Prec) :-
  (
     Name = [_|_] ->
     name(Nm, Name) ;                         % make Name atomic

     Nm = Name
  ),
  (

% Nm \= :-,
     Stop == 0'( ->                           % op with args is not an op
     Prec = 0,                                % call it a fnctr
     Functor = fnctr ;                        % term0 will notice and nest

     (                                        % no args
        current_op(Prec, _, Nm) ->            % an op
        Rand = 0,
        Functor = operator ;                  % call it an operator

        Rand = 1,
        Prec = 0,
        (                                     % not an op
           Fnctr == fnctr ->
           Functor = atom ;                   % call it an atom

           Functor = Fnctr                    % call it whatever
        )
     )
  ),
  depth(Depth),
  (isNeck(Name), Depth == 0 -> setIndent(tab) ;  true).

highestPrec(0, Highest, Highest, 0).          % set BreakPoint
highestPrec(Incumbent, Candidate, Highest, BreakPoint) :-
  Incumbent = principal(_, _, PrecIn, _),     % get PrecIn from Incumbent
  Candidate = principal(Name, Arity, Prec, _),
  (
     not isNeck(Name),                        % discount neck as operator
     Prec >= PrecIn ->                        % look for highest precedence
     Highest = Candidate,                     % this is highest or same
     (PrecIn >= 0 -> BreakPoint = 1 ;  BreakPoint = 0) ; % ray

     Highest = Incumbent,
     BreakPoint = 0
  ).

blankety :-                                   % fire blank if in range
  linoColIndent(_, Col, Indent),
  (Col < Indent + 1 ;  (Col > 78 ;  fireBlank)), !.

throwety(Style) :-                            % throw if you can
  (existsHandler(Style, Ball) -> throw(Ball) ;  true).

existsHandler(Style, tooLong(Hid, Handler, ArgIx, State)) :-
  member(handler(Hid, Handler, IndentIx, ArgIx, State), Style), % hndlr exists
  (IndentIx > 1 ;  not((Handler == wrap ;  Handler == write))). % valid 

checkCol(Style, MaxCol) :-
  col(Col),
  (Col > MaxCol -> throwety(Style) ;  true).

append([], X, X).
append([H|T], W, [H|Z]) :-
  append(T, W, Z).

member(X, [X|_]).
member(X, [_|Y]) :-
  member(X, Y).

/*
ftell(H, Pos) :-                      %  amzi5 has this, amzi4 does not
   fseek(H, 0, 1, Pos).

fseek(H, Pos, 0) :-
   fseek(H, Pos, 0, _).

flow(X):-
   put(0'>), write(X), nl.
flow(X) :-
   put(0'<), write(X), nl, 
   fail.

flow(X, Y, Y) :-
   put(0'>), write(X), nl.                      % threaded for dcg rules
flow(X, Y, Y) :-
   put(0'<), write(X), nl, 
   fail.
*/
