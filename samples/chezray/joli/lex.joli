%-*-Prolog-*-  
% lex indented on 3/19/2002 by 'JOLI' 1.0.

/* Philosophy.

Black chars are the program and therefore inviolate. White space is
perogative of JOLI. Apart from identifying lexemes, LEX suppresses
all white space and line feeds and also deals with comments, so that
the parser never sees these things.
However, user's line feeds cannot be simply discarded because they 
may adumbrate a comment line. Furthermore JOLI permits just one 
user-solicited blank line adjacent to a comment. The matter is
dealt with in the following way:

   After a comment LEX will insert lf0, which causes a new line but 
   does not indent.

   An acceptable user-solicited blank line produces a new line with
   Col set to a high number, so it appears that another new line is needed.

   When the parser issues a lf it will get one if Col exceeds Indent,
   but if Col is less than Indent it merely pads with spaces until Col
   is equal to Indent.
*/

/*
The DCG stream (the 'pipe') carries look-ahead chars between non-terminals.
Each non-terminal assumes at least one look-ahead char in the pipe,
and leaves one on exit.

'prime'   gets a char from source file and appends it to the pipe.
'fire'    puts one char from the pipe to the destination file.

'prime0'  returns every got char, and is used inside lexemes to find the 
          terminating char.
'prime1'  returns only black chars.
'prime'   is used between lexemes to find the next lexeme. 
          It calls prime1 and looks for lf (10) to check for comment lines
          or comment blocks. In fact it deals with all comments, whether 
          on a new line or not. 

'lf0'     performs a raw line feed.
'lf1'     does lf0 then indents,
'lf'      Col > Indent -> lf1; otherwise ensures Col == Indent+1.

The standard terminal syntax [ ... ] and -[ ... ] pops chars from
the pipe and appends chars onto the pipe, respectively.
*/
action(upper, variable).                      % action routine for each class
action(lower, fnctr).
action(rator, operator).
action(quote, quotor).
action(lbracket, aList).
action(digit, nmbr).
action(undef, undef).
action(punct, undef).
action(cut, undef).

inCol(Col) :-
  cntr_get(5, Col).

setInCol(Col) :-
  cntr_set(5, Col).

inColInc :-
  cntr_inc(5, _).

col(Col) :-
  cntr_get(2, Col).

setCol(Col) :-
  cntr_set(2, Col).

colInc(Col) :-
  cntr_inc(2, C),
  Col is C + 1.

colExceeds(X) :-
  col(Col),
  Col > X.

lino(Lino) :-
  cntr_get(1, Lino).

linoColIndent(Lino, Col, Indent) :-
  cntr_get(1, Lino),
  cntr_get(2, Col),
  cntr_get(4, Indent).

setLino(Lino) :-
  cntr_set(1, Lino).

linoInc :-
  cntr_inc(1, Lino).

depth(Depth) :-
  cntr_get(3, Depth).

setDepth(Depth) :-
  cntr_set(3, Depth).

depthInc :-
  cntr_inc(3, _).

depthDec :-
  cntr_dec(3, _).

indent(Indent) :-
  cntr_get(4, Indent).

setIndent(tab) :-
  tabSize(Tab),
  cntr_set(4, Tab).
setIndent(col) :-
  col(Col),
  Col1 is Col - 1,
  cntr_set(4, Col1).
setIndent(Indent) :-
  number(Indent),
  cntr_set(4, Indent).

tabIndent :-
  indent(Indent),
  tabSize(Tab),
  Indent1 is Indent + Tab,
  setIndent(Indent1).

untabIndent :-
  indent(Indent),
  tabSize(Tab),
  Indent1 is Indent - Tab,
  setIndent(Indent1).

stars :-
  cntr_get(8, Stars),
  Stars == 1.

resetStarFlag :-
  cntr_set(8, 0).

setStarFlag :-
  cntr_set(8, 1).

check(X, [X|Y], [X|Y]).                       % see next char

lf0 --> [],
  {lf0}.                                      % mandatory nl
lf0 :-                                    % ignore indent (for block comments)
  fire(10),
  setCol(1),                                  % 
  linoInc, !.

lf1 -->                                       % observe indent
  lf0,
  {padToIndent}, !.

lf2 --> [],
  {lf2}.
lf2 :-                                        % ensure at new line, no indent
  col(Col),
  (Col > 1 -> lf0 ;  true).

lf -->                                        % ensure at new line, indented
  {linoColIndent(_, Col, Indent)},
  (
     {Col - 1 > Indent} ->                    % past indent, so do it
     lf1 ;

     {padToIndent}                            % get to indent, no nl
  ), !.

rawIn(Char) :-
  hIn(H),
  get0(H, Ch),

% fread(H, Ch, 0), % ray
  (Ch == 13 -> get0(H, Char) ;  Char = Ch),
  (Char == 10 -> setInCol(0) ;  inColInc).    % line comment needs inCol

prime0([], [Char]) :-                         % get another one and pipe it.
  rawIn(Char).
prime0(X, X).                                 % use the one you have

prime1 -->
  prime0,
  [Char],                                     % extract a char
  (
     {class(Char, white)} ->
     prime1 ;                                 % skip white char

     - [Char]                                 % put back black char
  ), !.

prime -->
  prime1,                                     % get black char
  check(Char),
  (activeChar(Char) -> prime ;  []).          % do actives & recurse, or not

activeChar(manlf) -->                         % mandatory lf at my request
  [_],                                        % elide it
  lf0.
activeChar(10) -->
  newLine.
activeChar(0'%) -->
  lineComment(0).                             % ordinary line comment
activeChar(0'/) -->
  {hIn(Hin)},
  {stream_property(Hin, position(InPos))},    % remember InPos
  (
     "/",                                     % not a line comment 
     prime0,
     "*" ->                                   % a block comment
     - "/*",
     ({colExceeds(2)} -> - " " ;  []),
     blockComment ;

     {set_stream_position(Hin, InPos)},       % side effect endures
     {fail}
  ).
activeChar(lcom(Style, Size, Comment)) -->    % prime follows
  [_],                                        % elide lcom in pipe
  {fireComment(lcom(Style, Size, Comment))},
  lf0.                                        % new line after line comment

fireComment(lcom(anchored, Size, Comment)) :-
  hOut(H),
  lf2,                                        % ensure at col 1
  (
     Size < 79 ->                             % vanilla comment line
     write(H, Comment) ;

     (
        sub_string(Comment, _, 1, ` `) ->
        lastBlank(Comment, 79, Ix),           % chop at last blank
        Ix1 is Ix - 1,
        sub_string(Comment, 1, Ix1, First),
        write(H, First), nl(H),               % write 1st line
        linoInc,
        sub_string(Comment, Ix, _, Last),     % write tail
        write(H, `%%`), write(H, Last) ;      % mark with %%

        sub_string(Comment, 1, 79, Line),     % no blanks, so chop it
        write(H, Line)
     )
  ),
  resetLFs.
fireComment(lcom(free, Size, Comment)) :-     % not anchored
  commentCol(CCol),
  SCol is 80 - Size,                          % SCol to right justify
  min(CCol, SCol, StartCol),                  % start at StartCol
  col(Col),
  (Col >= SCol -> lf0 ;  true),               % lf for oversize comments 
  hOut(H),
  tab(H, 1), 
  Pad is StartCol - 2,                        % ??
  pad(Pad),
  write(H, Comment), 
  resetLFs.

lastBlank(S, Stop, Stop) :-                   % supports fireComments
  Stop < 3.
lastBlank(S, Stop, Ix) :-
  Stop > 2,
  sub_string(S, Stop, 1, C),
  (C == ` ` -> Ix = Stop ;  Stop1 is Stop - 1, lastBlank(S, Stop1, Ix)).

resetLFs :-
  cntr_set(7, 0).

wasBlankLine :-
  cntr_get(7, 2).

incLFs :-
  cntr_inc(7, _).

getLFs(X) :-
  cntr_get(7, X).

setLFs(X) :-
  cntr_set(7, X).

% Seen a users new line. Reset input col and black char count
% check if any widowed syntax ahead

newLine -->
  [10],                                       % discard user's new line
  prime0,                                     % char after lf 
  skiplfs,                                    % don't want any more
  prime1,
  (
     check(0'%) ->                            % line comment
     prime1,                                  % get black 
     lineComment(comLine) ;                   % let comment do lf

     (
        check(0'/) ->
        lf0,
        lf0,                                 % blank line before block comment
        lf2,
        blockComment ;                        % block comment

        (
           check(0'#) ->
           lf0,
           verbatimLine,                      % prep directive
           lf0 ;

           []                                 % no comment on new line
        )
     )
  ).

skiplfs -->                                   % skip nls
  prime0,
  ([10] -> skiplfs ;  []), !.                 % exit when not nl

verbatimLine -->
  check(C),
  fire,
  prime0,
  (
     [10] ->
     lf0,
     prime1,
     ((check(0'#) ;  {C == 0'\}) -> verbatimLine ;  []) ;

     verbatimLine
  ).

lookAhead(List, Char) -->              % succeed if next black Char is on List
% pushState(State),
  check(X),
  prime1,                                     % get black char
  check(Char),                                % look at it
% popState(State), !, % restore state
  {member(Char, List)}.                       % check if Char is on List

blockComment -->                              % empty pipe before repeat
  check(_), !,                                % fails when pipe empty
  fire,
  blockComment.
blockComment -->
  {lino(Lino)},
  blockAction,                                % get block body
  prime.

% skiplfs,
% {setWasCommentLine}. % do not anticipate indent
% Suprisingly tricky. Since action is io and flag setting, repeat can be used.

blockAction -->                               % 0'/0'* already seen, ignore
  {resetStarFlag},
  {repeat},                                 % pipe must be empty to read again
  prime0,                                     % get every char
  check(Char),
  fire,                                       % fire it
  {                                           % take special action
     (Char == 10 -> linoInc, resetStarFlag, fail ;  true),
     (Char == 0'* -> setStarFlag, fail ;  true),
     (Char == 0'/ -> stars ;  resetStarFlag, fail)
  },
  prime0.

prepdir -->                                   % skip prep directive
  check(C),
  (check(10) -> lf0 ;  ([0'\, 10] -> lf0 ;  firePrime0), prepdir).

/*
 * If widowed cuts, semics or periods follow line comments
 * they will be drawn forward to be before the comments
 * and a mandatory lf added.
 */

lineComment(ComLine) -->                      % just pipes the lexemes
  lC(Comments),                               % get list of comments
  prime1,
  (                                           % look ahead
     "," ->                                   % comma after comments
     prime1,
     (
        "!" ->                                % bang after comma
        prime1,
        [Char],                               % char after !
        - Comments,
        - [0',, 0'!, Char, manlf] ;

        [Char],                               % no bang, char after ,
        - Comments,
        - [0',, Char, manlf]
     ) ;

     (
        "!" ->                                % bang after comments
        prime1,
        [Char],                               % char after bang
        - Comments,
        - [0'!, Char, manlf] ;

        (
           ";" ->                             % semic after comments
           - Comments,
           - [0';, manlf] ;

           (
              "." ->                          % period after comments
              - Comments,
              - [0'., manlf] ;

              - Comments,
              (                               % none of those
                 {ComLine = comLine} ->
                 - [manlf, manlf] ;           % blank line before comment line

                 []
              )
           )
        )
     )
  ).

lC([lcom(Style, Size, ComStr)|Rest]) -->      % a block of line comments
  {(inCol(1) -> Style = anchored ;  Style = free)},
  lineCommentBody(Body), % read_string more efficient, but makes dirty strings
  {string_list(ComStr, Body)},
  {string_length(ComStr, Size)},
  prime1,                                     % black char after comment
  (
     [10] ->
     skiplfs ;                                % no more lfs

     []
  ),
  (
     {check(0'%)} ->                          % another comment
     lC(Rest) ;                               % recurse thru all comments 

     {Rest = []}                              % finish
  ).

lineCommentBody(Body) -->                     % get a line
  (
     [10] ->                                  % eol, consume it and return
     {Body = []} ;

     ([9] -> {Char = 0' } ;  [Char]),         % no tabs, can't measure them
     {Body = [Char|Rest]},
     ({Char == 32} -> prime1 ;  prime0),      % only 1 space
     lineCommentBody(Rest)
  ).

padToCommentCol(Slack) :-
  col(Col),
  commentCol(CCol),
  (
     Col < CCol ->
     Slack is CCol - Col,
     pad(CCol) ;

     Slack = 0,
     (Col < 80 -> fireBlank ;  true)
  ).

padToIndent :-
  indent(Indent),
  pad(Indent).

pad(Indent) :-
  col(Start),
  (
     Start < Indent ->                        % not there yet
     hOut(H),
     repeat,                                  % loop
     put(H, 0' ), 
     colInc(Col),
     Col >= Indent ;                          % got there

     true
  ).

/*
pad(Indent) :-
   hOut(H),
   col(Col),
   (for(I, Col, Indent, 1), put(H, 0' ), colInc, fail ;  true).
*/

fire -->
  [Char],                                     % get Char from pipe
  {fire(Char)}.

fire('end_of_file') :- !,
  throw(error(eof)).
fire([]) :- !.
fire([Char|Rest]) :-
  fire(Char),
  fire(Rest), !.
fire(10) :-                                   % no multiple blank lines
  (wasBlankLine ;  incLFs, colInc(_), hOut(H), nl(H)), !.
fire(Char) :-                                 % write it
  resetLFs,                                   % not lf
  colInc(_),
  hOut(H),
  put(H, Char).

firePrime0 -->
  fire,
  (check([_|_]) -> firePrimo ;  prime0), !.   % empty pipe first

firePrime1 -->
  fire,
  prime1, !.

firePrime -->
  fire,
  prime, !.

fireBlank :-                                  % conditional on not SOL
  hOut(H),
  put(H, ' '), 
  colInc(_), !.                               % write space
fireBlank -->
  {fireBlank}.

flushBuffer :-
  hOut(H),
  fflush(H).

nmbr(char, _) -->
  check(0''),
  firePrime0,
  firePrime1.
nmbr(Name, _) -->
  check(Digit),
  {class(Digit, digit)},
  firePrime0,
  (
     {Digit == 0'0},
     check(C),
     nmbr0(C, Name) ;

     {Name = [Digit|Rest]},
     decimal(Rest)
  ).

nmbr0(0'', [0'0, 0'', C]) --> !,
  firePrime0,
  check(C),
  firePrime1.
nmbr0(0'b, [0'0, 0'b|Rest]) --> !,
  firePrime0,
  bin(Rest).
nmbr0(0'x, [0'0, 0'x|Rest]) --> !,
  firePrime0,
  hex(Rest).

bin([Last|Rest]) -->
  check(Last),
  ({binClass(Last)}),
  firePrime0,
  bin(Rest).
bin([]) -->
  prime1.                                     % return

hex([Last|Rest]) -->
  check(Last),
  ({hexClass(Last)}),
  firePrime0,
  hex(Rest).
hex([]) -->
  prime1.                                     % return

decimal(Name) -->
  ".",                                        % we have a period
  prime0,
  check(LookAhead),
  - ".",                                      % now 2 in pipe
  (
     {class(LookAhead, digit)} ->
     firePrime0,                              % fire both
     {Name = [0'.|Rest]},
     decimal(Rest) ;

     {Name = []}                              % return with 2
  ).
decimal([Last|Rest]) -->
  check(Last),
  ({Last == 0'e ;  class(Last, digit)}),
  firePrime0,
  decimal(Rest).
decimal([]) -->
  prime1.                                     % return

fnctr([Last|Rest], _) -->                     % fire the functor name
  check(Last),
  {class(Last, Class)},
  {(lud(Class) ;  Last == 0'$)}, !,
  firePrime0,
  fnctr(Rest, _).
fnctr([], _) --> [].                          % lexeme wants very next char

variable([Last|Rest], _) -->
  check(Last),
  {class(Last, Class), lud(Class), !},
  firePrime0,
  variable(Rest, _).
variable([], _) -->
  prime1.

operator([Last|Rest], _) -->
  check(Last),
  ({class(Last, rator)} ;  {Last == 0'.}), !,
  firePrime0,
  operator(Rest, _).
operator([], _) --> [].                       % lexeme wants very next char

undef([Last], _) -->
  check(Last),
  firePrime0.

lud(upper).
lud(lower).
lud(digit).

quotor(Name, _) -->
  check(0''), !,
  quoted(Name).
quotor(noName, _) -->
  check(Quote),
  fire,
  {repeat},                                   % all side effects
  prime0,
  (
     [Quote] ->                               % probably exit quote
     prime0,                                  % look ahead
     (
        [Quote] ->
        {fire([Quote, Quote])},               % no, double quote, fire both
        {fail} ;                              % back to repeat

        [LookAhead],                          % remove LookAhead
        - [Quote],                            % insert first quote
        fire,                                 % fire exit quote
        - [LookAhead],                        % replace LookAhead
        prime1
     ) ;

     fire,
     {fail}                                   % if not quote, repeat 
  ).

quoted([0''|Rest]) -->
  check(0''),
  firePrime0,
  quotedBody(Rest),
  prime1.

quotedBody([Last|Rest]) -->
  check(Last),
  firePrime0,
  (
     {Last == 0''} ->
     ([0''] -> prime0, quotedBody(Rest) ;  {Rest = []}) ;

     quotedBody(Rest)
  ).

newHandler(Hid) :-                            % return a handler id
  cntr_inc(6, Hid).

pushState(State, P, P) :-
  State = state(Lino, Col, Dent, Depth, LFs, InCol, InPos, OutPos, P),
  hIn(Hin),
  stream_property(Hin, position(InPos)),
  getLFs(LFs),
  hOut(Hout),
  fflush(Hout),
  stream_property(Hout, position(OutPos)),
  inCol(InCol),
  linoColIndent(Lino, Col, Dent),
  depth(Depth).

popState(State, _, P) :-
  State = state(Lino, Col, Dent, Depth, LFs, InCol, InPos, OutPos, P),
  setInCol(InCol),
  setCol(Col),
  setLino(Lino),
  setIndent(Dent),
  setDepth(Depth),
  setLFs(LFs),
  hIn(Hin),
  set_stream_position(Hin, InPos),
  hOut(Hout),
  set_stream_position(Hout, OutPos), !.

aList(noName, Style) -->
  [0'[],
  {col(Col)},
  prime1,                                     % look ahead
  check(Char),
  - [0'[],                                    % restore [
  (
     {Char == 0']} ->                         % []
     fire,
     firePrime ;

     wrapNest(Style, principal("nest", Arity, _, _))
  ).

min(X, Y, X) :-
  X < Y, !.
min(_, Y, Y).
