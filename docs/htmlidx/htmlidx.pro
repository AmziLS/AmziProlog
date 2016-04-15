% HTMLIDX.PRO - A Program to generate an index and table of contents
%       for HTML files.
%       
% To use it, all anchors in the HTML source MUST have NO EMBEDDED SPACES
% or funny punctuation.
% 
% Create <!AMZI_INDEX  entries that have keywords, separated by ;,  which will be in the
% index, tied to the preceding anchors.

:- import(list).
:- import(misc).

main :-
  loadlsx('aosutils'),
  initialize,
  get_filenames,
  go.

%  catch(go, X, except(X)).

% Logic server start has files already asserted
% and calls initialize directly.

start :-
  catch(go, X, except(X)).

except(X) :-
  report(X).

%  These are the three extended predicates
%    defined in the C++ front-end, so comment
%    these lines out when building for embedded
%    use.


rnl :- report($\n$).
report(X) :- write(X).
progress(FileName, I, N) :-
  write(FileName), tab(2), write(I/N), nl.


initialize :-
  set_mode(string_esc, off),
  (delfile('temp1.pro', _) ; true ),
  (delfile('temp2.pro', _) ; true ),
  fopen(Htemp1, 'temp1.pro', w),
  assert(tempfile(Htemp1)),
  retractall(anchor(_,_,_)),
  retractall(file_title(_,_)),
  retractall(input_file(_,_)),
  retractall(number_of_files(_)),
  retractall(heading_level(_)),
  get_go_words,
  get_stop_words.

get_filenames :-
  reconsult(fig),
  indexfile(IFILE),
  (delfile(IFILE, _); true),
  tocfile(TFILE),
  (delfile(TFILE, _); true),
  files(FL),  % no need to hard wire these?
  %get_html_files(FL),
  !,
  setup_filenames(FL).
get_filenames :-
  write($Enter filenames (CR to end):\n> $),
  read_string(S),
  get_fns(S, FNLs),
  flatten(FNLs, FNs),
  setup_filenames(FNs), !,
  assert(indexfile($index.htm$)),
  assert(tocfile($toc.htm$)).

% nope, need to use files/1 in fig.pro in order
% to get them in the right order for the table of
% contents.  other option is to number them and
% use this.  

get_html_files(FL) :-
  findall(N,
     findfiles('*.htm', 0, fileinfo(N,_,_,_,_)),
     FL).

get_fns($$, []).
get_fns(S, [AFlist|Z]) :-
  string_atom(S,A),
  getallfiles(A, AFlist),
  write($> $),
  read_string(SN),
  get_fns(SN, Z).

getallfiles(Pat, AFlist) :-
  findall(Name,
          findfiles(Pat, 0, fileinfo(Name,_,_,_,_)),
          AFlist).

setup_filenames(FNs) :-
  length(FNs, L),
  assert(number_of_files(L)),
  assert_files(FNs, 1).

assert_files([], _).
assert_files([H|T], N) :-
  assert(input_file(H,N)),
  NN is N + 1,
  assert_files(T,NN).

go :-
  go_files,
  finis.

finis :-
  report($Building index...$), rnl,
  tempfile(H1),
  fclose(H1),
  build_toc,
  build_index,
  build_xml,
  report($Done$), rnl.

ini_test :-
  pro_trail(X,Y),
  string_term(S, pro_trail(X,Y)),
  report(S), rnl.

go_files :-
  number_of_files(N),
  input_file(FileName, I),
  progress(FileName, I, N),
%  rnl, report($Working on file $),
%  report(FileName), report($  $),
%  report(I/N), rnl,
  once do_file(FileName),
%  catch(do_file(FileName), Error, throw(htmlidxerror(FileName, Error)) ),
  fail.
go_files.

/* The file is read in as a list of strings, which are then
    treated by the DCG routines as a list of tokens, using
    an override of dcg_terminal (see further down in code).
    This is why the call to parse three has both a line list and
    token list in its initial call.
    */

do_file(FileName) :-
  re_initialize,
  assert(filename(FileName)),
  report($Reading file...$), rnl,
  get_file(FileName, Lines),
  report($Parsing file...$), rnl,
  ( parse([]:Lines, []:[]); throw(idxerror($Parse failed$)) ),
  report($Outputing entries...$), rnl,
  ( output_idx; throw(idxerror($output_idx failed$)) ),
  !.

re_initialize :-
  retractall(current_anchor(_)),
  retractall(idx(_,_,_)),
%  retractall(stop_word(_)),
  retractall(filename(_)),
write($pre off$), nl,
  set_flag(pre, off).

/*
get_stop_words :-
  file_exists('stopword.txt'),
  !,
  fopen(H, 'stopword.txt', r),
  repeat,
  read_string(H, S),
  (S == end_of_file -> true;
      string_atom(S, A),
      assert(stop_word(A)),
      fail ).
*/
get_stop_words.

/*
get_go_words :-
  file_exists('goword.txt'),
  !,
  fopen(H, 'goword.txt', r),
  repeat,
  read_string(H, S),
  (S == end_of_file -> true;
      string_atom(S, A),
      assert(go_word(A)),
      fail ).
*/
get_go_words.

/* The parsing does it's job via side effects.
   When a tag being parsed requires the generation
   of an index entry, it makes the entry in the dynamic
   database, which will get written to a temporary file. */

parse --> ['<'], gettag, !, parse.
parse --> word, !, parse.
parse --> [end_of_html_file].

gettag --> heading.
gettag --> anchor_tag.
gettag --> xindex_entry.
gettag --> start_pre.
gettag --> stop_pre.
gettag --> title.
gettag --> tag(_,_).

word --> [W],
  { W \= end_of_html_file },
  { (ok_word(W) -> index_entry(W, n); true) }.

skipword --> [_].

start_pre -->
  ['PRE', '>'],
  { set_flag(pre, on) }.

stop_pre -->
  ['/', 'PRE', '>'],
  { set_flag(pre, off) }.

heading -->
  [H, '>'],
  { get_heading_level(H, L),
    retractall(heading_level(_)),
    assert(heading_level(L)) }.

get_heading_level('H', 1).
get_heading_level(h, 1).
get_heading_level(HL, L) :-
  atom_codes(HL, [Hc, Lc]),
  (Hc = 0'H; Hc = 0'h),
  L is Lc - 0'0.

heading_text(T) --> read_to_H(T1), { deangle(T1, T, outside) }.

anchor_tag -->
  a_name(A),
  a_description([W|Z]),
  { report(anchor(A,[W|Z])), rnl },
  { set_anchor(A, [W|Z] ),
    %add_boldindex(Z, W, b),
    %add_toc_entry(Z, W) }.
    add_toc_entry([W|Z]) }.

a_name(A) -->
  ['A', 'NAME', '=', '"'],
  tag_words(W), ['"', '>'],
  { check_atomic(W, A) }.
a_name(A) -->
  ['a', 'name', '=', '"'],
  tag_words(W), ['"', '>'],
  { check_atomic(W, A) }.
a_name(A) -->
  ['a', 'NAME', '=', '"'],
  tag_words(W), ['"', '>'],
  { check_atomic(W, A) }.
a_name(A) -->
  ['A', 'name', '=', '"'],
  tag_words(W), ['"', '>'],
  { check_atomic(W, A) }.


check_atomic([A], A) :- !.
check_atomic(W, _) :-
   throw(non_atomic_anchor(W)).

tag_words([W|Ws]) --> [W], { W \= '"' }, tag_words(Ws).
tag_words([]) --> [].

/* Get the anchor description, up to <\a> or <\h> and
   then remove any embedded angle brackets in the description. */

a_description(D) --> a_descript(D1), { deangle(D1, D, outside) }.

deangle([], [], _) :- !.
deangle(['<'|Xs], Ys, _) :-
  !, deangle(Xs, Ys, inside).
deangle(['>'|Xs], Ys, _) :-
  !, deangle(Xs, Ys, outside).
deangle([X|Xs], [X|Ys], outside) :-
  !, deangle(Xs, Ys, outside).
deangle([_|Xs], Ys, inside) :-
  deangle(Xs, Ys, inside).

/* Anchor description, but might be empty, meaning we go
   to a heading endtag instead. */

a_descript(D) --> read_to_A(X), {X \= [], D = X}.
a_descript(D) --> read_to_H(D).

read_to_A([]) -->
  endtag('A'),  !.
read_to_A([]) -->
  endtag('a'),  !.
read_to_A([BOZON|Ds]) -->
   ['&',BOZ,';'], {is_boz(BOZ,BOZON)}, read_to_A(Ds).
read_to_A([D|Ds]) --> [D], read_to_A(Ds).

read_to_H([]) -->
  endtag(TAG),
  { atom_codes(TAG, [H|_]),
    (H = 0'H; H = 0'h) },
  %{ member(TAG, ['H', 'H1', 'H2', 'H3', 'H4', 'H5']) },
  !.
read_to_H(Ds) -->
  endtag('A'), read_to_H(Ds).
read_to_H([BOZON|Ds]) -->
   ['&',BOZ,';'], {is_boz(BOZ,BOZON)}, read_to_H(Ds).
read_to_H([D|Ds]) --> [D], read_to_H(Ds).

/* Fix index entry so it accepts semicolon delimited
   list of things to be indexed, (no quotes around
   them). */
/* Turn off title for index entries as well, maybe. */
/* Index entries are special comments that look like:
   <!AMZI_INDEX = words one; words two; etc> */

test_xindex :-
  Lines = [$!AMZI_INDEX = frogs; toads>$],
  xindex_entry([]:Lines, []:[]).

xindex_entry -->
  ['!', 'AMZI_INDEX', '='],
%  { compare_nocase(AMZI_INDEX, amzi_index) },
  aindex_entries.
xindex_entry -->
  ['!', 'amzi_index', '='],
%  { compare_nocase(AMZI_INDEX, amzi_index) },
  aindex_entries.

aindex_entries -->
  aind_entry([W|Z]),
  %{ add_boldindex(Z, W, b) },
  { add_boldindex([W|Z]) },
  next_aind.

next_aind --> [';'], aindex_entries.
next_aind --> ['>'].

aind_entry([BOZON|Ws] --> ['&',BOZ,';'], {is_boz(BOZ,BOZON)}, aind_entry(Ws).
aind_entry(Ws) --> [''''], aind_entry(Ws).   %ignore single quotes
aind_entry([W|Ws]) --> [W], { W \= '>', W \= ';' }, aind_entry(Ws).
aind_entry([]) --> [].

% string_tokens takes the ; off, so we're left with this
% part, note ambiguous use of ; in index entries.

is_boz('lt', '&lt;').
is_boz('gt', '&gt;').
is_boz('amp', '&amp;').
is_boz('quot', '&quot;').
is_boz('nbsp', ' ').
is_boz(';', ';').   % we use semicolon, so catch the unique ;/2 index.

title -->
  ['TITLE', '>'],
  title_words([W|Z]),
  ['<', '/', 'TITLE', '>'],
  %{ add_title(Z, W) }.
  { add_title([W|Z]) }.
title -->
  ['title', '>'],
  title_words([W|Z]),
  ['<', '/', 'title', '>'],
  %{ add_title(Z, W) }.
  { add_title([W|Z]) }.

title_words([X|Ys]) -->
  [X], { X \= '<' }, title_words(Ys).
title_words([]) --> [].

add_title(LL) :-
  insert_spaces(LL, SL),
  atomlist_concat(SL, ATOM_TITLE),
  filename(F),
  assert(file_title(F, ATOM_TITLE)).

add_boldindex(L) :-
  insert_spaces(L, SL),
  atomlist_concat(SL, A),
  index_entry(A, b).

insert_spaces([], []) :- !.
insert_spaces([A], [A]) :- !.
insert_spaces([A,B|Z1], [A,B2|Z2]) :-
   is_symbol(A),
   is_symbol(B),
   !, insert_spaces([B|Z1], [B2|Z2]).
insert_spaces([A,N|Z1], [A,N2|Z2]) :-   % leaders
   member(A, ['/', '-', '.', ' ', ':']),
   !, insert_spaces([N|Z1], [N2|Z2]).
insert_spaces([A,B|Z1], [A,B2|Z2]) :-   % trailers
   member(B, ['!', ';', ',', '?', '/', '.', '-', '. ', ' ', ':']),
   !, insert_spaces([B|Z1], [B2|Z2]).
insert_spaces(['. ',B|Z1], ['.',' ',B2|Z2]) :-
   !, insert_spaces([B|Z1], [B2|Z2]).
insert_spaces([A,B|Z1], [A,' ',B2|Z2]) :-
   insert_spaces([B|Z1], [B2|Z2]).

is_symbol(A) :-
  member(A, ['&gt;', '&lt;', '&', '*', '+', '-', '.', '/', '\\',
             ':', '<', '>', '=', '?', '@', '^', '~']).

is_digit(D) :-
  member(D, ['0', '1', '2', '3', '4',
             '5', '6', '7', '8', '9']).

add_toc_entry(LL) :-
  insert_spaces(LL, SL),
  atomlist_concat(SL, A),
  filename(F),
  heading_level(L),
  get_anchor(Anchor),
  (file_title(F, Title) ; string_atom(F, Title) ),
  !, assert(toc(F, Title, L, A, Anchor)).
  
tag(X,A) --> [X], attribs(A), ['>'].

endtag(X) --> ['<', '/', X, '>'].

attribs([X|Ys]) -->
  [X], { X \= '>' }, attribs(Ys).
attribs([]) --> [].

% filter words

ok_word(_) :- !, fail.  % turn of words for now
ok_word(W) :-
  get_flag(pre, off),
  atom_uplow(W, Wlow),
  ok_lowword(Wlow).

ok_lowword(W) :- stop_word(W), !, fail.
ok_lowword(W) :- go_word(W).
ok_lowword(W) :-
  atom_codes(W,[InitChar|Z]), 
  (  InitChar >= 0'a, InitChar =< 0'z ).

% Process the things that need processing.

set_anchor(A, D) :-
  insert_spaces(D, DS),
  atomlist_concat(DS, DA),
  filename(F),
  assert(anchor(A, F, DA)),
  retractall(current_anchor(_)),
  assert(current_anchor(A)).

get_anchor(A) :-
  current_anchor(A), !.
get_anchor(none).

index_entry(W, TA) :-
  get_anchor(A),
  ( idx(W,A,_) -> true; assert(idx(W,A,TA)) ).

output_idx :-
  tempfile(H),
  idx(W,A,TA),
  filename(F),
  (needs_quote(W) ->
     writeq(H, idxf(W,A,TA,F)),
     write(H, $.\n$)
    ;
     write(H, $idxf('$ ),
     write(H, W), write(H, $',$ ),
     writeq(H, A), write(H, $,$),
     writeq(H, TA), write(H, $,$),
     writeq(H, F),
     write(H, $).\n$ ) ),
  fail.
output_idx :-
  report($output_idx done$), rnl, rnl.

needs_quote(A) :-
  string_termq(S, A),
  string_length(S, SL),
  atom_length(A, AL),
  SL > AL.

% Build XML toc

build_xml :-
  write($\n\nBuilding XML File...$),
  xmlfile(TF),
  xmldocdir(Dir),
  write(TF), nl,
  tell(TF),

  bookname(BName),
  write($<toc label="$),
  write(BName),
  write($">\n$),

  retractall(last_chapter(_)),
  assert(last_level(0)),
  toc(FileName, Chapter, Level, Heading, Anchor),
  retract(last_level(N)),
  (Level =< N ->
      write_endtopics(N, Level)
      ;
      true ),
  Diff is (Level - N),
  (Diff > 1 ->
      Skip is Level - 1,
      asserta(level_skip(Skip))
  ;   true ),
  assert(last_level(Level)),

  tab(Level), 
  write($<topic label="$),
  write(Heading),
  write($" href="$),
  write(Dir),
  write(FileName),
  (xml_file_done(FileName) ->
    write($#$),
    write(Anchor)
  ;
    assert(xml_file_done(FileName))
  ),
  write($">\n$),
  fail.
build_xml :-
  retract(last_level(N)),
  write_endtopics(N, 1),

  xmldocdir(Dir),
  tocfile(ContentsFile),
  xmlindex(IndexFile),
  write(`<topic label="Index" href="`),
  write(Dir),
  write(IndexFile),
  write(`">\n</topic>\n`),
  
  write(`</toc>\n`),
  told.

  write_endtopics(StartLevel, EndLevel) :-
    for(X, StartLevel, EndLevel, -1),
      once(write_endtag(X)),
      fail.
  write_endtopics(_, _) :- !.
  write_endtag(X) :-
     level_skip(Top),
     Top = X, !,
     retract(level_skip(Top)).
  write_endtag(X) :-
     tab(X),
     write($</topic>\n$), !.

% Builds the table of contents

build_toc :-
  tocfile(TF),
  tell(TF),
  write($<HTML>\n$),
  tocbody(Body),
  write(Body),
  write($\n\n<P>\n<B>$),
  titlecolor(Color),
  write($<FONT COLOR="$),
  write(Color),
  write($">$),
  bookname(BName),
  write(BName),
  write($</FONT></B>\n$),
  write($<P><B>$),
  write($<FONT COLOR="$),
  write(Color),
  write($">$),
  write($Contents </FONT></B><A HREF="$),
  indexfile(IDXName),
  write(IDXName),
  write($"><B>Index</B></A><P>\n\n<PRE>$),

  retractall(last_chapter(_)),
  assert(last_chapter(zzz)),
  toc(FileName, Chapter, Level, Heading, Anchor),
  (last_chapter(Chapter) ->
      true
      ;
      retract(last_chapter(_)),
      assert(last_chapter(Chapter)),
      nl, write(Chapter), nl ),

  tab(Level), 
  write($<A HREF="$),
  write(FileName),
  write($#$),
  write(Anchor),
  write($" TARGET="content">$),
  write(Heading),
  write($</A>\n$),
  fail.
build_toc :-
  write($</PRE>\n$),
%  write($<P><I><FONT SIZE=-2>Copyright &copy;1992-2000 Amzi! inc</FONT></I>\n$),
  write($</BODY>\n</HTML>$),
  told.
  
% Builds an index from the dynamic database

build_index :-
  system($sort < temp1.pro > temp2.pro$),
  reconsult(temp2),
  indexfile(IF),
  xmlindex(XIF),
  fopen(H, IF, w),
  fopen(HX, XIF, w),
  write(H, $<HTML>\n$),
  write(HX, $<HTML>\n$),
  indexbody(Body),
  write(H, Body),
  write(HX, Body),
  titlecolor(Color),
  write(H, $\n\n<P>\n<B><FONT COLOR=$),
  write(H, Color),
  write(H, $>$),
  write(HX, $\n\n<P>\n<B><FONT COLOR=$),
  write(HX, Color),
  write(HX, $>$),
  bookname(BName),
  
  
  write(H, BName),
  write(HX, BName),
  write(H, $</FONT></B>\n$),     %<P>$),
  
  write(H, $<P><B>$),
  write(H, $<FONT COLOR="$),
  write(H, Color),
  write(H, $">$),
  write(H, $Index </FONT></B><A HREF="$),
  
% changed/added these lines to put in title 'index'

  write(HX, $</FONT></B>\n$),     %<P>$),
  
  write(HX, $<P><B>$),
  write(HX, $<FONT COLOR="$),
  write(HX, Color),
  write(HX, $">$),
  write(HX, $Index </FONT></B><A HREF="$),
  
  % write(H, $<A HREF="$),
  tocfile(TOCName),
  write(H, TOCName),
  write(H, $"><B>Contents</B></A><P>\n\n$),
  write(HX, $</FONT></B><P>\n\n$),
  write(H, $<A HREF="#A">A</A> <A HREF="#B">B</A> <A HREF="#C">C</A> <A HREF="#D">D</A> $),
  write(H, $<A HREF="#E">E</A> <A HREF="#F">F</A> <A HREF="#G">G</A> <A HREF="#H">H</A> $),
  write(H, $<A HREF="#I">I</A> <A HREF="#J">J</A> <A HREF="#K">K</A> <A HREF="#L">L</A> $),
  write(H, $<A HREF="#M">M</A> <A HREF="#N">N</A> <A HREF="#O">O</A> <A HREF="#P">P</A> $),
  write(H, $<A HREF="#Q">Q</A> <A HREF="#R">R</A> <A HREF="#S">S</A> <A HREF="#T">T</A> $),
  write(H, $<A HREF="#U">U</A> <A HREF="#V">V</A> <A HREF="#W">W</A> <A HREF="#X">X</A> $),
  write(H, $<A HREF="#Y">Y</A> <A HREF="#Z">Z</A><P>$),
  write(HX, $<A HREF="#A">A</A> <A HREF="#B">B</A> <A HREF="#C">C</A> <A HREF="#D">D</A> $),
  write(HX, $<A HREF="#E">E</A> <A HREF="#F">F</A> <A HREF="#G">G</A> <A HREF="#H">H</A> $),
  write(HX, $<A HREF="#I">I</A> <A HREF="#J">J</A> <A HREF="#K">K</A> <A HREF="#L">L</A> $),
  write(HX, $<A HREF="#M">M</A> <A HREF="#N">N</A> <A HREF="#O">O</A> <A HREF="#P">P</A> $),
  write(HX, $<A HREF="#Q">Q</A> <A HREF="#R">R</A> <A HREF="#S">S</A> <A HREF="#T">T</A> $),
  write(HX, $<A HREF="#U">U</A> <A HREF="#V">V</A> <A HREF="#W">W</A> <A HREF="#X">X</A> $),
  write(HX, $<A HREF="#Y">Y</A> <A HREF="#Z">Z</A><P>$),
  % write(H, $<DL><DT><DD>\n$),
  write(H, $<PRE>\n$),
  write(HX, $<PRE>\n$),
  output_index(H, HX),
  write(H, $</PRE>\n$),
  write(HX, $</PRE>\n$),
%  write($<P><I><FONT SIZE=-2>Copyright &copy;1992-2000 Amzi! inc</FONT></I>\n$),
  write(H, $</BODY>\n$),
  write(H, $</HTML>\n$),
  write(HX, $</BODY>\n$),
  write(HX, $</HTML>\n$),
  fclose(H),
  fclose(HX),
  !.
build_index :-
  throw(app_error($build_index failed$)).

% Main loop for writing the HTML index file

output_index(H, HX) :-
  retractall(current_letter(_)),
  assert(current_letter(z)),

  % Find all the words (already sorted)
  findall(W, idxf(W,_,_,_), WL1),

  % Remove all duplicate words
  dedup(WL1, WordList),
  out_index(H, HX, WordList).

% Walk word list outputting entries

out_index(_, _, []).
out_index(H, HX, [W|Z]) :-
  % Find all the index entries for a particular word
  findall(target(A,TA,F), idxf(W,A,TA,F), TargetList),

  % Output all the entries for that word
  out_target(H, HX, W, TargetList),
  out_index(H, HX, Z).

% There is only one entry for the word, so make the entry
% the word

out_target(H, HX, W, [target(A,TA,F)]) :-
  firstchar(W, FC),
  report(FC),
  set_current_letter(H, HX, FC),
  write(H, $<A HREF="$),
  write(H, F),
  write(HX, $<A HREF="$),
  write(HX, F),
  (A == none -> true
    ; write(H, $#$),
      write(H, A),
      write(HX, $#$),
      write(HX, A)
    ),
  write(H, $" TARGET="content">$),
  write(HX, $">$),
  write(H, W),
  write(H, $</A>\n$),
  write(HX, W),
  write(HX, $</A>\n$).

% For multiple entries put out the word then the list
% of entries

out_target(H, HX, W, TargetList) :-
  firstchar(W, FC),
  report(FC),
  set_current_letter(H, HX, FC),
  write(H, $<B>$),
  write(H, W),
  write(H, $</B>\n$),
  write(HX, $<B>$),
  write(HX, W),
  write(HX, $</B>\n$),
  out_targs(H, HX, W, TargetList).

% Outputs an indented list of entries

out_targs(H, HX, W, []).
out_targs(H, HX, W, [target(A,TA,F)|Z]) :-
  write(H, $  $),
  write(H, $<A HREF="$),
  write(H, F),
  write(HX, $  $),
  write(HX, $<A HREF="$),
  write(HX, F),
  (A == none -> true
    ; write(H, $#$),
      write(H, A),
      write(HX, $#$),
      write(HX, A)
    ),
  write(H, $" TARGET="content">$),
  write(HX, $">$),
  (file_title(F, Title) ; string_atom(F, Title) ),
  write(H, Title),
  write(HX, Title),
  (anchor(A, F, D) ->
%     (compare_atom_list(W, D) ; compare_atom_list(Title, D) -> true ;
     (W = D ; (Title = D) -> true ;
        write(H, $: $),
        write(HX, $: $),
%        write_anchor_desc(H, D) )
        write(H, D),
        write(HX, D) 
        )
   ; true),
  write(H, $</A>\n$),
  write(HX, $</A>\n$),
  out_targs(H, HX, W, Z).

% Remove dups from a sorted list

dedup([], []).
dedup([X,X|Y], Z) :-
  !, dedup([X|Y], Z).
%dedup([W,X|Y], Z) :-
%  compare_nocase(W, X),
%  !, dedup([X|Y], Z).
dedup([X|Y], [X|Z]) :-
  dedup(Y, Z).

% Compare an atom with a string token list

compare_atom_list(W, D) :-
  string_atom(WStr, W),
  string_tokens(WStr, TList),
  TList = D.

firstchar(W,Cup) :-
  sub_atom(W,1,1,C),
  atom_uplow(Cupper,C),
  ( Cupper @>= 'A', Cupper @=< 'Z', Cup = Cupper;
    Cup = ' ' ),
  !.

% Writes out the index letter

set_current_letter(_, _, L) :-
  current_letter(L), !.
set_current_letter(H, HX, L) :-
  retract(current_letter(_)),
  assert(current_letter(L)),
  rnl, report(L), report($ $),
  write(H, $\n\n<B>$),
  write(H, $<A NAME="$),
  write(H, L),
  titlecolor(Color),
  write(H, $"></A>  <FONT COLOR=$),
  write(H, Color),
  write(H, $ SIZE=4>$),
  write(H, L),
  write(H, $</FONT></B><P>\n$),
  write(HX, $\n\n<B>$),
  write(HX, $<A NAME="$),
  write(HX, L),
  write(HX, $"></A>  <FONT COLOR=yellow SIZE=4>$),
  write(HX, L),
  write(HX, $</FONT></B><P>\n$), !.

/* Extra DCG support - The file is read into memory as a
    list of strings.  By supplying our own version of dcg_terminal
    we can make it look to the DCG routines as if it were a
    list of tokens.  (Ray's extension) */

dcg_terminal(Ts, Ts:[], []:[]) :- !.
dcg_terminal(Ts, T0:Lines, T1:Lines) :-
  length_lte(Ts, T0),
  !, append(Ts, T1, T0).
dcg_terminal(Ts, T0:[Line|Lines], T1:Ls) :-
  string_tokens(Line, T, $<>/"=!;&%*+-.\\:?@^~$),
% nl, write(Line), nl,
  append(T0, T, T00),
  dcg_terminal(Ts, T00:Lines, T1:Ls).

get_file(FileName, Lines) :-
  fopen(Hin, FileName, r),
  read_lines(Hin, Lines),
  fclose(Hin).

read_lines(Hin, [S|Z]) :-
  read_string(Hin, S),
  S \= end_of_file,
  !, read_lines(Hin, Z).
read_lines(_, [$end_of_html_file$]).
