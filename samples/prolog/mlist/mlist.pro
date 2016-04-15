%----------------------------------------------------------------------
% MLIST.PRO - parse mailing list information
%
% MLIST reads mailing list labels and extracts various
% fields from each label, such as first name, last name,
% organization, city, state, zip, country, etc.
% This version was used for some of our direct mail
% and analyzes label formats from either Miller Freeman
% publications, such as AIExpert or Dr. Dobbs, and PCAI
% magazine.
%
% In the case of Miller Freeman lists, all the letters
% are upper case.  The program converts these to a correct
% mix of upper and lower case.
%
% For any list, the program creates an error file, SCAN.ERR,
% that contains labels that the program could not make
% sense of.
%
% main/0 is the entry point when the program is used as
% a stand-alone application, reading an input file of label
% information and outputing the parsed fields.  It takes
% as input the name of the label file to process, and
% the type of list, whether Miller Freeman, 'mf', or
% PCAI, 'pcai'.  The output from main/0 is a file, SCAN.LOG.
% that contains the fields.
%
% Run the program with the input file SCAN.INP and type
% mf to see how it works.
%

% list utilities

append([], X, X).
append([A|X], Y, [A|Z]) :- append(X,Y,Z).

member(A, [A|_]).
member(A, [_|Z]) :- member(A, Z).

reverse(A, Z) :- reverse(A, [], Z).

   reverse([], Z, Z).
   reverse([A|X], SoFar, Z) :- reverse(X, [A|SoFar], Z).

% main

main :-
  write($File to process: $),
  read_string(FName),
  write($File type [mf, pcai]: $),
  read_string(FType),
  string_atom(FType, FTa),
  ml_init(FTa, FName),
  repeat,
  ml_getrecord(R),
  R = eof,
  ml_close.

% The three predicates beginning with ml_ are designed to
% provide an interface to other applications that use this
% Prolog program as a service.  For the application it
% was designed for, the Prolog module is called from an
% Access DB application that reads the mailing list
% labels and saves the parsed information in the DB.
%
% ml_init(Type, Fin) - Type is mf or pcai, for the list
%    type, and Fin is the input file.  Called once at
%    beginning of run.
% ml_close() - Called once at end of run, to close the
%    files.
% ml_getrecord(Record) - called in a loop until the
%    end-of-file is read.  Record is a Prolog structure
%    with the database fields derived from the address
%    label.

ml_init(Type, Fin) :-
  retractall(ftype(_)),
  retractall(files(_,_,_)),
  assert(ftype(Type)),
  fopen(Hin, Fin, r),
  fopen(Herr, $scan.err$, w),
  fopen(Hlog, $scan.log$, w),
  assert(files(Hin, Hlog, Herr)).

ml_close :-
  files(Hin, Hlog, Herr),
  fclose(Hin),
  fclose(Hlog),
  fclose(Herr).

ml_getrecord(Record) :-
  files(Hin, Hlog, _),
  get_parse(Hin, ParseList),
  list_record(ParseList, Record),
  write_record(Hlog, Record),
  !.

% get_parse/2 reads a block of text that is a label,
% and converts it to a list of fields.  The list
% fields is of the form
%   [last('Campbell'), first(['Peter', 'L.']),
%    title(['Director', 'MIS']), ... region('UT')..]

get_parse(Hin, ParseList) :-
  read_block(Hin, TB), !,
  process_block(TB, ParseList).

% write_record/2 is used by the stand-alone version of the
% program.  It writes the fields of the record to the
% file SCAN.OUT.  This step could be kept in an embedded
% version for diagnostics, but in general the fields in
% the record would be entered in a database.

write_record(H, eof) :- !.
write_record(H, record(L,F,T,O,A1,A2,C,R,P,Ct,Tel,Fax)) :-
  write(adding:L), nl,
  write(H, last:L), nl(H),
  write(H, first:F), nl(H),
  write(H, title:T), nl(H),
  write(H, org:O), nl(H),
  write(H, addr1:A1), nl(H),
  write(H, addr2:A2), nl(H),
  write(H, city:C), nl(H),
  write(H, region:R), nl(H),
  write(H, post:P), nl(H),
  write(H, country:Ct), nl(H),
  write(H, tel:Tel), nl(H),
  write(H, fax:Fax), nl(H),
  nl(H).

% get_parse/2, described above, derives a list of fields
% from the input label information.  The list of fields
% is not that useful a format, although easy to build, so
% this code takes those lists and converts them into
% a record format with a fixed number of fields, where
% each field is a simple string, easily digested by a
% database application.

list_record(eof, eof).
list_record(L, record(Last, First, Title,
    Org, Addr1, Addr2, City, Region, Post, Country, Tel, Fax)) :-
  get_field(L, last, Last),
  get_field(L, first, First),
  get_field(L, title, Title),
  get_field(L, org, Org),
  get_field(L, addr1, Addr1),
  get_field(L, addr2, Addr2),
  get_field(L, city, City),
  get_field(L, region, Region),
  get_field(L, post, Post),
  get_field(L, country, Country),
  get_field(L, tel, Tel),
  get_field(L, fax, Fax).

get_field(L, F, Vs) :-
  X =.. [F,Vt],
  member(X, L),
  !,
  field_string(Vt, Vs).
get_field(L, _, $$).
  
field_string(L, Vs) :-
  list(L),
  flatten(L, [H|Lflat]),
  string_atom(S, H),
  atomlist_string(Lflat, S, Vs).
field_string(F, Vs) :-
  string_term(Vs, F).

atomlist_string([], S, S).
atomlist_string([H|T], SoFar, Ans) :-
  string_atom(S, H),
  strcat(SoFar, $ $, X),
  strcat(X, S, Next),
  atomlist_string(T, Next, Ans).

% process_block/2 is a simple front end on parse_block
% that deals with the cases of end-of-file and un-parsed
% labels, as well as ones that successfully parse.

process_block(eof, eof).
process_block(TB, Parse) :-
  parse_block(TB, Parse),
  !.
process_block(TB, _) :-
  files(_, _, Herr),
  write(Herr, $\n***** Failed to Parse:\n$),
  write_block(Herr, TB),
  !, fail.

% read_block
%   A text block is a list of strings, where each string is
%   a line from the input file.  Return 'eof' if end of file,
%   also skip over multiple blank lines between blocks
%   of text.

read_block(H, TB) :-
  read_string(H, FirstS),
  process_first_line(H, FirstS, TB),
  !,
  files(_, Hlog, _),
  write_block(Hlog, TB).

process_first_line(_, 'end_of_file', eof) :- !.
process_first_line(H, FirstS, TB) :-
  nonblank_string(FirstS),
  read_lines(H, FirstS, [], TB).
process_first_line(H, _, TB) :-
  read_string(H, FirstS),
  process_first_line(H, FirstS, TB).

% read_lines
%  Read input lines as strings until either an empty line
%  or the end of file is reached.  The list of input lines
%  is constructed by adding each new line to the head
%  of the forming list, so the list is in reverse order,
%  which is what we want because the parsing is going
%  to proceed from the last item, looking for address,
%  to the first.

read_lines(_, 'end_of_file', TB, TB).
read_lines(H, S, SoFar, TB) :-
  nonblank_string(S),
  read_string(H, Snext),
  read_lines(H, Snext, [S|SoFar], TB).
read_lines(_, _, TB, TB).

% write_block
%   Write a text block, simply echo the list of strings in the
%   block to the output device.  Used to report errors.

write_block(H, X) :- atomic(X), writeq(H, X), nl(H), !.
write_block(H, TB) :- reverse(TB, TBf), write_blk(H, TBf), nl(H).

write_blk(H, []).
write_blk(H, [X|Z]) :-
  writeq(H, X), nl(H),
  write_blk(H, Z).

% Parse the block of input lines using a two tiered DCG.
%
% The first tier is concerned with lines, and the input is
% a list of strings.  The DCG uses position in the list
% to help determine what type of information is in each
% string.  DCG rules working the list of strings are
% prefaced with 'ps_'.
%
% The second tier is concerned with characters within
% the lines.  Each line is converted to a list of characters
% and passed to second tier DCG rules to extract the
% information from that line.  The top predicates of each
% DCG rule that works on character lists is prefaced
% with the 'p_'.  Helper rules might have any name.

parse_block(TB, Parse) :-
  ftype(FT),
  parse(FT, Parse, TB, []).

% A Miller Freeman list might have phone information first
% (which is really last as the lines are treated in reverse
% order), whereas PCAI lists do not have phones.

parse(mf, P) -->
  ps_phones(Ph), !,
  ps_addrname(mf, AN),
  {append(Ph, AN, P)}.
parse(pcai, P) -->
  ps_addrname(pcai, P).

% setcontext/1 is used to save context information for use
% in interpreting various abbreviations.

setcontext(X) :-
  retractall(context(_)),
  assert(context(X)).

% parse phone numbers, either in North American format or
% as a list of digits for other continents.

ps_phones([P|T]) -->
  [X],
  {string_list(X, L),
   p_phone(P, L, [])},
   ps_phones(T).
ps_phones([]) --> [].

p_phone(tel(P)) --> spaces,  "TEL", spaces, p_phnum(P).
p_phone(fax(P)) --> spaces,  "FAX", spaces, p_phnum(P).

p_phnum(A/E-N) -->
  areacode(A),
  exchange(E),
  number(N).
p_phnum(P) -->
  phoneword(P).

areacode(AC) -->
  "(", digit(A), digit(B), digit(C), ")", {name(AC, [A,B,C])}.
exchange(E) -->
  spaces, digit(A), digit(B), digit(C), "-", {name(E, [A,B,C])}.
number(N) -->
  digit(A), digit(B), digit(C), digit(D), {name(N, [A,B,C,D])}.

phoneword(W) -->
  spaces,
  phonechar(X),
  phonechars(Y),
  {name(W, [X|Y])}.

phonechars([X|Y]) --> phonechar(X), phonechars(Y).
phonechars([]) --> [].

phonechar(X) --> [X], {numb(X)}.
phonechar(X) --> [X], {member(X, [0' , 0'-, 0'(, 0'), 0'/])}.

% parse geographic area information

ps_addrname(LType, AN) -->
  ps_usarea(A), !,
  ps_rest(LType, R),
  {append(A, R, AN)}.
ps_addrname(LType, AN) -->
  ps_country(C),
  ps_intarea(A2), !,
  ps_rest(LType, R),
  {append([country(C)|A2], R, AN)}.
ps_addrname(_, []) --> [].

ps_usarea(A) -->
  [X],
  {string_list(X, L),
   p_usarea(A, L, [])}.

p_usarea([city(C), region(S), post(Z)]) -->
  city(C),
  state(S),
  uszip(Z),
  spaces.

ps_intarea(A) -->
  [X],
  {string_list(X, L),
   p_intarea(A, L, [])}.

p_intarea([city(C)]) -->
  city(C),
  spaces.
p_intarea([city(C), post(P)]) -->
  city(C),
  post(P),
  spaces.
p_intarea([city(C)]) -->
  anycity(C),
  spaces.

city(C) --> placewords(C).

anycity(C) --> words(C).

state(S) --> upword(S). 

uszip(Z) -->
  spaces,digit(A),digit(B),digit(C),digit(D),digit(E),
  {name(Z,[A,B,C,D,E])}.
uszip(Z) -->
  spaces,digit(A),digit(B),digit(C),digit(D),digit(E),
  "-",digit(F),digit(G),digit(H),digit(I),
  {name(Z, [A,B,C,D,E,0'-,F,G,H,I])}.
   
post(P) --> postwords(P).

ps_country(C) -->
  [X],
  {string_list(X,L),
   p_country(C, L, [])}.

p_country(C) --> upwords(C), spaces.

% Parse remaining information, where, depending on the
% type of list and number of lines remaining, might
% be of many forms.

ps_rest(mf, [addr1(A1), org(O), title(T) | N]) -->
  ps_addr(A1),
  ps_org(O),
  ps_title(T),
  ps_names(N).
ps_rest(mf, [addr1(A1) | N]) -->
  ps_addr(A1),
  ps_names(N).
ps_rest(mf, [addr1(A1), org(O) | N]) -->
  ps_addr(A1),
  ps_org(O),
  ps_names(N).
ps_rest(mf, [addr1(A1), addr2(A2), org(O), title(T) | N]) -->
  ps_addr(A2),
  ps_addr(A1),
  ps_org(O),
  ps_title(T),
  ps_names(N).
ps_rest(pcai, [addr1(A1), org(O) | N]) -->
  ps_addr(A1),
  ps_names(N),
  ps_org(O).
ps_rest(pcai, [addr1(A1) | N]) -->
  ps_addr(A1),
  ps_names(N).

% Rules for address lines, organizations, titles
% and names.

ps_addr(A) -->
  [X],
  {string_list(X, L),
   p_addr(A, L, [])}.

p_addr(A) --> {setcontext(addr)},
  words(A), spaces.

ps_org(O) -->
  [X],
  {string_list(X, L),
   p_org(O, L, [])}.

p_org(O) --> {setcontext(org)}, words(O), spaces.

ps_title(T) -->
  [X],
  {string_list(X, L),
   p_title(T, L, [])}.

p_title(T) --> {setcontext(title)}, words(T), spaces.

ps_names([last(LN), first(FN)]) -->
  [X],
  {string_list(X, L),
   p_names(LN, FN, L, [])}.

p_names(L, F) --> {setcontext(name)},
  first(F), last(L), spaces.

first(F) --> words(F).
last(L) --> word(L).

% Lowest level character parsing rules, used to parse
% a line, which is at this point a list of characters,
% into a list of words.  The lists of characters are
% grouped into syntactic chunks which are converted
% to atoms by the name/2 built-in predicate.

words([X|Y]) --> spaces, word(X), words(Y).
words([]) --> [].

word(W) --> spaces, [X], {achar(X)}, wchars(Chs),
  {uplow([X|Chs], Z), name(W,Z)}.

upwords([X|Y]) --> spaces, upword(X), upwords(Y).
upwords([]) --> [].

upword(W) --> spaces, [X], {achar(X)}, wchars(Chs), {name(W,[X|Chs])}.

postwords([X|Y]) --> spaces, postword(X), postwords(Y).
postwords([]) --> [].

postword(W) --> spaces, [X], {achar(X)}, wchars(Chs),
  {hasdigit([X|Chs]), name(W, [X|Chs])}.

placewords([X|Y]) --> spaces, placeword(X), placewords(Y).
placewords([]) --> [].

placeword(W) --> spaces, [X], {achar(X)}, wchars(Chs),
  {not(hasdigit([X|Chs])), uplow([X|Chs], Z), name(W,Z)}.

wchars([X|Y]) --> [X], {achar(X)}, wchars(Y), !.
wchars([]) --> [].

hasdigit([]) :- !, fail.
hasdigit([X|Y]) :- numb(X), !.
hasdigit([X|Y]) :- hasdigit(Y).

digit(X) --> [X], {X >= 0'0, X =< 0'9}.

achar(X) :- letter(X), !.
achar(X) :- numb(X), !.
achar(X) :- member(X,[0'', 0'., 0'-, 0'/, 0'#, 0'&]), !.

letter(X) :- X >= 0'a, X =< 0'z, !.
letter(X) :- X >= 0'A, X =< 0'Z, !.

numb(X) :- X >= 0'0, X =< 0'9.

spaces --> space, spaces, !.
spaces --> [].

space --> [X], {X =< 32}.
space --> ",".

lf --> [10].

% Convert upper case words to initial cap and lower case
% when we have a Miller Freeman mailing list.  Preserve
% certain forms as all caps, and add punctuation to some
% abbreviations.

uplow(X, X) :- not(ftype(mf)), !.
uplow(X, X) :- not(allletters(X)), !.
uplow(X, Y) :- wired(X, Y), !.
uplow([C|U], [C|L]) :- ullist(U,L).

allletters([]).
allletters([H|T]) :-
  (letter(H); member(H, [0'-, 0''])),
  !,
  allletters(T).

ullist([], []).
ullist([U|X], [L|Y]) :-
  ulchar(U,L),
  ullist(X,Y).

ulchar(U,L) :-
  Offset is U - 0'A,
  Offset >= 0,
  Offset < 26,
  !,
  L is 0'a + Offset.
ulchar(X,X).

wired("OF", "of").
wired("DU", "du").
wired("DE", "de").
wired(X, Y) :-
  context(name), !,
  wname(X, Y).
wired(X, Y) :-
  context(addr), !,
  waddr(X, Y).
wired(X, Y) :-
  context(title), !,
  wtitle(X, Y).
wired(X, Y) :-
  context(org), !,
  worg(X,Y).

wname([X], [X, 0'.]).
wname("DR", "Dr.").
wname("PHD", "PhD.").
wname("MS", "Ms.").
wname("MR", "Mr.").
wname("JR", "Jr.").

wtitle("GM", "G.M.").
wtitle("MGR", "Mgr.").
wtitle("SW", "SW").
wtitle("IT", "IT").
wtitle("QA", "QA").
wtitle("SE", "SE").
wtitle("TECH", "Tech.").
wtitle("DIR", "Dir.").
wtitle("CEO", "C.E.O.").
wtitle("PRES", "Pres.").
wtitle("ASST", "Asst.").
wtitle("SR", "Sr.").
wtitle("MIS", "MIS").
wtitle("VP", "V.P.").

waddr("PO", "P.O.").
waddr("NW", "NW").
waddr("SE", "SE").
waddr("NE", "NE").
waddr("SW", "SW").
waddr("ST", "St.").
waddr("RD", "Rd.").
waddr("AVE", "Ave.").
waddr("DR", "Dr.").
waddr("CT", "Ct.").
waddr("CIR", "Cir.").
waddr("BLDG", "Bldg.").
waddr("RM", "Rm.").

worg([X], [X, 0'.]).
worg("USAF", "USAF").
worg("AFB", "AFB").
worg("INC", "Inc.").
worg("LTD", "Ltd.").
worg("CORP", "Corp.").
worg("CO", "Co.").
