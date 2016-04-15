/*
<HTML>
<HEAD>
<TITLE>Amzi! Miscellaneous Predicate Library</TITLE>
</HEAD>
<BODY>
<A NAME="contents"></A>
This file contains library predicates that perform
miscellaneous functions.
<p>
Note - requires that LIST.PLM be loaded as well.
<MENU>
<LI><A HREF="#compare_nocase">compare_nocase/2</A> - case insensitive compare
<LI><A HREF="#force_ext">force_ext/3</A> - put an extension on a file name
<LI><A HREF="#freeze">freeze/2</A> - freeze the variables in a term
<LI><A HREF="#get_flag">get_flag/2</A> - get the value of a flag
<LI><A HREF="#melt">melt/2</A> - thaw the variables in a frozen term
<LI><A HREF="#newcopy">newcopy/2</A> - copy a term with new variables
<LI><A HREF="#set_flag">set_flag/2</A> - set a flag to a value
</MENU>
<PRE>
*/

:- module(misc).

:- export( [
     compare_nocase/2,
     force_ext/3,
     freeze/2,
     get_flag/2,
     melt/2,
     newcopy/2,
     set_flag/2 ]).

:- import(list).

/*
</PRE>
<H2><A NAME="compare_nocase">compare_nocase(Atom1, Atom2)</A></H2>
compare_nocase/2 compares two atoms to see if they are
the same words.  Succeeds if they are, fails if they aren't.
<p><A HREF="#contents">Contents</A>
<PRE>
*/

compare_nocase(Atom, Atom) :- !.
compare_nocase(Atom1, Atom2) :-
  atom_uplow(Atom1, Low),
  atom_uplow(Atom2, Low).

/*
</PRE>
<H2><A NAME="force_ext">force_ext(FileName, Ext, NewName)</A></H2>
force_ext/3 takes an atom or string
FileName and file extenstion, Ext, as input, outputs a
filename with the new extension as either an atom or string.
<p><A HREF="#contents">Contents</A>
<PRE>
*/

force_ext(Name, Ext, NewName) :-
  % Backslashes are a pain in file names, so turn
  % off string esc before processing names, and then
  % restore it to whatever the user had.
  get_mode(string_esc, SE_Mode),
  set_mode(string_esc, off),
  ( force$ext(Name, Ext, NewName) ->
       set_mode(string_esc, SE_Mode)
     ; set_mode(string_esc, SE_Mode), fail).

  force$ext(SName, SExt, SNewName) :-
    string(SName),
    !,
    string_atom(SName, Name),
    (string(SExt) -> string_atom(SExt, Ext); Ext = SExt),
    string_atom(SName, Name),
    force_ext(Name, Ext, NewName),
    string_atom(SNewName, NewName).
  force$ext(Name, Ext, NewName) :-
    atom_codes(Name, CName),
    reverse(CName, RCName),
    remove$ext(RCName, RCNameNoExt),
    reverse(RCNameNoExt, CNameNoExt),
    atom_codes(Ext, CExt),
    force$dot(CExt, DotCExt),
    append(CNameNoExt, DotCExt, CNewName),
    atom_codes(NewName, CNewName).

  force$dot([0'.|Z], [0'.|Z]) :- !.
  force$dot(Z, [0'.|Z]).

  remove$ext([0'.|Z], Z) :- !.
  remove$ext([_, 0'.|Z], Z) :- !.
  remove$ext([_, _, 0'.|Z], Z) :- !.
  remove$ext([_, _, _, 0'.|Z], Z) :- !.
  remove$ext(Z, Z).

/*
</PRE>
<H2><A NAME="freeze">freeze(Term, FrozenTerm)</A></H2>
freeze/2 takes the argument in the first term and converts
it to a similar term, but with all the variables replaced
with atoms of the form '_1', '_2'.
<p><A HREF="#contents">Contents</A>
<PRE>
*/

freeze(Term, Frozen) :-
  newcopy(Term, Frozen),
  numbervars(Frozen, 1, _).

/*
</PRE>
<H2><A NAME="get_flag">get_flag(Flag, Value)</A></H2>
get_flag/2 simply gets the value of a flag.  It complements
set_flag/2 which sets flag values.
<p><A HREF="#contents">Contents</A>
<PRE>
*/

get_flag(FLAG, VALUE) :-
  flag(FLAG, VALUE).

/*
</PRE>
<H2><A NAME="melt">melt(FrozenTerm, Term)</A></H2>
melt/2 takes a frozen term, and replaces the atoms of the
form '_1' with unbound variables.
<p><A HREF="#contents">Contents</A>
<PRE>
*/

melt(Frozen, Term) :-
  string_term(TempString, Frozen),
  string_term(TempString, Term).

/*
</PRE>
<H2><A NAME="newcopy">newcopy(Term, Copy)</A></H2>
newcopy/2 makes a copy of a term with new variables.  This
allows you to manipulate the copy without unifying the
variables of the original. No longer necessary, as
copy_term/2 is a built-in that provides this function
<p><A HREF="#contents">Contents</A>
<PRE>
*/

newcopy(X, Y) :-
   copy_term(X,Y).

/*
</PRE>
<H2><A NAME="set_flag">set_flag(Flag, Value)</A></H2>
set_flag/2 sets the value of a flag, keeping a unique
value.
<p><A HREF="#contents">Contents</A>
<PRE>
*/

set_flag(FLAG, VALUE) :-
  (retract(flag(FLAG, _)); true),
  asserta(flag(FLAG, VALUE)).

:- end_module(misc).