% <PRE>
% xml.pro
% a Prolog to XML translator

% a main entry point just for testing
% purposes.

:- module(amzi_xml).
:- export xml_term/2.

test :-
   test(TERM),
   writeq(TERM), nl,
   xml_term(XML, TERM),
   writeq(XML), nl,
   xml_term(XML, TERM2),
   writeq(TERM2), nl,
   nl,
   fail.
test.

% various test cases

test(hello).
test('Daffy Duck isn''t').
test($this is a $$5 string$).
test([23, 2.3, 2.3e4, 2.3e23]).
test([a,b, [c,d, [e,f]]]).
test(X).
test([X,Y,X]).
test(duck(leona)).
test(same(X,X)).
test(different(X,Y)).
test(X = [a,b,foo(2.3, 4)]).
test(complex(first, X = [a,b,foo(2.3, 4)], nest(X, [Y,$a string$,X]))).
test(complex(first, X = [a,b,foo(2.3, 4)], nest(X, [Y,$a string$,X]))).

% Convert between a Prolog term and a string
% of XML describing that term.  This uses well-formed
% XML, but not 'valid' because it doesn't include
% the DTD specification.  See next predicates for that.

xml_term(XML_STRING, TERM) :-
   var(XML_STRING),
   !,
   term(TERM, VARS, XML_CHARS, ""),
   string_list(XML_STRING, XML_CHARS).
xml_term(XML_STRING, TERM) :-
   string_list(XML_STRING, XML_CHARS),
   term(TERM, VARS, XML_CHARS, ""),
   !.

% A valid XML document has a document specification DTD
% included.  This predicate provides well-formed and
% valid XML.

valid_xml_term(XML_STRING, TERM) :-
   var(XML_STRING),
   !,
   xmldoc(TERM, XML_CHARS, ""),
   string_list(XML_STRING, XML_CHARS).
valid_xml_term(XML_STRING, TERM) :-
   string_list(XML_STRING, XML_CHARS),
   xmldoc(TERM, XML_CHARS, ""),
   !.

xmlhead -->
   "<?xml version=""1.0""?>".

doctype -->
   "<!DOCTYPE prolog [",
   "<!ELEMENT prolog (atom|string|number|variable|structure|list)>",
   "<!ELEMENT atom (#PCDATA)>",
   "<!ELEMENT string (#PCDATA)>",
   "<!ELEMENT number (#PCDATA)>",
   "<!ELEMENT variable (#PCDATA)>",
   "<!ELEMENT list (item*)>",
   "<!ELEMENT item (atom|string|number|variable|structure|list)>",
   "<!ELEMENT structure (name, arg+)>",
   "<!ELEMENT name (#PCDATA)>",
   "<!ELEMENT arg (atom|string|number|variable|structure|list)>",
   "]>".

xmldoc(TERM) -->
   xmlhead,
   doctype,
   "<prolog>",
   term(TERM, VARS),
   "</prolog>".
   
% Check which way we're going.  Need to check
% the XML string, because T might be a variable
% either way.

term(T, VARS, XML, REST) :-
   var(XML),
   !,
   term_to_xml(T, VARS, XML, REST).
term(T, VARS) -->
   xml_to_term(T, VARS).

% These clauses cover the case where the XML
% is provided and we're creating the term.

xml_to_term(T, VARS) -->
   tag(string),
   !,
   term_codes(CODES),
   endtag(string),
   { string_list(T, CODES) }.
xml_to_term(T, VARS) -->
   tag(atom),
   !,
   term_codes(CODES),
   endtag(atom),
   { atom_codes(T, CODES) }.
xml_to_term(T, VARS) -->
   tag(number),
   !,
   term_codes(CODES),
   endtag(number),
   { string_list(S, CODES), string_term(S, T) }.
xml_to_term(T, VARS) -->
   tag(list),
   !,
   items(T, VARS),
   endtag(list).
xml_to_term(T, VARS) -->
   tag(structure),
   !,
   tag(name),
   term_codes(CODES),
   { atom_codes(NAME, CODES) },
   endtag(name),
   args(ARGS, VARS),
   { T =.. [NAME|ARGS] },
   endtag(structure).

xml_to_term(T, VARS) -->
   tag(variable),
   !,
   term_codes(CODES),
   { atom_codes(VNAME, CODES), open_member(VNAME=T, VARS) },
   endtag(variable).

% These clauses are for the cases when the
% term is provided and we're generating XML.

term_to_xml(T, VARS) --> { number(T), ! },
   { string_term(S, T), string_list(S, CODES) },
   tag(number),
   term_codes(CODES),
   endtag(number).
term_to_xml(T, VARS) --> { string(T), ! },
   { string_list(T, CODES) },
   tag(string),
   term_codes(CODES),
   endtag(string).
term_to_xml(T, VARS) --> { atom(T), ! },
   { atom_codes(T, CODES) },
   tag(atom),
   term_codes(CODES),
   endtag(atom).
term_to_xml(T, VARS) --> { islist(T), ! },
   tag(list),
   items(T, VARS),
   endtag(list).
term_to_xml(T, VARS) --> { structure(T), ! },
   { T =.. [NAME|ARGS], atom_codes(NAME, CODES) },
   tag(structure),
   tag(name),
   term_codes(CODES),
   endtag(name),
   args(ARGS, VARS),
   endtag(structure).
term_to_xml(T, VARS) --> { var(T), ! },
   tag(variable),
   { string_term(S, T), string_list(S, CODES) },
   term_codes(CODES),
   endtag(variable).

% Pick up the value of the list of codes for that
% stuff that resides between two tags.  So the argument
% in these clauses is the list of codes.  The parsed
% input is from the XML string.

term_codes([X|Y]) --> term_code(X), !, term_codes(Y).
term_codes([]) --> [].

% In the code list, that is between the tags, a
% < or > is represented specially.  A real < read
% from the XML string signifies the end of the
% stuff between the tags.  Note this works both
% ways, when the code list is bound or the XML
% text is bound.  The last clause is only in play
% when it's the XML string that is bound.

term_code(0'<) --> "&lt;".
term_code(0'>) --> "&gt;".
term_code(0'&) --> "&amp;".
term_code(X) --> [X], { X \= 0'< }.

% Pick up a tag.

tag(TAG) -->
   white_space, "<", word(TAG), ">".

endtag(TAG) -->
   "</", word(TAG), ">".

solotag(TAG) -->
   "<", word(TAG), "/>".

word(WORD) --> { var(WORD), ! },
   chars(CHARS), { atom_codes(WORD, CHARS) }.
word(WORD) --> { nonvar(WORD) },
   { atom_codes(WORD, CHARS) }, chars(CHARS).

white_space --> white, white_space.
white_space --> [].

white --> [X], { nonvar(X), X =< 32 }.

chars([X|Y]) --> char(X), !, chars(Y).
chars([]) --> [].

char(X) --> [X], { is_char(X) }.

is_char(X) :- X >= 0'a, X =< 0'z, !.
is_char(X) :- X >= 0'A, X =< 0'Z, !.
is_char(X) :- X >= 0'0, X =< 0'9, !.
is_char(0'_).

% Items in a list

items([X|Y], VARS) --> item(X, VARS), !, items(Y, VARS).
items([], _) --> [].

item(X, VARS) -->
   tag(item),
   term(X, VARS),
   endtag(item).

% A list of structure arguments

args([X|Y], VARS) --> sarg(X, VARS), !, args(Y, VARS).
args([], _) --> [].

sarg(X, VARS) -->
   tag(arg),
   term(X, VARS),
   endtag(arg).

% open_member is just like member, but named it this
% just to indicate its working on an open list, and
% adds values that aren't already on the list.

open_member(X, [X|_]).
open_member(X, [_|Y]) :- open_member(X,Y).

:- end_module(amzi_xml).
