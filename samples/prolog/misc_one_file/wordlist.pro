/*  WORDLIST.PRO -- Read word list
**
**  Read word list reads in a list of chars (terminated with a !, . or ?)
**  and converts it to a list of atomic entries (including numbers).
**  Uppercase is converted to lower case.  Based on code in
**  "Programming in Prolog" by Clocksin & Mellish.
**
**  A 'word' is one item in our generated list
**
**  This code can be modified to include whatever separators you need
**  and whatever termination criteria you need.
*/

test :-
	write($Enter a sentence (end with . ! or ?$), nl,
	read_word_list(List),
	write(List), nl.

read_word_list([W|Ws]) :-
	get0(C),
	readword(C, W, C1),  	% Read word starting with C, C1 is first new
	restsent(C1, Ws), !.    % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !.  % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
	readword(C,W1,C1),	 	% Else read next word and rest of sentence
	restsent(C1,Ws).

readword(C,W,C1) :-			% Some words are single characters
	single_char(C),	    	% i.e. punctuation
	!, 
	name(W, [C]),	      	% get as an atom
	get0(C1).
readword(C, W, C1) :-
	is_num(C),		 			% if we have a number --
	!,
	number_word(C, W, C1, _).  % convert it to a genuine number
readword(C,W,C2) :-			% otherwise if charcter does not
	in_word(C, NewC),	  		% delineate end of word - keep
	get0(C1),		  			% accumulating them until 
	restword(C1,Cs,C2),		% we have all the word     
	name(W, [NewC|Cs]).		% then make it an atom
readword(C,W,C2) :-			% otherwise
	get0(C1),       
	readword(C1,W,C2).	 	% start a new word

restword(C, [NewC|Cs], C2) :-
	in_word(C, NewC),
	get0(C1),
	restword(C1, Cs, C2).
restword(C, [], C).

single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).

in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
	is_num(C),
	!,
	get0(C2),
	number_word(C2, W1, C1, P10),
	Pow10 is P10 * 10,
	W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).

is_num(C) :-
	C =< 0'9,
	C >= 0'0.

% These symbols delineate end of sentence.  If you prefer to simply
% read an input line, rather than a possibly multiline sentence,
% use the newline character as lastword (10).

%lastword(10).  % uncomment this line, to read single lines
lastword(0'.).
lastword(0'!).
lastword(0'?).

