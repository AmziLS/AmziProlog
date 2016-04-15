/*****************************************************************************/
/* ELIZA in Prolog                                                           */
/*                                                                           */
/*    Viren Patel                                                            */
/*    Artificial Intelligence Programs                                       */
/*    University of Georgia, Athens, Georgia                                 */
/*    Email: vpatel@aisun1.ai.uga.edu                                        */
/*                                                                           */
/* Reference                                                                 */
/*                                                                           */
/*    Weizenbaum, J., (1966) ELIZA - A computer program for the study of     */
/*    natural language communication between man and machine. Communications */
/*    of the ACM, 9.1:36-45.                                                 */
/*                                                                           */
/* Acknowledgments                                                           */
/*                                                                           */
/*    read_atomics/1 and suporting clauses are courtesy of Dr. Michael A.    */
/*    Covington, AI Programs, University of Georgia, Athens, Georgia from    */
/*    his forthcoming book, Natural language processing for Prolog           */
/*    programmers.                                                           */
/*                                                                           */
/*    match/2 and its supporting clauses make up the pattern matcher. The    */
/*    basic code for the pattern matcher was obtained from the book by       */
/*    R. A. O'Keefe, The craft of Prolog.                                    */
/*                                                                           */
/* Requires: Quintus Prolog (Runs on Amzi! Prolog as well)                   */
/*                                                                           */
/* To run:  ?- consult(eliza).                                               */
/*          ?- main.                                                         */
/* To stop: > quit (`>' is the Eliza prompt)                                 */
/*                                                                           */
/* Last Revised: April 10, 1992                                              */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
% simplification rules

sr([do,not|X],[dont|Y],X,Y).
sr([can,not|X],[cant|Y],X,Y).
sr([cannot|X],[cant|Y],X,Y).
sr([will,not|X],[wont|Y],X,Y).
sr([dreamed|X],[dreamt|Y],X,Y).
sr([dreams|X],[dream|Y],X,Y).
sr([how|X],[what|Y],X,Y).
sr([when|X],[what|Y],X,Y).
sr([alike|X],[dit|Y],X,Y).
sr([same|X],[dit|Y],X,Y).
sr([certainly|X],[yes|Y],X,Y).
sr([maybe|X],[perhaps|Y],X,Y).
sr([deutsch|X],[xfremd|Y],X,Y).
sr([francais|X],[xfremd|Y],X,Y).
sr([espanol|X],[xfremd|Y],X,Y).
sr([machine|X],[computer|Y],X,Y).
sr([machines|X],[computer|Y],X,Y).
sr([computers|X],[computer|Y],X,Y).
sr([am|X],[are|Y],X,Y).
sr([your|X],[my|Y],X,Y).
sr([were|X],[was|Y],X,Y).
sr([me|X],[you|Y],X,Y).
sr([you,are|X],[im|Y],X,Y).      % im = i'm = i am
sr([i,am|X],[youre|Y],X,Y).      % youre = you're = you are =\= your
sr([myself|X],[yourself|Y],X,Y).
sr([yourself|X],[myself|Y],X,Y).
sr([mom|X],[mother|Y],X,Y).
sr([dad|X],[father|Y],X,Y).
sr([i|X],[you|Y],X,Y).
sr([you|X],[i|Y],X,Y).
sr([my|X],[your|Y],X,Y).
sr([everybody|X],[everyone|Y],X,Y).
sr([nobody|X],[everyone|Y],X,Y).


/*****************************************************************************/
% Make the rule base modifiable
%    this is specific to Quintus Prolog (Amzi! as well)

:- dynamic(rules/1).


/*****************************************************************************/
% The rule base
%    The format of the rules is:
%
% rules([[keyword, importance of keyword],[
%         [pattern #, [the pattern], last response used,
%             [response 1],
%             [response 2],
%             ...
%             [response n]]]]).

rules([[sorry,0],[
	[1,[_],0,
		[please,do,not,apologize,.],
		[apologies,are,not,necessary,.],
		[what,feelings,do,you,have,when,you,apologize,?],
		['I',have,told,you,that,apologies,are,not,required,.]]]]).

rules([[remember,5],[
	[1,[_,you,remember,Y],0,
		[do,you,often,think,of,Y,?],
		[does,thinking,of,Y,bring,anything,else,to,mind,?],
		[what,else,do,you,remember,?],
		[why,do,you,remember,Y,just,now,?],
		[what,in,the,present,situation,reminds,you,of,Y,?],
		[what,is,the,connection,between,me,and,Y,?]],
	[2,[_,do,i,remember,Y],0,
		[did,you,think,'I',would,forget,Y,?],
		[why,do,you,think,i,should,recall,Y,now,?],
		[what,about,Y,?],
		[equal,[what,0]],
		[you,mentioned,Y,.]],
	[3,[_],0,
		[newkey]]]]).

rules([[if,3],[
	[1,[_,if,Y],0,
		[do,you,think,it,is,likely,that,Y,?],
		[do,you,wish,that,Y,?],
		[what,do,you,think,about,Y,?],
		[really,',',if,Y,?]]]]).

rules([[dreamt,4],[
	[1,[_,you,dreamt,Y],0,
		[really,',',Y,?],
		[have,you,ever,fantasied,Y,while,you,were,awake,?],
		[have,you,dreamt,Y,before,?],
                [equal,[dream,3]],
		[newkey]]]]).

rules([[dream,3],[
	[1,[_],0,
		[what,does,that,dream,suggest,to,you,?],
		[do,you,dream,often,?],
		[what,persons,appear,in,your,dreams,?],
		[do,you,believe,that,dreaming,has,something,to,do,with,your,problem,?],
		[newkey]]]]).

rules([[none,0],[
	[1,[_],0,
		['I',am,not,sure,'I',understand,you,fully,.],
		[please,go,on,.],
		[what,does,that,suggest,to,you,?],
		[do,you,feel,strongly,about,discussing,such,things,?]]]]).

rules([[perhaps,0],[
	[1,[_],0,
		[you,do,not,seem,quite,certain,.],
		[why,the,uncertain,tone,?],
		[can,you,not,be,more,positive,?],
		[you,are,not,sure,?],
		[do,you,not,know,?]]]]).

rules([[name,15],[
	[1,[_],0,
		['I',am,not,interested,in,names,.],
		['I',have,told,you,before,',','I',do,not,care,about,names,.],
		[please,continue,.]]]]).

rules([[xfremd,75],[
	[1,[_],0,
		['I',am,sorry,',','I',only,speak,'English',.]]]]).

rules([[hello,0],[
	[1,[_],0,
		[how,do,you,do,'.','Please',state,your,problem,.]]]]).

rules([[computer,50],[
	[1,[_],0,
		[do,computers,worry,you,?],
		[why,do,you,mention,computers,?],
		[what,do,you,think,machines,have,to,do,with,your,problem,?],
		[do,you,not,think,computers,can,help,people,?],
		[what,about,machines,worries,you,?],
		[what,do,you,think,about,machines,?]]]]).

rules([[are,0],[
	[1,[_,are,you,Y],0,
		[do,you,believe,you,are,Y,?],
		[would,you,want,to,be,Y,?],
		[you,wish,'I',would,tell,you,you,are,Y,?],
                [what,would,it,mean,if,you,were,Y,?],
		[equal,[what,0]]],
	[2,[_,are,i,Y],0,
		[why,are,you,interested,in,whether,'I',am,Y,or,not,?],
		[would,you,prefer,if,'I',were,not,Y,?],
		[perhaps,'I',am,Y,in,your,fantasies,.],
                [do,you,sometimes,think,'I',am,Y,?],
		[equal,[what,0]]],
	[3,[_,are,Y],0,
		[did,you,think,they,might,not,be,Y,?],
		[would,you,like,it,if,they,were,not,Y,?],
		[what,if,they,were,not,Y,?],
		[possibly,they,are,Y,.]],
	[4,[_],0,
		[why,do,you,say,am,?],
		['I',do,not,understand,that,.]]]]).


rules([[my,0],[
	[1,[_,my,Y],0,
		[why,are,you,concerned,over,my,Y,?],
		[what,about,your,own,Y,?],
		[are,you,worried,about,someone,elses,Y,?],
		[really,',',my,Y,?]]]]).

rules([[was,2],[
	[1,[_,was,you,Y],0,
		[what,if,you,were,Y,?],
		[do,you,think,you,were,Y,?],
		[were,you,Y,?],
		[what,would,it,mean,if,you,were,Y,?],
                [what,does,Y,suggest,to,you,?],
		[equal,[what,0]]],
	[2,[_,you,was,Y],0,
		[were,you,really,?],
		[why,do,you,tell,me,you,were,Y,just,now,?],
		[perhaps,'I',already,knew,you,were,Y,.]],
	[3,[_,was,i,Y],0,
		[would,you,like,to,believe,'I',was,Y,?],
		[what,suggests,that,'I',was,Y,?],
		[what,do,you,think,?],
		[perhaps,'I',was,Y,.],
		[what,if,'I',had,been,Y,?]],
	[4,[_],0,
		[newkey]]]]).

rules([[im,0],[
        [1,[_,im,_],0,
		[equal,[i,0]]]]]).

rules([[youre,0],[
        [1,[_,youre,_],0,
		[equal,[you,0]]]]]).

rules([[you,0],[
	[1,[_,you,cant,Y],0,
		[how,do,you,know,you,can,not,Y,?],
		[have,you,tried,?],
		[perhaps,you,could,Y,now,?],
		[do,you,really,want,to,be,able,to,Y,?]],
	[2,[_,you,dont,Y],0,
		[do,you,not,really,Y,?],
		[why,do,you,not,Y,?],
		[do,you,wish,to,be,able,to,Y,?],
		[does,that,trouble,you,?]],
	[3,[_,you,feel,Y],0,
		[tell,me,more,about,such,feelings,.],
		[do,you,often,feel,Y,?],
		[do,you,enjoy,feeling,Y,?],
		[of,what,does,feeling,Y,remind,you,?]],
        [4,[_,you,was,_],0,
		[equal,[was,2]]],
	[5,[_,you,Y,i,_],0,
		[perhaps,in,your,fantasy,we,Y,each,other,?],
		[do,you,wish,to,Y,me,?],
		[you,seem,to,need,to,Y,me,.],
		[do,you,Y,anyone,else,?]],
	[6,[_,you,[*,want,need,_],Y],0,
		[what,would,it,mean,to,you,if,you,got,Y,?],
		[why,do,you,want,Y,?],
		[suppose,you,got,Y,soon,?],
		[what,if,you,never,got,Y,?],
		[what,would,getting,Y,mean,to,you,?],
		[what,does,wanting,Y,have,to,do,with,this,discussion,?]],
	[7,[_,you,[*,feel,think,believe,wish,_],you,Y],0,
		[do,you,really,think,so,?],
		[but,you,are,not,sure,you,Y,?],
		[do,you,really,doubt,you,Y,?]],
        [8,[_,you,_,[*,feel,think,believe,wish,_],_,i,_],0,
		[equal,[you,0]]],
	[9,[_,youre,_,[*,sad,unhappy,depressed,sick,M],_],0,
		['I',am,sorry,to,hear,you,are,M,.],
		[do,you,think,coming,here,will,help,you,not,to,be,M,?],
		['I',am,sure,it,is,not,pleasant,to,be,M,.],
		[can,you,explain,what,made,you,M,?]],
	[10,[_,youre,_,[*,happy,elated,glad,better,M],_],0,
		[how,have,'I',helped,you,to,be,M,?],
		[has,your,treatment,made,you,M,?],
		[what,makes,you,M,just,now,?],
		[can,you,explain,why,you,are,suddenly,M,?]],
	[11,[_,youre,Y],0,
		[is,it,because,you,are,Y,that,you,came,to,me,?],
		[how,long,have,you,been,Y,?],
		[do,you,believe,it,normal,to,be,Y,?],
		[do,you,enjoy,being,Y,?]],
	[12,[X],0,
		[you,say,X],
		[can,you,elaborate,on,that,?],
		[do,you,say,X,for,some,special,reason,?],
		[that,is,quite,interesting,.]]]]).

rules([[i,0],[
        [1,[_,i,remind,you,of,_],0,
		[equal,[dit,10]]],
	[2,[_,im,Y],0,
		[what,makes,you,think,'I',am,Y,?],
		[does,it,please,you,to,believe,'I',am,Y,?],
		[do,you,sometimes,wish,you,were,Y,?],
		[perhaps,you,would,like,to,be,Y,?]],
	[3,[_,i,Y,you],0,
		[why,do,you,think,'I',Y,you,?],
		[you,like,to,think,'I',Y,you,',',do,you,not,?],
		[what,makes,you,think,'I',Y,you,?],
		[really,',','I',Y,you,.],
		[do,you,wish,to,believe,'I',Y,you,?],
		[suppose,'I',did,Y,you,',',what,would,that,mean,to,you,?],
		[does,someone,else,believe,'I',Y,you,?]],
	[4,[_,i,Y],0,
		[we,were,discussing,you,',',not,me,.],
		[oh,',',i,Y,.],
		[you,are,not,really,talking,about,me,',',are,you,?],
		[what,are,your,feelings,now,?]]]]).

rules([[yes,0],[
	[1,[_],0,
		[you,seem,quite,positive,.],
		[you,are,sure,?],
		[i,see,.],
		[i,understand,.]]]]).

rules([[no,0],[
	[1,[_],0,
		[are,you,saying,no,just,to,be,negative,?],
		[you,are,being,a,bit,negative,.],
		[why,not,?],
		[why,no,?]]]]).

rules([[your,2],[
	[1,[_,your,_,[*,mother,father,brother,sister,children,wife,M],Z],0,
		[tell,me,more,about,your,family,.],
		[who,else,in,your,family,Z,?],
		[your,M,?],
		[what,else,comes,to,mind,when,you,think,of,your,M,?]],
	[2,[_,your,Y],0,
		[your,Y,?],
		[why,do,you,say,your,Y,?],
		[does,that,suggest,anything,else,which,belongs,to,you,?],
		[is,it,important,to,you,that,your,Y,?]]]]).

rules([[memory,0],[
	[1,[_,your,Y],0,
		[lets,discuss,further,why,your,Y,.],
		[earlier,you,said,your,Y,.],
		[but,your,Y,?],
		[does,that,have,anything,to,do,with,the,fact,that,your,Y,?]]]]).
		
rules([[can,0],[
	[1,[_,can,i,Y],0,
		[you,believe,'I',can,Y,',',do,you,not,?],
		[equal,[what,0]],
		[you,want,me,to,be,able,to,Y,?],
		[perhaps,you,would,like,to,be,able,to,Y,yourself,?]],
	[2,[_,can,you,Y],0,
		[whether,or,not,you,can,Y,depends,on,you,more,than,on,me,.],
		[do,you,want,to,be,able,to,Y,?],
                [perhaps,you,do,not,want,to,Y,.],
		[equal,[what,0]]]]]).

rules([[what,0],[
	[1,[_],0,
		[why,do,you,ask,?],
		[does,that,question,interest,you,?],
		[what,is,it,you,really,want,to,know,?],
		[are,such,questions,much,on,your,mind,?],
		[what,answer,would,please,you,most,?],
		[what,do,you,think,?],
		[what,comes,to,your,mind,when,you,ask,that,?],
		[have,you,asked,such,questions,before,?],
		[have,you,asked,anyone,else,?]]]]).

rules([[because,0],[
	[1,[_],0,
		[is,that,the,real,reason,?],
		[do,any,other,reasons,not,come,to,mind,?],
		[does,that,reason,seem,to,explain,anything,else,?],
		[what,other,reasons,might,there,be,?]]]]).

rules([[why,0],[
	[1,[_,why,dont,i,Y],0,
		[do,you,believe,'I',do,not,Y,?],
		[perhaps,'I',will,Y,in,good,time,.],
		[should,you,Y,yourself,?],
                [you,want,me,to,Y,?],
		[equal,[what,0]]],
	[2,[_,why,cant,you,Y],0,
		[do,you,think,you,should,be,able,to,Y,?],
		[do,you,want,to,be,able,to,Y,?],
		[do,you,believe,this,will,help,you,to,Y,?],
                [have,you,any,idea,why,you,can,not,Y,?],
		[equal,[what,0]]]]]).

rules([[everyone,2],[
	[1,[_,[*,everyone,everybody,nobody,noone,M],_],0,
		[really,',',M,?],
		[surely,not,M,?],
		[can,you,think,of,anyone,in,particular,?],
		[who,',',for,example,?],
		[you,are,thinking,of,a,very,special,person,?],
		[who,',',may,i,ask,?],
		[someone,special,perhaps,?],
		[you,have,a,paticular,person,in,mind,',',do,you,not,?],
		[who,do,you,think,you,are,talking,about,?]]]]).

rules([[always,1],[
	[1,[_],0,
		[can,you,think,of,a,specific,example,?],
		[when,?],
		[what,incident,are,you,thinking,of,?],
		[really,',',always,?]]]]).

rules([[like,10],[
        [1,[_,[*,im,youre,am,is,are,was,_],_,like,_],0,
		[equal,[dit,10]]],
	[2,[_],0,
		[newkey]]]]).

rules([[dit,10],[
	[1,[_],0,
		[in,what,way,?],
		[what,resemblance,do,you,see,?],
		[what,does,that,similarity,suggest,to,you,?],
		[what,other,connections,do,you,see,?],
		[what,so,you,suppose,that,resemblance,means,?],
		[what,is,the,connection,',',do,you,suppose,?],
		[could,there,really,be,some,connection,?],
		[how,?]]]]).

rules([[quit,100],[
	[1,[_],0,
		[goodbye,.,'My',secretary,will,send,you,a,bill,.]]]]).


/*****************************************************************************/
% char_type(+Char,?Type)
%    Char is an ASCII code.
%    Type is whitespace, punctuation, numeric, alphabetic, or special.

char_type(46,period) :- !.
char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
char_type(X,whitespace) :- X =< 32, !.
char_type(X,punctuation) :- X >= 33, X =< 47, !.
char_type(X,punctuation) :- X >= 58, X =< 64, !.
char_type(X,punctuation) :- X >= 91, X =< 96, !.
char_type(X,punctuation) :- X >= 123, X =< 126, !.
char_type(_,special).


/*****************************************************************************/
% lower_case(+C,?L)
%   If ASCII code C is an upper-case letter, then L is the
%   corresponding lower-case letter. Otherwise L=C.

lower_case(X,Y) :- 
	X >= 65,
	X =< 90,
	Y is X + 32, !.

lower_case(X,X).
                   

/*****************************************************************************/
% read_lc_string(-String)
%  Reads a line of input into String as a list of ASCII codes,
%  with all capital letters changed to lower case.

read_lc_string(String) :-
	get0(FirstChar),
	lower_case(FirstChar,LChar),
	read_lc_string_aux(LChar,String).

read_lc_string_aux(10,[]) :- !.  % end of line

read_lc_string_aux(-1,[]) :- !.  % end of file

read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).


/*****************************************************************************/
% extract_word(+String,-Rest,-Word) (final version)
%  Extracts the first Word from String; Rest is rest of String.
%  A word is a series of contiguous letters, or a series
%  of contiguous digits, or a single special character.
%  Assumes String does not begin with whitespace.

extract_word([C|Chars],Rest,[C|RestOfWord]) :-
	char_type(C,Type),
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(special,Rest,Rest,[]) :- !.
   % if Char is special, don't read more chars.

extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
	char_type(C,Type), !,
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.


/*****************************************************************************/
% remove_initial_blanks(+X,?Y)
%   Removes whitespace characters from the
%   beginning of string X, giving string Y.

remove_initial_blanks([C|Chars],Result) :-
	char_type(C,whitespace), !,
	remove_initial_blanks(Chars,Result).

remove_initial_blanks(X,X).   % if previous clause did not succeed.


/*****************************************************************************/
% digit_value(?D,?V)
%  Where D is the ASCII code of a digit,
%  V is the corresponding number.

digit_value(48,0).
digit_value(49,1).
digit_value(50,2).
digit_value(51,3).
digit_value(52,4).
digit_value(53,5).
digit_value(54,6).
digit_value(55,7).
digit_value(56,8).
digit_value(57,9).


/*****************************************************************************/
% string_to_number(+S,-N)
%  Converts string S to the number that it
%  represents, e.g., "234" to 234.
%  Fails if S does not represent a nonnegative integer.

string_to_number(S,N) :-
	string_to_number_aux(S,0,N).

string_to_number_aux([D|Digits],ValueSoFar,Result) :-
	digit_value(D,V),
	NewValueSoFar is 10*ValueSoFar + V,
	string_to_number_aux(Digits,NewValueSoFar,Result).

string_to_number_aux([],Result,Result).


/*****************************************************************************/
% string_to_atomic(+String,-Atomic)
%  Converts String into the atom or number of
%  which it is the written representation.

string_to_atomic([C|Chars],Number) :-
	string_to_number([C|Chars],Number), !.

string_to_atomic(String,Atom) :- name(Atom,String).
  % assuming previous clause failed.


/*****************************************************************************/
% extract_atomics(+String,-ListOfAtomics) (second version)
%  Breaks String up into ListOfAtomics
%  e.g., " abc def  123 " into [abc,def,123].

extract_atomics(String,ListOfAtomics) :-
	remove_initial_blanks(String,NewString),
	extract_atomics_aux(NewString,ListOfAtomics).

extract_atomics_aux([C|Chars],[A|Atomics]) :-
	extract_word([C|Chars],Rest,Word),
	string_to_atomic(Word,A),       % <- this is the only change
	extract_atomics(Rest,Atomics).

extract_atomics_aux([],[]).


/*****************************************************************************/
% clean_string(+String,-Cleanstring)
%  removes all punctuation characters from String and return Cleanstring

clean_string([C|Chars],L) :- 
	char_type(C,punctuation),
	clean_string(Chars,L), !.
clean_string([C|Chars],[C|L]) :-
	clean_string(Chars,L), !.
clean_string([C|[]],[]) :-
	char_type(C,punctuation), !.
clean_string([C|[]],[C]).


/*****************************************************************************/
% read_atomics(-ListOfAtomics)
%  Reads a line of input, removes all punctuation characters, and converts
%  it into a list of atomic terms, e.g., [this,is,an,example].

read_atomics(ListOfAtomics) :-
	read_lc_string(String),
	clean_string(String,Cleanstring),
	extract_atomics(Cleanstring,ListOfAtomics).


/****************************************************************************/
% isalist(+List)
%    checks if List is actually a list

isalist([_|_]).


/****************************************************************************/
% member(?Element,+List)
%    checks if Element is in List

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).


/****************************************************************************/
% append(?List1, ?List2, ?List3)
%    appends List2 on the end of List1 and returns it as List3

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).


/****************************************************************************/
% flatten(+List,-FlatList)
%    flattens List with sublists into FlatList

flatten([],[]).
flatten([H|T],[H|T2]) :- \+ isalist(H),
                         flatten(T,T2).
flatten([H|T],L) :- isalist(H),
                    flatten(H,A),
                    flatten(T,B),
                    append(A,B,L).


/****************************************************************************/
% last_member(-Last,+List)
%    returns the last element of List in Last

last_member(End,List) :- append(_,[End],List).


/****************************************************************************/
% findnth(+List,+Number,-Element)
%    returns the Nth member of List in Element

findnth([E|_],1,E).
findnth([_|T],N,T1) :- V is N - 1,
                       findnth(T,V,T1).


/****************************************************************************/
% replace(+Element1,+List1,+Element2,-List2)
%    replaces all instances of Element1 in List1 with Element2 and returns
%       the new list as List2
%    does not replace variables in List1 with Element1

replace(_,[],_,[]).
replace(X,[H|T],A,[A|T2]) :- nonvar(H), H = X, !, replace(X,T,A,T2).
replace(X,[H|T],A,[H|T2]) :- replace(X,T,A,T2).


/****************************************************************************/
% simplify(+List,-Result)
%   implements non-overlapping simplification
%   simplifies List into Result

simplify(List,Result) :- sr(List,Result,X,Y), !,
			 simplify(X,Y).
simplify([W|Words],[W|NewWords]) :- simplify(Words,NewWords).
simplify([],[]).


/****************************************************************************/
% match(+MatchRule,+InputList)
%    matches the MatchRule with the InputList. If they match, the variables
%    in the MatchRule are instantiated to one of three things:
%       an empty list
%       a single word
%       a list of words

match(A,C) :- match_aux1(A,C),!.
match(A,C) :- match_aux2(A,C).

match_aux1(A,C) :-
	member([*|T],A),
	nonvar(T),
	member(Tm,T),
	nonvar(Tm),
	replace([*|T],A,Tm,B),
	match_aux2(B,C),
	!, last_member(L,T), L = Tm.

match_aux2([],[]).
match_aux2([Item|Items],[Word|Words]) :-
	match_aux3(Item,Items,Word,Words),!.
match_aux2([Item1,Item2|Items],[Word|Words]) :-
	var(Item1),
	nonvar(Item2),
	Item2 == Word,!,
	match_aux2([Item1,Item2|Items],[[],Word|Words]).
match_aux2([Item1,Item2|Items],[Word|Words]) :-
	var(Item1),
	var(Item2),!,
	match_aux2([Item1,Item2|Items],[[],Word|Words]).
match_aux2([[]],[]).

match_aux3(Word,Items,Word,Words) :-
	match_aux2(Items,Words), !.
match_aux3([Word|Seg],Items,Word,Words0) :-
	append(Seg,Words1,Words0),
	match_aux2(Items,Words1).


/****************************************************************************/
% makecomment(+KeyWordList,+InputList,-Comment)
%    returns ELIZA's Comment to the InputList based on the KeyWordList
%    takes care of special keywords 'your', and 'memory', which require
%       additional processing before a comment can be generated

makecomment([[your,2]|T],InputList,Comment) :-
	assertz(mem(InputList)),
	rules([[your,2],Reassembly]),
	mc_aux([[your,2]|T],Reassembly,InputList,Comment),!.
makecomment([[memory,0]|T],_,Comment) :-
	retract(mem(I2)),
	retractall(mem(I2)),
	rules([[memory,0],Reassembly]),
	mc_aux([[memory,0]|T],Reassembly,I2,Comment),!.
makecomment([[memory,0]|T],InputList,Comment) :-
	\+ retract(mem(_)),!,
	makecomment(T,InputList,Comment).
makecomment([Keyword|T],InputList,Comment) :-
	rules([Keyword,Reassembly]),
	mc_aux([Keyword|T],Reassembly,InputList,Comment),!.
makecomment([_|T],InputList,Comment) :-
	makecomment(T,InputList,Comment),!.


mc_aux(KeyWordList,[[DRuleNum,MatchRule,N|T]|_],InputList,Comment) :-
	match(MatchRule,InputList),
	mc_aux2(KeyWordList,DRuleNum,N,T,InputList,Comment),!.
mc_aux(KeyWordList,[_|T],InputList,Comment) :-
	mc_aux(KeyWordList,T,InputList,Comment).
mc_aux(_,[],_,_) :- !,fail.

mc_aux2(KeyWordList,DRuleNum,N,T,InputList,Comment) :-
	length(T,TLen),
	N < TLen, !,
	NewN is N + 1,
	findnth(T,NewN,Mn),
	mc_aux3(KeyWordList,DRuleNum,N,NewN,Mn,InputList,Comment).
mc_aux2(KeyWordList,DRuleNum,N,T,InputList,Comment) :-
	member(Mn,T),
	mc_aux3(KeyWordList,DRuleNum,N,0,Mn,InputList,Comment).


mc_aux3([Keyword|T],DRuleNum,N,NewN,[equal,MnT],InputList,Comment) :-
	!,
	updaterule(Keyword,DRuleNum,N,NewN),
	makecomment([MnT|T],InputList,Comment).
mc_aux3([Keyword|T],DRuleNum,N,NewN,[newkey],InputList,Comment) :-
	!,
	updaterule(Keyword,DRuleNum,N,NewN),
	makecomment(T,InputList,Comment).
mc_aux3([Keyword|_],DRuleNum,N,NewN,Mn,_,Mn) :-
	updaterule(Keyword,DRuleNum,N,NewN).


/****************************************************************************/
% process_input(+Input_List,+[],?Output)
%     returns part of input after a comma, or
%             part of input before a period

process_input([],L,L).
process_input(['.'|_],L,L) :- findkeywords(L,K), length(K,Kl), Kl >= 3,!.
process_input(['.'|T],_,L) :- !, process_input(T,[],L).
process_input([','|_],L,L) :- findkeywords(L,K), length(K,Kl), Kl >= 3,!.
process_input([','|T],_,L) :- !, process_input(T,[],L).
process_input([H|T],S,L) :- append(S,[H],S2), process_input(T,S2,L).


/****************************************************************************/
% findkeywords(+InputList,?KeyWordList)
%    returns a list with the keywords in the input list
%    if no keywords are found returns a list with keywords 'memory' and 'none'

findkeywords([],[[memory,0],[none,0]]).
findkeywords([H|T],[[H,I]|T1]) :- rules([[H,I]|_]), !, findkeywords(T,T1).
findkeywords([_|T],T1) :- findkeywords(T,T1).


/****************************************************************************/
% sortkeywords(+KeyWordList,?SortedList)
%    returns a list with the keywords sorted according to their importance
%    this routine implements a simple bubble sort, customized for this
%    application

sortkeywords(X,Y) :- sort_aux(X,A,1), !, sortkeywords(A,Y).
sortkeywords(X,Y) :- sort_aux(X,Y,_).

sort_aux([],[],0).
sort_aux([X],[X],0).
sort_aux([[A,X],[B,Y]|T],[[B,Y],[A,X]|T],1) :- X < Y.
sort_aux([X,Y|T],[X|T2],S) :- sort_aux([Y|T],T2,S).


/****************************************************************************/
% updaterule(+KeyList,+DRuleNum,+N,+NewN)
%    updates a rule by changing the number of the reassembly rule associated
%       with a decomposition rule. The main rule to modify is indicated by
%       KeyList. The decomposition rule within the main rule is indicated by
%       DRuleNum. N is the previous reassembly rule used. NewN is the new
%       one used. N is updated to NewN so that next time a different reassembly
%       (actually the next in sequence) in used.

updaterule(KeyList,DRuleNum,N,NewN) :-
	retract(rules([KeyList,Rt])),
	replace([DRuleNum,A,N|T],Rt,[DRuleNum,A,NewN|T],Rt2),
	assertz(rules([KeyList,Rt2])).


/****************************************************************************/
% writecomment(+CommentList)
%    prints the elements of CommentList. First Characater of first element is
%       converted to uppercase befor printing

writecomment([]).
writecomment(['I'|T]) :- !, write('I'), writecomment_aux(T).
writecomment([H|T]) :- !,
	name(H,[C|L]),
	D is C - 32,
        name(Z,[D|L]),
	write(Z),
	writecomment_aux(T).

writecomment_aux([]).
writecomment_aux([H|T]) :- 
	name(H,[C]),
	char_type(C,punctuation), !,
	write(H),
	writecomment_aux(T).
writecomment_aux([H|T]) :- 
	write(' '),
	write(H),
	writecomment_aux(T).


/****************************************************************************/
% quittime(+InputList)
%    checks if the atom 'quit' is in the InputList

quittime(X) :- member('quit',X).


/****************************************************************************/
% eliza
%    main routine of ELIZA

eliza :-
%	reconsult('eliza.rls'),
	retractall(mem(_)),nl,nl,
        write('Hello. I am ELIZA. How can I help you?'),nl,write('> '),
	repeat,
	   read_atomics(Input),nl,
           process_input(Input,[],Input2),
           simplify(Input2,Input3),
           findkeywords(Input3,KeyWords),
           sortkeywords(KeyWords,KeyWords2),
           makecomment(KeyWords2,Input3,Comment),
           flatten(Comment,Comment2),
           writecomment(Comment2),nl,write('> '),
           quittime(Input3),
           !.

main :- eliza.



