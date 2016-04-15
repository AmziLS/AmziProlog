% Clam - expert system shell with EMYCIN type certainty factors

% This system is an imitation of the EMYCIN imitators.  It does backward
% chaininging (goal directed) inference with uncertainty.  The uncertainty
% is modelled using the MYCIN certainty factors.

% The only data structure is an attribute:value pair.

% NOTE - CF calculation in update only good for positive CF

main :-
	do_over,
	super.

% The main command loop

super :-
	repeat,
	write('consult  restart  load  list  trace on/off  how  exit'),nl,
	write('> '),
	read_line([X|Y]),
	doit([X|Y]),
	X == exit.

doit([consult]) :- top_goals,!.
doit([restart]) :- do_over,!.
doit([load]) :- load_rules,!.
doit([list]) :- list_facts,!.
doit([trace,X]) :- set_trace(X),!.
doit([how|Y]) :- how(Y),!.
doit([exit]).
doit([X|Y]) :-
	write('invalid command : '),
	write([X|Y]),nl.

% top_goals works through each of the goals in sequence

top_goals :-
	ghoul(Attr),
	top(Attr),
	print_goal(Attr),
	fail.
top_goals.

% top starts the backward chaining by looking for rules that reference
% the attribute in the RHS.  If it is known with certainty 100, then
% no other rules are tried, and other candidates are eliminated.  Otherwise
% other rules which might yield different values for the attribute 
% are tried as well

top(Attr) :-
	findgoal(av(Attr,Val),CF,[goal(Attr)]),!.
top(_) :- true.

% prints all hypotheses for a given attribute

print_goal(Attr) :-
	nl,
	fact(av(Attr,X),CF,_),
	CF >= 20,
	outp(av(Attr,X),CF),nl,
	fail.
print_goal(Attr) :-write('done with '),write(Attr),nl,nl.

outp(av(A,V),CF) :-
	output(A,V,PrintList),
	pretty(av(A,V), X),
	printlist(X),
	tab(1),write(cf(CF)),write(': '),
	printlist(PrintList),!.
outp(av(A,V),CF) :-
	pretty(av(A,V), X),
	printlist(X),
	tab(1),write(cf(CF)).

printlist([]).
printlist([H|T]) :-
	write(H),tab(1),
	printlist(T).

% findgoal is the guts of the inference.  It copes with already known
% attribute value pairs, multivalued attributes and single valued
% attributes.  It uses the EMYCIN certainty factor arithmetic to
% propagate uncertainties.

% 1 - if its recorded and the value matches, we're done, if the
%     value doesn't match, but its single valued and known with
%     certainty 100 definitely fail

findgoal(X,Y,_) :- bugdisp(['  ',X]),fail.

findgoal(not Goal,NCF,Hist) :-
	findgoal(Goal,CF,Hist),
	NCF is - CF, !.
findgoal(Goal,CF,Hist) :-
	fact(Goal,CF,_), !.
%findgoal(av(Attr,Val),CF) :-
%	bound(Val),
%	fact(av(Attr,V,_),CF),
%	Val \= V,
%	single_valued(Attr),
%	CF=100,
%	!,fail.

% 2 - if its askable, just ask and record the answer

findgoal(Goal,CF,Hist) :-
	can_ask(Goal,Hist),
	!,
	findgoal(Goal,CF,Hist).

% 3 - find a rule with the required attribute on the RHS.  try to prove
%     the LHS.  If its proved, use the certainty of the LHS combined
%     with the certainty of the RHS to compute the cf of the derived
%     result

findgoal(Goal,CurCF,Hist) :-
	fg(Goal,CurCF,Hist).
	
fg(Goal,CurCF,Hist) :-
	rule(N, lhs(IfList), rhs(Goal,CF)),
	bugdisp(['call rule',N]),
	prove(N,IfList,Tally,Hist),
	bugdisp(['exit rule',N]),
	adjust(CF,Tally,NewCF),
	update(Goal,NewCF,CurCF,N),
	CurCF == 100,!.
fg(Goal,CF,_) :- fact(Goal,CF,_).

% can_ask shows how to query the user for various types of goal patterns

can_ask(av(Attr,Val),Hist) :-
	not asked(av(Attr,_)),
	askable(Attr,Menu,Edit,Prompt),
	query_user(Attr,Prompt,Menu,Edit,Hist),
	asserta( asked(av(Attr,_)) ).

% answer the how question at the top level, to explain how an answer was
% derived.  It can be called successive times to get the whole proof.

how([]) :-
	write('Goal? '),read_line(X),nl,
	pretty(Goal,X),
	how(Goal).
how(X) :-
	pretty(Goal,X),
	nl,
	how(Goal).

how(not Goal) :-
	fact(Goal,CF,Rules),
	CF < -20,
	pretty(not Goal,PG),
	write_line([PG,was,derived,from,'rules: '|Rules]),
	nl,
	list_rules(Rules),
	fail.	
how(Goal) :-
	fact(Goal,CF,Rules),
	CF > 20,
	pretty(Goal,PG),
	write_line([PG,was,derived,from,'rules: '|Rules]),
	nl,
	list_rules(Rules),
	fail.
how(_).

list_rules([]).
list_rules([R|X]) :-
	list_rule(R),
%	how_lhs(R),
	list_rules(X).

list_rule(N) :-
	rule(N, lhs(Iflist), rhs(Goal,CF)),
	write_line(['rule  ',N]),
	write_line(['  If']),
	write_ifs(Iflist),
	write_line(['  Then']),
	pretty(Goal,PG),
	write_line(['   ',PG,CF]),nl.

write_ifs([]).
write_ifs([H|T]) :-
	pretty(H,HP),
	tab(4),write_line(HP),
	write_ifs(T).

pretty(av(A,yes),[A]) :- !.
pretty(not av(A,yes), [not,A]) :- !.
pretty(av(A,no),[not,A]) :- !.
pretty(not av(A,V),[not,A,is,V]).
pretty(av(A,V),[A,is,V]).

how_lhs(N) :-
	rule(N, lhs(Iflist), _),
	!, how_ifs(Iflist).
	
how_ifs([]).
how_ifs([Goal|X]) :-
	how(Goal),
	how_ifs(X).
	
% get input from the user.  either a straight answer from the menu, or
% an answer with cf N appended to it.

query_user(Attr,Prompt,[yes,no],_,Hist) :-
	!,
	write(Prompt),nl,
	get_user(X,Hist),
	get_vcf(X,Val,CF),
	asserta( fact(av(Attr,Val),CF,[user]) ).
query_user(Attr,Prompt,Menu,Edit,Hist) :-
	write(Prompt),nl,
	menu_read(VList,Menu,Hist),
	assert_list(Attr,VList).

menu_read(X,Menu,Hist) :-
	write_list(2,Menu),
	get_user(X,Hist).

get_user(X,Hist) :-
	repeat,
	write(': '),
	read_line(X),
	process_ans(X,Hist).

process_ans([why],Hist) :- nl,write_hist(Hist), !, fail.
process_ans(X,_).	

write_hist([]) :- nl.
write_hist([goal(X)|T]) :-
	write_line([goal,X]),
	!, write_hist(T).
write_hist([N|T]) :-
	list_rule(N),
	!, write_hist(T).

write_list(N,[]).
write_list(N,[H|T]) :-
	tab(N),write(H),nl,
	write_list(N,T).

assert_list(_,[]).
assert_list(Attr,[not,Val,cf,CF|X]) :-
	!,
	NCF is - CF,
	asserta( fact(av(Attr,Val),NCF,[user]) ),
	assert_list(Attr,X).
assert_list(Attr,[not,Val|X]) :-
	!,
	asserta( fact(av(Attr,Val),-100,[user]) ),
	assert_list(Attr,X).
assert_list(Attr,[Val,cf,CF|X]) :-
	!,
	asserta( fact(av(Attr,Val),CF,[user]) ),
	assert_list(Attr,X).
assert_list(Attr,[Val|X]) :-
	asserta( fact(av(Attr,Val),100,[user]) ),
	assert_list(Attr,X).

get_vcf([no],yes,-100).
get_vcf([no,CF],yes,NCF) :- NCF is -CF.
get_vcf([no,cf,CF],yes,NCF) :- NCF is -CF.
get_vcf([Val,CF],Val,CF).
get_vcf([Val,cf,CF],Val,CF).
get_vcf([Val],Val,100).
get_vcf([not,Val],Val,-100).
get_vcf([not,Val,CF],Val,NCF) :- NCF is -CF.
get_vcf([not,Val,cf,CF],Val,NCF) :- NCF is -CF.

% prove works through a LHS list of premises, calling findgoal on
% each one.  the total cf is computed as the minimum cf in the list

prove(N,IfList,Tally,Hist) :-
	prov(IfList,100,Tally,[N|Hist]),!.
prove(N,_,_) :-
	bugdisp(['fail rule',N]),
	fail.

prov([],Tally,Tally,Hist).
prov([H|T],CurTal,Tally,Hist) :-
	findgoal(H,CF,Hist),
	minimum(CurTal,CF,Tal),
	Tal >= 20,
	prov(T,Tal,Tally,Hist).

% update - if its already known with a given cf, here is the formula
% for adding in the new cf.  this is used in those cases where multiple
% RHS reference the same attr :val

update(Goal,NewCF,CF,RuleN) :-
	fact(Goal,OldCF,_),
	combine(NewCF,OldCF,CF),
	retract( fact(Goal,OldCF,OldRules) ),
	asserta( fact(Goal,CF,[RuleN | OldRules]) ),
	(CF == 100, single_valued(Attr), erase_other(Attr);
	 true),!.
update(Goal,CF,CF,RuleN) :-
	asserta( fact(Goal,CF,[RuleN]) ).

erase_other(Attr) :-
	fact(av(Attr,Val),CF,_),
	CF < 100,
	retract( fact(av(Attr,Val),CF,_) ),
	fail.
erase_other(Attr) :-true.

adjust(CF1,CF2,CF) :-
	X is CF1 * CF2 / 100,
	int_round(X,CF).

combine(CF1,CF2,CF) :-
	CF1 >= 0,
	CF2 >= 0,
	X is CF1 + CF2*(100 - CF1)/100,
	int_round(X,CF).
combine(CF1,CF2,CF) :-
	CF1 < 0,
	CF2 < 0,
	X is - ( -CF1 -CF2 * (100 + CF1)/100),
	int_round(X,CF).
combine(CF1,CF2,CF) :-
	(CF1 < 0; CF2 < 0),
	(CF1 > 0; CF2 > 0),
	abs_minimum(CF1,CF2,MCF),
	X is 100 * (CF1 + CF2) / (100 - MCF),
	int_round(X,CF).

abs_minimum(A,B,X) :-
	absolute(A, AA),
	absolute(B, BB),
	minimum(AA,BB,X).

absolute(X, X) :- X >= 0.
absolute(X, Y) :- X < 0, Y is -X.

%minimum(A,B,A) :- A =< B.
%minimum(A,B,B) :- B > A.

%min([],X,X).
%min([H|T],Z,X) :-
%	H < Z,
%	min(T,H,X).
%min([H|T],Z,X) :-
%	H >= Z,
%	min(T,Z,X).

minimum(X,Y,X) :- X =< Y,!.
minimum(X,Y,Y) :- Y =< X.

int_round(X,I) :-
	X >= 0,
	I is integer(X + 0.5).
int_round(X,I) :-
	X < 0,
	I is integer(X - 0.5).

set_trace(off) :-
	ruletrace,
	retract( ruletrace ).
set_trace(on) :-
	not ruletrace,
	asserta( ruletrace ).
set_trace(_).

single_valued(A) :-multivalued(A),!,fail.
single_valued(A) :-true.

list_facts :-
	fact(X,Y,_),
	write(fact(X,Y)),nl,
	fail.
list_facts :-true.

do_over :-
	abolish(asked,1),
	abolish(fact,3).

clear :-
	abolish(asked,1),
	abolish(fact,3),
	abolish(rule,1),
	abolish(multivalued,1),
	abolish(askable,1),
	abolish(ghoul,1).
	
blank_lines(0).
blank_lines(N) :-
	nl,
	NN is N - 1,
	blank_lines(NN).

bugdisp(L) :-
	ruletrace,
	write_line(L), !.
bugdisp(_).

write_line(L) :-
	flatten(L,LF),
	write_lin(LF).
	
write_lin([]) :- nl.
write_lin([H|T]) :-
	write(H), tab(1),
	write_lin(T).

flatten([],[]) :- !.
flatten([[]|T],T2) :-
	flatten(T,T2), !.
flatten([[X|Y]|T], L) :-
	flatten([X|[Y|T]],L), !.
flatten([H|T],[H|T2]) :-
	flatten(T,T2).                   

member(X,[X|Y]).
member(X,[Y|Z]) :- member(X,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LDRULS - this module reads a rule file and translates it to internal
%          Prolog format for the Clam shell

load_rules :-
	write('Enter file name in single quotes (ex. ''car.ckb''.): '),
	read(F),
	load_rules(F).

load_rules(F) :-
	clear_db,
	see(F),
	lod_ruls,
	write('rules loaded'),nl,
	seen, !.

lod_ruls :-
	repeat,
	read_sentence(L),
%	bug(L),
	process(L),
	L == [end_of_file].

process([end_of_file]) :- !.
process(L) :-
	trans(R,L,[]),
	bug(R),
	assertz(R), !.
process(L) :-
	write('trans error on:'),nl,
	write(L),nl.

clear_db :-
	abolish(cf_model,1),
	abolish(ghoul,1),
	abolish(askable,4),
	abolish(output,3),
	abolish(rule,3).

bug(cf_model(X)) :- write(cf_model(X)),nl,!.
bug(ghoul(X)):- write(ghoul(X)),nl,!.
bug(askable(A,_,_,_)):- write('askable '),write(A),nl,!.
bug(output(A,V,PL)):- write('output '),write(V),nl,!.
bug(rule(N,_,_)):- write('rule '),write(N),nl,!.
bug(X) :- write(X),nl.

% trans - translates a list of atoms in external rule form to internal
%         rule form

trans(cf_model(X)) --> [cf,model,X].
trans(cf_model(X)) --> [cf,model,is,X].
trans(cf_model(X)) --> [cf,X].
trans(ghoul(X)) --> [goal,is,X].
trans(ghoul(X)) --> [goal,X].
trans(askable(A,M,E,P)) --> 
	[ask,A],menux(M),editchk(E),prompt(A,P).
trans(output(A,V,PL)) --> 
	[output],phraz(av(A,V)),plist(PL). 
trans(rule(N,lhs(IF),rhs(THEN,CF))) --> id(N),if(IF),then(THEN,CF).
trans(multivalued(X)) --> [multivalued,X].
trans('Parsing error'-L,L,_).

% default(D) -->  [default,D].
% default(none) -->  [].

menux(M) -->  [menu,'('], menuxlist(M).

menuxlist([Item]) -->  [Item,')'].
menuxlist([Item|T]) -->  [Item],menuxlist(T).

editchk(E) -->  [edit,E].
editchk(none) -->  [].

prompt(_,P) -->  [prompt,P].
prompt(P,P) -->  [].

id(N) --> [rule,N].

if(IF) --> [if],iflist(IF).

iflist([IF]) --> phraz(IF),[then].
iflist([Hif|Tif]) --> phraz(Hif),[and],iflist(Tif).
iflist([Hif|Tif]) --> phraz(Hif),[','],iflist(Tif).

then(THEN,CF) --> phraz(THEN),[cf],[CF].
then(THEN,100) --> phraz(THEN).

phraz(not av(Attr,yes)) --> [not,Attr].
phraz(not av(Attr,yes)) --> [not,a,Attr].
phraz(not av(Attr,yes)) --> [not,an,Attr].
phraz(not av(Attr,Val)) --> [not,Attr,is,Val].
phraz(not av(Attr,Val)) --> [not,Attr,are,Val].
phraz(av(Attr,Val)) --> [Attr,is,Val].
phraz(av(Attr,Val)) --> [Attr,are,Val].
phraz(av(Attr,yes)) --> [Attr].

plist([Text]) --> [Text].
plist([Htext|Ttext]) --> [Htext],plist(Ttext).

%%
%% end LDRULS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                                                                

read_line(L) :- read_word_list([13,10], L), !.

read_sentence(S) :- read_word_list([0'.], S), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% From the Cogent Prolog Toolbox
%% 
%% rwl.pro - read word list, based on Clocksin & Mellish
%%
%% Read word list reads in a list of chars (terminated with a !, . or ?)
%% and converts it to a list of atomic entries (including numbers).
%% Uppercase is converted to lower case.
%% A 'word' is one item in our generated list

%% This version has been modified for CLAM by allowing an additional
%% argument, Xs, that is a list of the ending characters.  This allows the
%% code to be used for both command input, terminated by the Enter key, and
%% reading the knowledge base files, terminated after multiple lines by
%% a period.

%% It has further been modified to skip everything between a % and the
%% end of line, allowing for Prolog style comments.

read_word_list(LW,[W|Ws]) :-
        get0(C),
        readword(C, W, C1),        % Read word starting with C, C1 is first new
        restsent(LW, C1, Ws).      % character - use it to get rest of sentence

restsent(_, end_of_file, []).
restsent(LW,C,[]) :-				     % Nothing left if hit last-word marker
        member(C,LW), !.
restsent(LW,C,[W1|Ws]) :-
        readword(C,W1,C1),         % Else read next word and rest of sentence
        restsent(LW,C1,Ws).

readword(end_of_file,end_of_file,end_of_file).
readword(0'%,W,C2) :-               % allow Prolog style comments
        !,
        skip(13),
        get0(C1),
        readword(C1,W,C2).
readword(0'',W,C2) :-
        !,
        get0(C1),
        to_next_quote(C1,Cs),
        name(W, [0''|Cs]),
        get0(C2).        
readword(C,W,C1) :-                % Some words are single characters
        single_char(C),            % i.e. punctuation
        !, 
        name(W, [C]),              % get as an atom
        get0(C1).
readword(C, W, C1) :-
        is_num(C),                 % if we have a number --
        !,
        number_word(C, W, C1, _).  % convert it to a genuine number
readword(C,W,C2) :-                % otherwise if charcter does not
        in_word(C, NewC),          % delineate end of word - keep
        get0(C1),                  % accumulating them until 
        restword(C1,Cs,C2),        % we have all the words
        name(W, [NewC|Cs]).        % then make it an atom
readword(C,W,C2) :-                % otherwise
        get0(C1),       
        readword(C1,W,C2).         % start a new word

restword(C, [NewC|Cs], C2) :-
        in_word(C, NewC),
        get0(C1),
        restword(C1, Cs, C2).
restword(C, [], C).

to_next_quote(0'', [0'']).
to_next_quote(C,[C|Rest]) :-
        get0(C1),
        to_next_quote(C1,Rest).

single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).
single_char(0'().
single_char(0')).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, C) :- C >= 0'A, C =< 0'Z.
in_word(0'-, 0'-).
in_word(0'_, 0'_).

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

% These symbols delineate end of sentence

%lastword(0'.).
%lastword(0'!).
%lastword(0'?).
%lastword(13).		% carriage return
%lastword(10).		% line feed

%%
%% end RWL.PRO from Cogent Prolog Toolbox
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

