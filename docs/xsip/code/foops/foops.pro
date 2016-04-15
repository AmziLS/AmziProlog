% FOOPS - an integration of frames, forward chaining with LEX and MEA,
% and Prolog.
% Copyright (c) Dennis Merritt, 1986 - Permission granted for 
% non-commercial use

% The first section of the code contains the basic OOPS code, the
% second section contains the FRAMES code.

% operator definitions

:-op(800,xfx,==>).          % used to separate LHS and RHS of rule
:-op(500,xfy,:).            % used to separate attributes and values
:-op(810,fx,rule).          % used to define rule
:-op(700,xfy,#).            % used for unification instead of =
:-op(700,xfy,\=).	          % not equal
:-op(600,xfy,with).		    % used for frame instances in rules

main :- welcome, supervisor.

welcome  :-
	write($FOOPS - A Toy Production System$),nl,nl,
	write($This is an interpreter for files containing rules coded in the$),nl,
	write($FOOPS format.$),nl,nl,
	write($The => prompt accepts four commands:$),nl,nl,
	write($   load. -  prompts for name of rules file$),nl,
	write($            enclose in single quotes$),nl,
	write($   go.   -  starts the inference$),nl,
	write($   list. -  list working memory$),nl,
	write($   exit. -  does what you'd expect$),nl,nl.

% the supervisor, uses a repeat fail loop to read and process commands
% from the user

supervisor :-
	repeat,
	write('=>'),
	read(X),
	doit(X),
	X = exit.

doit(X) :- do(X).

% actions to take based on commands

do(exit) :- !.
do(go) :-
	initialize,
	timer(T1),
	go,
	timer(T2),
	T is 10 * (T2 - T1),
	write(time-T),nl,!.
do(load) :- load, !.
do(list) :- lst, !.       % lists all of working storage
do(list(X)) :- lst(X), !. % lists all which match the pattern
do(_) :- write('invalid command'),nl.

% loads the rules (Prolog terms) into the Prolog database

load :-
	write('Enter the file name in single quotes (ex. ''room.fkb''.): '),
	read(F),
	reconsult(F).            % loads a rule file into interpreter work space

% assert each of the initial conditions into working storage

initialize :-
	setchron(1),
	abolish(instantiation,1),
	delf(all),
	assert(mea(no)),
	assert(gid(100)),
	initial_data(X),
	assert_list(X), !.
initialize :-
	error(301,[initialization,error]).

% working storage is represented by database terms stored
% under the key "fact"

assert_list([]) :- !.
assert_list([H|T]) :-
	getchron(Time),
	assert_ws( fact(H,Time) ),
	!,assert_list(T).

% the main inference loop, find a rule and try it.  if it fired, say so
% and repeat the process.  if not go back and try the next rule.  when
% no rules succeed, stop the inference

go :-
	conflict_set(CS),
	write_cs(CS),
	select_rule(CS,r(Inst,ID,LHS,RHS)),
	write($Rule Selected $),write(ID),nl,
	(process(RHS,LHS); true),
	asserta( instantiation(Inst) ),
	write($Rule fired $),write(ID),nl,
	!,go.
go.

write_cs([]).
write_cs([r(I,ID,L,R)|X]) :-
	write(ID),nl,
	writeinst(I),
	write_cs(X).

writeinst([]).
writeinst([H|T]) :-
	tab(5),
	write(H),nl,
	writeinst(T).

conflict_set(CS) :-
	bagof(r(Inst,ID,LHS,RHS),
		(rule ID: LHS ==> RHS, match(LHS,Inst)), CS).

select_rule(CS,R) :-
	refract(CS,CS1),
	mea_filter(0,CS1,[],CSR),
	lex_sort(CSR,R).

list_cs([]).
list_cs([K-r(_,ID,_,_)|T]) :-
	write(ID-K),nl,
	list_cs(T).

% eliminate those rules which have already been tried

refract([],[]).
refract([r(Inst,_,_,_)|T],TR) :-
	instantiation(Inst), 
	!, refract(T,TR).
refract([H|T],[H|TR]) :-
	refract(T,TR).

% sort the rest of the conflict set according to the lex strategy

lex_sort(L,R) :-
	build_keys(L,LK),
%	keysort(LK,X),
	sort(LK,X),
	reverse(X,[K-R|_]).

% build lists of time stamps for lex sort keys

build_keys([],[]).
build_keys([r(Inst,A,B,C)|T],[Key-r(Inst,A,B,C)|TR]) :-
	build_chlist(Inst,ChL),
	sort(ChL,X),
	reverse(X,Key),
	build_keys(T,TR).

% build a list of just the times of the various matched attributes
% for use in rule selection

build_chlist([],[]).
build_chlist([_/Chron|T],[Chron|TC]) :-
	build_chlist(T,TC).	

% add the test for mea if appropriate that emphasizes the first attribute
% selected.

mea_filter(_,X,_,X) :- not mea(yes), !.
mea_filter(_,[],X,X).
mea_filter(Max,[r([A/T|Z],B,C,D)|X],Temp,ML) :-
	T < Max,
	!, mea_filter(Max,X,Temp,ML).
mea_filter(Max,[r([A/T|Z],B,C,D)|X],Temp,ML) :-
	T = Max,
	!, mea_filter(Max,X,[r([A/T|Z],B,C,D)|Temp],ML).
mea_filter(Max,[r([A/T|Z],B,C,D)|X],Temp,ML) :-
	T > Max,
	!, mea_filter(T,X,[r([A/T|Z],B,C,D)],ML).

% recursively go through the LHS list, matching conditions against
% working storage

match([],[]).
match([Prem|Rest],[Prem/Time|InstRest]) :-
	mat(Prem,Time),
	match(Rest,InstRest).

mat(N:Prem,Time) :-
	!,fact(Prem,Time).
mat(Prem,Time) :-
	fact(Prem,Time).
mat(Test,0) :-
	test(Test).
	
fact(Prem,Time) :-
	conv(Prem,Class,Name,ReqList),
	getf(Class,Name,ReqList,Time).

assert_ws( fact(Prem,Time) ) :-
	conv(Prem,Class,Name,UList),
	addf(Class,Name,UList).

update_ws( fact(Prem,Time) ) :-
	conv(Prem,Class,Name,UList),
	uptf(Class,Name,UList).

retract_ws( fact(Prem,Time) ) :-
	conv(Prem,Class,Name,UList),
	delf(Class,Name,UList).

conv(Class-Name with List, Class, Name, List).
conv(Class-Name, Class, Name, []).

% various tests allowed on the LHS

test(not(X)) :-
	fact(X,_),
	!,fail.
test(not(X)) :- !.
test(X#Y) :- X=Y,!.
test(X>Y) :- X>Y,!.
test(X>=Y) :- X>=Y,!.
test(X<Y) :- X<Y,!.
test(X=<Y) :- X=<Y,!.
test(X \= Y) :- not X=Y, !.
%test(X = Y) :- X=Y, !.
test(X = Y) :- X is Y,!.
test(is_on(X,Y)) :- is_on(X,Y),!.
test(call(X)) :- call(X).

% recursively execute each of the actions in the RHS list

process([],_) :- !.
process([Action|Rest],LHS) :-
	take(Action,LHS),
	!,process(Rest,LHS).
process([Action|Rest],LHS) :-
	error(201,[Action,fails]).

% if its retract, use the reference numbers stored in the Lrefs list,
% otherwise just take the action

take(retract(N),LHS) :-
	(N == all; integer(N)),
	retr(N,LHS),!.
take(A,_) :-take(A),!.

take(retract(X)) :- retract_ws(fact(X,_)), !.
take(assert(X)) :-
	getchron(T),
	assert_ws(fact(X,T)),
	write(adding-X),nl,
	!.
take(update(X)) :-
	getchron(T),
	update_ws(fact(X,T)),
	write(updating-X),nl,
	!.
take(X # Y) :- X=Y,!.
take(X = Y) :- X is Y,!.
take(write(X)) :- write(X),!.
take(write_line(X)) :- write_line(X),!.
take(nl) :- nl,!.
take(read(X)) :- read(X),!.
take(prompt(X,Y)) :- nl,write(X),read(Y),!.
take(cls) :- cls, !.
take(is_on(X,Y)) :- is_on(X,Y), !.
take(list(X)) :- lst(X), !.
take(call(X)) :- call(X).

% logic for retraction

retr(all,LHS) :-retrall(LHS),!.
retr(N,[]) :- error(202,['retract error, no ',N]), !.
retr(N,[N:Prem|_]) :- retract_ws(fact(Prem,_)),!.
retr(N,[_|Rest]) :- !,retr(N,Rest).

retrall([]).
retrall([N:Prem|Rest]) :-
	retract_ws(fact(Prem,_)),
	!, retrall(Rest).
retrall([Prem|Rest]) :-
	retract_ws(fact(Prem,_)),
	!, retrall(Rest).
retrall([_|Rest]) :-		% must have been a test
	retrall(Rest).

% list all of the terms in working storage

lst :-
	fact(X,_),
	write(X),nl,
	fail.
lst.

% lists all of the terms which match the pattern

lst(X) :-
	fact(X,_),
	write(X),nl,
	fail.
lst(_).

% utilities

member(X,[X|Y]).
member(X,[Y|Z]) :- member(X,Z).

reverse(F,R) :- rever(F,[],R).

rever([],R,R).
rever([X|Y],T,R) :- rever(Y,[X|T],R).

% maintain a time counter

setchron(N) :-
	retract( chron(_) ),
	asserta( chron(N) ),!.
setchron(N) :-
	asserta( chron(N) ).

getchron(N) :-
	retract( chron(N) ),
	NN is N + 1,
	asserta( chron(NN) ), !.
	
%
% this section implements a frame based scheme for knowledge representation
%

:- op(600,fy,val).
:- op(600,fy,calc).
:- op(600,fy,def).
:- op(600,fy,add).
:- op(600,fy,del).

% prep_req takes a request of the form Slot-Val, and forms it into the
% more accurate req(Class,Slot,Facet,Value).  If no facet was mentioned
% in the original request, then the facet of "any" is used to indicate
% the system should use everything possible to find a value.

prep_req(Slot-X,req(C,N,Slot,val,X)) :- var(X), !.
prep_req(Slot-X,req(C,N,Slot,Facet,Val)) :-
	nonvar(X),
	X =.. [Facet,Val],
	facet_list(FL),
	is_on(Facet,FL), !.
prep_req(Slot-X,req(C,N,Slot,val,X)).

facet_list([val,def,calc,add,del,edit]).


% retrieve a list of slot values

get_frame(Class, ReqList) :-
	frame(Class, SlotList),
	slot_vals(Class,_,ReqList,SlotList).

getf(Class,Name,ReqList) :-
	getf(Class,Name,ReqList,_).
	
getf(Class,Name,ReqList,TimeStamp) :-
	frinst(Class, Name, SlotList, TimeStamp),
	slot_vals(Class, Name, ReqList, SlotList).

slot_vals(_,_,ReqL,SlotL) :-
	var(ReqL),
	!,
	ReqL = SlotL.
slot_vals(_,_,[],_).
slot_vals(C,N,[Req|Rest],SlotList) :-
	prep_req(Req,req(C,N,S,F,V)),
	find_slot(req(C,N,S,F,V),SlotList), 
	!, slot_vals(C,N,Rest,SlotList).
slot_vals(C,N, Req, SlotList) :-
	not(list(Req)),
	prep_req(Req,req(C,N,S,F,V)),
	find_slot(req(C,N,S,F,V), SlotList).

find_slot(req(C,N,S,F,V), SlotList) :-
	nonvar(V), !,
	find_slot(req(C,N,S,F,Val), SlotList), !,
	(Val = V; list(Val),is_on(V,Val)).
find_slot(req(C,N,S,F,V), SlotList) :-
	is_on(S-FacetList, SlotList), !,
	facet_val(req(C,N,S,F,V),FacetList).
find_slot(req(C,N,S,F,V), SlotList) :-
	is_on(ako-FacetList, SlotList),
	facet_val(req(C,N,ako,val,Ako),FacetList),
	(is_on(X,Ako); X = Ako),
	frame(X, HigherSlots),
	find_slot(req(C,N,S,F,V), HigherSlots), !.
find_slot(Req,_) :-
	error(99,['frame error looking for:',Req]).

facet_val(req(C,N,S,F,V),FacetList) :-
	FV =.. [F,V],
	is_on(FV,FacetList), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(val ValList,FacetList),
	is_on(V,ValList), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(calc Pred,FacetList),
	CalcPred =.. [Pred,C,N,S-V],
	call(CalcPred), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(def V,FacetList), !.

% add a list of slot values

add_frame(Class, UList) :-
	old_slots(Class,SlotList),
	add_slots(Class,_,UList,SlotList,NewList),
	retract(frame(Class,_)),
	asserta(frame(Class,NewList)), !.

addf(Class,Nm,UList) :-
	(var(Nm),genid(Name);Name=Nm),
	add_slots(Class,Name,[ako-Class|UList],SlotList,NewList),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.

uptf(Class,Name,UList) :-
	frinst(Class,Name,SlotList,_),
	add_slots(Class,Name,UList,SlotList,NewList),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.
uptf(Class,Name,UList) :-
	error(105,[update,failed,Class,Name,UList]).

genid(G) :-
	retract(gid(N)),
	G is N + 1,
	asserta(gid(G)).

old_slots(Class,SlotList) :-
	frame(Class,SlotList), !.
old_slots(Class,[]) :-
	asserta(frame(Class,[])).

add_slots(_,_,[],X,X).
add_slots(C,N,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(C,N,S,F,V)),
	add_slot(req(C,N,S,F,V),SlotList,Z),
	!, add_slots(C,N,Rest,Z,NewList).
add_slots(C,N,X,SlotList,NewList) :-
	prep_req(X,req(C,N,S,F,V)),
	add_slot(req(C,N,S,F,V),SlotList,NewList).

add_slot(req(C,N,S,F,V),SlotList,[S-FL2|SL2]) :-
	delete(S-FacetList,SlotList,SL2),
	add_facet(req(C,N,S,F,V),FacetList,FL2).

add_facet(req(C,N,S,F,V),FacetList,[FNew|FL2]) :-
	FX =.. [F,OldVal],
	delete(FX,FacetList,FL2),
	add_newval(OldVal,V,NewVal),
	!, check_add_demons(req(C,N,S,F,V),FacetList),
	FNew =.. [F,NewVal].

add_newval(X,Val,Val) :- var(X), !.
add_newval(OldList,ValList,NewList) :-
	list(OldList),
	list(ValList),
	append(ValList,OldList,NewList), !.
add_newval([H|T],Val,[Val,H|T]).
add_newval(_,Val,Val).

check_add_demons(req(C,N,S,F,V),FacetList) :-
	get_frame(C,S-add(Add)), !,
	AddFunc =.. [Add,C,N,S-V],
	call(AddFunc).
check_add_demons(_,_).


% delete a list of slot values

del_frame(Class) :-
	retract(frame(Class,_)).
del_frame(Class) :-
	error(203,['No frame',Class,'to delete']).

del_frame(Class, UList) :-
	old_slots(Class,SlotList),
	del_slots(Class,_,UList,SlotList,NewList),
	retract(frame(Class,_)),
	asserta(frame(Class,NewList)).

delf(all) :-
	retract( frinst(_,_,_,_) ),
	fail.
delf(all).

delf(Class,Name) :-
	retract( frinst(Class,Name,_,_) ),
	!.
delf(Class,Name) :-
	error(103,['No instance of ',Class,' for ',Name]).

delf(Class,Name,UList) :-
	old_flots(Class,Name,SlotList),
	del_slots(Class,Name,UList,SlotList,NewList),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ).

del_slots(_,_,[],X,X).
del_slots(C,N,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(C,N,S,F,V)),
	del_slot(req(C,N,S,F,V),SlotList,Z),
	del_slots(C,N,Rest,Z,NewList).
del_slots(C,N,X,SlotList,NewList) :-
	prep_req(X,req(C,N,S,F,V)),
	del_slot(req(C,N,S,F,V),SlotList,NewList).

del_slot(req(C,N,S,F,V),SlotList,[S-FL2|SL2]) :-
	remove(S-FacetList,SlotList,SL2),
	del_facet(req(C,N,S,F,V),FacetList,FL2).
del_slot(Req,_,_) :-
	error(104,['del_slot - unable to remove',Req]).

del_facet(req(C,N,S,F,V),FacetList,FL) :-
	FV =.. [F,V],
	remove(FV,FacetList,FL),
	!, check_del_demons(req(C,N,S,F,V),FacetList).
del_facet(req(C,N,S,F,V),FacetList,[FNew|FL]) :-
	FX =.. [F,OldVal],
	remove(FX,FacetList,FL),
	remove(V,OldVal,NewValList),
	FNew =.. [F,NewValList],	
	!, check_del_demons(req(C,N,S,F,V),FacetList).
del_facet(Req,_,_) :-
	error(105,['del_facet - unable to remove',Req]).

check_del_demons(req(C,N,S,F,V),FacetList) :-
	get_frame(C,S-del(Del)), !,
	DelFunc =.. [Del,C,N,S-V],
	call(DelFunc).
check_del_demons(_,_).

% print a frame

print_frames :-
	frame(Class, SlotList),
	print_frame(Class),
	fail.
print_frames.

print_frame(Class) :-
	frame(Class,SlotList),
	write_line(['Frame:',Class]),
	print_slots(SlotList), nl.

printfs :-
	frame(Class,_),
	printf(Class,_),
	fail.
printfs.

printf(Class,Name) :-
	frinst(Class,Name,SlotList,Time),
	write_line(['Frame:',Class,Name,Time]),
	print_slots(SlotList), nl.

printf(Class) :-
	frinst(Class,Name,SlotList,Time),
	write_line(['Frame:',Class,Name,Time]),
	print_slots(SlotList), nl, fail.
printf(_).

print_slots([]).
print_slots([Slot|Rest]) :-
	write_line(['  Slot:',Slot]),
	print_slots(Rest).

% utilities

delete(X,[],[]).
delete(X,[X|Y],Y) :- !.
delete(X,[Y|Z],[Y|W]) :- delete(X,Z,W).

remove(X,[X|Y],Y) :- !.
remove(X,[Y|Z],[Y|W]) :- remove(X,Z,W).

is_on(X,[X|Y]).
is_on(X,[Y|Z]) :- is_on(X,Z).

error_threshold(100).

error(NE,_) :- error_threshold(N), N > NE, !, fail.
error(NE,E) :-
	nl, write('*** '),write(error-NE),tab(1),
	write_line(E),
	!, fail.

write_line([]) :- nl.
write_line([H|T]) :-
	write(H),tab(1),
	write_line(T).
	
time_test :-
	write('TT> '),
	read(X),
	timer(T1),
	X,
	timer(T2),
	nl,nl,
	T is T2 - T1,
	write(time-T).                                                                  


