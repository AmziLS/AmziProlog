% FRAMES - just the frame system from FOOPS for stand alone use.

:- op(100,fy,val).
:- op(100,fy,calc).
:- op(100,fy,def).
:- op(100,fy,add).
:- op(100,fy,del).

member(X,[X|Y]).
member(X,[Y|Z]) :- member(X,Z).

% prep_req takes a request of the form Slot-Val, and forms it into the
% more accurate req(Thing,Slot,Facet,Value).  If no facet was mentioned
% in the original request, then the facet of "any" is used to indicate
% the system should use everything possible to find a value.

prep_req(Slot-X,req(T,Slot,val,X)) :- var(X), !.
prep_req(Slot-X,req(T,Slot,Facet,Val)) :-
	nonvar(X),
	X =.. [Facet,Val],
	facet_list(FL),
	member(Facet,FL), !.
prep_req(Slot-X,req(T,Slot,val,X)).

facet_list([val,def,calc,add,del,edit]).


% retrieve a list of slot values

get_frame(Thing, ReqList) :-
	frame(Thing, SlotList),
	slot_vals(Thing, ReqList, SlotList).

slot_vals(_,[],_).
slot_vals(T,[Req|Rest],SlotList) :-
	prep_req(Req,req(T,S,F,V)),
	find_slot(req(T,S,F,V),SlotList), 
	!, slot_vals(T,Rest,SlotList).
slot_vals(T, Req, SlotList) :-
	prep_req(Req,req(T,S,F,V)),
	find_slot(req(T,S,F,V), SlotList).

find_slot(req(T,S,F,V), SlotList) :-
	nonvar(V), !,
	find_slot(req(T,S,F,Val), SlotList), !,
	(Val == V; member(V,Val)).
find_slot(req(T,S,F,V), SlotList) :-
	member(S-FacetList, SlotList), !,
	facet_val(req(T,S,F,V),FacetList).
find_slot(req(T,S,F,V), SlotList) :-
	member(ako-FacetList, SlotList),
	facet_val(req(T,ako,val,Ako),FacetList),
	(member(X,Ako); X = Ako),
	frame(X, HigherSlots),
	find_slot(req(T,S,F,V), HigherSlots), !.
find_slot(Req,_) :-
	error(['frame error looking for:',Req]).

facet_val(req(T,S,F,V),FacetList) :-
	FV =.. [F,V],
	member(FV,FacetList), !.
facet_val(req(T,S,val,V),FacetList) :-
	member(val ValList,FacetList),
	member(V,ValList), !.
facet_val(req(T,S,val,V),FacetList) :-
	member(def V,FacetList), !.
facet_val(req(T,S,val,V),FacetList) :-
	member(calc Pred,FacetList),
	CalcPred =.. [Pred,req(T,S,val,V)],
	call(CalcPred).

% add a list of slot values

add_frame(Thing, UList) :-
	old_slots(Thing,SlotList),
	add_slots(Thing,UList,SlotList,NewList),
	retract(frame(Thing,_)),
	asserta(frame(Thing,NewList)), !.	

old_slots(Thing,SlotList) :-
	frame(Thing,SlotList), !.
old_slots(Thing,[]) :-
	asserta(frame(Thing,[])).

add_slots(_,[],X,X).
add_slots(T,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(T,S,F,V)),
	add_slot(req(T,S,F,V),SlotList,Z),
	add_slots(T,Rest,Z,NewList).
add_slots(T,X,SlotList,NewList) :-
	prep_req(X,req(T,S,F,V)),
	add_slot(req(T,S,F,V),SlotList,NewList).

add_slot(req(T,S,F,V),SlotList,[S-FL2|SL2]) :-
	delete(S-FacetList,SlotList,SL2),
	add_facet(req(T,S,F,V),FacetList,FL2).

add_facet(req(T,S,F,V),FacetList,[FNew|FL2]) :-
	FX =.. [F,OldVal],
	delete(FX,FacetList,FL2),
	add_newval(OldVal,V,NewVal),
	!, check_add_demons(req(T,S,F,V),FacetList),
	FNew =.. [F,NewVal].

add_newval(X,Val,Val) :- var(X), !.
add_newval(OldList,ValList,NewList) :-
	list(OldList),
	list(ValList),
	append(ValList,OldList,NewList), !.
add_newval([H|T],Val,[Val,H|T]).
add_newval(_,Val,Val).

check_add_demons(req(T,S,F,V),FacetList) :-
	get_frame(T,S-add(Add)), !,
	AddFunc =.. [Add,req(T,S,F,V)],
	call(AddFunc).
check_add_demons(_,_).


% delete a list of slot values

del_frame(Thing) :-
	retract(frame(Thing,_)).
del_frame(Thing) :-
	error(['No frame',Thing,'to delete']).

del_frame(Thing, UList) :-
	old_slots(Thing,SlotList),
	del_slots(Thing,UList,SlotList,NewList),
	retract(frame(Thing,_)),
	asserta(frame(Thing,NewList)).	

del_slots(_,[],X,X).
del_slots(T,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(T,S,F,V)),
	del_slot(req(T,S,F,V),SlotList,Z),
	del_slots(T,Rest,Z,NewList).
del_slots(T,X,SlotList,NewList) :-
	prep_req(X,req(T,S,F,V)),
	del_slot(req(T,S,F,V),SlotList,NewList).

del_slot(req(T,S,F,V),SlotList,[S-FL2|SL2]) :-
	remove(S-FacetList,SlotList,SL2),
	del_facet(req(T,S,F,V),FacetList,FL2).
del_slot(Req,_,_) :-
	error(['del_slot - unable to remove',Req]).

del_facet(req(T,S,F,V),FacetList,FL) :-
	FV =.. [F,V],
	remove(FV,FacetList,FL),
	!, check_del_demons(req(T,S,F,V),FacetList).
del_facet(req(T,S,F,V),FacetList,[FNew|FL]) :-
	FX =.. [F,OldVal],
	remove(FX,FacetList,FL),
	remove(V,OldVal,NewValList),
	FNew =.. [F,NewValList],	
	!, check_del_demons(req(T,S,F,V),FacetList).
del_facet(Req,_,_) :-
	error(['del_facet - unable to remove',Req]).

check_del_demons(req(T,S,F,V),FacetList) :-
	get_frame(T,S-del(Del)), !,
	DelFunc =.. [Del,req(T,S,F,V)],
	call(DelFunc).
check_del_demons(_,_).

% print a frame

print_frames :-
	frame(Thing, SlotList),
	print_frame(Thing),
	fail.
print_frames.

print_frame(Thing) :-
	frame(Thing, SlotList),
	write_line(['Frame:',Thing]),
	print_slots(SlotList), nl.

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

errors(off).

error(_) :- errors(off), !, fail.
error(E) :-
	nl, write('*** '),
	write_line(E),
	write(' ***'), nl,
	fail.

write_line([]) :- nl.
write_line([H|T]) :-
	write(H),tab(1),
	write_line(T).
	
time_test :-
	write('TT> '),
	read(X),
	time(T1),
	X,
	time(T2),
	nl,nl,
	T is T2 - T1,
	write(time-T).  

