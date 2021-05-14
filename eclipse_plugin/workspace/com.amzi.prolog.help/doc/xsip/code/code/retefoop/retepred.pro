% rete - the predicates which implement the Rete pattern matching algorithm.

% It should be modified some day
% to use pointers to working memory in the memory predicates rather
% than the full tokens - this would save a lot of space.

% retecomp - compile rules into a rete network

:-op(800,xfx,==>).         % used to separate LHS and RHS of rule
:-op(500,xfy,#).           % used to separate attributes and values
:-op(810,fx,rule).         % used to define rule
:-op(700,xfy,#).           % used for unification instead of =
:-op(700,xfy,\=).		      % not equal
:-op(600,xfy,with).		   % used for frame instances in rules

rete_compile :-
	abolish(root,3),
	abolish(bi,4),
	abolish(tes,4),
	abolish(rul,3),
	abolish(varg,1),
	abolish(nid,1),
	asserta(nid(0)),
	rete_compil.
%	display_net.

display_net :-
	display_roots,nl,
	display_bis,nl,
	display_teses,nl,
	display_ruls.

display_roots :-
	root(N,A,B),
	write( root(N,A,B) ),nl,
	fail.
display_roots.

display_bis :-
	bi(A,B,C,D),
	write( bi(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),
	write(D),nl,nl,
	fail.
display_bis.

display_teses :-
	tes(A,B,C,D),
	write( tes(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),nl,
	write(D),nl,nl,
	fail.
display_teses.

display_ruls :-
	rul(A,B,C),
	write( rul(A) ),nl,
	write_list([left|B]),
	write_list([right|C]),nl,
	fail.
display_ruls.

write_list([]).
write_list([H|T]) :-
	write(H),nl,
	wr_lis(T).
	
wr_lis([]).
wr_lis([H|T]) :-
	tab(5),write(H),nl,
	wr_lis(T).

% compile each rule into the rete net

rete_compil :-
	rule N# LHS ==> RHS,
	rete_comp(N,LHS,RHS),
	fail.
rete_compil :-
	message(201).

% compile an individual rule into the net

rete_comp(N,[H|T],RHS) :-
	term(H,Hw),
	check_root(RN,Hw,HList),
	retcom(root(RN),[Hw/_],HList,T,N,RHS),
	message(202,N), !.
rete_comp(N,_,_) :-
	message(203,N).

% the main compile loop
% 	PNID - the id of the previous node
%	OutTok - list of tokens from previous node
%	PrevList - transfer list from previous node 
%	[H|T] - list of remaining clauses in rule
%	N - The rule ID, for building the rule at the end
%	RHS - the rhs of the rule for building the rule at the end

retcom(PNID,OutTok,PrevList,[],N,RHS) :-
	build_rule(OutTok,PrevList,N,RHS),
	update_node(PNID,PrevList,rule-N),
	!.
retcom(PNID,PrevNode,PrevList,[H|T],N,RHS) :-
	term(H,Hw),
	check_root(RN,Hw,HList),
	check_node(PrevNode,PrevList,[Hw/_],HList,NID,OutTok,NList),
	update_node(PNID,PrevList,NID-l),
	update_root(RN,HList,NID-r),
	!,
	retcom(NID,OutTok,NList,T,N,RHS).	
retcom(PNID,PrevNode,PrevList,[H|T],N,RHS) :-	%some kind of tester call
	check_tnode(PrevNode,PrevList,[H/0],HList,NID,OutTok,NList),
	update_node(PNID,PrevList,test-NID),
	!,
	retcom(test-NID,OutTok,NList,T,N,RHS).	

term(Class-Name with List,Class-Name with List).
term(Class-Name, Class-Name with []).

check_root(NID,Term,[]) :-
	not(root(_,Term,_)),
	gen_nid(NID),
	assertz( root(NID,Term,[]) ), !.
check_root(N,Term,List) :-
	asserta(temp(Term)),
	retract(temp(T1)),
	root(N,Term,List),
	root(N,T2,_),
	comp_devar(T1,T2), !.
check_root(NID,Term,[]) :-
	gen_nid(NID),
	assertz( root(NID,Term,[]) ).


% if this node was already on the list do nothing, otherwise add it
% to the list

update_root(RN,HList,NID) :-
	member(NID,HList), !.
update_root(RN,HList,NID) :-
	retract( root(RN,H,HList) ),
	asserta( root(RN,H,[NID|HList]) ).

update_node(root(RN),HList,NID) :-
	update_root(RN,HList,NID), !.
update_node(X,PrevList,NID) :-
	member(NID,PrevList), !.
update_node(test-N,PrevList,NID) :-
	retract( tes(N,L,T,_) ),
	asserta( tes(N,L,T,[NID|PrevList]) ), !.
update_node(PNID,PrevList,NID) :-
	retract( bi(PNID,L,R,_) ),
	asserta( bi(PNID,L,R,[NID|PrevList]) ).

% check to see if there is a node which already fits, otherwise 
% create a new one
%	PNode - token list from previous node
%	PList - list of successor nodes from previous node
%	H - new token being added
%	HList - successor nodes from root for token H
%	NID - returned ID of the node
%	OutTok - returned tokenlist from the node
%	NList - returned list of successor nodes from the node

%	first case - there isn't a matching rule using Prolog's match, so 
%		build a new one

check_node(PNode,PList,H,HList,NID,OutTok,[]) :-
	not (bi(_,PNode,H,_)),
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( bi(NID,PNode,H,[]) ),
	!.

%	second case - there was a matching rule using Prolog's match, so
%		match again using generated constants instead of variables.  If
%		this matches then we have a match, otherwise we had a match
%		where variables don't line up and its no good. (asserts and
%		retracts allow different variables to have same information and
%		prevent binding of variables in one from affecting the other)

check_node(PNode,PList,H,HList,NID,OutTok,NList) :-
	append(PNode,H,OutTok),
	asserta(temp(OutTok)),
	retract(temp(Tot1)),
	bi(NID,PNode,H,NList),
	bi(NID,T2,T3,_),
	append(T2,T3,Tot2),
	comp_devar(Tot1,Tot2),
	!.

%	third case - the variables didn't line up from the second rule, so
%		make a new node.

check_node(PNode,PList,H,HList,NID,OutTok,[]) :-
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( bi(NID,PNode,H,[]) ).

% check for test node - similar to check for regular node

check_tnode(PNode,PList,H,HList,NID,OutTok,[]) :-
	not (tes(_,PNode,H,_)),
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( tes(NID,PNode,H,[]) ),
	!.

%	second case - there was a matching rule using Prolog's match, so
%		match again using generated constants instead of variables.  If
%		this matches then we have a match, otherwise we had a match
%		where variables don't line up and its no good. (asserts and
%		retracts allow different variables to have same information and
%		prevent binding of variables in one from affecting the other)

check_tnode(PNode,PList,H,HList,NID,OutTok,NList) :-
	append(PNode,H,OutTok),
	asserta(temp(OutTok)),
	retract(temp(Tot1)),
	tes(NID,PNode,H,NList),
	tes(NID,T2,T3,_),
	append(T2,T3,Tot2),
	comp_devar(Tot1,Tot2),
	!.

%	third case - the variables didn't line up from the second rule, so
%		make a new node.

check_tnode(PNode,PList,H,HList,NID,OutTok,[]) :-
	append(PNode,H,OutTok),
	gen_nid(NID),
	assertz( tes(NID,PNode,H,[]) ).

build_rule(OutTok,PrevList,N,RHS) :-
	assertz( rul(N,OutTok,RHS) ).
	
gen_nid(NID) :-
	retract( nid(N) ),
	NID is N+1,
	asserta( nid(NID) ).

% the hard part, undo Prolog's pattern matching so variables match just
% variables and not constants.  de-var replaces all the variables with
% generated constants - this ensures only variables will match variables.

comp_devar(T1,T2) :-
	de_vari(T1),
	de_vari(T2),
	T1=T2.

de_vari([]).
de_vari([H|T]) :-
	de_var(H),
	de_vari(T).
de_vari(X) :- de_var(X).

de_var(X/_) :- de_var(X).
de_var(X-Y with List) :-
	init_vargen,
	de_v(X-Y),
	de_vl(List), !.
de_var(X-Y) :-
	init_vargen,
	de_v(X-Y), !.

de_vl([]).
de_vl([H|T]) :-
	de_v(H),
	de_vl(T).

de_v(X-Y) :-
	d_v(X),
	d_v(Y).

d_v(V) :-
	var(V),
	var_gen(V), !.
d_v(_).

init_vargen :-
	abolish(varg,1),
	asserta(varg(1)).
	
var_gen(V) :-
	retract(varg(N)),
	NN is N+1,
	asserta(varg(NN)),
	string_integer(NS,N),
	string_list(NS,NL),
	append("#VAR_",NL,X),
	name(V,X).

% predicates to update the rete network

% add a token to the rete net.  a token is of the form C-N with [S-V,...]
% ReqList gets bound with the values from the term added to the database.

addrete(Class,Name,TimeStamp) :-
	root(ID,Class-Name with ReqList, NextList),
	ffsend(Class,Name,ReqList,TimeStamp,NextList),
	fail.
addrete(_,_,_).

% fullfill the request list from the token, and send the instantiated
% token through the net.

ffsend(Class,Name,ReqList,TimeStamp,NextList) :-
	getf(Class,Name,ReqList),
	send(tok(add,[(Class-Name with ReqList)/TimeStamp]), NextList),
	!.

delrete(Class,Name,TimeStamp) :-
	root(ID,Class-Name with ReqList, NextList),
	delr(Class,Name,ReqList,TimeStamp),
	fail.
delrete(_,_,_).

delr(Class,Name,ReqList,TimeStamp) :-
	getf(Class,Name,ReqList),
	!, send(tok(del,[(Class-Name with ReqList)/TimeStamp]), NextList).
delr(Class,Name,ReqList,TimeStamp).

% send the new token to each of the succesor nodes

send(_,[]).
send(Tokens, [Node|Rest]) :-
	sen(Node, Tokens),
	send(Tokens, Rest).

% add or delete the new token from the appropriate memory, build new
% tokens from left or right and send them to successor nodes.

sen(rule-N,tok(AD,TokenList)) :-
	rul(N,TokenList,Actions),
	(AD = add, add_conflict_set(N,TokenList,Actions);
	 AD = del, del_conflict_set(N,TokenList,Actions)),
	!.
sen(Node-l,tok(AD,TokenList)) :-
	bi(Node,TokenList,Right,NextList),
	(AD = add, asserta( memory(Node-l,TokenList) );
	 AD = del, retract( memory(Node-l,TokenList) )),
	!,matchRight(Node,AD,TokenList,Right,NextList).
sen(Node-r,tok(AD,TokenList)) :-
	bi(Node,Left,TokenList,NextList),
	(AD = add, asserta( memory(Node-r,TokenList) );
	 AD = del, retract( memory(Node-r,TokenList) )),
	!,matchLeft(Node,AD,TokenList,Left,NextList).
sen(test-N,tok(AD,TokenList)) :-
	tes(N,TokenList,[Test/0],NextList),
	test(Test),
	append(TokenList,[Test/0],NewToks),
	!,send(tok(AD,NewToks),NextList).

matchRight(Node,AD,TokenList,Right,NextList) :-
	memory(Node-r,Right),
	append(TokenList,Right,NewToks),
	send(tok(AD,NewToks),NextList),
	fail.
matchRight(_,_,_,_,_).
	
matchLeft(Node,AD,TokenList,Left,NextList) :-
	memory(Node-l,Left),
	append(Left,TokenList,NewToks),
	send(tok(AD,NewToks),NextList),
	fail.
matchLeft(_,_,_,_,_).


	                                                                                                                           

