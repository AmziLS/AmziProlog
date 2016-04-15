% build rules use dcg in reverse to make clam rules from Prolog rules
% You can use bldrules.pro to convert, for example, the native Prolog
% rules of the birds.pro into clam syntax.

main :-
	write($From file: $), read(From),
	write($To file: $), read(To),
	doit(From,To).

doit(From,To) :-
	see(From),
	tell(To),
	test.

test :- 
	cntr_set(1,1),
	repeat,
	read(X),
	tran(X,Ans,[]),
	write_nice(Ans),nl,
	X == end_of_file.
test :- told,seen,write(done).
	
xxif(Body) --> [if],xxbody(Body).
xxthen(Head) --> {Head =.. [F,A]},[then,F,is,A].

xxbody((H,T)) --> {!,H =.. [F,A]}, [F,is,A,and], xxbody(T).
xxbody(H) --> {H =.. [F,A]}, [F,is,A].

tran(A,B,C) :- trans(A,B,C),!.
tran(X,X,_).

trans(end_of_file,end_of_file,_).

trans((Head :- true)) --> {Head =.. [F,A]}, [F,is,A], !.
trans((Head :- Body)) --> {cntr_get(1,ID)}, [rule, ID],
	xxif(Body), xxthen(Head), {ID2 is ID + 1, cntr_set(1,ID2)}.

write_nice(X) :- wr_nice(X), !.

wr_nice([]) :- !,write('.'),nl.
wr_nice([if|T]):- !,nl,write('  if    '),wr_nice(T).
wr_nice([then|T]):- !,nl,write('  then  '),wr_nice(T).
wr_nice([and|T]):- !,write(and),nl,write('        '),
	wr_nice(T).
wr_nice([H|T]) :- !,write(H),write(' '),wr_nice(T).
wr_nice(X) :- write(X).

