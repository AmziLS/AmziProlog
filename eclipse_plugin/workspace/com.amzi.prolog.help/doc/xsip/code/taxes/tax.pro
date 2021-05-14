% This is the source code for the Prolog version of Tax program
% for a subset of US federal taxes in 1987

main :-
   consult('tax.dat'),
   tax.

tax :-
	getl(1040,63,X),
	X > 0,
	nl,write('*** good news : '),
	write(X),nl,!.
tax :-
	getl(1040,65,X),
	X >= 0,
	nl,write('*** bad news : '),
	write(X),nl,!. 


%------------
% form 1040
%------------

%----- income -----

line(1040,'6a','exemption self',1).
line(1040,'6b','exemption spouse',1) :-
	status(married_joint).
line(1040,'6b','',0).
line(1040,'6c','dependent children',X) :-
	children(X).
line(1040,'6e','total dependents',X) :-
	sum_lines(1040,['6a','6b','6c'],X).

line(1040,7,'wages salaries etc',X) :-
	wages(X).
line(1040,8,'interest income',X) :-
	getl(b,3,X).
line(1040,13,'business profit or loss',X) :-
	getl(c,31,X).
line(1040,22,'total income',X) :-
	sum_lines(1040,[7,8,13], X).


%----- adjusted gross income -----

line(1040,25,'health insurance',X) :-
	health_insurance(A),
	B is integer( 0.25 * A + 0.5 ),
	getl(c,31,C),
	minimum([B,C],X).
line(1040,30,'adjusted gross income',X) :-
	getl(1040,22,X).

%----- tax computation -----

line(1040,31,'adjusted gross income',X) :-
	getl(1040,30,X).
line(1040,'33a','itemized deductions',X) :-
	getl(a,26,X).
line(1040,'33b','',2540) :-
	status(single).
line(1040,'33b','',3760) :-
	status(married_joint).
line(1040,'33b','',1880) :-
	status(married_separate).
line(1040,34,'less itemized deductions',X) :-
	getl(1040,'33a',A),
	getl(1040,'33b',B),
	A > B,
	line_dif(1040,31,'33a',X), !.
line(1040,34,'less standard deductions',X) :-
	line_dif(1040,31,'33b',X).
line(1040,35,'dependent deductions',X) :-
	getl(1040,'6e',D),
	X is 1900 * D.
line(1040,36,'taxable income',X) :-
	line_dif(1040,34,35,X).
line(1040,37,'tax computation',X) :-
	getl(1040,36,A),
	compute_tax(A,X).
line(1040,39,'',X) :-
	getl(1040,37,X).
	
%----- other taxes -----

line(1040,53,'total taxes',X) :-
	getl(1040,39,X).

%----- payments -----

line(1040,54,'withheld',X) :-
	withheld(X).
line(1040,55,'estimated paid',X) :-
	estimated_paid(X).
line(1040,61,'total payments',X) :-
	sum_lines(1040,[54,55],X).

%----- refund or amount owed -----

line(1040,63,'refund',X) :-
	getl(1040,53,Owed),
	getl(1040,61,Paid),
	X is Paid - Owed,
	X > 0.
line(1040,63,'refund',0).
line(1040,65,'pay',X) :-
	getl(1040,53,Owed),
	getl(1040,61,Paid),
	X is Owed - Paid,
	X >= 0.

%-------------
% schedule a
%-------------

line(a,2,'medical fees',X) :-
	medical_fees(A),
	getl(1040,25,B),
	X is A - B.
line(a,3,'7.5% of income',X) :-
	getl(1040,31,A),
	X is integer(0.075 * A + 0.5).
line(a,4,'total medical',X) :-
	line_dif(a,2,3,X).

line(a,5,'state taxes',X) :-
	getl(mass,50,X).
line(a,6,'real estate taxes',X) :-
	real_estate_taxes(X).
line(a,7,'excise taxes',X) :-
	excise_taxes(X).
line(a,8,'total taxes',X) :-
	sum_lines(a,[5,6,7],X).

line(a,'9a','mortgage interest',X) :-
	mortgage_interest(X).
line(a,'12a','personal interest',X) :-
	personal_interest(X).
line(a,'12b','',X) :-
	getl(a,'12a',A),
	X is integer( 0.65 * A + 0.5 ).
line(a,13,'total interest',X) :-
	sum_lines(a,['9a','12b'],X).

line(a,26,'total itemized',X) :-
	sum_lines(a,[4,8,13],X).

%-------------
% schedule b
%-------------

line(b,2,'interest accounts',table(int_inc_tab)).
line(b,3,'total interest income',X) :-
	getl(b,2,_),
	interest_income(X).

%-------------
% schedule c
%-------------

line(c,'1a','gross receipts',X) :-
	gross_receipts(X).
line(c,'1b','returns',X) :-
	returns(X).
line(c,'1c','receipts',X) :-
	line_dif(c,'1a','1b',X).
line(c,2,'cost of goods',X) :-
	cost_of_goods(X).
line(c,3,'gross profit',X) :-
	line_dif(c,'1c',2,X).
line(c,4,'other income',X) :-
	other_income(X).
line(c,5,'gross income',X) :-
	sum_lines(c,[3,4],X).

line(c,6,'advertising',X) :- 
	deduct('advertising',X).
line(c,8,'bank charges',X) :-
	deduct('bank charges',X).
line(c,9,'car expenses',X) :-
	deduct('car expenses',X).
line(c,12,'depreciation',X) :-
	getl(4562,5,A),
	getl(4562,11,B),
	X is A + B.
line(c,13,'dues & pubs',X) :-
	deduct('dues & pubs',X).
line(c,16,'insurance',X) :-
	deduct('insurance',X).
line(c,20,'office expense',X) :-
	deduct('office expense',X).
line(c,22,'rent',X) :-
	deduct('rent',X).
line(c,'26a','travel',X) :-
	deduct('travel',X).
line(c,'26b','meals & entertainment',X) :-
	deduct('meals & entertainment',X).
line(c,'26c','meals * 20%',X) :-
	getl(c,'26b',A),
	X is integer( 0.20 * A + 0.05 ).
line(c,'26d','meals & entertainment',X) :-
	line_dif(c,'26b','26c',X).
line(c,27,'utilities & phone',X) :-
	deduct('utilities & phone',X).

line(c,30,'total deductions',X) :-
	sum_lines(c,[6,8,9,12,13,16,20,22,'26a','26d',27],X).
line(c,31,'net profit',X) :-
	line_dif(c,5,30,X).
	
%-------------------------
% form 4562 depreciation
%-------------------------

line(4562,2,'listed property table',table(listed_prop_tab)).
line(4562,3,'listed property',X) :-
	getl(4562,2,_),
	depreciation_prop_179(X).
line(4562,5,'179 election',X) :-
	getl(4562,3,X),
	X =< 10000 .
line(4562,5,'179 election',_) :-
	write('*** error - 179 election on 4562 > 10000'),nl.
	
line(4562,11,'depreciation',X) :- 
	depreciation(X).

%-----------------
% mass form
%-----------------

line(mass,50,total,X) :-
	mass_tax(X).

%------------------
% tax computation
%------------------

compute_tax(A,Tax) :-				% adjust for tax table calc
	B is integer(A / 50),
	C is B * 50 + 25,
	comput_tax(C,Tax).

comput_tax(A,Tax) :-
	status(single),
	rate_single(A,T), !,
	Tax is integer(T + 0.5).
comput_tax(A,Tax) :-
	status(married_joint),
	rate_joint(A,T), !,
	Tax is integer(T + 0.5).
comput_tax(A,Tax) :-
	status(married_separate),
	rate_separate(A,T), !,
	Tax is integer(T + 0.5).

rate_single(A,T) :-
	A =< 1800,
	T is 0.11 * A.
rate_single(A,T) :-
	A =< 16800,
	T is 198 + 0.15 * (A - 1800).
rate_single(A,T) :-
	A =< 27000,
	T is 2448 + 0.28 * (A - 16800).
rate_single(A,T) :-
	A =< 54000,
	T is 5304 + 0.35 * (A - 27000).
rate_single(A,T) :-
	T is 14754 + 0.385 * (A - 54000).
	
rate_joint(A,T) :-
	A =< 3000,
	T is 0.11 * A.
rate_joint(A,T) :-
	A =< 28000,
	T is 330 + 0.15 * (A - 3000).
rate_joint(A,T) :-
	A =< 45000,
	T is 4080 + 0.28 * (A - 28000).
rate_joint(A,T) :-
	A =< 90000,
	T is 8840 + 0.35 * (A - 45000).
rate_joint(A,T) :-
	T is 24590 + 0.385 * (A - 90000).

rate_separate(A,T) :-
	A =< 1500,
	T is 0.11 * A.
rate_separate(A,T) :-
	A =< 14000,
	T is 165 + 0.15 * (A - 1500).
rate_separate(A,T) :-
	A =< 22500,
	T is 2040 + 0.28 * (A - 14000).
rate_separate(A,T) :-
	A =< 45000,
	T is 4420 + 0.35 * (A - 22500).
rate_separate(A,T) :-
	T is 12295 + 0.385 * (A - 45000).


%--------------------------
% database, reports, etc.
%--------------------------

forms([1040,a,b,c,4562,mass]).

guess(X) :-
	assertz( unsure(X) ).

getl(Form,Line,Amount) :-
	lin(Form,Line,_,Amount), !.
getl(Form,Line,Amount) :-
	line(Form,Line,Desc,Amount),
	assertz( lin(Form,Line,Desc,Amount) ), !.
getl(Form,Line,Amount) :-
	nl,
	write('*** getl failure ***'),
	write_list([Form,Line,Amount]),
	nl.

report :-
	forms(L),
	rep(L),
	report(guess).

rep([]).
rep([H|T]) :- report(H), rep(T).

report(guess) :-
	rep_guess.
report(Form) :-
	nl,
	write('----- '),write(Form),write(' -----'),nl,nl,
	lin(Form,Line,Desc,Amount),
	process(Line,Desc,Amount),
	fail.
report(_).

process(Line,Desc,table(T)) :-
	write(Line),
	tab(5),write(Desc),nl,
	T, !.
process(Line,Desc,Amount) :-
	write(Line),
	tab(5),write(Desc),
	tab(45),write(Amount),nl.

rep_guess :-
	nl,
	write('----- '),write('Guesses'),write(' -----'),nl,nl,
	unsure(G),
	write(G),nl,
	fail.
rep_guess.

clear :-
	retractall( lin(_,_,_,_) ),
	retractall( unsure(_) ).

clear(guess) :-
	retractall( unsure(_) ).
clear(Form) :-
	retractall( lin(Form,_,_,_) ).

%------------
% utilities
%------------

write_list([]) :- nl.
write_list([H|T]) :-
	write(H),tab(1),
	write_list(T).

line_dif(F,A,B,X) :-
	getl(F,A,AX),
	getl(F,B,BX),
	X is AX - BX.

sum_lines(F,L,X) :- sumlin(F,L,0,X).

sumlin(F,[],X,X).
sumlin(F,[H|T],X,Y) :-
	getl(F,H,A),
	XX is X + A,
	sumlin(F,T,XX,Y).

list_sum(L,X) :- ls(L,0,X).

ls([],X,X).
ls([Amount|T],X,Y) :-
	XX is X + Amount,
	ls(T,XX,Y).

minimum([H|T],X) :-
	min(T,H,X).

min([],X,X).
min([H|T],M,X) :-
	H < M,
	min(T,H,X).
min([H|T],M,X) :-
	min(T,M,X).                                                                                                  

