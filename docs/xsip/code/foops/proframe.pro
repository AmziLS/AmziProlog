% Code which I think converts Prolog to Frame notation.


convert :-
	abolish(frame,2),
	member(Level,[order,family,bird]),
	H =.. [Level,Thing],
	clause(H,B),
	change(B,AList),
	write_frame(frame(Thing,[level-[val Level]|AList])),
	fail.

change([],[]).
change([G|X],[S|Y]) :-
	ch(G,S),!,
	change(X,Y).

ch(G,ako-[val Val]) :-
	G =.. [L,Val],
	member(L,[order,family,bird]).
ch(G,Attr-[val Val]) :-
	G =.. [Attr,Val].
	
write_frame(frame(Thing,AList)) :-
	write_line([frame,'(',Thing,', [' ]),nl,
	write_alist(AList),
	write(' ]).'),nl,nl.

write_line([]).
write_line([H|T]) :-
	write(H),
	write_line(T).

write_alist([L]) :-
	tab(5),write_slot(L), !.
write_alist([H|T]) :-
	tab(5),write_slot(H),write(','),nl,
	write_alist(T).

write_slot(S-[val V]) :-
	write_line([S,-,'[',val,' ',V,']']).                                                                                                        

