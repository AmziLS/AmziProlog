main:-
	open('rec1.pro', read, H),
	repeat,
	read(H,X),
	write(X),nl,
	X = end_of_file.
