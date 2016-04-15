parent(javier, nano).
parent(pablo, nano).
parent(santiago, nano).
parent(miguel, michael).

brother(X, Y) :-
   parent(X, P),
   parent(Y, P).
   
