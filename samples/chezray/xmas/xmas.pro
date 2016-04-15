%-*-Prolog-*-  
% xmas indented on 9/5/1999 by 'JOLI' 1.0.


 /*                The Christmas Shopping Puzzle

... 12 couples were waited on consecutively, as they bought a total 
of 8 each of the following items:

	1. Aris Gloves  	2. Airplane Book  	3. COCO Perfume
   4. Pearl Strands  	5. A Football Sweater	6. A Handbag

Each husband and wife were waited on together.  Each couple bought 4 items.
No two couples bought the same combination of items, and none of the couples 
bought two or more of the same item.  Using the following clues, can you find:

        1. The full name of each husband and wife.
        2. What order they were waited on.
        3. What items each couple bought.

Hint: One husband is Bob, one wife is Elizabeth, and one surname is Stanton.

1. The Craigs, who bought a handbag, were waited on before the
	Murphys, who were not waited on last.
2. The Collins bought Aris gloves, a sweater, and handbag, and COCO.
3. The couples waited on 8th and 10th bought the Airplane Book.
4. These five couples were waited on consecutively:  the Smiths; Gary
	and his wife; a couple who bought the Airplane Book and a handbag; 
	the Swains; and Bill and his wife.
5. Geraldine and her husband did not buy either a handbag or a sweater.
6. The couple who were waited on last did not buy pearls.
7. One of the items Tom and his wife bought was the Airplane Book.
8. The Marshalls did not buy COCO or pearls.
9. Evelyn and her husband bought Aris gloves but not COCO.
10. These five couples were waited on consecutively:  Martha and her
	husband; Jack and his wife; the couple who bought Aris gloves, COCO,
	the Airplane Book, and a handbag; the couple who did not buy either
	pearls or the Airplane Book; and Margaret and her husband.
11. The first five couples waited on all bought COCO.
12. Chuck and his wife did not buy Aris gloves.
13. The couples waited on first, second, and fourth did not buy a sweater.
14. Eleanor and her husband did not buy COCO.
15. Neither Allen and his wife, who did not buy a handbag, nor
	the Anthonys bought Aris gloves.
16. Cheryl and her husband, who were not waited on 10th or 12th, and John
	and his wife are two couples who bought both a sweater and a handbag.
17. The Douglases, who did not buy Aris gloves or a sweater, were
	waited on 9th.
18. Adam and his wife, who did not buy a handbag, were waited on
	immediately before the Days.
19. Steve and his wife bought pearls, the Airplane Book, a sweater,
	and one other item.
20. The last three couples waited on did not buy Aris gloves.
21. The Joneses did not buy a sweater.
22. Susan and her husband bought pearls.
23. George and his wife bought a sweater.
24. The four couples who did not buy Aris gloves are (in no particular
	order): Dorothy and her husband; the Craigs; Joe and his wife; 
	and Rosalyn and her husband (who did not buy a sweater).
25. The O'Connors bought both COCO and a sweater.
26. Sandra and her husband, who did not buy a sweater, were waited on
	immediately before Cathleen and her husband.
*/

 /*  The known attributes of the data are all described in the same format. */
index([ff(1, _, _, _, selected(_, gloves, _, _, perfume, 0)), %11,13
       ff(2, _, _, _, selected(_, gloves, _, _, perfume, 0)), %11,13
       ff(3, _, _, _, selected(_, gloves, _, _, perfume, _)), %11
       ff(4, _, _, _, selected(_, gloves, _, _, perfume, 0)), %11,13
       ff(5, _, _, _, selected(_, gloves, _, _, perfume, _)), %11
       ff(6, _, _, _, selected(_, gloves, _, _, _, _)), %20,24
       ff(7, _, _, _, selected(_, gloves, _, _, _, _)), %20,24
       ff(8, _, _, _, selected(book, gloves, _, _, _, _)), %3
       ff(9, 'Douglas', _, 'Rosalyn', 
          selected(book, 0, handbag, pearls, perfume, 0)), %17
       ff(10, _, _, _, selected(book, 0, _, _, _, _)), %3,20
       ff(11, _, _, _, selected(_, 0, _, _, _, _)), %20
       ff(12, _, _, _, selected(book, 0, handbag, 0, perfume, sweater))
                                               %6,20
      ]).

surnames([ff(_, 'Anthony', _, _, selected(_, 0, _, _, _, _)), %15
          ff(_, 'Craig', _, _, selected(_, 0, handbag, _, _, _)), %1,24
          ff(_, 'Jones', _, _, selected(_, _, _, _, _, 0)), %21
          ff(_, 'Marshall', _, _, 
             selected(book, gloves, handbag, 0, 0, sweater)), %8
          ff(_, 'O_Connor', _, _, selected(_, _, _, _, perfume, sweater)), %25
          [                                    %4
           ff(_, 'Smith', _, _, _), ff(_, _, 'Gary', _, _), 
           ff(_, _, _, _, selected(book, _, handbag, _, _, _)), 
           ff(_, 'Swain', _, _, _), ff(_, _, 'Bill', _, _)], 
          ff(_, 'Murphy', _, _, _), ff(_, 'Stanton', _, _, _)]).

males([[                                       %18
        ff(_, _, 'Adam', _, selected(_, _, 0, _, _, _)), %18
        ff(_, 'Day', _, _, _)], 
       ff(_, _, 'Allen', _, selected(book, 0, 0, pearls, perfume, sweater)),
                                               %15
       ff(_, _, 'Chuck', _, selected(_, 0, _, _, _, _)), %12
       ff(_, _, 'George', _, selected(_, _, _, _, _, sweater)), %23
       ff(_, _, 'John', _, selected(_, _, handbag, _, _, sweater)), %16
       ff(_, _, 'Joe', _, selected(_, 0, _, _, _, _)), %24
       ff(_, _, 'Steve', _, selected(book, _, _, pearls, _, sweater)), %19
       ff(_, _, 'Tom', _, selected(book, _, _, _, _, _)), %7
       ff(_, _, 'Bob', _, _)]).

females([ff(_, _, _, 'Cheryl', selected(_, _, handbag, _, _, sweater)),
                                               %16
         ff(_, _, _, 'Dorothy', selected(_, 0, _, _, _, _)), %24
         ff(_, _, _, 'Eleanor', selected(_, _, _, _, 0, _)), %14
         ff(_, _, _, 'Evelyn', selected(_, gloves, _, _, 0, _)), %9
         ff(_, _, _, 'Geraldine', 
            selected(book, gloves, 0, pearls, perfume, 0)), %5
         [                                     %10
          ff(_, _, _, 'Martha', _), ff(_, _, 'Jack', _, _), 
          ff(_, _, _, _, selected(book, gloves, handbag, 0, perfume, 0)), 
          ff(_, 'Collins', _, _, 
             selected(0, gloves, handbag, 0, perfume, sweater)), 
          ff(_, _, _, 'Margaret', _)], 
         [                                     %26
          ff(_, _, _, 'Sandra', selected(_, _, _, _, _, 0)), %26
          ff(_, _, _, 'Cathleen', _)], 
         ff(_, _, _, 'Susan', selected(_, _, _, pearls, _, _)), %22
         ff(_, _, _, 'Elizabeth', _)]).

selection([ff(_, _, _, _, selected(book, gloves, handbag, pearls, 0, 0)), 
           ff(_, _, _, _, selected(book, gloves, handbag, 0, perfume, 0)), %10
           ff(_, 'Marshall', _, _, 
              selected(book, gloves, handbag, 0, 0, sweater)), %8
           ff(_, _, _, 'Geraldine', 
              selected(book, gloves, 0, pearls, perfume, 0)), %5
           ff(_, _, _, _, selected(book, gloves, 0, pearls, 0, sweater)), 
           ff(_, _, _, _, selected(book, gloves, 0, 0, perfume, sweater)), 
           ff(9, _, _, _, selected(book, 0, handbag, pearls, perfume, 0)), %17
           ff(_, _, _, _, selected(book, 0, handbag, pearls, 0, sweater)), 
           ff(12, _, _, _, selected(book, 0, handbag, 0, perfume, sweater)),
                                               %6,20
           ff(_, _, 'Allen', _, 
              selected(book, 0, 0, pearls, perfume, sweater)), %15
           ff(_, _, _, _, selected(0, gloves, handbag, pearls, perfume, 0)), 
           ff(_, _, _, _, selected(0, gloves, handbag, pearls, 0, sweater)), 
           ff(_, 'Collins', _, _, 
              selected(0, gloves, handbag, 0, perfume, sweater)), %10
           ff(_, _, _, _, selected(0, gloves, 0, pearls, perfume, sweater)), 
           ff(_, _, _, _, selected(0, 0, handbag, pearls, perfume, sweater))]).

main :-
   fugit(xmas).

xmas :-
   index(Index),
   surnames(Surnames),
   males(Males),
   females(Females),
   selection(Selection),
                                               %
   solve(problem(Index, perm(Surnames), perm(Males), perm(Females), 
                 perm(Selection)), Solution),
   displaySolutions(Solution),
   nl, nl.

displaySolution(ff(Index, Last, He, She, Items)) :-
   (Index < 10, !, write(' ');  true),
   write(Index), write(' '), write(He), write(' and '), write(She), 
   write(' '), write(Last), write(' '), write(Items), nl.
/* The rest is the filter specification for this problem. */
   

1 - Past - ff(Index, Last, He, She, Selected) :-
   (
      Index == 10, !,
      (member(ff(_, 'Craig', _, _, _), Past) -> true;  Last = 'Craig');

      true
   ).                                          %1
2 - Past - ff(Index, Last, He, She, Selected) :-
   filter(Index, Last).
3 - Past - ff(Index, Last, He, She, Selected) :-
   (Last == 'Murphy', !, member(ff(_, 'Craig', _, _, _), Past);  true), %1
   filter(Index, Last, He).
4 - Past - ff(Index, Last, He, She, Selected) :-
   filter(Index, Last, He, She).
5 - Past - ff(Index, Last, He, She, Selected) :-
   (
      Index > 9, !,
      countItems([ff(Index, Last, He, She, Selected)|Past], 
                 count(0, 0, 0, 0, 0, 0));

      true
   ),
   (Selected = selected(book, gloves, handbag, 0, perfume, 0), !, %10
   Past = [ff(_, _, 'Jack', _, _)|_];  true),
   (
      Selected = selected(0, gloves, handbag, 0, perfume, sweater), !, %10
      Past = 
      [ff(_, _, _, _, selected(book, gloves, handbag, 0, perfume, 0))|_];

      true
   ),
   filter(Index, Last, He, She, Selected).

filter(_).

filter(12, 'Murphy') :- !,
   fail.                                       %1
filter(_, _).                                  % otherwise, succeed

filter(_, 'Day', 'Adam') :- !,
   fail.                                       %18
filter(_, 'Craig', 'Joe') :- !,
   fail.                                       %24
filter(_, 'Anthony', 'Allen') :- !,
   fail.                                       %15
filter(_, _, _).                               % otherwise, succeed

filter(_, _, 'John', 'Cheryl') :- !,
   fail.                                       %16
filter(10, _, _, 'Cheryl') :- !,
   fail.                                       %16
filter(12, _, _, 'Cheryl') :- !,
   fail.                                       %16
filter(_, 'Craig', _, 'Dorothy') :- !,
   fail.                                       %24
filter(_, 'Craig', _, 'Rosalyn') :- !,
   fail.                                       %24
filter(_, _, 'Joe', 'Dorothy') :- !,
   fail.                                       %24
filter(_, _, 'Joe', 'Rosalyn') :- !,
   fail.                                       %24
filter(_, _, _, _).                            % otherwise, succeed

filter(_, _, _, _, _).                         % otherwise, succeed

countItems([], _).
countItems([This|Rest], Count) :-
   countItem(This, Count, Count1),
   countItems(Rest, Count1).

countItem(ff(_, _, _, _, 
             selected(Book, Gloves, Handbag, Pearls, COCO, Sweater)), 
          count(B, G, H, P, C, S), count(B1, G1, H1, P1, C1, S1)) :-
   (Book == 0 -> B1 is B;  B1 is B + 1, B1 < 9),
   (Gloves == 0 -> G1 is G;  G1 is G + 1, G1 < 9),
   (Handbag == 0 -> H1 is H;  H1 is H + 1, H1 < 9),
   (Pearls == 0 -> P1 is P;  P1 is P + 1, P1 < 9),
   (COCO == 0 -> C1 is C;  C1 is C + 1, C1 < 9),
   (Sweater == 0 -> S1 is S;  S1 is S + 1, S1 < 9).

