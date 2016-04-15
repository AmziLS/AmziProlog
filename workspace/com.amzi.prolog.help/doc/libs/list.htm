% <PRE>
% Amzi! inc. list.pro library
%
% This file contains library predicates that perform
% various list utilities.  The .plm file must be loaded
% or linked with the project, and the module must be
% imported.
%
% To use from the listener:
% ?- load(list).
% ?- import(list).
%
% To use in a compiled application, link list.plm with the
% project and put the directive
% :- import(list).
% in the modules that use the list predicates.

:- module(list).
:- export( [
     append/3,            % join or split lists
     compare_lists/3,     % returns difference of two lists
     deleteN/4,           % delete the Nth element of a list
     flatten/2,           % flatten a list of nested lists
     insert/3,            % insert an item in a sorted list
     last_item/2,         % get last element of a list
     length/2,            % get the length of a list
     length_lte/2,        % compare lengths of lists
     length_gte/2,        % compare lengths of lists
     length_lt/2,         % compare lengths of lists
     length_gt/2,         % compare lengths of lists
     member/2,            % find or generate members of list
     nth_elem/3,          % find the nth element of a list
     permutation/3,       % permute elements of a list
     random_elem/2,       % pick a random element from a list
     remove/3,            % remove an element from a list
     remove_dups/2,       % remove duplicate elements from a list
     replace_elem/4,      % replace one occurence of element in a list
     replace_all_elem/4,  % replaces all occurrences of element in a list
     reverse/2,           % reverse a list
     shuffle/2,           % randomly shuffle a list
     sub_list/4,          % find a sub list in a list
     write_formatted/1,   % write a formatted list
     write_formatted/2,   % write a formatted list
     write_list/2,        % write separated list elements
     write_list/3,        % write separated list elements
     writeq_list/2,       % writeq separated list elements
     writeq_list/3        % writeq separated list elements
     ]).


%-----------------------------------------
% append(?L1, ?L2, ?L12)
%
% append/3 defines the relationship that lists L1 and L2,
% appended together, equal list L12.
%
% append/3 can be used in a number of different ways, depending
% on which arguments are instantiated.  If the first two are, it
% simply joins the two lists.  If just the third argument is, it
% generates all possible splittings of the list on backtracking.
%

append([], X, X).
append([A|X], Y, [A|Z]) :- append(X,Y,Z).

%-------------------------------------------
% compare_lists(+L1, +L2, -L3)
%
% compare_lists/3 returns, in L3, the elements that are in
% the first list, L1, and not in the second list, L2.
%

compare_lists([], _, []).
compare_lists([H|T], L, D) :-
  is_member(H, L), !,
  compare_lists(T, L, D).
compare_lists([H|T], L, [H|D]) :-
  compare_lists(T, L, D).

%-----------------------------------------
% deleteN(+NTH, -ELEM, +IN_LIST, -OUT_LIST)
%
% Delete the NTH elem of the list IN_LIST.  ELEM is bound
% to the deleted element and OUT_LIST is bound to the remaining
% list.
%

deleteN(1, H, [H|Z], Z).
deleteN(_, _, [], []) :- !, fail.
deleteN(N, H, [X|Z], [X|Z2]) :-
  NN is N - 1,
  deleteN(NN, H, Z, Z2).


%-----------------------------------------
% flatten(+L1, -L2)
%
% Take a list L1, that might have nested lists in it,
% and flatten it into list L2, that does not have any
% lists as elements.
%

/* The slow way.
flatten(X, X):-
	var(X).
flatten([], []).
flatten([H|T],L):-
	flatten(H,H1),
	flatten(T,T1),!,
	append(H1,T1,L).
flatten(H,[H]).
*/

% The fast way from Clocksin's Clause & Effect
flatten(In,Flat) :- flatpair(In,Flat-[]).

flatpair([],L-L) :- !.
flatpair([H|T], L1-L3) :- !, flatpair(H,L1-L2), flatpair(T,L2-L3).
flatpair(X,[X|Z]-Z).


%-----------------------------------------
% insert(+A, +L1, -L2)
% 
% Insert an item, A, in sorted order in a list L1, with
% resulting list L2.
%

insert(A, [A|Z], [A|Z]) :-
   !.
insert(A, [], [A]) :-
   !.
insert(A, [B|Z], [A,B|Z]) :-
   A @< B,
   !.
insert(B, [A|Y], [A|Z]) :-
   insert(B,Y,Z).

%-----------------------------------------
% last_item(+List, ?Elem)
%
% Find or test the last item of a list.
%

last_item([], _) :-
   !,
   fail.
last_item([X], X) :-
   !.
last_item([_|Z], X) :-
   last_item(Z, X).
   
%-----------------------------------------
% length(+L, -LENGTH)
% 
% Return the length of input list L.
%

length(L, N) :- length(L, 0, N).

  length([], N, N) :- !.
  length([_|Y], X, N) :-
    XX is X + 1,
    length(Y, XX, N).


%-----------------------------------------
% length_x(+L1, +L2)
%
% These predicates compare list lengths according to
% the pattern 'x', so length_lte(L1, L2) tests if the
% length of L1 is less than or equal the length of L2.
%

length_lte([], _).
length_lte([X1|Z1], [X2|Z2]) :-
  length_lte(Z1, Z2).

length_gte(L1, L2) :-
  length_lte(L2, L1).

length_lt([], [_|_]).
length_lt([X1|Z1], [X2|Z2]) :-
  length_lt(Z1, Z2).

length_gt(L1, L2) :-
  length_lt(L2, L1).


%-----------------------------------------
% member(?ITEM, ?LIST)
% 
% Find a member of a list, or generate all members
% of a list.
%

member(A, [A|_]).
member(A, [_|Z]) :- member(A, Z).


%-----------------------------------------
% nth_elem(+L, ?X, ?N)
% 
% Given a list, L, nth_elem finds either the position,
% starting at 1, of the elem X, or the elem at position
% N.
%

nth_elem(L, X, N) :-
  nth_elem(L, X, 1, N).

  nth_elem([X|Z], X, N, N).
  nth_elem([_|Z], X, A, N) :-
    AA is A + 1,
    nth_elem(Z, X, AA, N).  


%-----------------------------------------
% permutation(ListOfVars, List, LV)
% 
% permutation/3 assigns different values to the variables in the first
% list from the values in the second.  The third list is the list of
% unassigned values.  It works by simply deleting elements from the
% list using remove/3.  Because it is deleting an element which is an
% unbound variable, remove/3 simply deletes the next element and binds
% its value to the variable, thus providing a simple way to assign
% permuted values to a list of variables.  On backtracking, of course,
% delete simply binds the variable to the next element of the list and
% deletes it, thus eventually generating all permutations of a list.
%
% Example
% 
% ?- permutation([X,Y,Z], [a,b,c,d,e], L).
% 
% X = a
% Y = b
% Z = c
% L = [d, e] ;
% 
% X = a
% Y = b
% Z = d
% L = [c, e] ;
% ....
%

permutation([],L,L).
permutation([A|X],Y,L) :- atomic(A), permutation(X,Y,L).
permutation([A|X],Y,L) :- remove(A,Y,Y1), permutation(X,Y1,L).

%-----------------------------------------
% random_elem(+LIST, -ITEM)
%
% Pick a random element, ITEM, from a list, LIST.
%

random_elem(L, A) :-
   length(L, N),
   R is 1 + integer( random * N ),
   nth_elem(L, A, R).
   
%-----------------------------------------
% remove(?ITEM, +L1, -L2)
%
% Remove ITEM from list L1, leaving list L2.  Fails if
% no item to remove.
%

remove(A,[A|X],X).
remove(A,[B|X],[B|Y]) :- remove(A,X,Y).


%-----------------------------------------
% remove_duplicates(+L1, -L2)
% 
% remove_duplicates removes all the duplicates from L1 and
% returns the list of unique elements L2.  It is implemented
% to keep the first instance of each repeated element.
%

remove_dups(Lin, Lout) :-
   remove_dups(Lin, [], Lout).
   
remove_dups([], ACC, OUT) :-
   reverse(ACC, OUT).
remove_dups([X|Z], ACC, OUT) :-
   is_member(X, ACC),
   !,
   remove_dups(Z, ACC, OUT).
remove_dups([X|Z], ACC, OUT) :-
   remove_dups(Z, [X|ACC], OUT).

%-----------------------------------------
% replace_elem(+OldElem, +NewElem, +Lin, -Lout)
% 
% Replace first OldElem in list Lin, with NewElem, returning the new
% list in Lout.  Fails if nothing to replace.
%

replace_elem(_, _, [], _) :- !, fail.
replace_elem(Old, New, [Old|Z], [New|Z]) :- !.
replace_elem(Old, New, [X|Z], [X|Z2]) :-
  replace_elem(Old, New, Z, Z2).

%-----------------------------------------
% replace_all_elem(+OldElem, +NewElem, +Lin, -Lout)
% 
% Replace OldElem in list Lin, with NewElem, returning the new list
% in Lout.  Succeeds if nothing to replace.
%

replace_all_elem(_, _, [], []) :- !.
replace_all_elem(Old, New, [Old|Z], [New|Z2]) :-
  !,
  replace_all_elem(Old, New, Z, Z2).
replace_all_elem(Old, New, [X|Z], [X|Z2]) :-
  !,
  replace_all_elem(Old, New, Z, Z2).


%-----------------------------------------
% reverse(L1, L2)
% 
% Reverses L1 to L2.
%

reverse(A, Z) :- reverse(A, [], Z).

   reverse([], Z, Z).
   reverse([A|X], SoFar, Z) :- reverse(X, [A|SoFar], Z).


%-----------------------------------------
% shuffle(+L1, -L2)
% 
% Randomly shuffles the list L1 and returns L2, the
% shuffled list.  To get the same shuffling each time,
% use the built-in predicate seed_random/1 to provide an
% integer starting seed for random.
%

shuffle(Tin, Tout) :-
  shuffle1(Tin, [], Tout).

  shuffle1([], A, A).
  shuffle1(Tin, A, Tout) :-
    length(Tin, L),
    N is 1 + integer( random * L ),
    deleteN(N, Elem, Tin, Tx),
    shuffle1(Tx, [Elem|A], Tout).

%----------------------------------------
% sublist(Lin, Start, Finish, Lout)
%
% Either finds the sublist or returns it.
%

sub_list(List, Start, Finish, SubList) :-
   find_sublist(List, 1, Start, Finish, SubList).

find_sublist([X|Xs], S, S, F, [X|Ys]) :-
   find_finish(Xs, S, F, Ys).
find_sublist([X|Xs], Si, S, F, Sub) :-
   Sii is Si + 1,
   find_sublist(Xs, Sii, S, F, Sub).

find_finish(Xs, F, F, []) :-
   !.
find_finish([X|Xs], Fi, F, [X|Ys]) :-
   Fii is Fi + 1,
   find_finish(Xs, Fii, F, Ys).

%----------------------------------------
% write_formatted(+Formatted_List)
%
% write_formatted/1 writes a formatted list, allowing the
% format specifications in the form {Format}.  Formats
% can be nl, tab(N), quote, noquote, precision(N).
%

write_formatted(List) :-
   current_output(Out),
   write_formatted(Out, List).

write_formatted(Out, List) :-
   write_formatted(Out, List, []).

write_formatted(H, [], Formats) :-
   clear_formats(Formats).
write_formatted(H, [{F}|Z], InFormats) :-
   apply_format(H, F, InFormats, OutFormats),
   !, write_formatted(H, Z, OutFormats).
write_formatted(H, [A|Z], Formats) :-
   (member(quote, Formats) -> writeq(H,A); write(H,A)),
   write_formatted(H, Z, Formats).

apply_format(H, nl, Formats, Formats) :-
   !, nl(H).
apply_format(H, tab(N), Formats, Formats) :-
   !, tab(H,N).
apply_format(H, quote, InFormats, [quote|InCleared]) :-
   !,
   (remove(quote, InFormats, InCleared) -> true; InCleared = InFormats).
apply_format(H, noquote, InFormats, InCleared) :-
   !,
   (remove(quote, InFormats, InCleared) -> true; InCleared = InFormats).
apply_format(H, precision(N), InFormats, OutFormats) :-
   ( member(old_precision(_), InFormats) ->
       OutFormats = InFormats
       ;
       current_prolog_flag(decimal_places, DP),
       OutFormats = [old_precision(DP)|InFormats] ),
   set_prolog_flag(decimal_places, N).
   
clear_formats([]).
clear_formats([old_precision(P)|Z]) :-
   set_prolog_flag(decimal_places, P),
   !, clear_formats(Z).
clear_formats([_|Z]) :-
   clear_formats(Z).
   

%-----------------------------------------
% write_list(+L, +Separator)
% 
% write_list/2 writes each of the elements of a list, writing the
% Separator between elements.  For example, write_list(L, `\n  `),
% will write the elements of list L on newlines, indented two spaces.
%

write_list(List, Separator) :-
   current_output(Out),
   write_list(Out, List, Separator).
  
write_list(H, [], _).
write_list(H, [X], _) :-
  !, write(H, X).
write_list(H, [X|Y], Separator) :-
  write(H, X),
  write(H, Separator),
  write_list(H, Y, Separator).
  

%-----------------------------------------
% writeq_list(L, Separator)
% 
% writeq_list/2 writeqs each of the elements of a list, writing the
% Separator between elements.  For example, writeq_list(L, `\n  `),
% will writeq the elements of list L on newlines, indented two spaces.
%

writeq_list(List, Separator) :-
  current_output(Out),
  writeq_list(Out, List, Separator).

writeq_list(H, [X], _) :-
  !, writeq(H, X).
writeq_list(H, [X|Y], Separator) :-
  writeq(H, X),
  write(H, Separator),
  writeq_list(H, Y, Separator).


:- end_module(list).