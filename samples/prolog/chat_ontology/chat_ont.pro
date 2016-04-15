% Simple program to illustrate chatbot and ontology software for
% AI Expert newsletter

:- ensure_loaded(list).
:- import(list).

main :-
   write('DDJ AI Expert ChatBot Sample'), nl,
   repeat,
   write('> '), read_string(InputString),
   respond(InputString, ResponseString),
   write(ResponseString), nl,
   InputString == `quit`.

respond(InputString, ResponseString) :-
   string_to_list(InputString, InputWordList),
   swap_person(InputWordList, SwappedWordList),
   form_response(SwappedWordList, ResponseWordList),
   list_to_string(ResponseWordList, ResponseString),
   !.

string_to_list(String, List) :-
   string_tokens(String, List).

list_to_string(List, String) :-
   stringlist_concat(List, ` `, String).

swap_person([], []).
swap_person([X|Xs], [Y|Ys]) :-
   swap_word(X,Y),
   !, swap_person(Xs,Ys).

swap_word(X,Y) :- me_you(X,Y).
swap_word(X,Y) :- me_you(Y,X).
swap_word(W,W).

form_response(Input,Response) :-
   response(InputPattern, ResponsePatterns),
   match(InputPattern, Input),
   random_elem(ResponsePatterns, ResponsePattern),
   flatten(ResponsePattern, Response).

match([], []).
match([Xs], IWs) :-
   var(Xs),
   !,
   Xs = IWs.
match([Xs,W|PWs], IWs) :-
   var(Xs),
   !,
   fill_var(Xs, W, IWs, IWsLeft),
   match([W|PWs], IWsLeft).
match([property(W,P,V)|PWs], [W|IWs]) :-
   property(W,P,V),
   !,
   match(PWs,IWs).
match([W|PWs], [W|IWs]) :-
   !,
   match(PWs, IWs).

fill_var([], W, [W|IWs], [W|IWs]) :-
   !.
fill_var([], property(W,P,V), [W|IWs], [W|IWs]) :-
   property(W,P,V),
   !.
fill_var([X|Xs], W, [X|IWs], IWsLeft) :-
   fill_var(Xs, W, IWs, IWsLeft).

property(Word, Property, Value) :-
   word(Word, PropertyList),
   member(Property = Value, PropertyList).
property(Word, Property, Value) :-
   transitive(Property),
   word(Word, PropertyList),
   member(Property = Word2, PropertyList),
   property(Word2, Property, Value).


%-----------------------------------------------------------
% The knowledge
%

me_you(i,you).
me_you(me,you).
me_you(my,your).
me_you(am,are).

response( [_,blue,screen,_],
  [ [why,do,you,use,windows,?],
    [wipe,your,computer,clean,and,install,linux,'.'] ]).
response( [_,property(W,kind_of,windows),_],
  [ [you,should,dump,W,and,switch,to,unix,'.'] ]).
response( [_,property(W,kind_of,unix),_],
  [ [you,should,dump,W,and,switch,to,windows,'.'] ]).
response( [_,property(W,kind_of,food),_],
  [ [are,you,talking,about,W,because,you,are,hungry,'?'],
    [when,did,you,last,eat,W,'?'] ]).
response( [yes,_],
  [ [why,?] ]).
response( [no,_],
  [ [why,not,?] ]).
response( [_,you,like,X],
  [ [how,can,you,like,X,?],
    [is,it,strange,to,like,X,?] ]).
response( [_,blank,screen,_],
  [ [try,plugging,it,in,'.'] ]).
response( [_,linux,_],
  [ [nobody,supports,linux,'.'] ]).
response( [_,learn,_,ai,_],
  [ [subscribe,to,ddj,ai,expert,newsletter,'.'] ]).
response( [_,want,to,X],
  [ [why,would,you,want,to,X,?],
    [you,can,not,X,'.'],
    [is,it,dangerous,to,X,?] ]).
response( [X],
  [ [X,?] ]).

word(windows, [
  kind_of = operating_system ]).
word(unix, [
  kind_of = operating_system ]).
word(xp, [
  kind_of = windows ]).
word(w2000, [
  kind_of = windows ]).
word(linux, [
  kind_of = unix ]).
word(pizza, [
  kind_of = food,
  contains = cheese,
  contains = spaghetti_sauce ]).
word(broccoli, [
  kind_of = food,
  color = green ]).
word(donut, [
  kind_of = food,
  contains = sugar ]).
word(spaghetti_sauce, [
  kind_of = food,
  contains = tomato ]).

word(joe, [
  instance_of = person,
  allergy = tomato ]).
word(jill, [
  instance_of = person,
  allergy = cheese ]).

transitive(kind_of).
transitive(contains).

can_eat(Person, Food) :-
  property(Person, instance_of, person),
  property(Person, allergy, Allergy),
  property(Food, kind_of, food),
  not property(Food, contains, Allergy).

cannot_eat(Person, Food) :-
  property(Person, instance_of, person),
  property(Person, allergy, Allergy),
  property(Food, kind_of, food),
  property(Food, contains, Allergy).



