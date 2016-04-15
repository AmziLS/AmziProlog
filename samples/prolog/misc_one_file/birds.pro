% BIRDS

% Copyright (c) 1990-1995 Amzi! inc.
% All rights reserved

% This is a sample of a classification expert system for identification
% of certain kinds of birds.  The rules are rough excerpts from "Birds of
% North America" by Robbins, Bruum, Zim, and Singer.

% This type of expert system can easily use Prolog's built in inferencing
% system.  While trying to satisfy the goal "bird" it tries to satisfy
% various subgoals, some of which will ask for information from the
% user.

% The information is all stored as attribute-value pairs.  The attribute
% is represented as a predicate, and the value as the argument to the
% predicate.  For example, the attribute-value pair "color-brown" is
% stored "color(brown)".

% "identify" is the high level goal that starts the program.  The
% predicate "known/3" is used to remember answers to questions, so it
% is cleared at the beginning of the run.

% The rules of identification are the bulk of the code.  They break up
% the problem into identifying orders and families before identifying
% the actual birds.

% The end of the code lists those attribute-value pairs which need
% to be asked for, and defines the predicate "ask" and "menuask"
% which are used to get information from the user, and remember it.

main :- identify.

identify:-
  retractall(known(_,_,_)),   % clear stored information
  bird(X),
  write('The bird is a '),write(X),nl.
identify:-
  write('I can''t identify that bird'),nl.

order(tubenose):-
  nostrils(external_tubular),
  live(at_sea),
  bill(hooked).
order(waterfowl):-
  feet(webbed),
  bill(flat).
order(falconiforms):-
  eats(meat),
  feet(curved_talons),
  bill(sharp_hooked).
order(passerformes):-
  feet(one_long_backward_toe).

family(albatross):-
  order(tubenose),
  size(large),
  wings(long_narrow).
family(swan):-
  order(waterfowl),
  neck(long),
  color(white),
  flight(ponderous).
family(goose):-
  order(waterfowl),
  size(plump),
  flight(powerful).
family(duck):-
  order(waterfowl),
  feed(on_water_surface),
  flight(agile).
family(vulture):-
  order(falconiforms),
  feed(scavange),
  wings(broad).
family(falcon):-
  order(falconiforms),
  wings(long_pointed),
  head(large),
  tail(narrow_at_tip).
family(flycatcher):-
  order(passerformes),
  bill(flat),
  eats(flying_insects).
family(swallow):-
  order(passerformes),
  wings(long_pointed),
  tail(forked),
  bill(short).

bird(laysan_albatross):-
  family(albatross),
  color(white).
bird(black_footed_albatross):-
  family(albatross),
  color(dark).
bird(fulmar):-
  order(tubenose),
  size(medium),
  flight(flap_glide).
bird(whistling_swan):-
  family(swan),
  voice(muffled_musical_whistle).
bird(trumpeter_swan):-
  family(swan),
  voice(loud_trumpeting).
bird(canada_goose):-
  family(goose),
  season(winter),                % rules can be further broken down
  country(united_states),        % to include regions and migration
  head(black),                   % patterns
  cheek(white).
bird(canada_goose):-
  family(goose),
  season(summer),
  country(canada),
  head(black),   
  cheek(white).
bird(snow_goose):-
  family(goose),
  color(white).
bird(mallard):-
  family(duck),          % different rules for male
  voice(quack),
  head(green).
bird(mallard):-
  family(duck),          % and female
  voice(quack),
  color(mottled_brown).
bird(pintail):-
  family(duck),
  voice(short_whistle).
bird(turkey_vulture):-
  family(vulture),
  flight_profile(v_shaped).
bird(california_condor):-
  family(vulture),
  flight_profile(flat).
bird(sparrow_hawk):-
  family(falcon),
  eats(insects).
bird(peregrine_falcon):-
  family(falcon),
  eats(birds).
bird(great_crested_flycatcher):-
  family(flycatcher),
  tail(long_rusty).
bird(ash_throated_flycatcher):-
  family(flycatcher),
  throat(white).
bird(barn_swallow):-
  family(swallow),
  tail(forked).
bird(cliff_swallow):-
  family(swallow),
  tail(square).
bird(purple_martin):-
  family(swallow),
  color(dark).

country(united_states):- region(new_england).
country(united_states):- region(south_east).
country(united_states):- region(mid_west).
country(united_states):- region(south_west).
country(united_states):- region(north_west).
country(united_states):- region(mid_atlantic).

country(canada):- province(ontario).
country(canada):- province(quebec).
country(canada):- province(etc).

region(new_england):-
  state(X),
  member(X, [massachusetts, vermont, etc]).
region(south_east):-
  state(X),
  member(X, [florida, mississippi, etc]).

region(canada):-
  province(X),
  member(X, [ontario,quebec,etc]).

nostrils(X):- ask(nostrils,X).
live(X):- ask(live,X).
bill(X):- ask(bill,X).
size(X):- menuask(size,X,[large,plump,medium,small]).
eats(X):- ask(eats,X).
feet(X):- ask(feet,X).
wings(X):- ask(wings,X).
neck(X):- ask(neck,X).
color(X):- ask(color,X).
flight(X):- menuask(flight,X,[ponderous,powerful,agile,flap_glide,other]).
feed(X):- ask(feed,X).
head(X):- ask(head,X).
tail(X):- menuask(tail,X,[narrow_at_tip,forked,long_rusty,square,other]).
voice(X):- ask(voice,X).
season(X):- menuask(season,X,[winter,summer]).
cheek(X):- ask(cheek,X).
flight_profile(X):- menuask(flight_profile,X,[flat,v_shaped,other]).
throat(X):- ask(throat,X).
state(X):- menuask(state,X,[massachusetts,vermont,florida,mississippi,etc]).
province(X):- menuask(province,X,[ontario,quebec,etc]).

% "ask" is responsible for getting information from the user, and remembering
% the users response.  If it doesn't already know the answer to a question
% it will ask the user.  It then asserts the answer.  It recognizes two
% cases of knowledge: 1) the attribute-value is known to be true,
%                     2) the attribute-value is known to be false.

% This means an attribute might have mulitple values.  A third test to
% see if the attribute has another value could be used to enforce
% single valued attributes. (This test is commented out below)

% For this system the menuask is used for attributes which are single
% valued

% "ask" only deals with simple yes or no answers.  a "yes" is the only
% yes value.  any other response is considered a "no".

ask(Attribute,Value):-
  known(yes,Attribute,Value),     % succeed if we know its true
  !.                              % and dont look any further
ask(Attribute,Value):-
  known(_,Attribute,Value),       % fail if we know its false
  !, fail.

% ask(Attribute,_):-
%   known(yes,Attribute,_),         % fail if we know its some other value.
%   !, fail.                        % the cut in clause #1 ensures that if
                                    % we get here the value is wrong.
ask(A,V):-
  write(A:V),                     % if we get here, we need to ask.
  write('? (yes or no): '),
  read(Y),                        % get the answer
  asserta(known(Y,A,V)),          % remember it so we dont ask again.
  Y = yes.                        % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer.  In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,Value,_):-
  known(yes,Attribute,Value),     % succeed if we know
  !.
menuask(Attribute,_,_):-
  known(yes,Attribute,_),         % fail if its some other value
  !, fail.

menuask(Attribute,AskValue,Menu):-
  nl,write('What is the value for '),write(Attribute),write('?'),nl,
  display_menu(Menu),
  write('Enter the number of choice> '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(yes,Attribute,AnswerValue)),
  AskValue = AnswerValue.         % succeed or fail based on answer

display_menu(Menu):-
  disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]):-            % recursively write the head of
  write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
  NN is N + 1,
  disp_menu(NN,Rest).

pick_menu(N,Val,Menu):-
  integer(N),                     % make sure they gave a number
  pic_menu(1,N,Val,Menu), !.      % start at one
pick_menu(Val,Val,_).             % if they didn't enter a number, use
                                  % what they entered as the value

pic_menu(_,_,none_of_the_above,[]).  % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).       % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]):-
  NextCtr is Ctr + 1,                % try the next one
  pic_menu(NextCtr, N, Val, Rest).
