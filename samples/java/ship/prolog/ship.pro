%-----------------------------------------
% SHIP.PRO  -  A Sample Rule-Based System
%
% This is a sample program that presents various shipping
% options for packages.  It has rules for packages shipped
% within the USA by either the US Mail or UPS.  It is not
% meant to be a complete system, and was put together for
% demonstration purposes only.
%
% It can be expanded to include international shipping,
% including all the interesting rules about various regulations
% for different countries.
%

% These operator definitions allow weights to be entered as
% pounds or ounces.  For example, 3 lb or 5 0z.  (Weight is
% carried internally in this application as Lbs:Ozs.)
 
:- op(300, xf, [lb, oz]).

% The program starts by gathering some predetermined data.
% Other data is asked for as needed.  After gathering the
% initial data, the program then determines all of the
% shipping options that apply to the situation, and present
% the cost and delivery time, which is always the shipping
% trade-off.

main :-
  get_inputs,
  get_options.

get_inputs :-
  retractall(known(_,_)),
  weight(_),
  destination(_),
  type(_),
  output_input.

output_input :-
  nl,
  weight(W),      write($weight       $), wr_weight(W), nl,
  destination(D), write($destination  $), write(D), nl,
  type(T),        write($type         $), write(T), nl,
  nl.

get_options :-
  option(Shipper, Service, Cost, Delivery),
  output_option(Shipper, Service, Cost, Delivery),
  fail.
get_options :-
  write($No more options\n$).

output_option(Shipper, Service, Cost, Delivery) :-
  write(Shipper:Service), nl,
  tab(2), wr_money(2,Cost), tab(2), write(Delivery), nl.

% These predicates provide for more readable displays of
% monetary quantities and weights.

wr_weight(Lb:Oz) :-
  (Lb > 0, write(Lb), tab(1), write(lbs), tab(1) ; true),
  (Oz > 0, write(Oz), tab(1), write(ozs) ; true).

wr_money(FieldWidth, N) :-
  Dollars is N // 100,
  Cents is N mod 100,
  write($$$$),
  Digits is Dollars // 10,
  Pad is FieldWidth - Digits,
  tab(Pad), write(Dollars),
  write($.$),
  (Cents = 0, write('00') ;
   Cents < 10, write('0'), write(Cents) ;
   write(Cents)),
  !.

% The main options are stored as shipper/4 rules.  Each shipper
% rule applies constraints for the particular service, and, if
% the service is available for this situation, computes the
% cost and delivery time.

option(Shipper, Service, Cost, Delivery) :-
  % The following is a call back to a Java method.  Uncomment
  % the extended predicate code in amzijava.c and rebuild
  % amzijava.dll to use it.
%  displayStatus($Analyzing shipping options...$),
  shipper(Shipper, Service, Cost, Delivery).

shipper('USMail', 'First Class Mail', Cost, 'Approximately two days') :-
  weight(W),
  W @=< 0:11,
  destination('USA'),
  once(first_class_mail(W, Cost)).
shipper('USMail', 'Priority Mail', Cost, 'Approximately two days') :-
  weight(W),
  W @=< 5:0,
  destination('USA'),
  once(priority_mail(W, Cost)).
shipper('USMail', 'Express Mail', Cost, 'Next day') :-
  weight(W),
  W @=< 7:0,
  destination('USA'),
  once(express_mail(W, Cost)).
shipper('UPS', 'Ground Service', Cost, Delivery) :-
  weight(Lb:Oz),
  (Oz > 0, Wlb is Lb + 1; Oz = 0, Wlb = Lb),
  destination('USA'),
  once(ups_ground(Wlb, C, Delivery)),
  ups_add_ons(A),
  Cost is C + A.
shipper('UPS', '2nd Day Air', Cost, 'Two days guaranteed') :-
  weight(Lb:Oz),
  (Oz > 0, Wlb is Lb + 1; Oz = 0, Wlb = Lb),
  destination('USA'),
  once(ups_blue(Wlb, C)),
  ups_add_ons(A),
  Cost is C + A.
shipper('UPS', 'Next Day Air', Cost, 'Next day guaranteed') :-
  weight(Lb:Oz),
  (Oz > 0, Wlb is Lb + 1; Oz = 0, Wlb = Lb),
  destination('USA'),
  once(ups_red(Wlb, C)),
  ups_add_ons(A),
  Cost is C + A.
  
% Supporting rules and tables for computing costs and delivery
% time.  This section is a mixture of tables, rules, and
% formulas for computing costs and delivery times.

first_class_mail(0:Oz, X) :-
  X is 32 + 23 * integer(Oz-1).

priority_mail(Lb:Oz, X) :-
  (Oz > 0, W is Lb + 1; W = Lb),
  X is 300 + 100 * (W - 2).

express_mail(W, X) :-
  express_mail_table(Wlim, X),
  W @=< Wlim.

express_mail_table(0:8, 1075).
express_mail_table(2:0, 1500).
express_mail_table(3:0, 1725).
express_mail_table(4:0, 1940).
express_mail_table(5:0, 2155).
express_mail_table(6:0, 2540).
express_mail_table(7:0, 2645).

ups_ground(Wlb, Cost, Delivery) :-
  zone(Z),
  ups_ground_table(Wlb, Z2, Z3, Z4, Z5, Z6, Z7, Z8),
  arg(Z, ups_ground_table(Wlb, Z2, Z3, Z4, Z5, Z6, Z7, Z8), Cost),
  ups_ground_delivery(Z, Delivery).

ups_ground_table(1, 228, 243, 266, 274, 283, 291, 297).
ups_ground_table(2, 230, 246, 293, 303, 324, 334, 358).
ups_ground_table(3, 240, 263, 312, 327, 353, 363, 395).
ups_ground_table(4, 250, 278, 324, 343, 366, 385, 423).
ups_ground_table(5, 261, 290, 331, 351, 383, 403, 444).

ups_ground_delivery(2, 'Two days').
ups_ground_delivery(3, 'Three days').
ups_ground_delivery(4, 'Three days').
ups_ground_delivery(5, 'Four days').
ups_ground_delivery(6, 'Five days').
ups_ground_delivery(7, 'Five days').
ups_ground_delivery(8, 'Six days').

ups_blue(_, 550) :-
  type(letter).
ups_blue(Wlb, Cost) :-
  not type(letter),
  ups_air_table(Wlim, Cost, _),
  Wlb < Wlim.

ups_red(_, 1050) :-
  type(letter).
ups_red(Wlb, Cost) :-
  not type(letter),
  ups_air_table(Wlim, _, Cost),
  Wlb < Wlim.

ups_air_table(1, 575, 1475).
ups_air_table(2, 650, 1525).
ups_air_table(3, 725, 1725).
ups_air_table(4, 775, 1850).
ups_air_table(5, 850, 2000).

ups_add_ons(X) :-
  findall(A, ups_add_on(A), L),
  sumlist(L, X).

ups_add_on(450) :-
  cod(yes).
ups_add_on(X) :-
  declared_value(V),
  V > 100,
  X is 30 * ((V-1) // 100).
  
% This section defines the various attributes used by the system,
% and defines the prompts used to get the value of the attribute
% from the user.

cod(X) :-
  menuask($Is it a cod package? $, cod, X, [yes,no]).
declared_value(X) :-
  ask($What is the declared value (in dollars)? $, declared_value, X).
destination(X) :-
  menuask($Where's it going? $, destination, X, ['USA']).
type(X) :-
  menuask($What type of package is it? $, type, X, [letter, brochure, package]).
weight(X) :-
  ask($How much does it weigh (lb:oz)? $, weight, W),
  once(fix_weight(W, X)).
zone(X) :-
  ask($What UPS zone is it going to? $, ups_zone, X).

% Weight might be in a number of formats, so these rules convert
% those formats to the Lb:Oz format that is used internally.

fix_weight(Lb:Oz, Lb:Oz).
fix_weight(Lb lb, Lb:0).
fix_weight(Z oz, Lb:Oz) :-
  Oz is Z mod 16,
  Lb is Z // 16.
fix_weight(N, W) :-
  integer(N),
  fix_weight(N oz, W).
fix_weight(N, W) :-
  float(N),
  Ni is integer(N),
  (N > Ni, Nr is Ni + 1; Nr = Ni), 
  fix_weight(Nr oz, W).

%-------------------------------------------------------------------------
% utilities
%

% Sum up a list of numbers.

sumlist(L, Sum) :-
  sumlist(L, 0, Sum).

  sumlist([], Sum, Sum).
  sumlist([X|Y], SoFar, Sum) :-
    Temp is SoFar + X,
    sumlist(Y, Temp, Sum).

% Ask the user for an attribute's value, and save the answer
% so we don't have to ask again.

ask(_, Attribute, Value):-
  known(Attribute,X),             % we've already asked
  !,                              % so don't ask again
  Value = X.                      % succeed or fail based on query
ask(Prompt, A, V):-
  write(Prompt),                  % ask for the value
  read_string(S),                 % use read_string so period not needed
  string_term(S, X),
  assert(known(A,X)),             % remember the answer
  !,
  V = X.                          % succeed or fail based on answer

% "menuask" is like ask, only it gives the user a menu to choose
% from rather than a yes on no answer.

menuask(_, Attribute, Value,_) :-
  known(Attribute, X),
  !,
  Value = X.
menuask(Prompt, Attribute, Value, Menu):-
  write(Prompt), nl,
  display_menu(Menu),
  write($Enter the number of choice > $),
  pick_menu(Num, AnswerValue, Menu),
  asserta(known(Attribute,AnswerValue)),
  !,
  Value = AnswerValue.         % succeed or fail based on answer

  display_menu(Menu):-
    disp_menu(1,Menu), !.           % make sure we fail on backtracking

    disp_menu(_,[]).
    disp_menu(N,[Item | Rest]):-            % recursively write the head of
      write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
      NN is N + 1,
      disp_menu(NN,Rest).

  pick_menu(N,Val,Menu):-
    read_string(S),
    string_term(S,N),
    integer(N),                     % make sure they gave a number
    pic_menu(1,N,Val,Menu),
    Val \= none_of_the_above,
    !.
  pick_menu(N,Val,Menu) :-
    write($Invalid response, enter number of choice > $),
    pick_menu(N,Val,Menu).

    pic_menu(_,_,none_of_the_above,[]).  % if we've exhausted the list
    pic_menu(N,N, Item, [Item|_]).       % the counter matches the number
    pic_menu(Ctr,N, Val, [_|Rest]):-
      NextCtr is Ctr + 1,                % try the next one
      pic_menu(NextCtr, N, Val, Rest).
