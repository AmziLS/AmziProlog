% CUSTORD 

% copyright (c) 1990-1995 Amzi! inc.
% All rights reserved

% This is a sample Prolog program which implements a portion
% of a customer order inventory application.  It is not intended to
% be complete, and only illustrates the concept of writing a database
% application in Prolog.

% This example extends the concept of an intelligent database to include
% a full database application.  It is really a rule based approach to
% transaction processing.  In fact a large percentage of the procedural
% code normally written in database applications has to do with
% enforcing semantic integrity rules involving multiple records.

% The distinction between data and process is thoroughly blurred.  Both
% reside together in the same logicbase.

% There is pure data as it might be defined in a relational database
% (customer, item, inventory, order); there are rules which really
% represent data views (item_quant); there are rules which add
% intelligence to the database (good_customer, valid_order); and there
% are rules which are processes (order, report_inventory).  

main :- order.

% customer(Name, Town, Credit-rating).

:- dynamic customer/3.

customer(dennis, winchester, xxx).
customer(dave, lexington, aaa).
customer(ron, lexington, bbb).
customer(julie, winchester, aaa).
customer(jawaid, cambridge, aaa).
customer(tom, newton, ccc).

% item(Number, Name, Reorder-quantity).

:- dynamic item/3.

item(p1,thing,10).
item(p2,stuff,10).
item(p3,article,10).
item(p4,object,10).
item(p5,substance,10).
item(p6,piece,10).
item(p7,matter,10).

% inventory(Number, Quantity).

:- dynamic inventory/2.

inventory(p1,10).
inventory(p2,10).
inventory(p3,10).
inventory(p4,78).
inventory(p5,23).
inventory(p6,14).
inventory(p7,8).

% item-inv view or join

item_quant(Item, Quantity):-
  item(Partno, Item, _),
  inventory(Partno, Quantity).

% reorder if inventory below reorder point

reorder(Item):-
  item(Partno, Item, Reorder_point),
  inventory(Partno, Quantity),
  Quantity < Reorder_point,
  write('Time to reorder '),
  write(Item), nl.
reorder(Item):-
  write('Inventory level ok for '),
  write(Item), nl.

% a good customer has a credit rating of aaa 
% or lives in winchester
% or has ordered something

good_customer(Cust):-
  customer(Cust, _, aaa).
good_customer(Cust):-
  customer(Cust, winchester, _).
good_customer(Cust):-
  order(Cust, _, _).

% process order

order:-
  write('Customer: '),
  read(Customer),
  write('Item: '),
  read(Item),
  write('Quantity: '),
  read(Quantity),
  valid_order(Customer,Item,Quantity),
  asserta(order(Customer,Item,Quantity)),
  update_inventory(Item,Quantity),
  reorder(Item).

% an order is valid if
% it doesn't go below zero inventory and
% the customer is a good customer

valid_order(C, I, Q):-
  item(Partno, I, _),
  inventory(Partno, Onhand),
  Q =< Onhand,
  good_customer(C).
valid_order(C, I, Q):-
  write('Bad order'),
  nl,
  fail.

% update the inventory

update_inventory(I,Q):-
  item(Pn, I, _),
  inventory(Pn, Amount),
  NewQ is Amount - Q,
  retract(inventory(Pn, Amount)),
  asserta(inventory(Pn, NewQ)).

% inventory report

report_inventory:-
  item_quant(I, Q),
  write(I), tab(1),
  write(Q), nl,
  fail.
report_inventory:-true.  
