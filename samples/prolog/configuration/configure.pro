% Configuration System

% The idea is to separate the general purpose reasoning
% logic for the application from the domain-specific
% knowledge.  So this code knows nothing about doors, windows,
% lawn mowers, whatever.

:- ensure_loaded(list).

:- module(configure).

:- import(rules).
:- import(list).

:- export initialize/0.
:- export get_product/2.
:- export get_bill_of_materials/2.

:- export get_bom/2.
:- export get_component/3.
:- export ask/2.
:- export known/2.

% Initialize, which so far just means wiping out what
% might have been 'known' from the last run of the system.

initialize :-
   abolish(known/2).

% A special test case entry point that asserts the known
% facts without exercising the dialog with the user part of
% the system.  To use it, rename it to initialize, and rename
% the real one above to something else.

initialize2 :-
   abolish(known/2),
   assert( known(frame_quantity, 2) ),
   assert( known(frame_height, 55) ),
   assert( known(frame_width, 44) ),
   assert( known(frame_material, wood) ),
   assert( known(frame_color, white) ),
   assert( known(frame_texture, smooth) ),
   assert( known(door_quantity, 2) ),
   assert( known(door_motion, swinging) ),
   assert( known(hinge_metal, wrought_iron) ).

% A predicate to get the main product, which will be a
% door_unit for the sample.

get_product(Product, assembly(Product, 1, Properties, Contains)) :-
   get_component(Product, Properties, Contains).

% Just a layer between generic and the actual knowledge.  No
% use is made of the layer right now, but, for example, tracing
% information could be including for testing and debugging
% the knowledge base.

get_bill_of_materials(Unit, BoM) :-
   bill_of_materials(Unit, BoM).

% Called from the knowledge base to get all the required parts
% for the assemblies in a contained list.  In other words, it is
% used in recursive calls as the assembly is walked.

get_bom(contains(Cs), BoM) :-
   get_bom(Cs, [], BoM).

get_bom([], BoM, BoM) :- !.
get_bom([Component|Cs], Acc, BoM) :-
   get_bill_of_materials(Component, B1),
   append(B1, Acc, Acc2),
   !,
   get_bom(Cs, Acc2, BoM).
   
% Again, a simple layering, not taken advantage of, but
% could be used to add diagnostic information.

get_component(Component, Properties, Contains) :-
   component(Component, Properties, Contains).

% The basic ask/2 predicate.  The first time it will
% ask a question, and remember the answer in a known/2
% fact.  Subsequent queries for a given attribute
% will just used the saved result.
%
% This will also allow for the editing of previous answers,
% changing the configuration and the like.

ask(A, V) :-
   known(A, X),
   !,
   X = V.
ask(A, V) :-
   question(A, Prompt, Choices),
   prompt(Prompt, Choices, String),
   string_term(String, X),
   assert(known(A, X)),
   !,
   X = V.

:- end_module(configure).