% Prolog user interface for configuration system.
% These same capabilities could be defined in
% VB, Java, or the Web.  By defining them in a separate
% file, like this, they can be used to test both
% the interface and the program.  Debugging of the
% logic can occur in a pure Prolog environment.

% A sample run looks like this:
%
% ?- [configure_ui].
% 
% yes
% ?- main.
% How many frames in the door unit?  1 - 4
% 2
% How tall is the frame?  inches
% 62
% How wide is the frame?  inches
% 56
% What frame material do you want?  [wood, clad, pvc]
% wood
% What color frame do you want?  [prime, white, grey]
% white
% What frame texture do you want?  [smooth]
% smooth
% How many doors in the frame?  [1, 2, 4]
% 2
% What type of door motion do you want?  [swinging, fixed]
% swinging
% Why type of metal for the hinge?  [brass, wrought_iron]
% brass
% 
% Order
% 
% door_unit(1)
%   frame(2)
%   height = 62
%   width = 56
%     material = wood
%     texture = smooth
%     color = white
%     door(2)
%       motion = swinging
%       hinge(2)
%         metal = brass
%       lock(1)
% 
% 
% Bill of Materials
% 
% frame_box(1*2)
% door_side(2*2)
% door_top(1*2)
% door_bottom(1*2)
% basic_lock(1*1)
% screws(4*2)
% brass_hinge(1*2)
% 
% 
% yes
% ?- 

% uses list library

:- ensure_loaded(list).

% makes sure the other files are consulted

:- reconsult(configure).
:- reconsult(configure_door).

% imports the two module's definitions

:- import(configure).
:- import(list).

main :-
   initialize,
   get_product(door_unit, Configuration),
   nl, write('Order'), nl, nl,
   output_assembly(0, Configuration), nl, nl,
   write('Bill of Materials'), nl, nl,
   get_bill_of_materials(Configuration, BoM),
   write_list(BoM, '\n'), nl, nl.

% prompt/3 called from the configure engine when it wants
% to ask the user a question.  would be implemented
% as an extended predicate in VB or other host language

prompt(Prompt, Choices, String) :-
   write(Prompt), tab(1), write(Choices), nl,
   read_string(String).

% display of the assembly tree, takes advantage of Prolog,
% might implement differently for host language

output_assembly(Indent, assembly(Component, Quantity, properties(Ps), contains(Cs))) :-
   tab(Indent), write(Component), write('('), write(Quantity), write(')'), nl,
   II is Indent + 2,
   output_properties(II, Ps),
   output_contains(II, Cs).

output_properties(_, []) :- !.
output_properties(Indent, [P|Ps]) :-
   tab(Indent), write(P), nl,
   !, output_properties(Indent, Ps).

output_contains(_, []) :- !.
output_contains(Indent, [A|As]) :-
   output_assembly(Indent, A),
   !, output_contains(Indent, As).
