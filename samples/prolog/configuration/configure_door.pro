% Configuration rules for doors
%
% This is the product-specific knowledge base for doors.
% The idea is this file could be replaced for other
% domains.
%
% It is a bit of a hybrid.  With more time, a better
% representation might be found.  The idea is this is just
% the declarative specification of the configuration and
% bill of materials rules.  (Pricing could be added as well.)
%
% With the current version of the system, the experts would
% edit this text file.  But it would not be difficult to create
% a GUI front-end that served as an editor on such a file, so
% the user wouldn't have to worry about all the punctuation. In
% other words, the GUI would create these structures.
%
% That is exactly what KnowledgeWright (KW) does.  If we were to
% build a configuration jig for KW we would do a little more
% work on this knowledge representation, and allow it to be
% edited with the KW GUI test and development environment.
%
% The file configure.pro contains the reasoning logic for
% this file, and it would be the basis for implementing a
% customized reasoning engine for a KW Configure Jig.
%
% There are two ways to represent and build the tree structure
% representing the product.  The more classic approach would
% be to create linked clauses, like database rows, that defined
% the emerging configuration.  This would be done with a
% data-driven forward-chaining reasoning engine.  This is NOT
% the approach taken here.
%
% This approach takes advantage of the ability to define
% a Prolog structure with 'hole's and fill in those holes
% later.
%
% To simplify, the system starts by looking to configure
% the door_unit.  So it creates (simplifying) something
% like:
%
%    assembly(door_unit, contains([assembly(frame, X)]))
%
% where 'X' (anything beginning with an upper case letter is
% a variable) is a variable, or hole in the structure.  It will
% be filled in with the contents of the frame.  The rule for
% door_unit then calls the rule for frame to find that value.
%
% The rule(s) for frame return similar structures, also with
% holes for doors, then hinges, and anything else that needs
% to be included.
%
% This way a full structure is built up, from the top down,
% that represents the final configuration.
%

:- ensure_loaded(list).

:- module(rules).
:- import(configure).
:- import(list).
:- export component/3.
:- export question/3.
:- export bill_of_materials/2.

% Each component has a name, property list, and contents list.  The :- symbol
% (neck) separates the data for the component from the conditions that are
% applied in getting the component.
%
% In the case of the door_unit, the rule part gets the quantity of frames
% from the user, and then calls get_component to get the details about
% the type of frame to be included in the door_unit.  Those items beginning
% with upper case letters, like FrameProperties, are variables, that will
% be resolved in the call to component clause for frames.

component( door_unit,
      properties([]),
      contains([assembly(frame, FrameQuantity, FrameProperties, FrameContents)]) )
   :-
   ask(frame_quantity, FrameQuantity),
   get_component( frame, FrameProperties, FrameContents ).

% The frame component, has a bunch of questions for the properties.  It just
% contains doors for now.

component( frame,
      properties([
         height = H,
         width = W,
         material = M,
         texture = T,
         color = C ]),
      contains( [assembly(door, DoorQuantity, DoorProperties, DoorContents)] ) )
   :-
   ask(frame_height, H),
   ask(frame_width, W),
   ask(frame_material, M),
   ask(frame_color, C),
   ask(frame_texture, T),
   ask(door_quantity, DoorQuantity),
   get_component( door, DoorProperties, DoorContents ).

% The door has the only bit of trickiness coded so far.  There
% are two types of doors, which one is picked will depend on
% whether the motion of the door is swinging, or fixed.

component( door,
      properties([motion = swinging]),
      contains( [
         assembly(hinge, 2, HingeProperties, HingeContents),
         assembly(lock, 1, LockProperties, LockContents)] ) )
   :-
   ask(door_motion, swinging),
   get_component( hinge, HingeProperties, HingeContents ),
   get_component( lock, LockProperties, LockContenrs ).
   
component( door,
      properties([swinging = fixed]),
      contains( [] ) )
   :-
   ask(door_motion, fixed).

% Finally the hinges and lock.  A choice on hinge, but just
% one type of lock for now.

component( hinge,
      properties([metal = M]),
      contains( [] ) )
   :-
   ask(hinge_metal, M).

component( lock,
      properties([]),
      contains( [] ) ).

% frame material, texture, color

frame_properties(wood, smooth, prime).
frame_properties(wood, smooth, white).
frame_properties(wood, smooth, grey).
frame_properties(clad, smooth, white).
frame_properties(clad, smooth, almond).
frame_properties(clad, grain, brown).
frame_properties(clad, grain, tan).
frame_properties(pvc, smooth, prime).
frame_properties(pvc, smooth, white).
frame_properties(pvc, smooth, grey).
frame_properties(pvc, wrinkle, white).
frame_properties(pvc, wrinkle, brown).
frame_properties(pvc, wrinkle, sand).
frame_properties(pvc, smooth, white).

% The questions to ask the user.  The ask/2 goals
% above will result in the use of these bits of
% knowledge in posing questions to the user.

question(frame_quantity,
   `How many frames in the door unit? `,
   1-4 ).

question(frame_height,
   `How tall is the frame? `,
   inches).

question(frame_width,
   `How wide is the frame? `,
   inches).

% The question objects can be rules as well.  So the
% menu of choices for the next three is determined by
% what has already been picked.  A bit of Prolog trickiness
% here, which is probably inappropriate for the final system.
% It uses findall, and either a unified texture, for example,
% or not.  That is, if texture is known, T will be unified
% but if T is not known, T will be a variable and any texture
% will be accepted in the final list.

question(frame_material,
   `What frame material do you want? `,
   Choices ) :-
      (known(frame_texture, T); true),
      (known(frame_color, C); true),
      findall(M, frame_properties(M,T,C), Ms),
      remove_dups(Ms, Choices).

question(frame_texture,
      `What frame texture do you want? `,
      Choices )
   :-
   (known(frame_material, M); true),
   (known(frame_color, C); true),
   findall(T, frame_properties(M,T,C), Ts),
   remove_dups(Ts, Choices).

question(frame_color,
    `What color frame do you want? `,
      Choices )
   :-
   (known(frame_material, M); true),
   (known(frame_texture, T); true),
   findall(C, frame_properties(M,T,C), Cs),
   remove_dups(Cs, Choices).

question(door_motion,
   `What type of door motion do you want? `,
   [swinging, fixed] ).

question(door_quantity,
   `How many doors in the frame? `,
   [1,2,4] ).

question(hinge_metal,
   `Why type of metal for the hinge? `,
   [brass, wrought_iron] ).

% A catch all in case we forget a question, it will
% just prompt with the attribute name.

question(Attr, Attr, []).

% bill_of_materials rules, what do we actually need
% to build the assembly determined by the configuration
% rules.  No real smarts except for the two types of
% hinges require different amounts of screws.

bill_of_materials( assembly(hinge, Q, properties(Ps), _), [screws(4*Q), brass_hinge(1*Q)] ) :-
   member(metal=brass, Ps).

bill_of_materials( assembly(hinge, Q, properties(Ps), _), [screws(6*Q), iron_hinge(1*Q)] ) :-
   member(metal=wrought_iron, Ps).

bill_of_materials( assembly(lock, Q, _, _), [basic_lock(1*Q)] ).

bill_of_materials( assembly(door, Q, _, Contains), [door_side(2*Q), door_top(1*Q), door_bottom(1*Q)|Rest] ) :-
   get_bom(Contains, Rest).

bill_of_materials( assembly(frame, Q, _, Contains), [frame_box(1*Q)|Rest] ) :-
   get_bom(Contains, Rest).

bill_of_materials( assembly(door_unit, Q, _, Contains), BoM ) :-
   get_bom(Contains, BoM).

% pricing rules would be implemented in an analagous manor.  Not done
% at this time.

:- end_module(rules).

