%-------------------------------------------------------------------
% ALIS.PRO - Amzi! Prolog Command Line Interface
%
% Copyright (c) 1992-2012 Amzi! inc. All Rights Reserved.
%
%-------------------------------------------------------------------

%-------------------------------------------------------------------
% imports and exports
%

main$ :- amzi_listener:main.

:- body(amzi_listener).

%-------------------------------------------------------------------
% Main entry point - Special main$ for both CPI & CPC, see CPLIB.PRO
%
% This entry point is not used by the IDE listener!

main :-
   not(call(amzi_listener:'{sys}cline$files$loaded')),  % So if we abort we don't reconsult
   not(not(load_any_files)),
   assert(amzi_listener:'{sys}cline$files$loaded'),
   user:main, !.
main :-
%   assert(amzi_listener:'{sys}command'),   % indicate this is a command-line version
   %greetings,
   %write(`\nType 'quit.' to exit\n`),
   %top$loop(`\n?- `), !.
   go_listen.
   
%-------------------------------------------------------------------
% Utility predicates
%

load_any_files :-
   command_line(List),
   load_files(List), !.
load_any_files.

load_files([]) :- !.
load_files(["-?"|_]) :-
   about$mzi.
load_files([[H|T]|Rest]) :-
   H \= 0'-,                                   % ignore flags
   atom_codes(F, [H|T]),
   (
      consult(F) ->
      true ;

      write(`Warning: Cannot load file `), write(F), write(`... `), 
      press_any_key,
      nl
   ), !,
   load_files(Rest).

about$mzi :-
   write(`Usage: alis <filename>[.pro | .plm] \n`), 
   write(`  Either a .pro or .plm file can be listed on command line.\n`), 
   write(`  It will be automatically consulted, and \n`), 
   write(`  if main/0 is defined, it will be called \n`), write(`\n`), 
   write(`  alis with no arguments enters the interactive runtime.\n`), 
   halt.

:- end_body(amzi_listener).
