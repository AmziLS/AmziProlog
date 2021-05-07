% Logic Base Query, A Tcl/Tk Sample
%
% This sample illustrates using Tcl/Tk to put a graphical
% inteface on a program that does logicbase queries.
%
% See the help.txt file for more details, which is also available
% by running the program.
%

:- loadlsx(atcltk).
:- import(gui).

main :-
   gui:go.

%----------------------------------------------------------------
% Module to hold GUI code
%

:- module(gui).
:- export quit/0.
:- export log/1.
:- export open/1.
:- export help/0.

go :-
   create_gui,
   tk_mainloop.

% To create the GUI, first call tcl_init to get a
% Tcl interpreter.  Save it's ID for other calls to use.
% This is necessary because you can have multiple Tcl
% intepreters going at the same time.

create_gui :-
   tcl_init(TclInterp, amzi, _),
   assert(tcl_interp(TclInterp)),
   !,
   initial_widgets(TclInterp).

% The main Tcl/Tk code for creating the GUI is, in this case,
% stored as a list of strings in widgets/1, which are then fed
% recursively to tcl_eval/3.  This keeps the Tcl/Tk code
% embedded in the logic program. Alternatively, the Tcl/Tk code
% could have been in an external .tcl file, which is then
% called from tcl_eval_file/3 instead.

initial_widgets(TclInterp) :-
   widgets(W),
   reconsult('wides.lb'),
   do_tcl(TclInterp, W).

% Tcl/Tk might throw errors that we can catch from Prolog

tcl_error(E, C) :-
   write('**** Tcl/Tk Error ****':E),
   nl,
   write('  While evaluating':C),
   nl,
   tcl_finish,
   fail.

do_tcl(_, []) :-
   !.
do_tcl(I, [C|Cs]) :-
   catch(
      tcl_eval(I, C, _),
      E,
      tcl_error(E, C) ),
   !,
   do_tcl(I, Cs).

% All the original widgets for the GUI are kept as a list of
% Tcl/Tk command strings.

widgets([

% Create a main window ( . ) with title Listen.

      `wm title . "Listen"`,

% Add a menubar with File, Edit and Help, but only File
% will have anything useful under it.

      `menu .menubar`,
      `. config -menu .menubar`,
      `foreach m {File Edit Help} { set $m [menu .menubar.m$m]
             .menubar add cascade -label $m -menu .menubar.m$m }`,

% Add two submenus under the main File menu. Open will open
% the Tcl/Tk file dialog, and pass the file name to the logic
% program to open.  Quit simply calls the logic program to quit.            

      `$File add command -label Open -command {
            ls_exec "open('[
                  tk_getOpenFile -title "Open Logic Base" -filetypes { {"Logic Base" {".lb"} {}} }]')" }`,
      `$File add command -label Quit -command { ls_exec "quit" }`,
      
      `$Help add command -label Help -command { ls_exec "help" }`,
      
% Create a frame for buttons illustrating the various query options.

      `frame .buts -borderwidth 10`,
      
      `button .buts.once -text Once -command {
             set answer [ls_query once $query];
             display $log $answer;
             set query ""; }`,
      `button .buts.first -text First -command {
             display $log $query;
             set answer [ls_query first $query];
             display $log $answer;
             set query ""; }`,
      `button .buts.next -text Next -command {
             set answer [ls_query next];
             display $log $answer;
             if {$answer == "no"} { set query ""; }; }`,
      `button .buts.clear -text Clear -command {
             set query "";
             ls_query clear; }`,
      `button .buts.echo -text Echo -command {
             display $log $query; }`,
             
% A procedure to format the output list

      `proc display {log names_values} {
           $log insert end "\n";
           foreach {name value} $names_values {
               $log insert end "$name = $value \n";  }
           $log yview moveto 1;  }`,

% Create an input area for the user to enter a query.

      `entry .query -width 40 -textvariable query`,
      
% Create an output area for logging results.

      `frame .answer -padx 4 -pady 4`,
%      `set log [text .answer.log -height 10 -width 80 -relief raised -font MingLiU -setgrid true -yscrollcommand {.answer.scroll set}]`,
      `set ufont MingLiU`,
%      `set ufont "Cambria Math"`,
      `set log [text .answer.log -height 10 -width 80 -relief raised -font $ufont -setgrid true -yscrollcommand {.answer.scroll set}]`,
      `scrollbar .answer.scroll -command {.answer.log yview}`,
      
% Pack it all together.

      `pack .buts -side top`,
      `pack .buts.once .buts.first .buts.next .buts.clear .buts.echo -side left`,
      `pack .query -side top -padx 4 -pady 2`,
      `pack .answer -side top -padx 4 -pady 2 -fill both -expand true`,
      `pack .answer.scroll -side right -fill y`,
      `pack .answer.log -side left -fill both -expand true`,
      `focus .query` ]).

% Allow a predicate to write to the log window,
% and keep positioned at the end.

log(X) :-
   tcl_interp(I),
   stringlist_concat([`$log insert end "`, X, `\n"; $log yview moveto 1`], TclStr),
   tcl_eval(I, TclStr, _).

% Shut down the Tcl interpreter.

quit :-
   tcl_interp(I),
   tcl_finish(I).

% Reconsult a file the user picked from the file/open menu.

open(F) :-
   stringlist_concat([`Opening `, F], Msg),
   log(Msg),
   reconsult(F),
   log(`Opened`).

help :-
   tcl_init(HelpInterpreter, amzi, _),
   show_help(HelpInterpreter).

show_help(HI) :-
   help_widgets(W),
   do_tcl(HI, W).

help_widgets( [
      `wm title . "Help"`,
      `text .help -height 10 -width 80 -relief raised -setgrid true -yscrollcommand {.help.scroll set}`,
      `scrollbar .help.scroll -command {.help yview}`,
      `pack .help.scroll -side right -fill y`,
      `pack .help -side top -padx 4 -pady 2 -fill both -expand true`,
      `set help_channel [open "help.txt" r]`,
      `while { [gets $help_channel line] >= 0 } {
            .help insert end "$line \n"  }`,
      `close $help_channel`
   ]).

:- end_module(gui).