%
% AmziCGI -- Amzi CGI Interface
% Copyright (c) 1992-2002 Amzi! inc. All Rights Reserved
%
% Requirements:
%   Amzi! Prolog + Logic Server
%   Standard CGI Compliant Web Server for Windows, Solaris or Linux
%
% Architecture:
%
% This allows the use of Amzi! Prolog + Logic Server as a standard
% CGI script.  
%
% The Amzi! Prolog CGI Interface is a collection of routines that depend upon C
% functions in acgi[.exe] (amzicgi.c/amzisub.c) to process CGI input and output.
% There are routines to write the output file, keep a log, handle uncaught errors, 
% etc.  Generally you will not change these.  All the cgi routines have names 
% beginning with 'cgi'.
%
% This is a self-contained module except that it expects you to provide the
% definition for these predicates:
%    cgiLoad/0 -- to setup a logfile when debugging and to consult or load 
%                 additional Prolog code
%    cgiMain/0 -- the script itself
%    cgiErrorNote/0 -- optionally outputs additional information for 
%                 fatal errors
%
% The C portion asserts the following dynamic predicates:
%    system(Name, Value) - CGI system variable values
%    accept(Name, Value) - CGI accept variable values
%    extraheaders(Name, Value) - CGI extraheaders variable values
%    cgi(Name, Value) - CGI variable values
%    fact(Name, Value) - CGI web form user inputs
%
% The following dynamic predicates are used by this module, if available:
%    system('Log File', LogFilePathName) - The pathname of the log file. If
%       set, then calls to the cgi logging functions output to that file.
%    system('Log File URL', LogURL) - The URL to the log file. Used on the
%       error screen for the user to display the log file.
%
% The following dynamic predicates are used internally by this module:
%    system('Log Handle', LogHandle)
%
%


:- import(list).


% ------------------------------------------------------------------- %
%                        Prolog CGI Interface                         %
% ------------------------------------------------------------------- %

%
% cgi_main
%
% The main program.  
%
cgi_main :-
% dump(init, '/usr/dennis/acgidump.txt'),
% dump(init, 'c:\\InetPub\\Scripts\\acgidump.txt'),

   % amzicgi.c asserts the CGI facts into the dynamic database before
   % running main

   catch(
      (
      % Open the CGI output file
      once(cgiInitOutput),

      % Let the script load its logic-base (if not compiled in)
% dump(time, start_cgiLoad),
      once(cgiLoad),
% dump(time, end_cgiLoad),

% dump(time,start_cgiMain),
      % Do the work
      once(cgiMain)
      ), X, cgiError(X)),
% dump(time,end_cgiMain),
    cgiFinish,
% dump(time,end_cgiFinish),
% dump(close),
    true.
cgi_main :-
   cgiError($cgiInit*, Main or Finish failed$),
   cgiFinish,
% dump(close),
   true.

/* Predicates that provide a dump file with timing
   information in it. */

/* New version of dump_time predicates, the dump is stored
   in the dynamic database, and then just reported at the
   end.  This is to eliminate the I/O for the dump from
   the timing statistics. */

dump(init, F) :-
   assert(dump_file(F)),
   T is cputime,
   assert(last_dump(start, T)).

dump(time, X) :-
  T is cputime,
  assert(last_dump(X,T)).

dump(close) :-
   dump_file(F),
   fopen(Hnd, F, w),
   time(H,M,S),
   write(Hnd, H:M:S), nl(Hnd), nl(Hnd),
   findall(Text:Time, last_dump(Text,Time), L),
   report_dump(Hnd, L),
   !, fclose(Hnd).

% --- dump support predicates ---

   report_dump(H, []).
   report_dump(H, [Text:T]) :-
     write(H, Text:T),
     nl(H).
   report_dump(H, [Text:T1, NextText:T2|Z]) :-
     DeltaT is integer(100 * (T2 - T1)),
     write(H, DeltaT:Text:T1),
     nl(H),
     report_dump(H, [NextText:T2|Z]).

%
% cgiInitOutput
%
% Opens stdout through amzisub.c
%
cgiInitOutput :-
   % Let cgiOpen set the output file to stdout
   cgiOpen($$).   
cgiInitOutput :-
   throw($cgiInitOutput failed\n$).

%
% cgiFinish
%
% Closes the output file and log file so we can exit
%
cgiFinish :-
   cgiClose,
   cgiCloseLog.
cgiFinish.

%
% cgiError(Message)
%
% Handles thrown errors--rewinds the output file and sends out the error 
% message in an HTML page.
%
cgiError(Message) :-
   % If the log file is open, then write the message to it. If its not open
   % assume the problem that got us here was opening it
   (system('Log Handle', Handle) -> cgiLog(Message) ; true),
   cgiErrorHeader,
   cgiDisplayErrorMessage(Message),
   cgiSend($<P>$), 
   (cgiErrorNote ; true), 
   cgiSend($<P><HR><CENTER>$), 
   (system('Log File URL', LFU) -> 
      cgiSend([$<P><CENTER><A HREF="$, LFU, $" TARGET="_blank">View Debugging Trace</A></CENTER>$]) 
      ; 
      true
   ),
   cgiSend($</CENTER></BODY></HTML>$),
   cgiSend($$).
% Never fail
cgiError(_).

cgiDisplayErrorMessage(error(Type, AttrList)) :-
   cgiSend([$<B>$, Type, $</B>:<P>$]),
   cgiSendErrorList(AttrList).
cgiDisplayErrorMessage(Message) :-
   ( string(Message) ; list(Message) ),
   cgiSend(Message).
cgiDisplayErrorMessage(Message) :-
   structure(Message),
   string_term(SMsg, Message),
   string_list(SMsg, LMsg),
   cgiExpandChars(LMsg, LMsg2),
   string_list(SMsg2, LMsg2),
   cgiSend(Message).
cgiDisplayErrorMessage(Message) :-
   cgiSend(Message).

cgiSendErrorList([]) :- !.
cgiSendErrorList([type = Value | Rest]) :-
   cgiSend([$Type: $, Value, $<P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([rc = Value | Rest]) :-
   cgiSend([$Number: $, Value, $<P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([message = Value | Rest]) :-
   cgiSend([$Message: $, Value, $<P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([read_buffer = Value | Rest]) :-
   cgiSend([$Error occured while reading the following text:<BR><UL>$, Value, $</UL><P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([read_file = Value | Rest]) :-
   cgiSend([$Text was being read from file: $, Value, $<P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([predicate = Value | Rest]) :-
   cgiSend([$Current Predicate: $, Value, $<P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([callstack = Value | Rest]) :-
   cgiSend([$Current Call Stack:<BR><UL>$, Value, $</UL><P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([line_number = Value | Rest]) :-
   cgiSend([$The line number in the file was: $, Value, $<P>$]),
   cgiSendErrorList(Rest).
cgiSendErrorList([Item = Value | Rest]) :-
   cgiSend([Item, $: $, Value, $<P>$]),
   cgiSendErrorList(Rest).

cgiErrorHeader :-
%   cgiRewind,
   cgiSendHTTPHeader($Content-type: text/html$),
   cgiSendHTTPHeader($$),
   cgiSend($<HTML><HEAD><TITLE>Error</TITLE></HEAD>$),
   cgiSend($<BODY bgcolor=#FFB5DA text=#000000><HR><H1>Error</H1>$).

%
% cgiExpandChars(InputList, OutputList)
%
% Given a character list, these clauses expand special characters into their
% proper HTML representations, e.g. > becomes &gt;  This could be used by the
% error handler when reporting syntax (read) errors as the buffer contains
% parts of the logic-base which include HTML tags which are not to be
% interpreted, simply displayed.
%
cgiExpandChars([], []).
cgiExpandChars([0'> | T1], [0'&,0'g,0't,0'; | T2]) :- !, cgiExpandChars(T1, T2).
cgiExpandChars([0'< | T1], [0'&,0'l,0't,0'; | T2]) :- !, cgiExpandChars(T1, T2).
cgiExpandChars([0'& | T1], [0'&,0'a,0'm,0'p,0'; | T2]) :- !, cgiExpandChars(T1, T2).
cgiExpandChars([0'" | T1], [0'&,0'q,0'u,0'o,0't,0'; | T2]) :- !, cgiExpandChars(T1, T2).
cgiExpandChars([X | T1], [X | T2]) :- cgiExpandChars(T1, T2).

%
% cgiSend(List) or cgiSend(Term)
%
% Outputs the list of terms or the term (which may be a string, atom,
% number, etc. to the output file.
%
cgiSend([]) :-
   !,
   cgiWriteUnicode($\n$).
cgiSend(Stuff) :-
   list(Stuff), !,
   cgiSendList(Stuff).
cgiSend(Stuff) :-
   cgiWriteUnicode(Stuff),
   cgiWriteUnicode($\n$).

cgiSendList([]) :-
   cgiWriteUnicode($\n$).
cgiSendList([H | T]) :-
   cgiWriteUnicode(H),
   cgiSendList(T).

%
% cgiSendHTTPHeader()
%
% Outputs the list of terms or the term (which may be a string, atom,
% number, etc. to the output file.
%
cgiSendHTTPHeader(Stuff) :-
   cgiWrite(Stuff),
   cgiWrite($\n$).

%
% cgiSendLocalHTMLFile(FileName)
%
% Outputs the contents of an existing HTML text file to the output file.
% You must output the header separately before calling this function.
% This allows you to intermix "canned" HTML with generated HTML
%
cgiSendLocalHTMLFile(FileName) :-
   fopen(Handle, FileName, r),
   repeat,
   ( read_string(Handle, Str) -> true ; throw([$Unable to read local HTML file $, FileName, $\n$]) ),
   (Str == 'end_of_file' -> true ; cgiSend(Str)),
   Str == 'end_of_file',
   fclose(Handle), 
   !.
cgiSendLocalHTMLFile(FileName) :-
   throw( [$Unable to open local HTML file $, FileName, $\n$] ).

%
% cgiSendURL(URL)
%
% Tells the server to redirect to the specified URL
%
cgiSendURL(URL) :-
  cgiSend([$Location: $, URL]),
  cgiSend($$).

%
% cgiAskField(Key, Prompt, Length, Separator, DefaultValue)
%
% Outputs the Prompt into the current HTML output file with the
% input box of the specified Length and DefaultValue followed by 
% the Separator.
%
cgiAskField(Key, Prompt, Length, Separator, DefaultValue) :-
   cgiSend(Prompt),
   (Length == 0 -> 
      cgiSend([$<INPUT NAME="$, Key, $">$, Separator])
   ; 
      cgiSend([$<INPUT SIZE=$, Length, $ NAME="$, Key, $" VALUE="$, DefaultValue, $">$, Separator]) 
   ).

%
% cgiAskYesNo(Key, Prompt, Separator)
%
% Outputs the Prompt and asks for a yes, no or unknown radio button to
% be checked.
%
cgiAskYesNo(Key, Prompt, Separator) :-
   cgiSend(Prompt),
   cgiSend([$<INPUT NAME="$, Key, $" TYPE="RADIO" VALUE="yes">Yes$, Separator]),
   cgiSend([$<INPUT NAME="$, Key, $" TYPE="RADIO" VALUE="no">No$, Separator]).

%
% cgiAskMenu(Key, Prompt, Choices, Type, Length, Separator)
%
% Outputs the Prompt and a checkbox, radio or drop-down menu containing 
% the items in the Choices list, each separated by the Separtor.
%
cgiAskMenu(Key, Prompt, Choices, radio, _, Separator) :-
   cgiSend(Prompt),
   cgiAskInputChoices(Choices, Key, $"RADIO"$, Separator).
cgiAskMenu(Key, Prompt, Choices, checkbox, _, Separator) :-
   cgiSend(Prompt),
   cgiAskInputChoices(Choices, Key, $"CHECKBOX"$, Separator).
cgiAskMenu(Key, Prompt, Choices, listbox, Length, Separator) :-
   cgiSend(Prompt),
   cgiSend([$<SELECT NAME="$, Key, $" >$]),
   cgiAskListChoices(Choices, Separator),
   cgiSend($</SELECT>$).
cgiAskMenu(Key, Prompt, Choices, listboxMultiple, Length, Separator) :-
   cgiSend(Prompt),
   (Length = 0 ->
      cgiLength(Choices, N),
      (N < 8 -> NN is N ; NN is N // 2),
      cgiSend([$<SELECT NAME="$, Key, $" MULTIPLE SIZE=$, NN, $ >$])
   ;
      cgiSend([$<SELECT NAME="$, Key, $" MULTIPLE SIZE=$, Length, $ >$])
   ),
   cgiAskListChoices(Choices, Separator),
   cgiSend($</SELECT>$).

cgiAskListChoices([], _).
cgiAskListChoices([item(ChoiceName, OptionsList) | Rest], Separator) :-
   (member(text = ChoiceText, OptionsList) ; ChoiceText = ChoiceName),
   (member(selected, OptionsList) ->
      cgiSend([$<OPTION SELECTED VALUE="$, ChoiceName, $">$, ChoiceText])
   ;
      cgiSend([$<OPTION VALUE="$, ChoiceName, $">$, ChoiceText])
   ),
   cgiAskListChoices(Rest, Separator).
cgiAskListChoices([Choice | Rest], Separator) :-
   cgiSend([$<OPTION>$, Choice]),
   cgiAskListChoices(Rest, Separator).

cgiAskInputChoices([], _, _, _).
cgiAskInputChoices([item(ChoiceName, OptionsList) | Rest], Key, Type, Separator) :-
   (member(text = ChoiceText, OptionsList) ; ChoiceText = ChoiceName),
   (member(selected, OptionsList) ->
      cgiSend([$<INPUT CHECKED NAME="$, Key, $" TYPE=$, Type, $ VALUE="$, ChoiceName, $">$, ChoiceText, Separator])
   ;
      cgiSend([$<INPUT NAME="$, Key, $" TYPE=$, Type, $ VALUE="$, ChoiceName, $">$, ChoiceText, Separator])
   ),
   cgiAskInputChoices(Rest, Key, Type, Separator).
cgiAskInputChoices([Choice | Rest], Key, Type, Separator) :-
   cgiSend([$<INPUT NAME="$, Key, $" TYPE=$, Type, $ VALUE="$, Choice, $">$, Choice, Separator]),
   cgiAskInputChoices(Rest, Key, Type, Separator).

%
% cgiAskTextbox(Key, Prompt, Length, Separator)
%
% Outputs a textarea with Length rows followed by Separator
%
cgiAskTextbox(Key, Prompt, Length, Separator) :-
   cgiSend([Prompt, $<TEXTAREA NAME="$, Key, $" COLS=60 ROWS=$, Length, $></TEXTAREA>$]).

%
% cgiLog(Stuff)
%
% If logging is enabled (by the existence of a system fact named 'Log File')
% then write Stuff to it.  The first write causes the file to be opened,
% and all the system, cgi and facts to be written to it.  The file is
% closed automatically when the script finishes.
%

cgiLog(_) :-
   not system('Log File', _),
   !.
cgiLog(Stuff) :-
   var(Stuff), !,
   %system('Log File', LF),
   %cgiOpenLog(LF, Handle),
   cgiGetLogHandle(Handle),
   write(Handle, var(Stuff)),
   fflush(Handle).
cgiLog(tab(N)) :-
   !,
   %system('Log File', LF),
   %cgiOpenLog(LF, Handle),
   cgiGetLogHandle(Handle),
   tab(Handle, N).
cgiLog(StuffList) :-
   list(StuffList), !,
   %system('Log File', LF),
   %cgiOpenLog(LF, Handle),
   cgiGetLogHandle(Handle),
   cgiLogListWrite(Handle, StuffList),
   fflush(Handle).
cgiLog(Stuff) :-
   !,
   %system('Log File', LF),
   %cgiOpenLog(LF, Handle),
   cgiGetLogHandle(Handle),
   write(Handle, Stuff),
   fflush(Handle).
cgiLog(Stuff).

cgiGetLogHandle(Handle) :-
   system('Log Handle', Handle), !.
cgiGetLogHandle(Handle) :-
   system('Log File', LF),
   cgiOpenLog(LF, Handle).

cgiOpenLog(FileName, Handle) :-
   system('Log Handle', Handle), !.		% Also used in cgiError
cgiOpenLog(FileName, Handle) :-
   once cgiIntro(FileName),
   fopen(Handle, FileName, a),
   asserta(system('Log Handle', Handle)),
   !.
cgiOpenLog(FileName, _) :-
   set_mode(file_errors, on),
   throw([$Unable to open log file $, FileName, $. Check if the directory path exists and is correct.\n$]).

cgiIntro(FileName) :-
   tell(FileName),
   write($<HTML><HEAD><TITLE>Logic-Base Debugging Trace</TITLE></HEAD>\n$),
   write($<BODY><BODY bgcolor=#FFFFF0 text=#000000><H2><FONT COLOR=blue>Logic-Base Debugging Trace</FONT></H2>$),
   write($<HR><H2><FONT COLOR=red>Press the 'Reload' button on your browser to see the latest trace!</FONT></H2><P><PRE>\n$),
   cgiLogFactsWrite,
   nl,
   told.
cgiIntro(_).

cgiLogFactsWrite :-
  cgiListing(system(_,_)),
  nl,
  cgiListing(accept(_,_)),
  nl,
  cgiListing(extraheaders(_,_)),
  nl,
  cgiListing(cgi(_,_)),
  nl.
cgiLogFactsWrite.

%
% cgiLogFacts([SkipList], TabLen)
%
% Write all the facts out to the log file skipping all those in the list
%
cgiLogFacts(SkipList, TabLen) :-
  fact(Name, Value),
  (member(Name, SkipList) ->
    true 
  ; 
    cgiLog(tab(TabLen)),
    cgiLog([Name, $ = $, Value, $\n$])  %+
  ),
  fail.
cgiLogFacts(_, _).

cgiListing(Term) :-
  call(Term),
  string_term(Str, Term),
  string_list(Str, CharList),
  cgiExpandChars(CharList, CharListOut),
  string_list(StrOut, CharListOut),
  write(StrOut), nl,
  fail.
cgiListing(_).
  
cgiLogListWrite(_, []).
cgiLogListWrite(Handle, [H | T]) :-
   write(Handle, H),
   cgiLogListWrite(Handle, T).
   
cgiCloseLog :-
   system('Log Handle', Handle), !,
   write(Handle, $\n\n</PRE><P><HR>See the <A HREF="http://www.amzi.com">Amzi! Web Site</A> for more information and documentation</BODY></HTML>$),
   fclose(Handle).
cgiCloseLog.

%
% cgiLogSuspend, cgiLogResume
%
% Temporarily suspends logging by fooling cgiLog
%
cgiLogSuspend :-
   retract(system('Log File', LF)),
   asserta(system('Suspended Log File', LF)).
cgiLogSuspend.   % Don't fail if no log file

cgiLogResume :-
   retract(system('Suspended Log File', LF)),
   asserta(system('Log File', LF)).
cgiLogResume.    % Don't fail if no log file

%
% Utilities
%

cgiLength(L, N) :-cgiLength(L, 0, N).
  cgiLength([], N, N).
  cgiLength([_|Y], X, N) :-
    XX is X + 1,
    cgiLength(Y, XX, N).
