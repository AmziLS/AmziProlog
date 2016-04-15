%
% infoform.pro -- a simple CGI application that takes a information request
%
% Copyright (c)1996-2003 Amzi! inc. All Rights Reserved.
%
%
% To use:
%   Change the location of the results file (inforeqs.pro)
%   Compile and link this file with acgi.pro (see infoform.ppj)
%   Compile and link amzicgi.c and amzisub.c and rename the executable
%     to 'infoform[.exe]'
%   Copy infoform[.exe], infoform.xpl and the correct version (windows
%     or unix) of infoform.htm to your CGI directory
%


%
% cgiLoad
%
% This is called by main after the CGI and system variables are set, 
% and is where you load the logic-base or other definitions, if they
% weren't compiled in. You can also specify a log file for debugging.
%
cgiLoad :-
%  asserta(system('Log File', 'C:\\logs\\trace.htm')),
%  asserta(system('Log File URL', 'file:///C:\\logs\\trace.htm')),
  true.

%
% cgiMain
%
% The main CGI script.
%
cgiMain :-
  cgi(request_method, RM),
  processMethod(RM).
cgiMain :-
  throw($cgiMain failed\n$).

cgiErrorNote :-
  true.

%
% For the initial get, simply return our HTML form
%
processMethod('GET') :-
  cgiSend($Content-type: text/html$),
  cgiSend($$),
  cgiSendLocalHTMLFile('infoform.htm').

%
% After the user has filled in the form, we need to check it, then
% send a response back.
%
processMethod('POST') :-
  sendHeader,
  checkFacts,
  writeRequestLog,
  cgiSend($Thank you!  Your information request has been successfully submitted!$),
  cgiSend($<P>Return to <A HREF="/index.html">Amzi!'s home page</A>.$),
  sendFooter.
processMethod('POST') :-
  cgiSend($<P>Press the 'Back' or '&lt;-' button on your browser to change your form and resubmit it.$),
  sendFooter.

%
% Standard HTML header and footer for this script
%
sendHeader :-
  cgiSend($Content-type: text/html$),
  cgiSend($$),
  cgiSend($<HTML><HEAD><TITLE>Amzi! Information Request</TITLE></HEAD><BODY bgcolor=#FFFFEE text=#000000>$),
  cgiSend($<H2>Amzi! Information Request</H2>$).
sendFooter :-
  cgiSend($<P><HR><BR><CENTER><I><A HREF="/products.htm">Logic Server</A></I> by Amzi!</CENTER>$),
  cgiSend($</BODY></HTML>$).

%
% checkFacts and its cousins check the validity of the form inputs.
% If an error is encountered it fails and a message is returned.
%
checkFacts :-
  once(checkRequest),
  once(checkNameEMail),
  once(checkAddress).

checkRequest :-
  fact(request, X), !.
checkRequest :-
  cgiSend($Please indicate if you want us to send you a catalog or add you to one of our mailing lists!$),
  fail, !.
checkNameEMail :-
  fact(lastname, LN),
  fact(email, EM), !.
checkNameEMail :-
  cgiSend($Please fill in your name and e-mail address.$),
  fail, !.
checkAddress :-
  fact(request, RL),
  not(member(catalog, RL)),
  not(member(newsletters, RL)), !.
checkAddress :-
  fact(address1, A1),
  fact(city, C1),
  fact(country, C2), !.
checkAddress :-
  cgiSend($Please fill in your mailing address including street, city, region and country.$),
  fail, !.

%
% writeRequestLog outputs the form inputs to a file for later downloading
%
writeRequestLog :-
   openRequestLog(Handle, $results.log$),
   gmtime_str(Time),
   write(Handle, $request($),
   writeq(Handle, Time),
   write(Handle, $, $),
   findall(X : Y, fact(X, Y), FL),
   writeq(Handle, FL),
   write(Handle, $).\n$),
   close(Handle).
writeRequestLog(_).

openRequestLog(Handle, FilePath) :-
   cntr_set(1, 1),
   repeat,
   cntr_inc(1, NewVal),
   (NewVal > 5 -> 
      throw([$ERROR: Cannot open results file: $, FilePath]) 
   ; 
      true 
   ),
   catch(open(FilePath, append, Handle), ERROR, fail),
   !.
