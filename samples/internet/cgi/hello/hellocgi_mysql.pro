%
% hellocgi.pro -- The simplist CGI application that returns a greeting
%
% Copyright (c)2000-2002 Amzi! inc. All Rights Reserved.
%
%
% To use:
%   Compile and link this file with acgi.plm (see hellocgi.ppj)
%   Compile and link amzicgi.c and amzisub.c and rename the executable
%     to 'hellocgi'
%   Copy hellocgi(.exe) and hellocgi.xpl along with amzi.dll to your 
%     CGI-BIN directory
%


%
% cgiLoad
%
% This is called by main after the CGI and system variables are set, 
% and is where you load the logic-base or other definitions, if they
% weren't compiled in. You can also specify a log file for debugging.
%

% :- write('loading amysql\n').
:- loadlsx(amysql).
% :- write('loaded amysql\n').

cgiLoad :-
%  asserta(system('Log File', '/usr/dennis/trace.htm')),
%  asserta(system('Log File URL', 'file://usr/dennis/trace.htm')),
  true.

%
% cgiMain
%
% The main CGI script.
%
cgiMain :-
  cgiSend($Content-type: text/html$),
  cgiSend($$),
  cgiSend($<html><body>Hello CGI User!</body></html>$).
cgiMain :-
  throw($cgiMain failed\n$).

cgiErrorNote :-
  true.

