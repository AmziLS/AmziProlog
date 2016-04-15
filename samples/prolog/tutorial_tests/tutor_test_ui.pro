% Tutorial test Prolog user interface

% The engine is designed to be called from
% any host language, and expects the host
% language to provide the I/O predicates it
% uses.
%
% These are prompt/2 and output/1.
%
% This way a Prolog test harness can be built
% debugging and testing in a pure Prolog
% environment.
%
% The system can then also be deployed from
% a language that supports GUIs, and have the
% I/O predicates implemented as extended predicates
% in that language.

:- reconsult(tutor_test_engine).
:- reconsult(tutor_test_math).

main :-
   tutor_test:begin.

prompt(Prompt, Answer) :-
   write(Prompt),
   read_string(Str),
   string_term(Str, Answer).

output(X) :-
   write(X), nl.