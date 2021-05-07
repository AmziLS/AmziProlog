%----------------------------------------------------------------------
% HELLO.PRO - hello for embedding
%

hello(Caller, Greeting) :-
  strcat($Greetings $, Caller, S1),
  strcat(S1, $, from Amzi! Prolog.$, Greeting).

