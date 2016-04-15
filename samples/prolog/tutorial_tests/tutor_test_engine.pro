% Tutorial Test Engine
% Amzi! inc.

% This is the reasoning engine for the
% tutorial testing knowledge representation.
%
% Knowledge about questions is represented in
% frame-like Prolog structures:
%
% question(Q_ID, [
%    ask = QUESTION_TEXT,
%    next = ANSWER_LIST
%    ]).
%
% The ANSWER_LIST has the possible answers the
% student might give and the actions to take for
% each.  It's format is:
%
%    [
%      ANSWER_1 : ACTIONS_1,
%      ANSWER_2 : ACTIONS_2,
%      ...
%      _ : DEFAULT_ACTIONS
%    ]
%
% The actions are a list of actions.  Only two are
% defined in this simple prototype.
%
%    goto(Q_ID) - the next question to go to
%    text(TEXT) - text to display to the student
%
% The reasoning engine is straight forward.  It
% starts with the first question and asks it. It
% gets the answer and then takes the actions
% associated with that answer, one of which will
% tell it the next question to ask.
%
% The engine is designed to be called from another
% program.  It uses two I/O predicates, prompt/2 and
% output/1.  Neither are defined in this module.
% This module expects them to be defined in the calling
% program, maybe as extended predicates in a host language.
%
% This architecture makes it possible to have alternative
% calling applications, either a Prolog command line one
% or a GUI one written in VB, Java or some other language.

:- module(tutor_test).
:- export begin/0.

begin :-
   start(Q),
   loop(Q).

loop(Q) :-
   question(Q, Attributes),
   member(ask = Prompt, Attributes),
   prompt(Prompt, Answer),
   member(next = Nexts, Attributes),
   member(Answer:Action, Nexts),
   !, take(Action).

take([]).
take([text(T)|Actions]) :-
   output(T),
   !, take(Actions).
take([goto(Q)]) :-
   !, loop(Q).

member(A, [A|_]).
member(A, [_|X]) :- member(A,X).

:- end_module(tutor_test).
