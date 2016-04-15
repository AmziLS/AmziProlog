% Tutorial Test Example for Arithmetic

% This is the knowledge representation for
% the tutorial testing application. Prolog
% structures and lists are used to create a
% frame-like syntax for each question.
%
% There are only two slots in this simple
% example.  The first is the question to ask.
% The second is more complex, containing a list
% of possible answers and the actions to take
% if the student chose that answer.  The last
% entry is the default for answers not specifically
% covered.
%
% The example could be expanded with all sorts of
% good stuff, like the inclusion of multiple choice
% questions and links to HTML documentation
% covering pertinent topics for the student.
%
% This sample test has only three questions, the first
% two addition problems indicating how the flow through
% the test is controlled by the student's answers.

start(add_1).

question(add_1,  [
   ask = `what's 2 + 5 `,
   next = [
      7: [text(`good`), goto(mult_1)],
      10: [text(`you're confusing + with *`), goto(add_2)],
      _: [text(`read addition chapter`), goto(add_2)]
   ] ] ).

question(add_2,  [
   ask = `what's 3 + 5 `,
   next = [
      8: [text(`good`), goto(mult_1)],
      15: [text(`you're confusing + with *`), goto(mult_1)],
      _: [text(`read addition chapter`), goto(mult_1)]
   ] ] ).

question(mult_1,  [
   ask = `what's 2 * 5 `,
   next = [
      10: [text(`good`)],
      _: [text(`read multiplication chapter`)]
   ] ] ).
