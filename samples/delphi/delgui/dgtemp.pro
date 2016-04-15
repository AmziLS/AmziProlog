% Almost blank Prolog file serving as a template
% for DelGUI application-specific code.

test :-
  w_message($DelGUI is alive and well$),
  memobox(1, MBAddr),
  memo_add(MBAddr, $This text was added from Prolog.$).