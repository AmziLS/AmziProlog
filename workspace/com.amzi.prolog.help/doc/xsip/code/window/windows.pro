% WINDOWS - Windowing predicates, written for an old version
% of Arity Prolog.  While not operational at this time, they
% do illustrate the power of Prolog for systems type work.

:- module windows.
:- segment(ijseg2).

:- public window/2, window/3.
:- public showcurse/0, hidecurse/0.

:- default(invisible).

:- extrn curtype/2:asm.    % sets the cursor, can be replaced with:

showcurse:- curtype(7,8).

hidecurse:- curtype(39,40). 

%***********************************************************
%
% window/3 is the main predicate.  The windowing system is organized
% at the top in an object oriented fashon.  The window/3 arguments are:
%    arg1 - operation (message)
%    arg2 - window (the object)
%    arg3 - parameters (input or output - either a singleton or a list

% The objects can be one of four types of window -
%    display, menu, form, prompt
%
%***********************************************************

:- mode window(+,+).

window(W, Op):- window(W, Op, []).

:- mode window(+,+,?).

window([H|T], Op, Args):-
  window(temp_w, create, [H|T]),
  window(temp_w, Op, Args),
  window(temp_w, delete), !.
window(W, Op, Args):-
  get_type(W, T),
  find_proc(T, Op, Proc),
  doproc(Proc, W, Args), !.

% get_type/2 figures out what type of window we have

:- mode get_type(+,-).

get_type(W, X):- select_parm(W, [type(X)]), !.
get_type(W, window).                       % W is currently undefined

% find_proc/3 finds the appropriate procedure for the message
% and window type

:- mode find_proc(+,+,-).

find_proc(T, Op, Proc):- find_p(T, Op, Proc), !.
find_proc(T, Op, Proc):-
  error([Op, 'is illegal operation for a window of type', T]).

find_p(T, Op, Proc):- method(T, Op, Proc), !.
find_p(T, Op, Proc):-
  subclass(Super, T),
  !, find_p(Super, Op, Proc).

% table of objects

:- mode subclass(+,?).

subclass(window, display).
subclass(window, menu).
subclass(window, form).
subclass(window, prompt).

% table of procedures to use for various operations and types

:- mode method(+,+,-).

method(window, open, open_w).
method(window, close, close_w).
method(window, create, create_w).
method(window, change, change_w).
method(window, driver, driver_w).
method(window, display, display_w).
method(window, delete, delete_w).
method(window, erase, erase_w).

method(display, write, write_d).
method(display, writelist, writelist_d).
method(display, writeline, writeline_d).

method(menu, read, read_m).

method(form, read, read_f).
method(form, display, nop).

method(prompt, read, read_p).

% doproc - a faster way to do a call.

:- mode doproc(+,+,+).

doproc(create_w,W,A):- !, create_w(W,A).
doproc(open_w,W,A):- !, open_w(W,A).
doproc(close_w,W,A):- !, close_w(W,A).
doproc(delete_w,W,A):- !, delete_w(W,A).
doproc(display_w,W,A):- !, display_w(W,A).
doproc(erase_w,W,A):- !, erase_w(W,A).
doproc(change_w,W,A):- !, change_w(W,A).
doproc(write_d,W,A):- !, write_d(W,A).
doproc(writelist_d,W,A):- !, writelist_d(W,A).
doproc(writeline_d,W,A):- !, writeline_d(W,A).
doproc(read_m,W,A):- !, read_m(W,A).
doproc(read_f,W,A):- !, read_f(W,A).
doproc(read_p,W,A):- !, read_p(W,A).
doproc(driver_w,W,A):- !, driver_w(W,A).
doproc(nop,_,_):- !.
doproc(X,W,A):- error(['No window method ',X,' defined.']).

%******************************************************************
%
% methods for the super class "window".  these are used by default
% if they are not redefined for the subclass.
%
%     create_w - create new window from specs
%     open_w  - open a window for use
%     close_w - remove the window contents and viewport 
%     change_w - changes a windows definition
%     display_w - display a portion of contents in window
%     driver_w - gives control to user to view other windows
%
%******************************************************************

%------------------------------------------------------------------
%
% create_w/2 records a new window definition
%
%------------------------------------------------------------------

:- mode create_w(+,?).

create_w(W,L):- make_window(W,L), !.

%------------------------------------------------------------------
%
% open_w/1 calls make_viewport which opens up a viewport for the window
%
%------------------------------------------------------------------

:- mode open_w(+,?).

open_w(W,[]):- exists_window(W), make_viewport(W), !.

%------------------------------------------------------------------
%
% close_w/1 closes a viewport on the active list
%
%------------------------------------------------------------------

:- mode close_w(+,?).

close_w(W,[]):-
  del_viewport(W), !.

%------------------------------------------------------------------
%
% delete_w removes both viewport and dataarea
%
%------------------------------------------------------------------

:- mode delete_w(+,?).

delete_w(W,[]):-
  del_viewport(W),
  del_dataarea(W),
  del_stat(W),
  del_image(W),
  del_window(W), !.

%------------------------------------------------------------------
%
% erase_w removes the viewport and dataarea, but preserves
% window definition
%
%------------------------------------------------------------------

:- mode erase_w(+,?).

erase_w(W,_):-
  del_viewport(W),
  del_dataarea(W),
  del_stat(W),
  del_image(W), !.

%------------------------------------------------------------------
%
% change_w/2 changes a windows position on the viewport
%
%------------------------------------------------------------------

:- mode change_w(+,?).

change_w(W,L):-
  recorded_w(windef,wd(W,Lold),_),
  merge_wl(L,Lold,Lnew),
  w_clrb(W),
  recorded_w(active,AL,_),
  remove(W,AL,NL),
  redraw(NL,W),
  recorded_w(active,AL,DBRef), erase(DBRef),
  recorda(active,NL,_),
  window(W,create,Lnew),
  window(W,open), !.

merge_wl([],Lnew,Lnew):-!.
merge_wl([Hn|Tn], Lold, Lnew):-
  rep_we(Hn, Lold, Temp),
  !, merge_wl(Tn, Temp, Lnew).

rep_we(X,[],[X]):- !.
rep_we(X, [H|T], [X|T]):-
  functor(X,F,A),
  functor(H,F,A),!.
rep_we(X,[H|T],[H|T2]):-
  !, rep_we(X,T,T2).

%------------------------------------------------------------------
%
% display_w writes a page of a window from a given starting point
%
%------------------------------------------------------------------

:- mode display_w(+,?).

display_w(W, [_, 0]):-!.
display_w(W, [Line, NN]):-          % add NN lines to the top
  NN < 0,
  N is -NN,
  select_parm(W, [coord(R1, C1, R2, C2)]),
  RLL is R1 + N - 1,
  (RLL =< R2, RL = RLL; RL = R2),
  display_viewport(W, Line, R1, RL, C1), !.
display_w(W, [Line, NN]):-          % add NN lines to the bottom
  NN > 0,                           % note Line is line number at top of
  select_parm(W, [coord(R1, C1, R2, C2)]),       % window
  RFF is R2 - NN + 1,
  (RFF =< R1, RF = R1; RF = RFF),
  Offset is RF - R1,                    % if first line to be displayed
  Lineoff is Line + Offset,             % is mid viewport somewhere
  display_viewport(W, Lineoff, RF, R2, C1), !.

display_w(W, Line):-
  select_parm(W, [coord(R1, C1, R2, C2)]),
  display_viewport(W, Line, R1, R2, C1), !.

%------------------------------------------------------------------
%
% driver_w turns control over to the user for manipulating the
% current window.
%
%------------------------------------------------------------------

driver_w(_,_):-
  repeat,
  recorded_w(active,[W|_],_),
  select_parm(W,[coord(R1,C1,_,_)]),
  tmove(R1,C1),
  keyb(A,S),
  w_exec(S,Flag,W),
  Flag == end,!.

:- mode w_exec(+,-,+).

w_exec(71,xxx,W):-         % home
  scroll_window(W,top),!.
w_exec(79,xxx,W):-         % end
  scroll_window(W,bottom),!.
w_exec(81,xxx,W):-         % pgdn
  select_parm(W,[height(H)]),
  HH is H - 1,
  scroll_window(W,HH),!.
w_exec(73,xxx,W):-         % pgup
  select_parm(W,[height(H)]),
  HH is - H + 1,
  scroll_window(W,HH),!.
w_exec(72,xxx,W):-         % up arrow
  scroll_window(W,-1),!.
w_exec(80,xxx,W):-         % down arrow
  scroll_window(W,1),!.
w_exec(59,xxx,W):-         % f1 change windows
  recorded_w(active,List,_),
  last_item(List,NewW),
  window(NewW,open),!.  
w_exec(28,end,_):-!.       % enter - leave the driver
w_exec(_,xxx,_):-!.

%******************************************************************
%
% methods for subclass display
%
%     write_d - write to the window
%     writelist_d - write a list of terms to the window
%
%******************************************************************

%------------------------------------------------------------------
%
%  write_d
% write to the window.  The term can be a simple term, or a list of terms
% which make up a line.  See w_wlin for other allowed contructs.
%
%------------------------------------------------------------------

:- mode write_d(+,?).

write_d(W, Term):-
  scroll_window(W, bottom),
  select_stat(W,curnum,L1,NL),
  select_parm(W, [coord(R1,C1,R2,C2), attr(CC)]),
  Row is NL - L1 + 1 + R1,
  add_data(W, Term),
  write_viewport(W, Term, CC, L1, NL, Row, R2, C1, C2),
  !.

%------------------------------------------------------------------
%
% writelist_d
% write multiple lines to the window, using write_d
%
%------------------------------------------------------------------

:- mode writelist_d(+,?).

writelist_d(W, []):-!.
writelist_d(W, [H|T]):-
  w_writ(W,H),
  !, writelist_d(W,T).

w_writ(W,nl):- write_d(W,'').
w_writ(W,H):- write_d(W,H).

%------------------------------------------------------------------
%
% writeline_d
% write a list of terms on a line, without the list format
%
%------------------------------------------------------------------

:- mode writeline_d(+,?).

writeline_d(W, L):-
  make_line(L, $$, S),
  write_d(W, S), !.

make_line([], S, S).
make_line([H|T], Temp, S):-
  string_term(HS, H),
  concat(Temp, HS, Temp2),
  make_line(T, Temp2, S).

%******************************************************************
%
% methods for subclass menu
%
%     read_m - read a term from the menu
%
%******************************************************************

%------------------------------------------------------------------
%
% read_m
% w_menu - returns a menu choice from a menu window, the window may
% be dynamically built, using the first clause
%
%------------------------------------------------------------------

:- mode read_m(+,?).

read_m(W,X):-
  init_menu_dataarea(W),
  window(W,open),
  menu_select(W,X), !.
%  (string_term(X,XX); atom_string(XX,X)), !.

%******************************************************************
%
% methods for subclass form
%
%     read_f - read the fields in the form
%
%******************************************************************

%------------------------------------------------------------------
%
% read_f - updates the contents of the form window
%
%------------------------------------------------------------------

:- mode read_f(+,?).

read_f(W,[]):-
  init_form_dataarea(W),
  window(W,open),
  clear_viewport(W),
  display_form(W),
  select_parm(W,[coord(_,C1,R2,_)]),
  RR is R2 + 1,
  tmove(RR,C1), write(' F9 to enter '),
  fill_form(W),
  read_form(W), !.

%******************************************************************
%
% methods for subclass prompt
%
%******************************************************************

%------------------------------------------------------------------
%
% read_p read the prompt
%
%------------------------------------------------------------------

:- mode read_p(+,?).

read_p(W,X):-
  var(X),
  window(W,open),
  fill_prompt(W,Z),
  !, Z = X.
read_p(W,X):-
  atomic(X),
  window(W,open),
  fill_prompt(W,Z),
  !, Z == X.               % ok if X an atom and Z a string
read_p(W,[Def,X]):- 
  write_d(W,Def),
  fill_prompt(W,Z),
  !, Z = X.

%******************************************************************
%
%  Basic window data manipulation routines - a window is composed
%  of a viewport and dataarea
%
%******************************************************************

%------------------------------------------------------------------
%
% make a new window
%
%------------------------------------------------------------------

:- mode make_window(+,?).

make_window(W, Def):-
  (recorded_w(windef,wd(W,_),DBRef), erase(DBRef); true),
  recorda(windef,wd(W,Def),_), !.

%------------------------------------------------------------------
%
% del_window removes a window definition
%
%------------------------------------------------------------------

:- mode del_window(+).

del_window(W):-
  recorded_w(windef,wd(W,_),DBRef), erase(DBRef), !.
del_window(_).

%------------------------------------------------------------------
%
% exists window checks for existence of a window definition
%
%------------------------------------------------------------------

:- mode exists_window(+).

exists_window(W):-
  recorded_w(windef,wd(W,_),_), !.
exists_window(W):-
  error(['No window definition for ',W]),
  fail.

%------------------------------------------------------------------
%
% select_parm extracts various parameters from a window definition.
% The RequestList is a list of structures with keyword functors
% and variable arguments, which are bound by w_attr.  w_attr
% also contains the defaults and computed parameters (ie height)
%
%------------------------------------------------------------------

:- mode select_parm(+,+).

select_parm(W,RequestList):-
  recorded_w(windef,wd(W,AttrList),_),
  fullfill(RequestList, AttrList),!.

:- mode fullfill(+,+).

fullfill([],_):-!.
fullfill([Req|T], AttrList):-
  w_attr(Req, AttrList),
  !, fullfill(T, AttrList).

:- mode w_attr(+,+).

w_attr(height(H), AttrList):-
  w_attr(coord(R1,_,R2,_),AttrList),
  H is R2 - R1 + 1,!.
w_attr(width(W), AttrList):-
  w_attr(coord(_,C1,_,C2),AttrList),
  W is C2 - C1 + 1,!.
w_attr(attr(A), AttrList):-
  w_attr(contents(Color),AttrList),
  attr(Color,A),!.
w_attr(border_attr(A), AttrList):-
  w_attr(border(Color),AttrList),
  attr(Color,A),!.

w_attr(A, AttrList):-
  member(A, AttrList),!.

w_attr(coord(1,1,23,78),_).        % default values
w_attr(title(''),_).
w_attr(border(white),_).
w_attr(contents(white),_).
w_attr(type(display),_).

%------------------------------------------------------------------
%
% update_parm provides the facility to update and window parameters.
%
%------------------------------------------------------------------

:- mode update_parm(+,?).

update_parm(W,UpdateList):-
  recorded_w(windef,wd(W,AttrList),DBRef), erase(DBRef),
  modify(UpdateList, AttrList, NewList),
  recorda(windef,wd(W,NewList),_), !.

modify([],L,L):-!.
modify([Req|T], AttrList, NewList):-
  functor(Req,F,A),
  mod(F, Req, AttrList, [], NewL),
  !, modify(T, NewL, NewList).

mod(_,_,[],L,L).
mod(F,A,[OldA|AL],Temp,NewL):-
  functor(OldA,F,_),
  append([A|Temp],AL,NewL), !.
mod(F,A,[OldA|AL],Temp,NewL):-
  mod(F,A,AL,[OldA|Temp],NewL).

%------------------------------------------------------------------
%
% select_stat is used to get the status of a window
%
%------------------------------------------------------------------

:- mode select_stat(+,+,?).

select_stat(W,curline,L):- recorded_w(curline,cl(W,L,_),_),!.
select_stat(W,numlines,N):- recorded_w(curline,cl(W,_,N),_).

:- mode select_stat(+,+,?,?).

select_stat(W,curnum,L,N):- recorded_w(curline,cl(W,L,N),_).

%------------------------------------------------------------------
%
% update_stat is used to change the active status of a window
%
%------------------------------------------------------------------

:- mode update_stat(+,+,?).

update_stat(W,curline,L):-
  (recorded_w(curline,cl(W,_,NL),DBRef), erase(DBRef); NL=0),
  !, recorda(curline,cl(W,L,NL),_).
update_stat(W,numlines,N):-
  (recorded_w(curline,cl(W,L,_),DBRef), erase(DBRef); L=1),
  !, recorda(curline,cl(W,L,N),_).

:- mode update_stat(+,+,?,?).

update_stat(W,curnum,L,N):-
  (recorded_w(curline,cl(W,_,_),DBRef), erase(DBRef); true),
  !, recorda(curline,cl(W,L,N),_).

%------------------------------------------------------------------
%
% del_stat removes the windows status information
%
%------------------------------------------------------------------

:- mode del_stat(+).

del_stat(W):-
  recorded_w(curline,cl(W,_,_),DBRef), erase(DBRef),!.
del_stat(_).

%------------------------------------------------------------------
%
% select_content will repeatedly give the next record from
%     a given starting point, and its position
%
%------------------------------------------------------------------

:- mode select_content(+,?,?,?).

select_content(W,Start,Count,X):-
  ctr_set(20,0),
  recorded_w(W,X,_),
  ctr_inc(20,Count),
  Count >= Start.

%------------------------------------------------------------------
%
% add to a windows data area
%
%------------------------------------------------------------------

:- mode add_data(+,?).

add_data(W, Term):-
  (string(Term), S = Term; string_term(S,Term)),
  recordz(W, S, _), !.

%------------------------------------------------------------------
%
% make_viewport initializes a viewport.  If it is already the head of the 
% active list, do nothing.  If it is not on the active list, get it
% and put it on.  Otherwise move it to the head from where it is now.
%
%------------------------------------------------------------------
:- mode make_viewport(+).

make_viewport(W):-                   % If its on the top, clear the decks
  recorded_w(active,[W|T],_),
  del_image(W), !.
make_viewport(W):-                   % make sure active and curline exist
  w_inact(W),                        %      and go to next clause
  w_nocur(W),
  fail.
make_viewport(W):-                   % If its on the list somewhere
  recorded_w(active,[H|T],_),
  save_image(H),
  split(W,[H|T],L1,L2),
  w_chkover(W,L1,_),
  append([W|L1], L2, NL),
  recorded_w(active,_,DBRef), erase(DBRef),
  recorda(active,NL,_), !.
make_viewport(W):-
  recorded_w(active,L,DBRef), erase(DBRef),
  recorda(active,[W|L],_),
  w_ini(W), !.
make_viewport(W):- error(['Initializing viewport',W]).

w_inact(W):- recorded_w(active,_,_), !.
w_inact(W):- recorda(active,[],_).

w_nocur(W):- select_stat(W, curnum, _, _), !.
w_nocur(W):- update_stat(W, curnum, 1, 0).

w_ini(W):-
  w_box(W),
  clear_viewport(W),
  window(W,display,1),             % display from line 1
  set_arrows(W), !.

w_chkover(W, [], no).
w_chkover(W, [H|T], Stat):-
  w_nooverlap(W,H),
  w_chkover(W,T,Stat).
w_chkover(W,_,yes):-
  restore_image(W), !.

:- mode w_nooverlap(+,+).

w_nooverlap(Wa,Wb):-
  select_parm(Wa, [coord(R1a,C1a,R2a,C2a)]),
  select_parm(Wb, [coord(R1b,C1b,R2b,C2b)]),
  (R1a > R2b + 2;
   R2a < R1b - 2;
   C1a > C2b + 2;
   C2a < C1b - 2), !.

%------------------------------------------------------------------
%
% save the screen image
%
%------------------------------------------------------------------

:- mode save_image(+).

save_image(W):-
  del_image(W),
  select_parm(W,[coord(R1,C1,R2,C2)]),
  RR1 is R1 - 1, CC1 is C1 - 1,
  RR2 is R2 + 1, CC2 is C2 + 1,
  region_ca((RR1,CC1),(RR2,CC2),SA),
  recorda(image,W-SA,_), !.

%------------------------------------------------------------------
%
% restore the screen image
%
%------------------------------------------------------------------

:- mode restore_image(+).

restore_image(W):-
  select_parm(W,[coord(R1,C1,R2,C2)]),
  RR1 is R1 - 1, CC1 is C1 - 1,
  RR2 is R2 + 1, CC2 is C2 + 1,
  recorded_w(image,W-SA,Ref),
  region_ca((RR1,CC1),(RR2,CC2),SA), !.

%------------------------------------------------------------------
%
% delete an image
%
%------------------------------------------------------------------

:- mode del_image(+).

del_image(W):- recorded_w(image,W-_,R), erase(R), !.
del_image(W).

%------------------------------------------------------------------
%
% del_dataarea removes the contents of the window
%
%------------------------------------------------------------------

:- mode del_dataarea(+).

del_dataarea(W):-
  eraseall(W),
  update_stat(W,curnum,1,0), !.

%------------------------------------------------------------------
%
% clear_viewport clears the screen
%
%------------------------------------------------------------------

:- mode clear_viewport(+).

clear_viewport(W):-
  select_parm(W,[coord(R1,C1,R2,C2),attr(A)]),
  tmove(R2,C1),wca(1,0' ,A),tscroll(0,(R1,C1),(R2,C2)), !.
clear_viewport(_).

%------------------------------------------------------------------
%
% del_viewport removes a viewport updating active lists and overlays
%
%------------------------------------------------------------------

:- mode del_viewport(+).

del_viewport(W):-
  recorded_w(active,AList,_),
  member(W,AList),                  %fail and go away if nothing to delete
  AList = [H|_],
  (W == H; save_image(H)),          % in case closing a window other
  w_clrb(W),                        % than the uppermost
  remove(W,AList,Newl),
  del_image(W),
  redraw(Newl,W),
  recorded_w(active,AList,DBRef), erase(DBRef),
  recorda(active,Newl,_),!.
del_viewport(_).                 % always succeed

redraw(Newl,W):-                   % from back to front, redraw
  reverse(Newl,Backwards),         % affected windows
  redr(Backwards,W,[]),!.

redr([],_,_):-!.
redr([H|T], W, Redrawn):-
  w_nooverlap(H,W),
  w_chkover(H,Redrawn,Stat),
  (Stat == yes, Red = [H|Redrawn]; Red = Redrawn),
  !, redr(T,W,Red).
redr([H|T], W, Redrawn):-
  restore_image(H),
  !, redr(T, W, [H|Redrawn]).

% remove the window from the viewport

:- mode w_clrb(+).

w_clrb(W):-
  select_parm(W, [coord(R1, C1, R2, C2)]),
  RR1 is R1 - 1, CC1 is C1 - 1,
  RR2 is R2 + 1, CC2 is C2 + 1,
  tmove(RR2,CC1),wca(1,0' ,7),
  tscroll(0, (RR1, CC1), (RR2, CC2)),!.
w_clrb(W):-!.

%------------------------------------------------------------------
%
% writes a term to the specified line in the viewport
%
%------------------------------------------------------------------

:- mode write_viewport(+,+,+,+,+,+,+,+,+).

write_viewport(W, Term, CC, L1, NL, Row, R2, C1, C2):-
  (Row =< R2, R = Row, L2 is L1, !;
   scroll_viewport(W, 1), L2 is L1 + 1, R = R2),
  Width is C2 - C1 + 1,
  (string(Term), S = Term; string_term(S,Term)),
  w_wline(S, R, C1, CC, Width),
  NNL is NL + 1,
  update_stat(W,curnum,L2,NNL), !.

:- mode w_wline(+,+,+,+,+).

w_wline(Term, R, C1, CC, Width):-
  tmove(R,C1),
  wa(Width,CC),
  (string_length(Term,L), L =< Width, Term = S;
   substring(Term,0,Width,S)),  
  write(S).

%------------------------------------------------------------------
%
% display_viewport writes lines from R1 to R2 starting at line L
% from the dataarea to the viewport
%
%------------------------------------------------------------------

:- mode display_viewport(+,+,+,+,+).

display_viewport(W,Line,R1,R2,C1):-
  key(W,Key),
  nth_ref(W,Line,Ref),
  select_parm(W,[width(Wid), attr(A)]),
  RL is R2 + 1,
  w_disp(Ref, Key, R1, RL, C1, A, Wid), !.
display_viewport(_,_,_,_,_).             % succeed if no key yet

:- mode w_disp(+,+,+,+,+,+,+).

w_disp(Ref, Ref, _, _, _, _, _):-!.
w_disp(_, _, Row, Row, _, _, _):-!.
w_disp(Ref, Sref, Row, RL, C1, CC, Width):-
  instance(Ref, Term),
  w_wline(Term, Row, C1, CC, Width),
  Row2 is Row + 1,
  nref(Ref, Nref),
  !,
  w_disp(Nref, Sref, Row2, RL, C1, CC, Width).

%------------------------------------------------------------------
% 
% the box, only one choice, double line
%
%------------------------------------------------------------------

:- mode w_box(+).

w_box(W):-
  select_parm(W, [coord(R1, C1, R2, C2), title(T), border(C)]),
  box(R1, C1, R2, C2, C),
  Rt is R1 - 1,
  Ct is C1 + 3,
  tmove(Rt,Ct),
  write(T), !.

:- mode box(+,+,+,+,+).

box(R1, C1, R2, C2, C):-
  R0 is R1 - 1,
  C0 is C1 - 1,
  R3 is R2 + 1,
  C3 is C2 + 1,
  Width is C2 - C1 + 1,
  Height is R2 - R1 + 1,
  attr(C,CC),
  left_side(R1, C0, CC, R2),
  top(R0, C0, C1, C3, Width, CC),
  right_side(R1, C3, CC, R2),
  bottom(R3, C0, C1, C3, Width, CC),
  !.

:- mode left_side(+,+,+,+).

left_side(R1, C0, CC, R2):-
  w_vert(R1, C0, 186, CC, R2).

:- mode top(+,+,+,+,+,+).

top(R0, C0, C1, C3, Width, CC):-
  tmove(R0, C0),
  wca(1, 201, CC),
  tmove(R0, C1),
  wca(Width, 205, CC),
  tmove(R0, C3),
  wca(1, 187, CC).

:- mode right_side(+,+,+,+).

right_side(R1, C3, CC, R2):-
  w_vert(R1, C3, 186, CC, R2).

:- mode bottom(+,+,+,+,+,+).

bottom(R3, C0, C1, C3, Width, CC):-
  tmove(R3, C0),
  wca(1,200,CC),
  tmove(R3, C1),
  wca(Width, 205, CC),
  tmove(R3,C3),
  wca(1, 188, CC).

:- mode w_vert(+,+,+,+,+).

w_vert(R1, C1, Char, Color, R2):-
  ctr_set(0,R1),
  repeat,
  ctr_inc(0,R),
  tmove(R, C1),
  wca(1, Char, Color),
  R >= R2,
  !.

%------------------------------------------------------------------
%
% scroll the viewport
%
%------------------------------------------------------------------

:- mode scroll_viewport(+,?).

scroll_viewport(W, N):-
  select_parm(W, [coord(R1, C1, R2, C2)]),
  Height is R2 - R1 + 1,
  Heightm is -Height,
  (N > Heightm, N < Height, NN = N;
   NN = 0),
  tscroll(NN, (R1,C1), (R2,C2)).

%------------------------------------------------------------------
%
% scroll the window
%
%------------------------------------------------------------------

:- mode scroll_window(+,+).

scroll_window(W, top):-
  window(W,open),
  select_stat(W,curline,1),         % already at the top
  set_arrows(W), !.
scroll_window(W, top):-
  select_stat(W,curline,L),
  S is 1 - L,
  scroll_window(W,S), !.

scroll_window(W, bottom):-
  window(W,open),
  select_parm(W, [coord(R1, C1, R2, C2)]),
  Height is R2 - R1 + 1,
  select_stat(W, curnum, L, NL),
  NL < L + Height,                 % already at the bottom
  set_arrows(W),!.
scroll_window(W, bottom):-
  select_parm(W, [coord(R1, C1, R2, C2)]),
  Height is R2 - R1 + 1,
  select_stat(W, curnum, L, NL),
  Last is L + Height,
  S is NL + 1 - Last,
  scroll_window(W, S), !.

scroll_window(W, N):-
  window(W,open),
  select_stat(W, curnum, Line, NL),
  select_parm(W,[height(H)]),
  H < NL,                       % if it fits on one frame, no scroll
  MaxLine is NL - H + 1,        % biggest line # allowed at first row
  Newline is Line + N,
  real_nl(in(Line,MaxLine,Newline,N), out(Newl,NN)),
  set_arrow(W,MaxLine,Newl),
  update_stat(W, curnum, Newl, NL),
  scroll_viewport(W, NN),
  window(W, display, [Newl, NN]), !.
scroll_window(_,_).

%------------------------------------------------------------------
%
% set_arrows puts the little arrows at the top and bottom indicating
% that there is more to be seen.
%
%------------------------------------------------------------------

:- mode set_arrows(+).

set_arrows(W):-
  select_stat(W, curnum, Line, NL),
  select_parm(W, [height(H)]),
  Max is NL - H + 1,
  set_arrow(W, Max, Line),!.

set_arrow(W,Max,New):-
  select_parm(W, [coord(R1,C1,R2,C2)]),
  RR1 is R1 - 1,
  RR2 is R2 + 1,
  setar(Max,New,up(RR1:C1),down(RR2:C1)),!.

setar(_, 1, up(R:C), _):-
  tmove(R,C), put(0'Í), fail.
setar(Max, Max, _, down(R:C)):-
  tmove(R,C), put(0'Í), fail.
setar(_, New, up(R:C), _):-
  New > 1, tmove(R,C), put(0'), fail.
setar(Max, New, _, down(R:C)):-
  New < Max, tmove(R,C), put(0'), fail.
setar(_,_,_,_).

real_nl(in(Line,MaxLine,Newline,N), out(Newline,N)):-
  Newline > 0,
  Newline =< MaxLine, !.
real_nl(in(Line,MaxLine,Newline,N), out(1,NN)):-
  Newline =< 0,
  Newline =< MaxLine,
  NN is 1 - Line,
  Line =\= 1, !.          % fail if already at first line
real_nl(in(Line,MaxLine,Newline,N), out(MaxLine,NN)):-
  Newline > 0,
  Newline > MaxLine,
  NN is MaxLine - Line,
  Line =\= MaxLine, !.    % fail if already at last line

%------------------------------------------------------------------
%
% predicates to define and read maps, aka forms.
% they expect an initial definition of the map fields
% in a window spec as follows:
%
%      form([lit(Row:Col,Literal),
%            var(FieldName,Row:Col,Length,InitValue),
%            ...])
%
% This is converted to window records of the form
%    
%      recordz(W,lit(..),_). etc
%
% It is sometimes easier for the application to build this directly.
%
%------------------------------------------------------------------

%------------------------------------------------------------------
%
% copy the window definition specs to the dataarea
%
%------------------------------------------------------------------

:- mode init_form_dataarea(+).

init_form_dataarea(F):- recorded_w(F,_,_), !.
init_form_dataarea(F):-
  select_parm(F,[form(List)]),
  init_map(F,List), !.

:- mode init_map(+,?).

init_map(W,[]):-!.
init_map(W,[H|T]):-
  recordz(W,H,_),
  !, init_map(W, T).

%------------------------------------------------------------------
%
% put the form data on the viewport
%
%------------------------------------------------------------------

:- mode display_form(+).

display_form(S):-
  select_parm(S,[coord(R0,C0,_,_),attr(At)]),
  recorded_w(S,Field,_),
  write_field(R0:C0,At,Field),
  fail.
display_form(S):-true.

write_field(R0:C0, At, lit(R:C,Lit)):-
  RR is R0 + R, CC is C0 + C,
  tmove(RR,CC),
  write(Lit),!.
write_field(R0:C0, At, var(_,R:C,Length,Val)):-
  rev_attr(At, Rat),
  RR is R0 + R, CC is C0 + C,
  tmove(RR,CC),
  wa(Length,Rat),
  write(Val),!.

%------------------------------------------------------------------
%
% read the data from the viewport and store in the dataarea
%
%------------------------------------------------------------------

:- mode read_form(+).

read_form(S):-
  select_parm(S,[coord(R0,C0,_,_)]),
  recorded_w(S,var(Name,R:C,Length,_),Ref),
  RR is R0 + R, CC is C0 + C,
  C2 is CC + Length - 1,
  region_c((RR,CC),(RR,C2),Str),
  strcnv(Str,Val),
  replace(Ref,var(Name,R:C,Length,Val)),
  fail.
read_form(S).

strcnv(S,V):-
  strip_leading(S,S1),
  strip_trailing(S1,V),!.

%------------------------------------------------------------------
%
% capture keystrokes and drive form data entry
%
%------------------------------------------------------------------

:- mode fill_form(+).

fill_form(S):-
  build_field_list(S),
  recorded_w(field_list,S-[R:C:C2|T],_),
  tmove(R,C),
  set_flag(current_field,R:C:C2),
  get_keystrokes(S,[R:C:C2|T]).

build_field_list(S):-
  (recorded_w(field_list,S-L,Ref),erase(Ref); true),
  recorda(field_list,S-[],_),
  bfl(S),!.

bfl(S):-
  select_parm(S,[coord(R0,C0,_,_)]),
  recorded_w(S,var(_,R:C,Length,_),_),
  RR is R0 + R, CC is C0 + C,
  C2 is CC + Length - 1,
  add_field(S,RR:CC:C2),
  fail.
bfl(S):-
  recorded_w(field_list,S-L,Ref),       % If there is only one, make
  length(L,N), N == 1,                  % a dummy second one so next_item
  append(L,L,L2),                       % has something to find
  erase(Ref),
  recorda(field_list,S-L,Ref), !.
bfl(_).

add_field(S,F):-
  recorded_w(field_list,S-L,Ref),
  append(L,[F],L2),
  erase(Ref),
  recorda(field_list,S-L2,_),!.  

get_keystrokes(W,List):-
  select_parm(W,[attr(At)]),
  rev_attr(At, Rat),
  repeat,
  get_flag(current_field,F),
  keyb(A,S),
  put_viewport(A:S,F,List,Rat),!.

put_viewport(A:67, F, FList, Rat).   % f9
put_viewport(A:77, F, FList, Rat):-  % rt arrow
  curse_inc(F, FList), !, fail.
put_viewport(A:75, F, FList, Rat):-  % left arrow
  curse_dec(F, FList), !, fail.
put_viewport(A:72, F, FList, Rat):-  % up arrow
  prev_item(F, FList, R:C:C2),
  set_flag(current_field, R:C:C2),
  tmove(R,C), !, fail.
put_viewport(A:80, F, FList, Rat):-  % down arrow
  next_item(F, FList, R:C:C2),
  set_flag(current_field, R:C:C2),
  tmove(R,C), !, fail.
put_viewport(A:28, F, FList, Rat):-  % enter
  next_item(F, FList, R:C:C2),
  set_flag(current_field, R:C:C2),
  tmove(R,C), !, fail.
put_viewport(A:14, F, FList, Rat):-  % back space
  curse_dec(F, FList),
  wca(1, 0' , Rat), !, fail.
put_viewport(A:_,  F, FList, Rat):-  % letter
  wca(1, A, Rat),
  curse_inc(F, FList), !, fail.

curse_inc(Rx:Cx:C2x, FList):-
  tget(R,C),
  C < C2x,
  CC is C + 1,
  tmove(R,CC),!.
curse_inc(F, FList):-
  next_item(F, FList, R:C:C2),
  set_flag(current_field, R:C:C2),
  tmove(R,C),!.

curse_dec(Rx:Cx:C2x, FList):-
  tget(R,C),
  C > Cx,
  CC is C - 1,
  tmove(R,CC),!.
curse_dec(F, FList):-
  prev_item(F, FList, R:C:C2),
  set_flag(current_field, R:C:C2),
  tmove(R,C),!.

%------------------------------------------------------------------
%
% write the menu dataarea from the window definition
%
%------------------------------------------------------------------

:- mode init_menu_dataarea(+).

init_menu_dataarea(W):- recorded_w(W,_,_), !.
init_menu_dataarea(W):- m_init(W).

m_init(W):-
  select_parm(W,[menu(ItemList)]),
  m_create(W,ItemList,Nitems,0),
  update_stat(W,curnum,1,Nitems), !.

m_create(_,[],Nitems,Nitems):-!.
m_create(W,[Item|Rest],Nitems,X):-
  add_data(W,Item),
  XX is X + 1,
  !, m_create(W,Rest,Nitems,XX).

%------------------------------------------------------------------
%
% select an item from the menu
%
%------------------------------------------------------------------

:- mode menu_select(+,?).

menu_select(W,X):-
  select_parm(W,[coord(R1,C1,R2,_), width(L), attr(A)]),
  tmove(R1,C1),
  revideo(L,A),
  repeat,
  keyb(_,S),
  m_cur(S,Z,w(W,R1,R2,C1,L,A)),     % will fail until Z has a value
  !, Z = X.                         % might fail if X had a value

:- mode m_cur(+,-,+).

m_cur(80,_,w(W,R1,R2,C1,L,A)):-     % down arrow
  tget(R,_), R < R2,
  normvideo(L,A),
  RR is R + 1, tmove(RR,C1),
  revideo(L,A),
  !, fail.
m_cur(80,_,w(W,R1,R2,C1,L,A)):-     % down arrow at bottom
  tget(R,_), R >= R2,
  normvideo(L,A),
  scroll_window(W,1),
  tmove(R2,C1),
  revideo(L,A),
  !,fail.
m_cur(72,_,w(W,R1,R2,C1,L,A)):-     % up arrow
  tget(R,_), R > R1,
  normvideo(L,A),
  RR is R - 1, tmove(RR,C1),
  revideo(L,A),
  !, fail.
m_cur(72,_,w(W,R1,R2,C1,L,A)):-     % up arrow at top
  tget(R,_), R =< R1,
  normvideo(L,A),
  scroll_window(W,-1),
  tmove(R1,C1),
  revideo(L,A),
  !,fail.
m_cur(71,_,w(W,R1,R2,C1,L,A)):-     % home
  normvideo(L,A),
  scroll_window(W,top),
  tmove(R1,C1),
  revideo(L,A),
  !,fail.
m_cur(79,_,w(W,R1,R2,C1,L,A)):-     % end
  normvideo(L,A),
  scroll_window(W,bottom),
  tmove(R2,C1),
  revideo(L,A),
  !,fail.
m_cur(28,X,w(W,R1,R2,C1,L,A)):-     % enter
  tget(R,_),
  select_stat(W,curline,Line),
  Nth is Line + R - R1,
  nth_ref(W, Nth, Ref),
  instance(Ref,X),
  normvideo(L,A), !.

revideo(L,A):-
  rev_attr(A,RevAt),
  wa(L,RevAt).

normvideo(L,A):-
  wa(L,A).

%------------------------------------------------------------------
%
% read the prompt
%
%------------------------------------------------------------------

:- mode fill_prompt(+,?).

fill_prompt(W,Z):-
  select_parm(W,[coord(R1,C1,_,_), width(L), contents(Color)]),
  attr(Color,At),
  repeat,
  C2 is C1 + L - 1,
  tmove(R1,C1),
  lined(Y,R1:C1:C2,At),
  strip_leading(Y,Y1),
  strip_trailing(Y1,X),
  !, Z = X.                          % might fail if checking X value

% lined - a line editor, returns a string

% :- mode lined(?,+,+).
lined(NewS,R:C1:C2,At):-
  L is C2 - C1 + 1,
  tmove(R,C1),
  wa(L,At),
  repeat,
  keyb(A,S),
  modify(A:S,R:C1:C2,EndFlag,At),
  EndFlag == end,
  region_c((R,C1),(R,C2),NewS), !.

%:- mode modify(+,+,?,+).
modify(_:28,R:C1:C2,end,At):- !.        % CR - end edit
modify(_:1,R:C1:C2,x,At):-              % Esc - erase line
  tscroll(0,(R,C1),(R,C2)),
  tmove(R,C1), !.
modify(_:77,R:C1:C2,x,At):-             % cursor right
  tget(R,C),
  (C < C2, CC is C + 1;
   CC = C2),
  tmove(R,CC), !.
modify(_:75,R:C1:C2,x,At):-             % cursor left
  tget(R,C),
  (C > C1, CC is C - 1;
   CC = C1),
  tmove(R,CC), !.
modify(_:83,R:C1:C2,x,At):-             % Del - delete character
  tget(R,C),
  C < C2,
  CC is C + 1,
  hidecurse,
  region_ca((R,CC),(R,C2),S),
  list_text([32,At],LastChar),
  concat(S,LastChar,St),
  region_ca((R,C),(R,C2),St),
  tmove(R,C),
  showcurse, !.
modify(_:83,R:C1:C2,x,At):-             % Del - last space
  wca(1,0' ,At), !.
modify(_:14,R:C1:C2,x,At):-             % BS - move everything to the left
  tget(R,C),
  C > C1,
  CC is C - 1,
  hidecurse,
  region_ca((R,C),(R,C2),S),
  list_text([32,At],LastChar),
  concat(S,LastChar,St),
  region_ca((R,CC),(R,C2),St),
  tmove(R,CC),
  showcurse, !.
modify(_:14,_,_,_):- !.                 % BS - first space
modify(A:_,R:C1:C2,x,At):-              % any other character, insert
  tget(R,C),
  C < C2, CC is C + 1,
  hidecurse,
  C2a is C2 - 1,
  region_ca((R,C),(R,C2a),S),
  tmove(R,C),
  wca(1,A,At),
  region_ca((R,CC),(R,C2),S),
  tmove(R,CC),
  showcurse, !.
modify(A:_ ,_,_,At):-                    % last space
  wca(1,A,At).

%------------------------------------------------------------------
%
% debug dumps all the current window data
%
%------------------------------------------------------------------

debug:-
  write('window dump'),nl,
  debug_active,
  debug_windefs, !.

debug_active:-
  recorded_w(active,L,_),
  write(active:L), nl, !.
debug_active:- write('no active list'),nl.

debug_windefs:-
  recorded_w(windef,wd(W,A),_),
  write(W:A),nl,
  debug_curline(W),
  debug_data(W),
  fail.
debug_windefs:- write('no more windefs'),nl.

debug_curline(W):-
  recorded_w(curline,cl(W,Line,Num),_),
  write(W:curline:Line:Num),nl,!.
debug_curline(W):-
  write('no curline for':W),nl.

debug_data(W):-
  recorded_w(W,X,_),
  write(W:'first record':X), nl, !.
debug_data(W):-
  write('no data for':W),nl.

%******************************************************************
%
%     Utilities
%
%******************************************************************

% recorded_w_w - a rewrite of recorded_w for a far Prolog routine.

:- mode recorded_w_w(+,?,-).

recorded_w(K,T,R):-
  key(K,Kref),
  nref(Kref,Nref),
  rec_w(Kref,Nref,R,T).

:- mode rec_w(+,+,-,?).

rec_w(K,K,_,_):- !, fail.
rec_w(K,R,R,T):- instance(R,T).
rec_w(K,R,Ro,T):- nref(R,Rn), rec_w(K,Rn,Ro,T).

append([], X, X).
append([H|T], L, [H|Newt]):-
  append(T, L, Newt).

member(X, [X | Y]):-!.
member(X, [Y | Z]):-
  member(X, Z).

split(Item, List, Front, Back):-
  append(Front, [Item|Back], List),
  !.

remove(Item,List,NewList):-
  split(Item,List,Front,Back),
  append(Front,Back,NewList),!.

reverse(Forwards, Backwards):-
  rev(Forwards, [], Backwards), !.

rev([], B, B):- !.
rev([H|T], X, B):-
  rev(T, [H|X], B).

attr(black,0).
attr(blue,1).
attr(green,2).
attr(cyan,3).
attr(red,4).
attr(magenta,5).
attr(yellow,6).
attr(white,7).

attr(S:Fg:Bg, A):-
  attr(S:Fg, Af),
  attr(reverse:Bg, Ab),
  A is Af \/ Ab,!.

attr(bright:X,N):-
  attr(X,A),
  N is A \/ 8,!.
attr(reverse:X,N):-
  attr(X,A),
  rev_attr(A,N), !.

attr(Fg:Bg, A):-
  attr(Fg, Af),
  attr(reverse:Bg, Ab),
  A is Af \/ Ab,!.

rev_attr(At, Rat):-
  BG is (At /\ 7) << 4,
  FG is (At /\ 112) >> 4,
  A is At /\ 136,
  Rat is A \/ BG \/ FG.

% strip leading & trailing take the leading and trailing blanks
% off a string.  The final test is due to the unforgiveable nature
% of list_text to return either strings or atoms at its will
%

strip_leading(Si,So):-
  string_length(Si,Li),
  ctr_set(0,0),
  repeat,
  ctr_inc(0,Pos),
  (Pos >= Li, atom_string('',So);
    nth_char(Pos,Si,Char),
    Char \== 32,
    Lo is Li - Pos,
    substring(Si,Pos,Lo,So)), !.

strip_trailing(Si,So):-
  string_length(Si,Li),
  Last is Li - 1,
  ctr_set(0,Last),
  repeat,
  ctr_dec(0,Pos),
  (Pos < 0, atom_string('',So);
    nth_char(Pos,Si,Char),
    Char \== 32,
    Lo is Pos + 1,
    substring(Si,0,Lo,So)), !.

% flag setting predicates

get_flag(F,Val):-
  recorded_w(flag,F:Val,_).

set_flag(F,Val):-
  recorded_w(flag,F:_,Ref),
  replace(Ref,F:Val),!.
set_flag(F,Val):-
  recorda(flag,F:Val,_).

% wraps circularly around list

next_item(Item, List, NextItem):-
  split(Item, List, Front, Back),
  (first_item(Back, NextItem); first_item(Front, NextItem)),
  !.

prev_item(Item, List, PrevItem):-
  split(Item, List, Front, Back),
  (last_item(Front, PrevItem); last_item(Back, PrevItem)),
  !.

first_item([First|_],First):-true.

last_item([Last],Last):-!.
last_item([H|T],Last):-
  last_item(T,Last).

error(List):-
  tget(R,C),
  tscroll(0,(24,0),(24,79)),
  tmove(24,0),
  write('*** Window Error *** '),
  err(List),
  tmove(R,C), !.

err([]).
err([H|T]):- write(H), tab(1), err(T).


