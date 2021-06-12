% WINDEMO - demonstrates how to use windows

:- module windemo.

:- public main/0, restart/0.

:- extrn window/2:far, window/3:far.

main:-cls,go.

restart:-halt.

go:-
  create_windows,
  ctr_set(1,1),            % used by list2
  ctr_set(3,1),            % used by dummy
  repeat,
  window(wmain,read,X),
  do(X),
  fail.

create_windows:-
  window(wform,create,
    [type(form), coord(8,20,16,53), title(' Form not Function '),
      border(white:magenta),contents(white:magenta),
      form([lit(2:5,'First'),
            var(one,2:20,8,''),
            lit(4:5,'Second'),
            var(two,4:20,8,'two'),
            lit(6:5,'Third'),
            var(three,6:20,8,'')])]),
  window(wform2,create,
    [type(form), coord(12,24,14,49), title(' Form two '),
      border(white:green),contents(white:green),
      form([lit(2:3,'First and Last'),
            var(three,2:20,8,'')])]),
  window(wprompt,create,
    [type(prompt), coord(18,10,18,70),
      border(black:green),contents(black:green),
      title(' input ')]),
  window(wmain,create,
    [type(menu), coord(15,25,20,40),
      border(blue),contents(yellow),
      menu(['new numbers',
            'add numbers',
            'try prompt',
            'try dynamic',
            'try form',
             exit,
             one,two,three,four,five,six,seven])]),
  window(wexit,create,
    [type(display), coord(20,40,21,50),
      border(black:red),contents(black:red),
      title(' exit ')]),
  window(wdummy,create,
    [type(menu), coord(18,32,23,42),
      border(bright:green),contents(green:white),
      title(' dummy '),
      menu([return,one,two,three,four,five,six])]),
  window(wdummylog,create,
    [coord(1,1,10,15)]),
  window(wlist1,create,
    [type(display), coord(2,2,23,50),
      border(reverse:blue),contents(reverse:blue),
      title(' List One ')]),
  window(wlist2,create,
    [type(display), coord(2,20,23,78),
      border(yellow),contents(blue:yellow),
      title(' List Two ')]).

do('new numbers'):-list1,!.
do('add numbers'):-list2,!.
do('try prompt'):-prompt,!.
do('try dynamic'):-pop,!.
do('try form'):-form,!.
do(exit):-exit.
do(_):-dummy.

list1:-
  window(wlist1,open),
  ctr_set(0,1),
  repeat,
  ctr_inc(0,N),
  window(wlist1,write,'line number is ':N),
  N >= 50,
  window(wlist1,writelist,[nl]),
  window(wlist1,writelist,
     ['You can use home, end, pgup, & pgdn',
      'to examine the contents',
      'use enter to leave the '-wlist1-' window']),
  window(x,driver),
  window(wlist1,close), !.

list2:-
  window(wlist2,open),
  window(wlist2,write,'adding more numbers'),
  ctr_set(2,1),
  repeat,
  ctr_inc(1,N),
  ctr_inc(2,Test),
  window(wlist2,write,'adding number':N),
  Test >= 10,
  window(x,driver), !.

exit:-
  window(wexit,write,['Good Bye']),
  window(x,driver),
  cls, halt.

dummy:-
  repeat,
  window(wdummy,read,X),
  ctr_inc(3,N),
  window(wdummylog,write,[N:X]),
  X == return,
  window(wdummy,close), !.

prompt:-
  repeat,
  window(wprompt,read,['',X]),
  window(wdummylog,write,[X]),
  X == '',
  window(wprompt,close), !.

pop:-
  window([type(prompt),coord(23,2,23,10),
               title(' pop '),contents(white:blue)],
         read, ['',X]),
  window(wdummylog,write,[X]),
  window([type(menu),coord(20,2,21,5),
               contents(white:magenta),menu([yes,no])],
          read, Y),
  window(wdummylog,write,[Y]),!.

form:-
  window(wform,read,_),
  recorded(wform,var(Vname,_,_,Val),_),
  window(wdummylog,write,[Vname=Val]),
  window(wform2,read,_),
  window(wform2,erase),
  fail.
form.

