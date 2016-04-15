% <PRE>
% Amzi! inc. Date Time Library
%
% The Date Time Library contains predicates that
% perform logical arithmetic on dates and times.
% The distinction is most noticable when dealing
% with months, which have varying numbers of days.
% See the description of date_add/3 for details.
%
% The library uses three structures: date/3, time/3
% and datetime/6.  Predicates are provided for dealing
% with each.  The structure arguments are:
%
%   date(YEAR,MONTH,DAY)
%   time(HOUR,MINUTES,SECONDS)
%   datetime(YEAR,MONTH,DAY,HOUR,MINUTES,SECONDS)
%
% The various predicates try to do the correct thing
% when overflowing the various quantities, like taking
% into account the number of days in February when adding,
% say, 15 days to Feb 20th.  The result will be a different
% day in March depending on whether the year is a leap
% year or not.
%
% The predicates also recognize the last day of each month
% as a special case.  So if you add a month to a date which
% is the last day of a month, you get the last day of the
% next month.
%
% The structures represent absolute dates and times.
% Relative date/time quantities are represented by structures
% for each unit, with one argument which is the value.  These
% structures are:  years/1, months/1, weeks/1, days/1, hours/1
% mins/1, and secs/1.
%
% Each is defined as a postfix operator as well.  So you can
% ask for example:
%
% ?- date_add(date(2002,1,15), 2 weeks, X).
% X = date(2002, 1, 29)
% yes
%
% The library includes predicates to parse and create date
% and time strings.
%
% To use the library, either consult or load the date_time
% file, or include it in your Prolog project.  Then import
% it.  For example, from the listener:
%
% ?- load(date_time).
% yes
% ?- import(date_time).
% yes
%

:- op(50, xf, days).
:- op(50, xf, months).
:- op(50, xf, weeks).
:- op(50, xf, years).
:- op(50, xf, hours).
:- op(50, xf, mins).
:- op(50, xf, secs).
:- op(700, xfx, <=).

:- module(date_time).
:- export( [
      date_get/2,             % get a date for today, tomorrow, etc.
      date_create/4,          % create a new date structure
      date_extract/2,         % extract date fields from a date structure
      date_age/2,             % compute an age from a birthday
      date_compare/3,         % compare two dates
      date_add/3,             % add years/months/weeks/days to a date
      date_difference/3,      % find the difference between two dates
      date_interval/3,        % find difference in single interval type (year, month, etc.)
      date_string/3,          % convert between date structures and strings
      date_year_day/2,        % calculate the day number for the year
      date_1900_days/2,       % calculate the days since the 0th day of 1900
      is_date_expression/1,   % succeeds if expression is a special date expression
      is_date_interval/1,     % succeeds if expression is a date interval
      is_date/1,              % succeeds if expression is a date
      time_get/2,             % gets the current time
      time_compare/3,         % compares two times
      time_add/3,             % add hours/mins/secs to a time
      time_difference/3,      % find the difference between two times
      time_interval/3,        % find the difference in single interval type(hour, min, sec)
      time_string/2,          % convert between time structures and strings
      datetime_get/2,         % get the current date and time
      datetime_compare/3,     % compare two datetime structures
      datetime_add/3,         % add datetime quantities to a datetime
      datetime_difference/3,  % find the difference between two datetimes
      datetime_string/3,      % convert to/from datetime strings
      datetime_date_time/3,   % convert datetime to/from date and time structures
      datetime_extract/2,     % extract years, months etc. from date time structure
      is_datetime_interval/1, % succeeds if expression is a date or time interval
      is_datetime/1,          % succeeds if expression is a datetime
      week_dayn/2,            % returns number for day of the week, 0 = Monday, 1 = Tuesday, ...
      week_day/2              % returns the day of the week for a date or datetime
      ]).
:- end_module(date_time).

:- body(date_time).


%-----------------------------------------------------------
% date_get(+DATE_TYPE, -DATE)
%
% Given one of the DATE_TYPEs, seen below, returns
% the DATE structure.
%

date_get(today, date(Y,M,D)) :- date(M,D,Y).
date_get(yesterday, DATE) :- date_add(today, days(-1), DATE).
date_get(tomorrow, DATE) :- date_add(today, days(1), DATE).
date_get(last_week, DATE) :- date_add(today, weeks(-1), DATE).
date_get(next_week, DATE) :- date_add(today, weeks(1), DATE).
date_get(last_month, DATE) :- date_add(today, months(-1), DATE).
date_get(next_month, DATE) :- date_add(today, months(1), DATE).
date_get(last_year, DATE) :- date_add(today, years(-1), DATE).
date_get(next_year, DATE) :- date_add(today, years(1), DATE).


%-----------------------------------------------------------
% date_create(+YEAR, +MONTH, +DAY, -DATE)
%
% Creates a new DATE structure from an input
% YEAR, MONTH, and DAY.
%

date_create(Y, M, D, date(Y,M,D)).


%-----------------------------------------------------------
% date_extract(+DATE, -VALUE)
%
% Gets the VALUE of the year, month or day, as
% specified in the TYPE argument, from an input
% DATE structure.
%

date_extract(date(Y,_,_), years(Y)).
date_extract(date(_,M,_), months(M)).
date_extract(date(_,_,D), days(D)).


%-----------------------------------------------------------
% date_age(+BIRTHDAY, -AGE)
%
% Computes an AGE in years, given a birthday
% date structure.  But, when it gets close, the
% days might be negative, so need to check for
% that case to prevent premature aging.
%

date_age(BDAY, AGE) :-
   date_get(today, TODAY),
   date_difference(TODAY, BDAY, DIFF),
   member(A years, DIFF),
   member(M months, DIFF),
   member(D days, DIFF),
   (M == 0, D < 0 ->
      AGE is A - 1
      ;
      AGE is A).

%-----------------------------------------------------------
% date_compare(+DATE_1, ?OP, +DATE_2)
%
% Compares the two date structures, unifying
% the result with the comparison operator.  So,
% it can be used to find the relationship or
% test a relationship.
%
% ?- date_compare(date(2002,1,15), X, date(2002,2,24)).
% X = < 
% yes
% ?- date_compare(date(2002,1,15), =<, date(2002,2,24)).
% yes
%

date_compare(D1, =, D2) :- D1 = D2, !.
date_compare(D1, >, D2) :- D1 @> D2, !.
date_compare(D1, <, D2) :- D1 @< D2, !.
date_compare(D1, >=, D2) :- D1 @>= D2, !.
date_compare(D1, =<, D2) :- D1 @=< D2, !.
date_compare(D1, <=, D2) :- D1 @=< D2, !.


%-----------------------------------------------------------
% date_add(+DATE_1, +DATE_QUANTITIES, -DATE_2)
%
% Adds the DATE_QUANTITIES to DATE_1 structure,
% returning DATE_2 structure.  The DATE_QUANTITIES
% are either a single structure or list of structures
% of the form days(D), months(M), or weeks(W).  Each
% is an operator, so can be written as '3 months' for
% example.
%
% The arithmetic is pure date arithmetic.  That is
% it adds calendar months, so Feb 15th plus one
% month yields Mar 15th.  Adding years over leap
% years winds up on same days as well.  Dates are
% correctly fixed for the corner cases, so an intermediate
% result of Feb 30th will become Mar 2nd in a non leap year
% and Mar 1st in leap year.
%
% ?- date_add(date(2002,1,15), [1 months, 2 days], D).
% D = date(2002, 2, 17) 
% yes
%
% ?- date_add(date(2002,1,15), [1 years, 1 months, 15 days], D).
% D = date(2003, 3, 2) 
% yes
%
% The special case of the last day of the month is
% recognized as well, so adding one month to the last
% day of a month gets the last day of the next month.
%
% ?- date_add(date(2002,1,31), 1 months, X).
% X = date(2002, 2, 28) 
% yes
%
% ?- date_add(date(2002,2,28), 1 months, X).
% X = date(2002, 3, 31) 
% yes
%

date_add(DATE, [], DATE) :-
   !.
date_add(D1, -(A1 + A2), DATE) :-
   convert_exp(-(A1+A2), AList),
   date_add(D1, AList, DATE),
   !.
date_add(D1, -(A1 - A2), DATE) :-
   convert_exp(-(A1-A2), AList),
   date_add(D1, AList, DATE),
   !.
date_add(D1, A1 + A2, DATE) :-
   convert_exp(A1+A2, AList),
   date_add(D1, AList, DATE),
   !.
date_add(D1, A1 - A2, DATE) :-
   convert_exp(A1-A2, AList),
   date_add(D1, AList, DATE),
   !.
date_add(D1, [DUNIT|DUNITS], DATE) :-
   date_add(D1, DUNIT, D2),
   !,
   date_add(D2, DUNITS, DATE).
date_add(D1, - [DUNIT|DUNITS], DATE) :-
   !,
   reverse_unit_signs([DUNIT|DUNITS],[RUNIT|RUNITS]),
   date_add(D1, [RUNIT|RUNITS], DATE).
date_add(today, ADD, DATE) :-
   date_get(today, D1),
   !,
   date_add(D1, ADD, DATE).
date_add(D1, -ADD, DATE) :-
   ADD =.. [UNIT, AMOUNT],
   MADD =.. [UNIT, -AMOUNT],
   date_add(D1, MADD, DATE).
date_add(date(Y,M,D), days(D1), date(YY,MM,DD)) :-
   D2 is D + D1,
   date_fix(date(Y,M,D2), date(YY,MM,DD)).
date_add(date(Y,M,D), weeks(D1), date(YY,MM,DD)) :-
   D2 is D + 7 * D1,
   date_fix(date(Y,M,D2), date(YY,MM,DD)).
date_add(date(Y,M,D), months(M1), date(YY,MM,DD)) :-
   M2 is M + M1,
   date_islast(date(Y,M,D), D2),
   date_fix(date(Y,M2,D2), date(YY,MM,DD)).
date_add(date(Y,M,D), years(Y1), date(YY,MM,DD)) :-
   Y2 is Y + Y1,
   date_islast(date(Y,M,D), D2),
   date_fix(date(Y2,M,D2), date(YY,MM,DD)).

convert_exp(Exp, List) :-
   convert_exp(Exp, [], List).

convert_exp(-(I1+I2), SoFar, List) :-
   !, convert_exp(-I1, [-I2|SoFar], List).
convert_exp(-(I1-I2), SoFar, List) :-
   !, convert_exp(-I1, [I2|SoFar], List).
convert_exp(I1+I2, SoFar, List) :-
   !, convert_exp(I1, [I2|SoFar], List).
convert_exp(I1-I2, SoFar, List) :-
   !, convert_exp(I1, [-I2|SoFar], List).
convert_exp(-Int, SoFar, [-Int|SoFar]) :-
   !.
convert_exp(Int, SoFar, [Int|SoFar]) :-
   !.

reverse_unit_signs([], []) :- !.
reverse_unit_signs([- A|As], [A|Bs]) :-
   !, reverse_unit_signs(As, Bs).
reverse_unit_signs([+ A|As], [- A|Bs]) :-
   !, reverse_unit_signs(As, Bs).
reverse_unit_signs([A|As], [- A|Bs]) :-
   !, reverse_unit_signs(As, Bs).
   
%-----------------------------------------------------------
% date_difference(+DATE_1, +DATE_2, -DATE_QUANTITIES).
%
% Subtracts, in pure date mode, DATE_2 date structure
% from DATE_1 date structure, providing a result of
% a list of date quantities.  Note that years are
% rounded, but that the result in the days(D) structure
% might be negative.  This is to allow the correct
% behavior when reapplying the difference by adding it
% to another date.
%
% ?- date_difference(date(2002,3,2), date(2002,1,15), D).
% D = [0 years, 2 months, -13 days] 
% yes
%
% The special case of both dates being end of month
% is recognized as being just a difference of one month.
%
% ?- date_difference(date(2002,2,28), date(2002,1,31), X).
% X = [0 years, 1 months, 0 days] 
% yes
%

date_difference(date(Y1,M1,D1), date(Y2,M2,D2),
      [years(Y), months(M), days(D)]) :-
   (D2 > D1 ->
      (date_islast(date(Y1,M1,D1), last) ->
         M1a is M1,
         D1a is D2
         ;
         M1a is M1 - 1,
         date_month_days(M1a,Y1,Dprev),
         D1a is D1 + Dprev )
      ;
      D1a = D1,
      M1a = M1 ),
   (M2 > M1a ->
      M1b is M1a + 12,
      Y1b is Y1 - 1
      ;
      M1b = M1a,
      Y1b = Y1 ),
   Y is Y1b - Y2,
   M is M1b - M2,
   D is D1a - D2.
    
/* had negative days
date_difference(date(Y1,M1,D1), date(Y2,M2,D2),
      [years(Y), months(M), days(D)]) :-
   Y3 is Y1 - Y2,
   M3 is M1 - M2,
   ( (date_islast(date(Y1,M1,D1),last), date_islast(date(Y2,M2,D2),last)) ->
     D = 0
     ;
     D is D1 - D2 ),
   (M3 < 0 ->
      M4 is M3 + 12,
      Y4 is Y3 - 1
      ;
      Y4 = Y3,
      M4 = M3),
   (Y4 < 0 ->
      Y is Y4 + 1,
      M is M4 - 12
      ;
      Y = Y4,
      M = M4).
*/

%----------------------------------------------------------
% date_1900_days(Date, Days)
%
% express a date as the number of days since the
% 0th day of 1900, which is date(1900,1,0).
%

date_1900_days(date(Y,M,D), Days) :-
   var(Days),
   !,
   Years is Y - 1900,
   (Y > 2000 ->
      LeapDays is ((Years-1) // 4) - 1
      ;
      LeapDays is (Years-1) // 4 ),
   MM is M - 1,
   date_add_month_days(MM, Y, 0, MonthDays),
   Days is Years * 365 + LeapDays + MonthDays + D.
date_1900_days(Date, Days) :-
   YearEst is 1900 + (Days // 365),
   date_1900_days(date(YearEst,1,1), DaysUsed),
   DaysLeft is Days - DaysUsed,
   date_add(date(YearEst,1,1), DaysLeft days, Date).
   

%----------------------------------------------------------
% date_year_day(Date, YearDay)
%
% for a date, calculate the day number in the year of
% the day
%

date_year_day(date(Y,M,D), YearDay) :-
   MM is M - 1,
   date_add_month_days(MM, Y, 0, MonthDays),
   YearDay is MonthDays + D.

%-----------------------------------------------------------
% date_interval(Date1, Date2, Interval)
%
% The date difference where Interval is in a specific
% unit, such as days or weeks.
%
% ex.  date_interval(D1, D2, M months).
%

date_interval(D1, D2, D days) :-
   !,
   date_1900_days(D1, Days1),
   date_1900_days(D2, Days2),
   D is Days1 - Days2.
date_interval(D1, D2, W weeks) :-
   !,
   date_interval(D1, D2, D days),
   W is D // 7.
date_interval(D1, D2, MM months) :-
   !,
   date_difference(D1, D2, [Y years, M months|_]),
   MM is 12 * Y + M.
date_interval(D1, D2, Y years) :-
   !,
   date_difference(D1, D2, [Y years|_]).

%-----------------------------------------------------------
% Internal predicates used by exported
% date predicates.
%

/* not used
date_expression_ok(_ weeks).
date_expression_ok(_ days).
date_expression_ok(_ months).
date_expression_ok(_ years).
*/

% make a date correct

date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   M < 1,
   !,
   M2 is M + 12,
   Y2 is Y - 1,
   date_fix(date(Y2,M2,D), date(YY,MM,DD)).
date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   M > 12,
   !,
   M2 is M - 12,
   Y2 is Y + 1,
   date_fix(date(Y2,M2,D), date(YY,MM,DD)).
date_fix(date(Y,M,last), date(Y,M,MD)) :-
   !,
   date_month_days(M,Y,MD).
date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   D < 1,
   !,
   M2 is M - 1,
   date_month_days(M2,Y,MD),
   D2 is D + MD,
   date_fix(date(Y,M2,D2), date(YY,MM,DD)).
date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   date_month_days(M,Y,MD),
   D > MD,
   !,
   M2 is M + 1,
   D2 is D - MD,
   date_fix(date(Y,M2,D2), date(YY,MM,DD)).
date_fix(date(Y,M,D), date(Y,M,D)).

% date_islast(+DATE, -DAY)
%
% if the day is the last day of the month,
% mark it as 'last', instead of its number.

date_islast(date(Y,M,MD), last) :-
   date_month_days(M,Y,MD), !.
date_islast(date(Y,M,D), D).

date_month_days(0,_,31).
date_month_days(1,_,31).
date_month_days(2,Y,29) :- date_leap_year(Y), !.
date_month_days(2,_,28).
date_month_days(3,_,31).
date_month_days(4,_,30).
date_month_days(5,_,31).
date_month_days(6,_,30).
date_month_days(7,_,31).
date_month_days(8,_,31).
date_month_days(9,_,30).
date_month_days(10,_,31).
date_month_days(11,_,30).
date_month_days(12,_,31).
date_month_days(13,_,31).

date_leap_year(Y) :-
   ( ( 0 =:= Y mod 100, 0 =:= Y mod 400 ) ;
     ( 0 =\= Y mod 100, 0 =:= Y mod 4 ) ).

% one y2k method

date_year_chk(YYYY, YYYY) :- YYYY > 1000, !.
date_year_chk(Y, YYYY) :-
   Y > 50, !,
   YYYY is Y + 1900.
date_year_chk(Y, YYYY) :-
   YYYY is Y + 2000.

date_add_month_days(0, _, Days, Days) :-
   !.
date_add_month_days(M, Y, Acc, Days) :-
   date_month_days(M, Y, D),
   Acc2 is Acc + D,
   MM is M - 1,
   !, date_add_month_days(MM, Y, Acc2, Days).


%-----------------------------------------------------------
% is_date(+DATE)
%
% Succeeds if DATE is a date
%

is_date(date(_,_,_)).
is_date(today).

%-----------------------------------------------------------
% is_date_interval(+INTERVAL)
%
% Succeeds if INTERVAL is a date interval
%

is_date_interval(INTERVAL) :-
   INTERVAL =.. [UNITS, _],
   member(UNITS, [days, weeks, months, years]).
is_date_interval(I1 + I2) :-
   is_date_interval(I1),
   is_date_interval(I2).
is_date_interval(I1 - I2) :-
   is_date_interval(I1),
   is_date_interval(I2).
is_date_interval(- I2) :-
   is_date_interval(I2).
   

%-----------------------------------------------------------
% is_date_expression(+DATE)
%
% Returns if the expression is a special date one.
%

is_date_expression(date(_,_,_)).
is_date_expression(EXP) :-
   EXP =.. [UNITS, _],
   member(UNITS, [days, weeks, months, years]).


%-----------------------------------------------------------
% time_get(+WHEN, -TIME)
%
% Returns the current time.
%

time_get(now, time(H,M,S)) :- time(H,M,S).


%-----------------------------------------------------------
% time_compare(+TIME_1, ?OP, +TIME_2)
%
% Compares the two times, unifying the
% result with the comparison operator.
%

time_compare(T1, =, T2) :- T1 = T2, !.
time_compare(T1, >, T2) :- T1 @> T2, !.
time_compare(T1, <, T2) :- T1 @< T2, !.
time_compare(T1, >=, T2) :- T1 @>= T2, !.
time_compare(T1, =<, T2) :- T1 @=< T2, !.
time_compare(T1, <=, T2) :- T1 @=< T2, !.


%-----------------------------------------------------------
% time_add(+TIME_1, +TIME_QUANTITIES, -TIME_2)
%
% Adds the TIME_QUANTITIES to TIME_1 and
% returns TIME_2.  Time quantities can be
% hours/1, mins/1 or secs/1.
%

time_add(TIME, [], TIME) :- !.
time_add(T1, [TUNIT|TUNITS], TIME) :-
   time_add(T1, TUNIT, T2),
   !, time_add(T2, TUNITS, TIME).
time_add(now, ADD, TIME) :-
   time_get(now, T1),
   time_add(T1, ADD, TIME).
time_add(T1, -ADD, TIME) :-
   ADD =.. [UNIT, AMOUNT],
   MADD =.. [UNIT, -AMOUNT],
   time_add(T1, MADD, TIME).
time_add(time(H,M,S), secs(S1), time(HH,MM,SS)) :-
   S2 is S + S1,
   time_fix(time(H,M,S2), time(HH,MM,SS)).
time_add(time(H,M,S), mins(M1), time(HH,MM,SS)) :-
   M2 is M + M1,
   time_fix(time(H,M2,S), time(HH,MM,SS)).
time_add(time(H,M,S), hours(H1), time(HH,MM,SS)) :-
   H2 is H + H1,
   time_fix(time(H2,M,S), time(HH,MM,SS)).


%-----------------------------------------------------------
% time_difference(+TIME_1, +TIME_2, -TIME_QUANTITIES
%
% Subtracts two times, returning a list of time
% quantities representing the difference.
%

time_difference(time(H1,M1,S1), time(H2,M2,S2),
      [hours(H), mins(M), secs(S)] ) :-
   H3 is H1 - H2,
   M3 is M1 - M2,
   S3 is S1 - S2,
   (S3 < 0 ->
      M4 is M3 - 1,
      S is S3 + 60
      ;
      M4 = M3,
      S = S3),
   (M4 < 0 ->
      M is M4 + 60,
      H is H3 - 1
      ;
      H = H3,
      M = M4).

time_interval(time(H1,M1,_), time(H2,M2,_), mins(M)) :-
   !, M is 60*(H1-H2) + (M1-M2).
time_interval(time(H1,M1,S1), time(H2,M2,S2), secs(S)) :-
   !, S is 3600*(H1-H2) + 60*(M1-M2) + (S1-S2).
time_interval(time(H1,M1,_), time(H2,M2,_), hours(H)) :-
   !, S is (H1-H2) + (M1-M2)/60.
time_interval(datetime(Y,L,D,H1,M1,_), datetime(Y,L,D,H2,M2,_), mins(M)) :-
   !, M is 60*(H1-H2) + (M1-M2).
time_interval(datetime(Y,L,D,H1,M1,S1), datetime(Y,L,D,H2,M2,S2), secs(S)) :-
   !, S is 3600*(H1-H2) + 60*(M1-M2) + (S1-S2).
time_interval(datetime(Y,L,D,H1,M1,_), datetime(Y,L,D,H2,M2,_), hours(H)) :-
   !, S is (H1-H2) + (M1-M2)/60.
time_interval(datetime(Y1,L1,D1,H1,M1,_), datetime(Y2,L2,D2,H2,M2,_), mins(M)) :-
   !,
   date_interval(date(Y1,L1,D1), date(Y2,L2,D2), days(D)),
   M is 24*60*D + 60*(H1-H2) + (M1-M2).
time_interval(datetime(Y1,L1,D1,H1,M1,S1), datetime(Y2,L2,D2,H2,M2,S2), secs(S)) :-
   !,
   date_interval(date(Y1,L1,D1), date(Y2,L2,D2), days(D)),
   S is 24*60*60*D + 3600*(H1-H2) + 60*(M1-M2) + (S1-S2).
time_interval(datetime(Y1,L1,D1,H1,M1,_), datetime(Y2,L2,D2,H2,M2,_), hours(H)) :-
   !,
   date_interval(date(Y1,L1,D1), date(Y2,L2,D2), days(D)),
   S is 24*D + (H1-H2) + (M1-M2)/60.
   

%-----------------------------------------------------------
% Time internal predicates
%

time_fix(time(H,M,S), time(HH,MM,SS)) :-
   H < 0,
   !,
   H2 is H + 24,
   time_fix(time(H2,M,S), time(HH,MM,SS)).
time_fix(time(H,M,S), time(HH,MM,SS)) :-
   H > 23,
   !,
   H2 is H - 24,
   time_fix(time(H2,M,S), time(HH,MM,SS)).
time_fix(time(H,M,S), time(HH,MM,SS)) :-
   M < 0,
   !,
   M2 is M + 60,
   H2 is H - 1,
   time_fix(time(H2,M2,S), time(HH,MM,SS)).
time_fix(time(H,M,S), time(HH,MM,SS)) :-
   M > 59,
   !,
   M2 is M - 60,
   H2 is H + 1,
   time_fix(time(H2,M2,S), time(HH,MM,SS)).
time_fix(time(H,M,S), time(HH,MM,SS)) :-
   S < 0,
   !,
   S2 is S + 60,
   M2 is M - 1,
   time_fix(time(H,M2,S2), time(HH,MM,SS)).
time_fix(time(H,M,S), time(HH,MM,SS)) :-
   S > 59,
   !,
   S2 is S - 60,
   M2 is M + 1,
   time_fix(time(H,M2,S2), time(HH,MM,SS)).
time_fix(time(H,M,S), time(H,M,S)).


%-----------------------------------------------------------
% is_datetime(+DATETIME)
%
% Succeeds if DATETIME is a datetime
%

is_datetime(datetime(_,_,_,_,_,_)).

%-----------------------------------------------------------
% is_datetime_interval(+INTERVAL)
%
% Succeeds if INTERVAL is a date or time interval
%

is_datetime_interval(INTERVAL) :-
   INTERVAL =.. [UNITS, _],
   member(UNITS, [days, weeks, months, years, hours, mins, secs]).
is_datetime_interval(I1 + I2) :-
   is_datetime_interval(I1),
   is_datetime_interval(I2).
is_datetime_interval(I1 - I2) :-
   is_datetime_interval(I1),
   is_datetime_interval(I2).
is_datetime_interval(- I2) :-
   is_datetime_interval(I2).
   
%--------------------------------------------------------------
% datetime_get(+WHEN, -DATETIME)
%
% Returns the current date and time in a datetime/6
% structure.
%

datetime_get(now, datetime(YEAR,MON,DAY,HOUR,MIN,SEC)) :-
   date_get(today, date(YEAR,MON,DAY)),
   time_get(now, time(HOUR,MIN,SEC)).
datetime_get(today, datetime(YEAR,MON,DAY,0,0,0)) :-
   date_get(today, date(YEAR,MON,DAY)).


%--------------------------------------------------------------
% datetime_compare(+DT_1, ?OP, +DT_2)
%
% Compares the datetime structures, DT_1 and
% DT_2 and unifies with the operator OP.
%

datetime_compare(T1, =, T2) :- T1 = T2, !.
datetime_compare(T1, >, T2) :- T1 @> T2, !.
datetime_compare(T1, <, T2) :- T1 @< T2, !.
datetime_compare(T1, >=, T2) :- T1 @>= T2, !.
datetime_compare(T1, =<, T2) :- T1 @=< T2, !.
datetime_compare(T1, <=, T2) :- T1 @=< T2, !.


%--------------------------------------------------------------
% datetime_add(+DT_1, +DT_QUANTITIES, -DT_2)
%
% Adds the date and time quantities to datetime
% structure DT_1, returning the result in DT_2.
%

datetime_add(DT, [], DT) :- !.
datetime_add(DT1, [DTUNIT|DTUNITS], DT) :-
   datetime_add(DT1, DTUNIT, DT2),
   !, datetime_add(DT2, DTUNITS, DT).
datetime_add(now, ADD, TIME) :-
   datetime_get(now, T1),
   datetime_add(T1, ADD, TIME).
datetime_add(today, ADD, TIME) :-
   datetime_get(today, T1),
   datetime_add(T1, ADD, TIME).
datetime_add(T1, -ADD, TIME) :-
   ADD =.. [UNIT, AMOUNT],
   MADD =.. [UNIT, -AMOUNT],
   datetime_add(T1, MADD, TIME).
datetime_add(datetime(Y,L,D,H,M,S), years(Y1), datetime(YY,LL,DD,HH,MM,SS)) :-
   Y2 is Y + Y1,
   datetime_fix(datetime(Y2,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_add(datetime(Y,L,D,H,M,S), months(L1), datetime(YY,LL,DD,HH,MM,SS)) :-
   L2 is L + L1,
   datetime_fix(datetime(Y,L2,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_add(datetime(Y,L,D,H,M,S), days(D1), datetime(YY,LL,DD,HH,MM,SS)) :-
   D2 is D + D1,
   datetime_fix(datetime(Y,L,D2,H,M,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_add(datetime(Y,L,D,H,M,S), hours(H1), datetime(YY,LL,DD,HH,MM,SS)) :-
   H2 is H + H1,
   datetime_fix(datetime(Y,L,D,H2,M,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_add(datetime(Y,L,D,H,M,S), mins(M1), datetime(YY,LL,DD,HH,MM,SS)) :-
   M2 is M + M1,
   datetime_fix(datetime(Y,L,D,H,M2,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_add(datetime(Y,L,D,H,M,S), secs(S1), datetime(YY,LL,DD,HH,MM,SS)) :-
   S2 is S + S1,
   datetime_fix(datetime(Y,L,D,H,M,S2), datetime(YY,LL,DD,HH,MM,SS)).


%--------------------------------------------------------------
% datetime_difference(+DT_1, +DT_2, -DT_QUANTITIES)
%
% Subtracts two datetime structures, returning the
% datetime quantities.
%

datetime_difference(datetime(Y1,L1,D1,H1,M1,S1), datetime(Y2,L2,D2,H2,M2,S2),
      [years(Y), months(L), days(D), hours(H), mins(M), secs(S)] ) :-
   date_difference(date(Y1,L1,D1), date(Y2,L2,D2), [years(Y), months(L), days(D)]),
   time_difference(time(H1,M1,S1), time(H2,M2,S2), [hours(H), mins(M), secs(S)]).


%--------------------------------------------------------------
% datetime_date_time(?DT, ?DATE, ?TIME)
%
% convert between datetime and date and time structures.
%

datetime_date_time(datetime(YR,MO,DA,HR,MI,SE), date(YR,MO,DA), time(HR,MI,SE)).


%-----------------------------------------------------------
% datetime_extract(+DATETIME, -VALUE)
%
% Gets the VALUE of the year, month or day, as
% specified in the TYPE argument, from an input
% DATE structure.
%

datetime_extract(datetime(Y,_,_,_,_,_), years(Y)).
datetime_extract(datetime(_,M,_,_,_,_), months(M)).
datetime_extract(datetime(_,_,D,_,_,_), days(D)).
datetime_extract(datetime(_,_,_,H,_,_), hours(H)).
datetime_extract(datetime(_,_,_,_,M,_), mins(M)).
datetime_extract(datetime(_,_,_,_,_,S), secs(D)).

%--------------------------------------------------------------
% Internal predicates used in datetime calculations.
%

datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)) :-
   H < 0,
   !,
   H2 is H + 24,
   D2 is D - 1,
   datetime_fix(datetime(Y,L,D2,H2,M,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)) :-
   H > 23,
   !,
   H2 is H - 24,
   D2 is D + 1,
   datetime_fix(datetime(Y,L,D2,H2,M,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)) :-
   M < 0,
   !,
   M2 is M + 60,
   H2 is H - 1,
   datetime_fix(datetime(Y,L,D,H2,M2,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)) :-
   M > 59,
   !,
   M2 is M - 60,
   H2 is H + 1,
   datetime_fix(datetime(Y,L,D,H2,M2,S), datetime(YY,LL,DD,HH,MM,SS)).
datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)) :-
   S < 0,
   !,
   S2 is S + 60,
   M2 is M - 1,
   datetime_fix(datetime(Y,L,D,H,M2,S2), datetime(YY,LL,DD,HH,MM,SS)).
datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,HH,MM,SS)) :-
   S > 59,
   !,
   S2 is S - 60,
   M2 is M + 1,
   datetime_fix(datetime(Y,L,D,H,M2,S2), datetime(YY,LL,DD,HH,MM,SS)).
datetime_fix(datetime(Y,L,D,H,M,S), datetime(YY,LL,DD,H,M,S)) :-
   date_fix(date(Y,L,D), date(YY,LL,DD)).

%--------------------------------------------------
% date_string(?DATE, ?FORMAT, ?STRING)
%
% Convert between a date structure and a string,
% optionally based on a specified format atom.
% See ds_date below for the accepted formats for
% dates.
%
% ?- date_string(D, F, `24 Feb 1946`).
% D = date(1946, 2, 24)
% F = 'd mon y' 
% yes
%
% ?- date_string(D, F, `February 24, 1946`).
% D = date(1946, 2, 24)
% F = 'month d, y' 
% yes
%
% ?- date_string(date(1946,2,24), 'month d, y', X).
% X = `February 24, 1946` 
% yes
%
% ?- date_string(D, 'd/m/y', `24/2/1946`).
% D = date(1946, 2, 24) 
% yes
%
% ?- date_string(date(1946,2,24), F, X).
% F = 'm/d/y'
% X = `2/24/1946` 
% yes
%

date_string(DATE, FORMAT, STRING) :-
   nonvar(STRING), !,
   string_list(STRING, LIST),
   ds_date(DATE, FORMAT, LIST, []),
   !.
date_string(DATE, FORMAT, STRING) :-
   ds_date(DATE, FORMAT, LIST, []),
   !,
   string_list(STRING, LIST).
 
ds_date(date(Y,M,D), 'y/m/d') -->
   ds_year(Y), sp, "/", sp, ds_month(M), sp, "/", sp, ds_day(D), !.
ds_date(date(Y,M,D), 'm/d/y') -->
   ds_month(M), sp, "/", sp, ds_day(D), sp, "/", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'mm/dd/yyyy') -->
   ds_month2(M), sp, "/", sp, ds_day2(D), sp, "/", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'd/m/y') --> 
   ds_day(D), sp, "/", sp, ds_month(M), sp, "/", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'y-m-d') -->
   ds_year(Y), sp, "-", sp, ds_month(M), sp, "-", sp, ds_day(D), !.
ds_date(date(Y,M,D), 'm-d-y') -->
   ds_month(M), sp, "-", sp, ds_day(D), sp, "-", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'd-m-y') --> 
   ds_day(D), sp, "-", sp, ds_month(M), sp, "-", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'd mon y') -->
   ds_day(D), " ", sp, ds_short_month(M), " ", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'month d, y') -->
   ds_long_month(M), " ", sp, ds_day(D), ", ", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'mon d y') -->
   ds_short_month(M), " ", sp, ds_day(D), " ", sp, ds_year(Y), !.
ds_date(date(Y,M,D), 'month d y') -->
   ds_long_month(M), " ", sp, ds_day(D), " ", sp, ds_year(Y), !.

%--------------------------------------------------
% time_string(?TIME, ?STRING)
%
% Convert between time structures and strings of the
% form hh:mm:ss.
%
% ?- time_string(time(2,33,15), X).
% X = `2:33:15` 
% yes
%
% ?- time_string(T, `2:33:22`).
% T = time(2, 33, 22) 
% yes
%

time_string(TIME, STRING) :-
   nonvar(STRING), !,
   string_list(STRING, LIST),
   ds_time(TIME, LIST, []),
   !.
time_string(TIME, STRING) :-
   ds_time(TIME, LIST, []),
   !,
   string_list(STRING, LIST).

%--------------------------------------------------
% datetime_string(?DATE, ?FORMAT, ?STRING)
%
% Convert between a date structure and a string,
% optionally based on a specified format atom.
% See date_string and time_string for details.
%

datetime_string(DT, FORMAT, STRING) :-
   nonvar(STRING), !,
   string_list(STRING, LIST),
   ds_datetime(DT, FORMAT, LIST, []),
   !.
datetime_string(DT, FORMAT, STRING) :-
   ds_datetime(DT, FORMAT, LIST, []),
   !,
   string_list(STRING, LIST).

ds_datetime(datetime(YR,DY,MO,HR,MI,SE), FORMAT) -->
   ds_date(date(YR,DY,MO), FORMAT),
   " ",
   sp,
   ds_time(time(HR,MI,SE)).
   
%--------------------------------------------------
% Supporting predicates for string conversions
%

ds_time(time(H,M,S)) -->
   ds_hour(H), sp, ":", sp, ds_min(M), sp, ":", sp, ds_sec(S).

ds_year(YY) --> { var(YY), ! }, ds_number(Y), { date_year_chk(Y, YY) }.
ds_year(Y) --> { date_year_chk(Y, YY) }, ds_number(YY).
ds_month(M) --> ds_number(M).
ds_day(D) --> ds_number(D).

ds_hour(H) --> ds_number(H).
ds_min(M) --> ds_number(M).
ds_sec(S) --> ds_number(S).

ds_month2(MM) --> ds_number2(MM).
ds_day2(DD) --> ds_number2(DD).

ds_number(N) -->
   { var(N) }, !,
   ds_digits(D),
   { string_list(S, D), string_integer(S, N)}.
ds_number(N) -->
   { string_integer(S, N), string_list(S, D) },
   ds_digits(D).

ds_digits([X|Y]) --> [X], {ds_digit(X)}, ds_digits(Y).
ds_digits([X]) --> [X], {ds_digit(X)}.

ds_number2(N) -->
   { var(N) }, !,
   ds_digits(D),
   { string_list(S, D), string_integer(S, N)}.
ds_number2(N) -->
   { string_integer(S, N), string_list(S, D) },
   ds_digits2(D).

ds_digits2([N]) --> [0'0, N], {ds_digit(N)}.
ds_digits2([A,B]) --> [A,B], {ds_digit(A), ds_digit(B)}.

ds_digit(X) :- number(X), X >= 0'0, X =< 0'9.

sp --> "".
sp --> [W], { number(W), W =< 32 }, sp.

ds_short_month(1) --> "Jan".
ds_short_month(2) --> "Feb".
ds_short_month(3) --> "Mar".
ds_short_month(4) --> "Apr".
ds_short_month(5) --> "May".
ds_short_month(6) --> "Jun".
ds_short_month(7) --> "Jul".
ds_short_month(8) --> "Aug".
ds_short_month(9) --> "Sep".
ds_short_month(10) --> "Oct".
ds_short_month(11) --> "Nov".
ds_short_month(12) --> "Dec".

ds_long_month(1) --> "January".
ds_long_month(2) --> "February".
ds_long_month(3) --> "March".
ds_long_month(4) --> "April".
ds_long_month(5) --> "May".
ds_long_month(6) --> "June".
ds_long_month(7) --> "July".
ds_long_month(8) --> "August".
ds_long_month(9) --> "September".
ds_long_month(10) --> "October".
ds_long_month(11) --> "November".
ds_long_month(12) --> "December".

%---------------------------------------------
% week_day(DT, WD)
%
% Return the day of the week for a date or date time
%

week_day(date(Y,M,D), WD) :-
   date_1900_days(date(Y,M,D), N),
   DN is N mod 7,
   day_name(DN, WD),
   !.
week_day(datetime(Y,M,D,_,_,_), WD) :-
   date_1900_days(date(Y,M,D), N),
   DN is N mod 7,
   day_name(DN, WD),
   !.

week_dayn(date(Y,M,D), DN) :-
   date_1900_days(date(Y,M,D), N),
   DN is N mod 7,
   !.
week_dayn(datetime(Y,M,D,_,_,_), DN) :-
   date_1900_days(date(Y,M,D), N),
   DN is N mod 7,
   !.

day_name(0, 'Monday').
day_name(1, 'Tuesday').
day_name(2, 'Wednesday').
day_name(3, 'Thursday').
day_name(4, 'Friday').
day_name(5, 'Saturday').
day_name(6, 'Sunday').

%--------------------------------------------
% utils
%

member(X, [X|_]).
member(X, [_|Z]) :- member(X, Z).

reverse(A, Z) :- reverse(A, [], Z).

   reverse([], Z, Z).
   reverse([A|X], SoFar, Z) :- reverse(X, [A|SoFar], Z).



:- end_body(date_time).
