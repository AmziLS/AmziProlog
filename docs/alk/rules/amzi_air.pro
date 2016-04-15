:- set_prolog_flag(decimals, real).
:- set_prolog_flag(decimal_places, 2).

%---------------------------------------------
% pricing rules
%

main :-
   price('I68', 'WDR', X),
   write(test = X), nl.

price( _From, _To, _Price ) :-
   special( _From, _To, _Price ).
price( _From, _To, _Price ) :-
   normal( _From, _To, _Price ).

normal(_From, _To, _Price) :-
   flight( _Flight, _From, _To ),
   base_fare( _Flight, _Fare ),
   adjustment( _Flight, _Fare, _Price ).

special( 'Russellville, AL', 'Soddy Daisy, TN', 35.00 ) :-
   fact(month, _Mon),
   (_Mon = 'July'; _Mon = 'August').
special( 'Russellville, AL', 'Soddy Daisy, TN', 42.00 ) :-
   is_senior.
special( 'Lebanon, OH', _To, _Price ) :-
   normal( _From, _To, _Price1),
   _Price is _Price1 * 0.90.

adjustment( _Flight, _Fare, _Price ) :-
   is_vacation(_Flight),
   _Price is _Fare * 1.40.
adjustment( _Flight, _Fare, _Price ) :-
   is_weekend,
   _Price is _Fare * 1.20.
adjustment( _Flight, _Fare, _Price ) :-
   _Price is _Fare.

is_weekend :- fact(day, 'Saturday').
is_weekend :- fact(day, 'Sunday').

is_vacation(_Flight) :-
   flight(_Flight, _, 'St. Petersburg, FL'),
   fact(month, _Mon),
   (_Mon = 'December'; _Mon = 'January'; _Mon = 'February').
is_vacation(_Flight) :-
   flight(_Flight, _, 'Winder, GA'),
   fact(month, _Mon),
   (_Mon = 'April'; _Mon = 'September').

is_senior :-
   fact(age, _Age),
   _Age > 60.

% The base fare is the cost for making
% the flight.

base_fare(FlightID, Cost) :-
   distance(FlightID, Distance),
   fuel_cost(Distance, Cost).

% Fuel costs are based on a simple formula
% using the speed of the airplane, fuel consumption,
% and price of fuel, plus a fixed additional amount
% of fuel used for taking off.

dollars_per_gallon(2.00).
air_speed_knots(120).
gallons_per_hour(10.0).
takeoff_fuel(5.0).

fuel_cost(Distance, Cost) :-
   dollars_per_gallon(DpG),
   gallons_per_hour(GpH),
   air_speed_knots(AS),
   takeoff_fuel(TF),
   Cost is (TF + (Distance/AS) * GpH) * DpG.

%--------------------------------------------
% Flight Data for Amzi!Air
%

% Airports are refered to by 3 character identifiers.
% The airport/2 predicate relates identifiers and
% airport names.
%
% airport(AirportID, Location)

airport('I68', 'Lebanon, OH').
airport('WDR', 'Winder, GA').
airport('SPG', 'St. Petersburg, FL').
airport('M22', 'Russellville, AL').
airport('1A0', 'Soddy Daisy, TN').
airport('ARB', 'Ann Arbor, MI').
airport('BED', 'Bedford, MA').
airport('UNV', 'State College, PA').
airport('FDK', 'Frederick, MD').
airport('CPK', 'Norfolk, VA').
airport('3W2', 'Put in Bay, OH').
airport('DCA', 'Washington, DC').

% Flights have identifiers and go from one
% airport to another.
%
% flight(FlightID, FromAirportID, ToAirportID).

flight(F, From, To) :-
   airport(FromID, From),
   airport(ToID, To),
   flight(F, FromID, ToID).
flight(aa01, 'I68', 'WDR').
flight(aa02, 'WDR', 'I68').
flight(aa03, 'WDR', 'SPG').
flight(aa04, 'SPG', 'WDR').
flight(aa05, 'SPG', 'M22').
flight(aa06, 'M22', 'SPG').
flight(aa07, 'M22', '1A0').
flight(aa08, '1A0', 'M22').
flight(aa09, '1A0', 'I68').
flight(aa10, 'I68', '1A0').
flight(aa11, 'I68', 'UNV').
flight(aa12, 'UNV', 'I68').
flight(aa15, 'UNV', 'BED').
flight(aa16, 'BED', 'UNV').
flight(aa17, 'I68', 'ARB').
flight(aa18, 'ARB', 'I68').
flight(aa19, 'I68', 'FDK').
flight(aa20, 'FDK', 'I68').
flight(aa21, 'I68', 'CPK').
flight(aa22, 'CPK', 'I68').
flight(aa23, 'I68', '3W2').
flight(aa24, '3W2', 'I68').
flight(aa25, 'I68', 'DCA').
flight(aa26, 'DCA', 'I68').
flight(aa27, 'DCA', 'BED').
flight(aa28, 'BED', 'DCA').

% The distance of each flight is used to
% calculate the base fare for the flight.
% Distances are in nautical miles.
%
% distance(FlightID, Distance).

distance(aa01, 328).
distance(aa02, 328).
distance(aa03, 372).
distance(aa04, 372).
distance(aa05, 350).
distance(aa06, 350).
distance(aa07, 140).
distance(aa08, 140).
distance(aa09, 257).
distance(aa10, 257).
distance(aa11, 300).
distance(aa12, 300).
distance(aa15, 318).
distance(aa16, 318).
distance(aa17, 168).
distance(aa18, 168).
distance(aa19, 318).
distance(aa20, 318).
distance(aa21, 407).
distance(aa22, 407).
distance(aa23, 146).
distance(aa24, 146).
distance(aa25, 332).
distance(aa26, 332).
distance(aa27, 344).
distance(aa28, 344).
