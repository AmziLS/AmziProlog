airport('I68', 'Lebanon, OH').
airport('WDR', 'Winder, GA').
airport('SPG', 'St. Petersburg, FL').
airport('M22', 'Russellville, AL').
airport('1A0', 'Soddy Daisy, TN').
airport('ARB', 'Ann Arbor, MI').
airport('BED', 'Bedford, MA').
airport('UNV', 'State College, PA').
airport('EZF', 'Fredericksburg, VA').
airport('CPK', 'Norfolk, VA').
airport('3W2', 'Put in Bay, OH').
airport('DCA', 'Washington, DC').

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
flight(aa19, 'I68', 'EZF').
flight(aa20, 'EZF', 'I68').
flight(aa21, 'I68', 'CPK').
flight(aa22, 'CPK', 'I68').
flight(aa23, 'I68', '3W2').
flight(aa24, '3W2', 'I68').
flight(aa25, 'I68', 'DCA').
flight(aa26, 'DCA', 'I68').
flight(aa27, 'DCA', 'BED').
flight(aa28, 'BED', 'DCA').

base_fare(aa01, 100).
base_fare(aa02, 100).
base_fare(aa03, 100).
base_fare(aa04, 100).
base_fare(aa05, 90).
base_fare(aa06, 90).
base_fare(aa07, 60).
base_fare(aa08, 60).
base_fare(aa09, 90).
base_fare(aa10, 90).
base_fare(aa11, 100).
base_fare(aa12, 100).
base_fare(aa15, 100).
base_fare(aa16, 100).
base_fare(aa17, 60).
base_fare(aa18, 60).
base_fare(aa19, 100).
base_fare(aa20, 100).
base_fare(aa21, 100).
base_fare(aa22, 100).
base_fare(aa23, 70).
base_fare(aa24, 70).
base_fare(aa25, 140).
base_fare(aa26, 140).
base_fare(aa27, 120).
base_fare(aa28, 120).
