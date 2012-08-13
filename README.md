Data for MeyerPlanes project
============================

From the period before 1916 we have data on aeronautical clubs, patents, publications (articles), journals, exhibitions, letters, firms, and eventually individuals who have relations to these other things.
The data are intended to be open-sourced, and may open-sourced in early versions if anybody cares to.
We are not ready to call it a 1.0 version yet.  It will probably never be really finished, always improvable.

The data are in spreadsheets or Q&A format when being worked on to create them.
We put them in .csv format to use them in R.


Patents database
---------------------

Has these fields (variables):

### Year protection applied
This is the year the patent was approved, in most countries but can be the year it was filed.
I need to work this out ; probably it should be the year the patent was grantd.
Four digits, going back to the 1600s and with a few exceptional cases extending past 1916. I don't plan to graph those after 1916 but wish to keep them in the data.

### Where filed
Just four countries so far: br (for Britain), de (for Germany), fr (for France), and us (for the U.S.)

### Tech field
The technology field is generally a member of this set, or a couple of them separated by semicolons:

- LTA  -- lighter than air ; this includes any version of balloons or dirigibles. A dirigible is a balloon with an exoskeleton, whose rigidity helps it be directed.
- control -- something to do with control systems
- propulsion -- something to do with motors and engines
- propellers -- including water propellers, if thought to be relevant
- Kite -- rare, but kites are the ancestors of gliders
- Glider (unpowered)
- Airplane (powered glider)
- Helicopter (rare)
- Rocket (rare; includes anything ballistic)
- Parachute (rare)
- Instrument -- a device intended for the air, possibly relevant to airplanes, e.g. for measuring temperature, pressure, and windspeed
- DE77, DE77h, Group 1, Group 5, etc., 77h Class, Class 37 -- these are official classifications in the German patent system.  Their class 77 is sport equipment, and that's how they classified all aero stuff up to about 1900.  It's not a useful classification, though I want to keep it in the data since it is an actual real classification, but just toss it out for a technological analysis.


The technology field is not intended to carry much information about what the device (invention) is FOR but rather HOW it works, or WHAT is novel about it.

### Patent #
These are not necessarily unique ; patent numbers might be reused across years and countries.

### Global patent id
Can be filled in programmatically.  Will have a format like this example:
DE1914-295838
The first two identify the country where the patent was filed, the next four the year it was granted
(or filed, if never granted), then a hyphen and then the patent number in the original year and country in which it was filed.

### Sources and notes
Data on patents come from a bunch of eclectic sources, documented more fully in the second worksheet in this spreadsheet, not documented in the .csv file at all.

All fields, some yet to be documented: . . . . Patent #	more date info	Title/summary in English	Sources and notes  (see Sources worksheet for details)	Original language patent title (or closest available info)	Supplementary to	related to aircraft?	date applied for	date granted	Global patent id			


Publications database
---------------------

The data come from Brockett (1910) with some insertions of information from other sources.

Fields are:
1. Unique number for this entry 
2. Year
3. Language (1=French, 2=English, 3=German, 4=Italian ... and 10 others)
4. Authors
5. Key words describing the subject, separated by semicolons
6. Full text, with minor edits, of entry from Brockett (1910), including title



Clubs
-------
Not yet documented


Journals
----------
Not yet documented


Exhibitions and conferences
------------------------------
Not yet documented.


Letters between experimenters
------------------------------
Not yet documented 


Firms 
-------
Not yet documented


Individuals 
------------
Not yet documented; might be derived from the above.
