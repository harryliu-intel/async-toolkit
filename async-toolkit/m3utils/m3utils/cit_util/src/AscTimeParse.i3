(* $Id: AscTimeParse.i3,v 1.1 2009/05/05 08:07:02 mika Exp $ *)

INTERFACE AscTimeParse;
IMPORT TZ, Time;

(* 
   parse the 24-character format 

       Thu Nov 24 18:22:48 1986
*)

PROCEDURE Parse(tz : TZ.T; 
                ascTime : TEXT; 
                ripOutTZ := FALSE) : Time.T RAISES { IllegalFormat };

EXCEPTION IllegalFormat;

END AscTimeParse.
