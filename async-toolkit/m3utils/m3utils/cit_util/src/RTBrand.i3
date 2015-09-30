(* $Id: RTBrand.i3,v 1.4 2009/04/03 07:47:59 mika Exp $ *)
INTERFACE RTBrand;
IMPORT RT0;

EXCEPTION NotBranded;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } ;
PROCEDURE GetByTC(tc : RT0.Typecode;
                  nameIfNotBranded := FALSE) : TEXT RAISES { NotBranded } ;
PROCEDURE GetName(c : RT0.Typecode) : TEXT RAISES { NotBranded } ;

END RTBrand.
