(* $Id$ *)

UNSAFE MODULE RTBrandCM3 EXPORTS RTBrand;

IMPORT RTType, Text, RT0;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } = 
  BEGIN 
    RETURN GetByTC(TYPECODE(x));
  END Get;


PROCEDURE GetByTC(c : RT0.Typecode;
                  nameIfNotBranded := FALSE) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(c).brand_ptr;
  BEGIN 
    IF LOOPHOLE(b,INTEGER) = 0 OR b.length = 0 THEN RAISE NotBranded END;
    RETURN Text.FromChars(SUBARRAY(b.chars, 0, b.length));
  END GetByTC;

BEGIN END RTBrandCM3.
