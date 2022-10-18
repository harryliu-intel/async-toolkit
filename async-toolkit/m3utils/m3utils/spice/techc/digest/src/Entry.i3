INTERFACE Entry;

TYPE
  CsvCols = { Tech, Corn, Tran, Topo, Mode, Simu, Volt, Temp, Cycl, Curr };
  Entry = ARRAY CsvCols OF TEXT;
  T = REF Entry;

CONST Brand = "Entry";

PROCEDURE Compare(a, b : T) : [ -1 .. 1 ];
  (* sort by Volt *)

CONST
  CsvColNames = ARRAY CsvCols OF TEXT 
  { "Tech", "Corn", "Tran", "Topo", "Mode",
    "Simu", "Volt", "Temp", "Cycl", "Curr" };
  
END Entry.
