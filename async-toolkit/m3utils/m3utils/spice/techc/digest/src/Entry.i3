INTERFACE Entry;

TYPE
  CsvCols = { Tech, Corn, Tran, Topo, Mode, Simu, Volt, Temp, Cycl, Curr };
  Entry = ARRAY CsvCols OF TEXT;
  T = REF Entry;

CONST Brand = "Entry";

PROCEDURE Compare(a, b : T) : [0..1];
  (* sort by Volt *)

END Entry.
