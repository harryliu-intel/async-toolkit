MODULE Entry;
IMPORT LongrealType;
IMPORT Scan;

PROCEDURE Compare(a, b : T) : [ -1 .. 1 ] =
  BEGIN
    RETURN LongrealType.Compare(Scan.LongReal(a[CsvCols.Volt]),
                                Scan.LongReal(b[CsvCols.Volt]))
  END Compare;

PROCEDURE CompareLR(a, b : T; col : CsvCols) : [ -1 .. 1 ] =
  BEGIN
    RETURN LongrealType.Compare(Scan.LongReal(a[col]),
                                Scan.LongReal(b[col]))
  END CompareLR;

BEGIN END Entry.
