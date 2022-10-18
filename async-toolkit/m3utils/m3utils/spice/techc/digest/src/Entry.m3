MODULE Entry;
IMPORT LongrealType;
IMPORT Scan;

PROCEDURE Compare(a, b : T) : [0..1] =
  BEGIN
    RETURN LongrealType.Compare(Scan.LongReal(a[CsvCols.Volt]),
                                Scan.LongReal(b[CsvCols.Volt]))
  END Compare;

BEGIN END Entry.
