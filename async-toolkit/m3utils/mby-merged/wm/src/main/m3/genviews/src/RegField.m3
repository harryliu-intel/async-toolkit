MODULE RegField;
IMPORT Integer;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN RETURN Integer.Compare(a.lsb,b.lsb) END Compare;

BEGIN END RegField.
