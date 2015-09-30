MODULE NumberedObject;
IMPORT Integer;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN RETURN Integer.Compare(a.num, b.num) END Compare;

BEGIN END NumberedObject.
