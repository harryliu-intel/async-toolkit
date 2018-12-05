MODULE FieldData;

PROCEDURE ArrayGet(a : AP; idx : CARDINAL) : T =
  BEGIN RETURN a[idx] END ArrayGet;
  
PROCEDURE ArraySize(a : AP) : CARDINAL =
  BEGIN RETURN NUMBER(a^) END ArraySize;

BEGIN END FieldData.
