MODULE PDATransTally;
IMPORT Integer;
IMPORT PDATrans;
PROCEDURE Compare(a, b: T): [-1..1] =
  VAR
    result := Integer.Compare(a.key, b.key);
  BEGIN
    IF result # 0 THEN RETURN result; END;
    RETURN PDATrans.Compare(a.tr, b.tr);
  END Compare;
BEGIN
END PDATransTally.
