INTERFACE PDATransTally;
IMPORT PDATrans;
CONST
  Brand = "PDATransTally";
TYPE
  T = RECORD
    key: INTEGER;
    tr: PDATrans.T;
  END;
PROCEDURE Compare(a, b: T): [-1..1];
END PDATransTally.
