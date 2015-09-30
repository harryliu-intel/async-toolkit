MODULE BDDSetHash;
IMPORT BDDSetDef;
IMPORT BDD;
IMPORT Word;

REVEAL 
  T = BDDSetDef.T BRANDED Brand OBJECT
    hashV : Word.T;
  OVERRIDES
    insert := Insert;
    delete := AssertFalseDelete;
  END;

PROCEDURE Insert(t : T; v : BDD.T) : BOOLEAN =
  BEGIN
    WITH res = BDDSetDef.T.insert(t, v) DO
      IF NOT res THEN 
        t.hashV := Word.Plus(t.hashV, BDD.Hash(v))
      END;
      RETURN res
    END
  END Insert;

PROCEDURE Hash(t : T) : Word.T = BEGIN RETURN t.hashV END Hash;

<*NOWARN*>PROCEDURE AssertFalseDelete(t : T; v : BDD.T) : BOOLEAN =
  BEGIN
    <*ASSERT FALSE*>
  END AssertFalseDelete;

BEGIN END BDDSetHash.
    

