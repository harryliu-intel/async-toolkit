GENERIC INTERFACE DblTable(Key, Value, KeyValueTbl, KeySet);
IMPORT Word;

(* Key, Value, and KeyValueTbl go together as in the old Table.ig *)

TYPE
  T = KeyValueTbl.Default OBJECT METHODS
    getKeys(READONLY v : Value.T; VAR s : KeySet.T) : BOOLEAN;
  END;

  Default <: T OBJECT METHODS
    valueEqual(READONLY v1, v2: Value.T) : BOOLEAN;
    valueHash(READONLY v : Value.T) : Word.T
  END;
  
CONST DefaultBrand = "DblTable (" & Key.Brand & "," & Value.Brand & ")";

END DblTable.
