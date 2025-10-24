(* $Id$ *)

INTERFACE DatabaseClass;
FROM Database IMPORT Table, Vector, Matrix;
IMPORT DatabaseTable;

TYPE 
  PrivateTable = DatabaseTable.Public OBJECT
    fieldNames : Vector;
    data       : Matrix;
  END;

REVEAL Table <: PrivateTable;

END DatabaseClass.
