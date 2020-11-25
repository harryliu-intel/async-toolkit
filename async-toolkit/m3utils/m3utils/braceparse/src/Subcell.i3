INTERFACE Subcell;
IMPORT Atom;

TYPE
  T = RECORD
    type     : Atom.T;

    instance : InstanceName;
    (* representation: 
       if   ARRAY[7] = 0 then a C-style string
       else a little-endian byte index into an external list of names,
       zero-separated
    *)
  END;

  InstanceName = ARRAY [0..7] OF CHAR;

  LongNames <: ROOT;

PROCEDURE NewLongNames() : LongNames;
  
PROCEDURE EncodeName(longNames     : LongNames;
                     READONLY name : ARRAY OF CHAR;
                     VAR      tgt  : InstanceName);

PROCEDURE DecodeName(longNames     : LongNames;
                     READONLY inst : InstanceName;
                     VAR buffer : ARRAY OF CHAR  (* zero terminated *)
  );

CONST Brand = "Subcell";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

END Subcell.
