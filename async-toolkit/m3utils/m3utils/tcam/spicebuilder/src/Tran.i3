INTERFACE Tran;

TYPE 
  T = RECORD
    t,     (* start of transition  *)
    v,     (* target of transition *)
    rf     (* rise/fall time       *)
    : LONGREAL;
  END;

CONST Brand = "Tran";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Tran.
    
