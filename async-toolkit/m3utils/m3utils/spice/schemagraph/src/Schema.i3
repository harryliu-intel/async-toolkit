INTERFACE Schema;
IMPORT FDataSeq;

TYPE
  T = RECORD 
    nfields : CARDINAL;           (* total # of fields *)
    dfields : CARDINAL;           (* expected # of data fields *)
    fdata   : FDataSeq.T;         (* specific info on a field *)
  END;

CONST Brand = "Schema";

END Schema.
