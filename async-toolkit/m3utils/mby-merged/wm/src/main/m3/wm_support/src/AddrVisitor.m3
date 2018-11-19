MODULE AddrVisitor;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    internal := DefInternal;
  END;

  Internal = BRANDED Brand & " Internal" OBJECT END;

PROCEDURE DefInternal(<*UNUSED*>t : T;
                      <*UNUSED*>name, typeName : TEXT;
                      <*UNUSED*>type : Type;
                      <*UNUSED*>array : Array;
                      <*UNUSED*>parent : Internal) : Internal =
  BEGIN RETURN NEW(Internal) END DefInternal;

BEGIN END AddrVisitor.
