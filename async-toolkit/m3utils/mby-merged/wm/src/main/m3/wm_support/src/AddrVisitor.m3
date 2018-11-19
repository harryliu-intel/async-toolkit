MODULE AddrVisitor;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    internal := DefaultInternal;
  END;

  Internal = BRANDED Brand & " Internal" OBJECT END;

PROCEDURE DefaultInternal(<*UNUSED*>t : T;
                      name, typeName : TEXT;
                      type : Type;
                      array : Array;
                      parent : Internal) : Internal =
  BEGIN
    RETURN NEW(DefInternal,
               name := name, typeName := typeName,
               type := type,
               array := array,
               parent := parent
               )
  END DefaultInternal;

BEGIN END AddrVisitor.
