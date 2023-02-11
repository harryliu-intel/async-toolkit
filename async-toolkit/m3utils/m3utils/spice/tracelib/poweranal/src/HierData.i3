INTERFACE HierData;

TYPE
  T = RECORD
    instName : TEXT;
    typeName : TEXT;    (* type name of cell *)
    totalize : BOOLEAN; (* totalize or not when calc'ing energy *)
  END;

CONST Brand = "HierData";

END HierData.
