INTERFACE CspBoolean;

TYPE T = BOOLEAN;

CONST Brand = "CspBoolean";

PROCEDURE ToInteger(t : T) : INTEGER;
  (* return -1 for TRUE, 0 for FALSE *)
      
END CspBoolean.
  
