INTERFACE Scenario;
IMPORT Name;

TYPE   
  T = OBJECT             (* complete scenario    *)
    mutex : B := NIL;    (* transitions to block *)
    expr  : X;           (* expression to await, NIL evals to FALSE  *)
    next  : T;           (* next scenario        *)
  END;

  V = X OBJECT           (* value to await  *)
    node  : Name.T;
    to    : BOOLEAN;
  END;

  B = V OBJECT           (* transition to block  *)
    next : B := NIL;
  END;

  X = BRANDED OBJECT END;      (* expression *)
  
  (* branding necessary because else And and Or would be the same type *)
  Or  = X BRANDED OBJECT x0, x1 : X END;

  And = X BRANDED OBJECT x0, x1 : X END;

PROCEDURE Forever() : T; (* cycle forever / till deadlock *)

PROCEDURE Format(r : REFANY (* any object of type T or X *)) : TEXT;
  
END Scenario.
