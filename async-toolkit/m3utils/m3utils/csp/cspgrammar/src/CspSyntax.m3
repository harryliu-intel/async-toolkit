MODULE CspSyntax;
IMPORT SchemeObject;
IMPORT Debug;
IMPORT RTBrand;

PROCEDURE Lisp(of : T) : SchemeObject.T =
  VAR
    brand : TEXT;
  BEGIN
    TRY
      brand := RTBrand.Get(of)
    EXCEPT
      RTBrand.NotBranded => brand := "**NOT-BRANDED**"
    END;
    Debug.Out("CspSyntax.Lisp : " & brand);
    
    IF of = NIL THEN RETURN NIL ELSE RETURN of.lisp() END
  END Lisp;
  
BEGIN END CspSyntax.
