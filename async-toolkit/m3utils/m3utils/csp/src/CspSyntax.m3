MODULE CspSyntax;
IMPORT SchemeObject;

PROCEDURE Lisp(of : T) : SchemeObject.T =
  BEGIN
    IF of = NIL THEN RETURN NIL ELSE RETURN of.lisp() END
  END Lisp;
  
BEGIN END CspSyntax.
