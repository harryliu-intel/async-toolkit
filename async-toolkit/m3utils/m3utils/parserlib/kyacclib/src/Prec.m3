MODULE Prec;
IMPORT Fmt;
PROCEDURE Format(a: T): TEXT =
  CONST
    k = ARRAY Kind OF TEXT{"Left","Right","None"};
  BEGIN
    RETURN k[a.kind] & Fmt.Int(a.val);
  END Format;
BEGIN
END Prec.
