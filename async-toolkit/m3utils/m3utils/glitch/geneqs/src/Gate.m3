MODULE Gate;
IMPORT GateExpr;
IMPORT GateList;
IMPORT TextGateExprTbl;
FROM Fmt IMPORT F;

VAR last : T;
    
PROCEDURE New(named : TEXT; expr : GateExpr.T) : T =
  BEGIN
    WITH new = T { named, expr } DO
      gates := GateList.Cons(new, gates);
      last := new;
      RETURN new
    END
  END New;

PROCEDURE Last() : T = BEGIN RETURN last END Last;

PROCEDURE GetLiteral(named : TEXT) : GateExpr.T =
  VAR
    res : GateExpr.T;
  BEGIN
    IF NOT literals.get(named, res) THEN
      res := GateExpr.New(named);
      EVAL literals.put(named, res)
    END;
    RETURN res
  END GetLiteral;

VAR
  gates : GateList.T := NIL;
  literals           := NEW(TextGateExprTbl.Default).init();

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s <- %s", a.tgt, GateExpr.Format(a.expr))
  END Format;
    
BEGIN END Gate.
