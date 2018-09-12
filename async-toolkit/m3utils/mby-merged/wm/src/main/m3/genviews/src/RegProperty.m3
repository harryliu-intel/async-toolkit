MODULE RegProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueKeyword;
IMPORT RdlPropertyRvalueConstant;
IMPORT BigInt;

PROCEDURE GetKw(q : RdlPropertyAssignRhs.Const) : RdlPropertyRvalueKeyword.T =
  BEGIN
    TYPECASE q.const OF
      RdlPropertyRvalueConstant.Keyword(kw) => RETURN kw.kw
    ELSE
      <*ASSERT FALSE*>
    END
  END GetKw;

PROCEDURE GetNumeric(q : RdlPropertyAssignRhs.Const) : INTEGER =
  BEGIN
    TYPECASE q.const OF
      RdlPropertyRvalueConstant.Num(n) =>
      RETURN BigInt.ToInteger(n.num.x)
    ELSE
      <*ASSERT FALSE*>
    END
  END GetNumeric;

BEGIN END RegProperty.
