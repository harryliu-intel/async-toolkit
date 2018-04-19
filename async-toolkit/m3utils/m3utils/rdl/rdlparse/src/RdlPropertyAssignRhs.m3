MODULE RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueConstant;
IMPORT RdlPropertyRvalueKeyword;
IMPORT BigInt;
IMPORT Fmt;
FROM Fmt IMPORT F;

PROCEDURE Format(t : T) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    TYPECASE t OF
      Const(const) =>
      res := "Const:";
      TYPECASE const.const OF
        RdlPropertyRvalueConstant.Num(n) =>
        RETURN res & Fmt.Int(BigInt.ToInteger(n.num.x))
      |
        RdlPropertyRvalueConstant.Str(s) =>
        RETURN res & F("\"%s\"",s.str)
      |
        RdlPropertyRvalueConstant.Keyword(k) =>
        RETURN res & RdlPropertyRvalueKeyword.Names[k.kw]
      ELSE
        <*ASSERT FALSE*>
      END
    |
      Enum(enum) => RETURN "RdlEnumEntryList.T"
    |
      Iref(iref) => RETURN "RdlInstanceRef.T"
    |
      Concat(concat) => RETURN "RdlConcatElemList.T"
    ELSE
      <*ASSERT FALSE*>
    END
  END Format;
  
BEGIN END RdlPropertyAssignRhs.
