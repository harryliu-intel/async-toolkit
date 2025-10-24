MODULE GenCUtils;
IMPORT TextSeq, RdlArray, RegCGenState;
FROM Fmt IMPORT F;
IMPORT BigInt;
IMPORT RegComponent, RegGenState, RegAddrmap;

PROCEDURE FmtConstant(xDecls : TextSeq.T; val : TEXT; nm, sfx : TEXT) =
  BEGIN
    xDecls.addhi(F("static const unsigned int %s__%s = %s;", nm, sfx, val));
    xDecls.addhi(F("#define %s__%sd    %s", nm, sfx, val))
  END FmtConstant;
  
PROCEDURE FmtArrSz(xDecls : TextSeq.T; a : RdlArray.Single; nm : TEXT) =
  BEGIN
    IF a = NIL THEN
      RETURN
    ELSE
      FmtConstant(xDecls, BigInt.Format(a.n.x), nm, "n")
    END
  END FmtArrSz;

PROCEDURE PutXDecls(gs : RegCGenState.T; xDecls : TextSeq.T) =
  BEGIN
    FOR i := 0 TO xDecls.size()-1 DO
      gs.main(xDecls.get(i)); gs.main("\n")
    END;
    gs.main("\n")
  END PutXDecls;

PROCEDURE FmtFieldType(baseType : TEXT; ft : FieldType) : TEXT =
  BEGIN
    CASE ft OF
      FieldType.UInt    => RETURN baseType
    |
      FieldType.Pointer => RETURN baseType
    |
      FieldType.Id      => RETURN "field_id"
    END
  END FmtFieldType;

PROCEDURE FmtFieldModifier(ft : FieldType) : TEXT =
  CONST
    Modifier = ARRAY FieldType OF TEXT { "", "*", "" };
  BEGIN
    RETURN Modifier[ft]
  END FmtFieldModifier;
  
PROCEDURE ArrSz(a : RdlArray.Single) : CARDINAL =
  BEGIN
    IF a = NIL THEN
      RETURN 1
    ELSE
      RETURN BigInt.ToInteger(a.n.x)
    END
  END ArrSz;
  
PROCEDURE ComponentTypeName(c : RegComponent.T; gs : RegGenState.T) : TEXT =
  BEGIN
    TYPECASE c OF
      RegAddrmap.T(a) =>
      RETURN a.intfName(gs) 
    ELSE
      RETURN c.typeName(gs)
    END
  END ComponentTypeName;

BEGIN END GenCUtils.
