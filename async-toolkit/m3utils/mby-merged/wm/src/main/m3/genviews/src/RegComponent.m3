MODULE RegComponent;
IMPORT ParseError;
IMPORT RdlProperty, RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueConstant;
IMPORT RdlPredefProperty;
IMPORT RdlPropertyRvalueKeyword;
IMPORT BigInt;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    getRdlTextProperty       := GetRdlTextProperty;
    getRdlPredefTextProperty := GetRdlPredefTextProperty;
    getRdlIntProperty        := GetRdlIntProperty;
    getRdlPredefIntProperty  := GetRdlPredefIntProperty;
    getRdlKwProperty         := GetRdlKwProperty;
    getRdlPredefKwProperty   := GetRdlPredefKwProperty;
  END;

PROCEDURE GetRdlPredefTextProperty(t    : T;
                                   prop : RdlPredefProperty.T;
                                   VAR res : TEXT) : BOOLEAN
  RAISES { ParseError.E } =
  BEGIN
    RETURN GetRdlTextProperty(t, RdlPredefProperty.Names[prop], res)
  END GetRdlPredefTextProperty;
  
PROCEDURE GetRdlTextProperty(t      : T;
                             propNm : TEXT;
                             VAR res : TEXT) : BOOLEAN
  RAISES { ParseError.E } =
  BEGIN
    WITH prop = RdlProperty.Make(propNm),
         v    = t.props.lookup(prop) DO
      IF v =  NIL THEN
        RETURN FALSE
      ELSE
        TYPECASE v.rhs OF
          RdlPropertyAssignRhs.Const(const) =>
          TYPECASE const.const OF
            RdlPropertyRvalueConstant.Str(str) =>
            res := str.str;
            RETURN TRUE
          ELSE
            RAISE ParseError.E ("Wrong type of RHS of property assign : " &
                  propNm)
          END
        ELSE
          RAISE ParseError.E ("Wrong kind of RHS of property assign : " &
                propNm)
        END
      END(*IF*)
    END(*WITH*)
  END GetRdlTextProperty;

PROCEDURE GetRdlPredefIntProperty(t    : T;
                                   prop : RdlPredefProperty.T;
                                   VAR res : INTEGER) : BOOLEAN
  RAISES { ParseError.E } =
  BEGIN
    RETURN GetRdlIntProperty(t, RdlPredefProperty.Names[prop], res)
  END GetRdlPredefIntProperty;
  
PROCEDURE GetRdlIntProperty(t      : T;
                             propNm : TEXT;
                             VAR res : INTEGER) : BOOLEAN
  RAISES { ParseError.E } =
  BEGIN
    WITH prop = RdlProperty.Make(propNm),
         v    = t.props.lookup(prop) DO
      IF v =  NIL THEN
        RETURN FALSE
      ELSE
        TYPECASE v.rhs OF
          RdlPropertyAssignRhs.Const(const) =>
          TYPECASE const.const OF
            RdlPropertyRvalueConstant.Num(num) =>
            res := BigInt.ToInteger(num.num.x);
            RETURN TRUE
          ELSE
            RAISE ParseError.E ("Wrong type of RHS of property assign : " &
                  propNm)
          END
        ELSE
          RAISE ParseError.E ("Wrong kind of RHS of property assign : " &
                propNm)
        END
      END(*IF*)
    END(*WITH*)
  END GetRdlIntProperty;

PROCEDURE GetRdlPredefKwProperty(t    : T;
                                   prop : RdlPredefProperty.T;
                                   VAR res : RdlPropertyRvalueKeyword.T) : BOOLEAN
  RAISES { ParseError.E } =
  BEGIN
    RETURN GetRdlKwProperty(t, RdlPredefProperty.Names[prop], res)
  END GetRdlPredefKwProperty;
  
PROCEDURE GetRdlKwProperty(t      : T;
                             propNm : TEXT;
                             VAR res : RdlPropertyRvalueKeyword.T) : BOOLEAN
  RAISES { ParseError.E } =
  BEGIN
    WITH prop = RdlProperty.Make(propNm),
         v    = t.props.lookup(prop) DO
      IF v =  NIL THEN
        RETURN FALSE
      ELSE
        TYPECASE v.rhs OF
          RdlPropertyAssignRhs.Const(const) =>
          TYPECASE const.const OF
            RdlPropertyRvalueConstant.Keyword(kw) =>
            res := kw.kw;
            RETURN TRUE
          ELSE
            RAISE ParseError.E ("Wrong type of RHS of property assign : " &
                  propNm)
          END
        ELSE
          RAISE ParseError.E ("Wrong kind of RHS of property assign : " &
                propNm)
        END
      END(*IF*)
    END(*WITH*)
  END GetRdlKwProperty;
  
BEGIN END RegComponent.
