MODULE RegComponent;
IMPORT ParseError;
IMPORT RdlProperty, RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueConstant;
IMPORT RdlPredefProperty;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    getRdlTextProperty := GetRdlTextProperty;
    getRdlPredefProperty := GetRdlPredefProperty;
  END;

PROCEDURE GetRdlPredefProperty(t    : T;
                               prop : RdlPredefProperty.T) : TEXT
  RAISES { ParseError.E } =
  BEGIN
    RETURN GetRdlTextProperty(t, RdlPredefProperty.Names[prop])
  END GetRdlPredefProperty;

PROCEDURE GetRdlTextProperty(t      : T;
                             propNm : TEXT) : TEXT
  RAISES { ParseError.E } =
  BEGIN
    WITH prop = RdlProperty.Make(propNm),
         v    = t.props.lookup(prop) DO
      IF v =  NIL THEN
        RETURN NIL
      ELSE
        TYPECASE v.rhs OF
          RdlPropertyAssignRhs.Const(const) =>
          TYPECASE const.const OF
            RdlPropertyRvalueConstant.Str(str) =>
            RETURN str.str
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

BEGIN END RegComponent.
