INTERFACE RegProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueKeyword;
IMPORT ParseError;

PROCEDURE GetKw(q : RdlPropertyAssignRhs.Const) : RdlPropertyRvalueKeyword.T;

PROCEDURE GetNumeric(q : RdlPropertyAssignRhs.Const) : INTEGER;

PROCEDURE Unquote(str : TEXT) : TEXT RAISES { ParseError.E };

CONST Brand = "RegProperty";
      
END RegProperty.
