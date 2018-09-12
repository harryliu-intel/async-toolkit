INTERFACE RegProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueKeyword;

PROCEDURE GetKw(q : RdlPropertyAssignRhs.Const) : RdlPropertyRvalueKeyword.T;

PROCEDURE GetNumeric(q : RdlPropertyAssignRhs.Const) : INTEGER;

CONST Brand = "RegProperty";
      
END RegProperty.
