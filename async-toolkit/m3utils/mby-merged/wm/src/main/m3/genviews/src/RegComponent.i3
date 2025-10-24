INTERFACE RegComponent;
IMPORT RegGenState;
IMPORT OSError, Thread, Wr;
IMPORT RdlPropertySymtab;
IMPORT ParseError;
IMPORT RdlPredefProperty;
IMPORT RdlPropertyRvalueKeyword;
IMPORT Word, Refany;

TYPE
  T <: Public;

  Public = OBJECT
    nm    : TEXT;
    path  : TEXT;
    props : RdlPropertySymtab.T;
  METHODS
    (*******  T defines these  *******)

    getRdlTextProperty(propNm : TEXT; VAR r : TEXT) : BOOLEAN
      RAISES { ParseError.E };

    getRdlPredefTextProperty(prop : RdlPredefProperty.T; VAR r : TEXT) : BOOLEAN
      RAISES { ParseError.E };
    
    getRdlIntProperty(propNm : TEXT; VAR r : INTEGER) : BOOLEAN
      RAISES { ParseError.E };

    getRdlPredefIntProperty(prop : RdlPredefProperty.T; VAR r : INTEGER) : BOOLEAN
      RAISES { ParseError.E };
    
    getRdlKwProperty(propNm : TEXT; VAR r :  RdlPropertyRvalueKeyword.T) : BOOLEAN
      RAISES { ParseError.E };

    getRdlPredefKwProperty(prop : RdlPredefProperty.T; VAR r :  RdlPropertyRvalueKeyword.T) : BOOLEAN
      RAISES { ParseError.E };
    
    (*******  abstract methods below -- override  *******)
    
    typeName(state : RegGenState.T) : TEXT;
    (* in cases it makes sense to declare a type *)
    
    intfName(state : RegGenState.T) : TEXT;
    (* in cases it makes sense to declare an interface *)
    
    generate(state : RegGenState.T)
      RAISES { OSError.E, Thread.Alerted, Wr.Failure};
    (* do code generation *)
  END;

CONST Brand = "RegComponent";

CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;
  
END RegComponent.
