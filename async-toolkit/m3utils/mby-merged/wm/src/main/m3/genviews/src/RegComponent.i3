INTERFACE RegComponent;
IMPORT RegGenState;
IMPORT OSError, Thread, Wr;
IMPORT RdlPropertySymtab;
IMPORT ParseError;
IMPORT RdlPredefProperty;

TYPE
  T <: Public;

  Public = OBJECT
    nm    : TEXT;
    path  : TEXT;
    props : RdlPropertySymtab.T;
  METHODS
    (*******  T defines these  *******)

    getRdlTextProperty(propNm : TEXT) : TEXT RAISES { ParseError.E };
    (* NIL if not def'd *)

    getRdlPredefProperty(prop : RdlPredefProperty.T) : TEXT
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

END RegComponent.
