INTERFACE RegComponent;
IMPORT RegGenState;
IMPORT OSError, Thread, Wr;
IMPORT RdlPropertySymtab;

TYPE
  T = BRANDED Brand OBJECT
    nm    : TEXT;
    path  : TEXT;
    props : RdlPropertySymtab.T;
  METHODS
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
