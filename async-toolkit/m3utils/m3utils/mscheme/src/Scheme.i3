(* $Id$ *)

INTERFACE Scheme;
IMPORT SchemeEnvironmentSuper, SchemeObject;
IMPORT SchemeSymbol;
IMPORT SchemeString, SchemeVector;
IMPORT Pathname;
IMPORT Rd, OSError, Wr;

EXCEPTION E(TEXT);

TYPE 
  (* aliases for basic Scheme types *)
  Object    = SchemeObject.T;
  Symbol    = SchemeSymbol.T;
  String    = SchemeString.T;
  Vector    = SchemeVector.T;

  (* a Scheme interpreter *)
  T <: Public;

  Public = OBJECT METHODS
    defineInGlobalEnv(var, val : Object);

    init(READONLY files : ARRAY OF Pathname.T) : T RAISES { E, OSError.E };

    readEvalWriteLoop() RAISES { Wr.Failure };

    loadFile(fn : Object) : Object RAISES { E };

    loadRd(rd : Rd.T) : Object RAISES { E } ;

    loadPort(port : Object (* must be SchemeInputPort *)) : Object RAISES { E };

    eval(x : REFANY; env : SchemeEnvironmentSuper.T) : Object RAISES { E };

    evalInGlobalEnv(x : Object) : Object RAISES { E };

    evalList(list : Object; env : SchemeEnvironmentSuper.T) : Object RAISES { E };
    (* always a SchemePair *)

  END;

CONST Brand = "Scheme";

END Scheme.
