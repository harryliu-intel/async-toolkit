(* $Id$ *)

INTERFACE Scheme;
IMPORT SchemeInputPort, SchemeEnvironment, SchemePair;
IMPORT Pathname;
IMPORT Rd;

EXCEPTION E(TEXT);

TYPE 
  (* aliases for basic Scheme types *)
  Object   = REFANY;
  Boolean  = REF BOOLEAN
  LongReal = REF LONGREAL;
  Char     = REF CHAR;
  Symbol   = Atom.T;
  String   = REF ARRAY OF CHAR;
  Vector   = REF ARRAY OF Object;
  Pair     = SchemePair.T;

  (* a Scheme interpreter *)
  T <: Public;

  Public = OBJECT METHODS
    defineInGlobalEnv(var, val : Object);

    init(READONLY files : ARRAY OF Pathname.T) : T;

    readEvalWriteLoop();

    loadFile(fn : Pathname.T) : Object;

    loadRd(rd : Rd.T) : Object;

    loadPort(port : SchemeInputPort.T) : Object;

    eval(x : REFANY; env : SchemeEnvironment.T) : Object;

    evalInGlobalEnv(x : Object) : Object;

    evalList(list : Object; env : SchemeEnvironment.T) : SchemePair.T;

  END;

CONST Brand = "Scheme";

END Scheme.
