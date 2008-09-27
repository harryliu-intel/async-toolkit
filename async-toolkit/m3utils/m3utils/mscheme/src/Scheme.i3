(* $Id$ *)

INTERFACE Scheme;
IMPORT SchemeInputPort, SchemeEnvironment, SchemePair, SchemeObject;
IMPORT SchemeBoolean, SchemeLongReal, SchemeChar, SchemeSymbol;
IMPORT SchemeString, SchemeVector;
IMPORT Pathname;
IMPORT Rd;

EXCEPTION E(TEXT);

TYPE 
  (* aliases for basic Scheme types *)
  Object    = SchemeObject.T;
  Boolean   = SchemeBoolean.T;
  LongReal  = SchemeLongReal.T;
  Character = SchemeChar.T;
  Symbol    = SchemeSymbol.T;
  String    = SchemeString.T;
  Vector    = SchemeVector.T;
  Pair      = SchemePair.T;

  (* a Scheme interpreter *)
  T <: Public;

  Public = OBJECT METHODS
    defineInGlobalEnv(var, val : Object);

    init(READONLY files : ARRAY OF Pathname.T) : T;

    readEvalWriteLoop();

    loadFile(fn : Object) : Object;

    loadRd(rd : Rd.T) : Object;

    loadPort(port : SchemeInputPort.T) : Object;

    eval(x : REFANY; env : SchemeEnvironment.T) : Object;

    evalInGlobalEnv(x : Object) : Object;

    evalList(list : Object; env : SchemeEnvironment.T) : SchemePair.T;

  END;

CONST Brand = "Scheme";

END Scheme.
