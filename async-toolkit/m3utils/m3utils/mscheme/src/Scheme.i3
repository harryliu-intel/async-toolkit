(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE Scheme;
IMPORT SchemeEnvironmentSuper, SchemeObject;
IMPORT SchemeSymbol;
IMPORT SchemeString, SchemeVector;
IMPORT Pathname;
IMPORT Rd, OSError, Wr;
IMPORT SchemeJailBreak;

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

    eval(x : REFANY; env : SchemeEnvironmentSuper.T; isTailCall := FALSE) : Object RAISES { E };

    evalInGlobalEnv(x : Object) : Object RAISES { E };

    evalList(list : Object; env : SchemeEnvironmentSuper.T) : Object RAISES { E };
    (* always a SchemePair *)
     
    setJailBreak(jb : SchemeJailBreak.T);


    bind(var : Symbol; val : Object);
    (* bind some object to symbol in var from Modula-3 *)

  END;

CONST Brand = "Scheme";

END Scheme.
