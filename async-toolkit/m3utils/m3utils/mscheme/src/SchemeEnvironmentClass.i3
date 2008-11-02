(* $Id$ *)

INTERFACE SchemeEnvironmentClass;

IMPORT SchemeEnvironment;

FROM Scheme IMPORT Symbol, Object;

(* reveal basic put and get methods of SchemeEnvironment.T *)

REVEAL SchemeEnvironment.T <: Private;

TYPE
  Private = SchemeEnvironment.Public OBJECT METHODS
    put(var : Symbol; READONLY val : Object);
    get(var : Symbol; VAR val : Object) : BOOLEAN;
  END;

END SchemeEnvironmentClass.
