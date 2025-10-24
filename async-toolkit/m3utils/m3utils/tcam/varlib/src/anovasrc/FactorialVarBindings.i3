(* $Id: FactorialVarBindings.i3,v 1.2 2006/02/23 23:47:53 mika Exp $ *)

INTERFACE FactorialVarBindings;
IMPORT Refany, TextReader, RefList;
IMPORT FloatMode, Lex;

TYPE
  T = OBJECT varName : TEXT END;
  BoolBinding = T OBJECT val : REF ARRAY OF BOOLEAN END;
  IntBinding = T OBJECT val : REF ARRAY OF INTEGER END;
  LRBinding = T OBJECT val : REF ARRAY OF LONGREAL END;
  TextBinding = T OBJECT val : REF ARRAY OF TEXT END;


CONST Equal = Refany.Equal;
CONST Brand = "FactorialVarBindings";

(* in each, bindings is a list to objects of type T *)

PROCEDURE AddTextBinding(arg : TEXT; VAR bindings : RefList.T) 
  RAISES { TextReader.NoMore };

PROCEDURE AddIntBinding(arg : TEXT; VAR bindings : RefList.T) 
  RAISES { TextReader.NoMore, Lex.Error, FloatMode.Trap };

PROCEDURE AddRealBinding(arg : TEXT; VAR bindings : RefList.T) 
  RAISES { TextReader.NoMore, Lex.Error, FloatMode.Trap };

PROCEDURE AddBoolBinding(arg : TEXT; VAR bindings : RefList.T) 
  RAISES { TextReader.NoMore, Lex.Error };

END FactorialVarBindings.
