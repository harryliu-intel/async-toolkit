(* $Id$ *)

INTERFACE SchemeUtils;
FROM Scheme IMPORT String, Object, Symbol, Vector, Pair, E;
IMPORT Scheme, SchemeInputPort;
IMPORT Wr, Wx;

(* in JScheme, SchemeUtils is an abstract class but it has no non-static
   methods... see Peter Norvig's comments on this, about "unqualifying"
   names.  It's just a Java hack that's not relevant for Modula-3. *)

PROCEDURE Str(x : Object) : String;

PROCEDURE Sym(x : Object) : Symbol;

PROCEDURE Vec(x : Object) : Vector;

PROCEDURE InPort(x : Object; interp : Scheme.T) : SchemeInputPort.T;

(* PROCEDURE outPort(x : SchemeObject; interp : Scheme.T) : ??? *)

PROCEDURE Error(message : TEXT) : Object RAISES { E };

PROCEDURE Warn(message : TEXT) : Object;

PROCEDURE First(x : Object) : Object;
PROCEDURE Rest(x : Object) : Object;
PROCEDURE Second(x : Object) : Object;
PROCEDURE Third(x : Object) : Object;
PROCEDURE PedanticFirst(x : Object) : Object;
PROCEDURE PedanticRest(x : Object) : Object;

PROCEDURE SetFirst(x, y : Object) : Object;

PROCEDURE SetRest(x, y : Object) : Object;

PROCEDURE List1(x : Object) : Pair;
PROCEDURE List2(x, y : Object) : Pair;
PROCEDURE ListStar(x : Object) : Object;

PROCEDURE Cons(a, b : Object) : Pair;

PROCEDURE Reverse(x : Object) : Object;

PROCEDURE Equal(x, y : Object) : BOOLEAN;

PROCEDURE Eqv(x, y : Object) : BOOLEAN;

PROCEDURE Length(x : Object) : CARDINAL;

PROCEDURE ListToString(chars: Object) : String;

PROCEDURE ListToVector(objs : Object) : Vector;

PROCEDURE Write(x : Object; port : Wr.T; quoted : BOOLEAN) : Object;

PROCEDURE VectorToList(x : Object) : Pair;

PROCEDURE P(msg : TEXT; x : Object) : Object; (* for debugging *)

PROCEDURE Stringify(x : Object) : TEXT;
PROCEDURE StringifyQ(x : Object; quoted : BOOLEAN) : TEXT;
PROCEDURE StringifyB(x : Object; quoted : BOOLEAN; buf : Wx.T);

PROCEDURE DebugFormat(x : Object) : TEXT;
  (* for debugging, something not really needed in the Java version since
     everything has a .toString there *)

END SchemeUtils.
