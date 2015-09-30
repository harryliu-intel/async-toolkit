(* $Id: SchemeJailBreak.i3,v 1.1 2008/10/22 05:08:22 mika Exp $ *)

INTERFACE SchemeJailBreak;
IMPORT SchemeObject;

TYPE
  Object = SchemeObject.T;

  T = OBJECT METHODS
    apply(args : Object) : Object;
  END;

CONST Brand = "SchemeJailBreak";

END SchemeJailBreak.
