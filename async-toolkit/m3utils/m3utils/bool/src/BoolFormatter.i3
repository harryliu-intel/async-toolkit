(* $Id: BoolFormatter.i3,v 1.1 2001/11/22 01:53:32 mika Exp $ *)

INTERFACE BoolFormatter;
IMPORT Bool;

TYPE 
  T = OBJECT METHODS
    fmt(b : Bool.T) : TEXT
  END;

CONST Brand = "BoolFormatter";

END BoolFormatter.

