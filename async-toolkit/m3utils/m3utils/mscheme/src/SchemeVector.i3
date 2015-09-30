(* $Id: SchemeVector.i3,v 1.2 2008/10/06 08:12:46 mika Exp $ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeVector;
IMPORT SchemeObject;

TYPE T = REF ARRAY OF SchemeObject.T;

CONST Brand = "SchemeVector";

END SchemeVector.
