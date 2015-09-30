(* $Id: SchemeClosureClass.i3,v 1.3 2011/03/03 09:08:29 mika Exp $ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeClosureClass;
IMPORT SchemeClosure;
IMPORT SchemeObject, SchemeEnvironment;

REVEAL
  SchemeClosure.T <: Private;

TYPE
  Private = SchemeClosure.Public OBJECT
    params, body : SchemeObject.T;
    env          : SchemeEnvironment.Instance;
  END;

END SchemeClosureClass.
