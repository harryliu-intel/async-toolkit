(* $Id: ValueTranslator.i3,v 1.2 2009/03/29 18:54:25 mika Exp $ *)

INTERFACE ValueTranslator;

(* 
 * Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
 * Author : Mika Nystrom <mika@alum.mit.edu> 
 *)

(* Translate the SRC Value.T (a constant value declaration) 
   into a SchemeObject.T for the Mscheme environment *)

IMPORT Value, SchemeObject;

PROCEDURE Translate(value : Value.T) : SchemeObject.T;

CONST Brand = "ValueTranslator";

END ValueTranslator.
