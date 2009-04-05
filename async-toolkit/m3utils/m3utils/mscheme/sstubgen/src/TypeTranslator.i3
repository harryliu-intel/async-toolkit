(* $Id$ *)

INTERFACE TypeTranslator;

(* 
 * Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
 * Author : Mika Nystrom <mika@alum.mit.edu> 
 *)

(* Translate the SRC Type.T into a SchemeObject.T for the Mscheme 
   environment *)

IMPORT Type, SchemePair;
FROM Type IMPORT Qid;

PROCEDURE Translate(type : Type.T) : SchemePair.T;

PROCEDURE TranslateQid(q : Qid) : SchemePair.T;

CONST Brand = "TypeTranslator";

END TypeTranslator.
