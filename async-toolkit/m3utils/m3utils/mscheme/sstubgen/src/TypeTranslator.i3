(* $Id$ *)

INTERFACE TypeTranslator;

(* 
 * Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
 * Author : Mika Nystrom <mika@alum.mit.edu> 
 *)

(* Translate the SRC Type.T into a SchemeObject.T for the Mscheme 
   environment *)

IMPORT Type, SchemeObject;
FROM Type IMPORT Qid;

PROCEDURE Translate(type : Type.T;
                    alias : Qid) : SchemeObject.T;

PROCEDURE TranslateQid(q : Qid) : SchemeObject.T;

CONST Brand = "TypeTranslator";

END TypeTranslator.
