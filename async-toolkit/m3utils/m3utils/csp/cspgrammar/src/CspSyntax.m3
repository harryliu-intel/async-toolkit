(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspSyntax;
IMPORT SchemeObject;
IMPORT Debug;
IMPORT RTBrand;

PROCEDURE Lisp(of : T) : SchemeObject.T =
  VAR
    brand : TEXT;
  BEGIN
    TRY
      brand := RTBrand.Get(of)
    EXCEPT
      RTBrand.NotBranded => brand := "**NOT-BRANDED**"
    END;
    Debug.Out("CspSyntax.Lisp : " & brand);
    
    IF of = NIL THEN RETURN NIL ELSE RETURN of.lisp() END
  END Lisp;
  
BEGIN END CspSyntax.
