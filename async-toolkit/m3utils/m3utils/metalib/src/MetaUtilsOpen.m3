(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE MetaUtilsOpen EXPORTS MetaUtils;
IMPORT Pathname, Scheme, FileRd, SchemeBoolean, OSError;

PROCEDURE FileRdOpenOrFalse(pn : Pathname.T) : Scheme.Object =
  BEGIN
    TRY
      RETURN FileRd.Open(pn)
    EXCEPT 
      OSError.E =>
        RETURN SchemeBoolean.False()
    END
  END FileRdOpenOrFalse;

BEGIN END MetaUtilsOpen.
      
