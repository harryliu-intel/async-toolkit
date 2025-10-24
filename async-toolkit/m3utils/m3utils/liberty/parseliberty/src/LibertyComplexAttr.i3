(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyComplexAttr;
IMPORT LibertyComponent;
IMPORT LibertyHead;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    head : LibertyHead.T;
    semi : BOOLEAN;
  END;

CONST Brand = "LibertyComplexAttr";

END LibertyComplexAttr.
 
