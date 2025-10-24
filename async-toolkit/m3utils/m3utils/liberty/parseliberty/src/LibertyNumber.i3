(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyNumber;
IMPORT LibertyComponent;

TYPE
  T <: LibertyComponent.T;

  Integer = T OBJECT
    val : INTEGER;
  END;

  Floating = T OBJECT
    val : LONGREAL;
  END;

CONST Brand = "LibertyNumber";

END LibertyNumber.
