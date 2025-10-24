(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertySorI;
IMPORT LibertyComponent;

TYPE
  T <: LibertyComponent.T;

  String = T BRANDED Brand & " String" OBJECT
    val : TEXT;
  END;

  Ident = T BRANDED Brand & " Ident" OBJECT
    val : TEXT;
  END;

CONST Brand = "LibertySorI";

END LibertySorI.
