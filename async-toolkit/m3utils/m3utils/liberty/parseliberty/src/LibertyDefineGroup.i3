(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyDefineGroup;
IMPORT LibertyComponent;
IMPORT LibertySorI;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    s : ARRAY [0..1] OF LibertySorI.T;
  END;

CONST Brand = "LibertyDefineGroup";

END LibertyDefineGroup.
