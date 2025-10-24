(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspNode;
IMPORT CspPortObject;

TYPE
  T = CspPortObject.T BRANDED Brand OBJECT
    (* wait queue goes here *)
  END;

CONST Brand = "CspNode";

END CspNode.
