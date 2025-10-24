(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NumberedObject;

TYPE 
  T = BRANDED Brand OBJECT num : CARDINAL END;

CONST Brand = "NumberedObject";

PROCEDURE Compare(a, b : T) : [-1..1];

END NumberedObject.
