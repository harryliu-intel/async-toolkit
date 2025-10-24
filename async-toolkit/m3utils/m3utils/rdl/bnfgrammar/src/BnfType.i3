(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BnfType;

TYPE
  T <: Public;

  Public = BRANDED OBJECT END;

CONST Brand = "BnfType";

PROCEDURE Equal(a, b : T) : BOOLEAN;

END BnfType.
