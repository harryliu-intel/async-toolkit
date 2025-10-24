(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceGate;
IMPORT FetArray;

TYPE
  Pull = { Down, Up };

  T = ARRAY Pull OF FetArray.T;

CONST Brand = "SpiceGate";

END SpiceGate.
