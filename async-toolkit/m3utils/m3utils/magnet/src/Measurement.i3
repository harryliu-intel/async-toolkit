(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Measurement;

TYPE
  T = RECORD
    theta, f, P : LONGREAL;
  END;

CONST Brand = "Measurement";

END Measurement.
