(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE GlitchGate;
IMPORT Text;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.tgt, b.tgt)
  END Equal;
  
BEGIN
END GlitchGate.
