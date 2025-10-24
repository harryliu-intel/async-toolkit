(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ProgramSetter;
IMPORT Power;

TYPE
  T = PROCEDURE(VAR p                       : Power.Params;
                VAR Trunc                   : LONGREAL);

CONST Brand = "ProgramSetter";

END ProgramSetter.
