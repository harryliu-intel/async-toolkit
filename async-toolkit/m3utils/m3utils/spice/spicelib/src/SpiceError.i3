(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceError;
IMPORT Pathname;

TYPE
  Data = RECORD
    msg : TEXT := "";
    lNo : CARDINAL := 0;
    fn  : Pathname.T := NIL;
  END;
  
EXCEPTION E(Data);

CONST Brand = "SpiceError";

END SpiceError.
