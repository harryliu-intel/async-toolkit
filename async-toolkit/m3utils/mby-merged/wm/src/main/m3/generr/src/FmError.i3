(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FmError;

TYPE
  Id = [0..255]; (* one byte *)
  
  T = RECORD
    codeName : TEXT;
    id       : Id;
    desc     : TEXT;
  END;

CONST Brand = "FmError";

END FmError.
