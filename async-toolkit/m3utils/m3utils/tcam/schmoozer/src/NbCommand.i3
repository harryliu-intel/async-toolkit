(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NbCommand;
IMPORT Time;

TYPE
  T = OBJECT
    id   : CARDINAL;
    cmd  : TEXT;
    nbId : [-1..LAST(CARDINAL)] := -1;
    started : Time.T;
  END;

CONST Brand = "NbCommand";

END NbCommand.
