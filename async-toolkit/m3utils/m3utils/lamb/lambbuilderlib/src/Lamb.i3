(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Lamb;

TYPE
  T = RECORD
    N,   (* number of entries       *)
    W,   (* width of each entry     *)
    LN
                 : CARDINAL;
  END;

END Lamb.
