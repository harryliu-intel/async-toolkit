(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Tcam;

TYPE
  T = RECORD
    N,   (* number of entries       *)
    W,   (* width of each entry     *)
    LN, 
    CN,  (* number of config bits   *)
    SS,  (* depth of a slice        *)
    SN   (* number of slices        *)                 : CARDINAL;
  END;

END Tcam.
