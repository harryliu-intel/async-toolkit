(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TraceHeader;

TYPE
  T = RECORD
    start  : CARDINAL;
    end    : CARDINAL;
    steps  : CARDINAL;
    nNodes : CARDINAL;
  END;

END TraceHeader.
