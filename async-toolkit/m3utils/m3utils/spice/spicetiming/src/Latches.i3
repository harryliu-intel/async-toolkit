(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Latches;
IMPORT Trace;

TYPE
  LatchNodes = RECORD
    clk, d, q : TEXT;
  END;

PROCEDURE IsQNode(trace          : Trace.T;
                  idx            : CARDINAL;
                  VAR latchNodes : LatchNodes) : BOOLEAN;
  (* assuming the clock node is CLK, the D node is D, and the Q node is Q,
     check whether 

     (1) the given indexed node (trace[idx]) is a Q node
     and
     (2) there also exist D and CLK nodes for the same prefix.

     If so, then return TRUE and fill in latchNodes with the appropriate
     node names.
  *)
  
END Latches.
