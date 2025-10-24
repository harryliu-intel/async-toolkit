(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TransitionLeaderBoard;
IMPORT TransitionPickler;
IMPORT TraceOp;
IMPORT Transition;

TYPE
  T <: Public;

  
  Public = TraceOp.Pickle OBJECT
    maxCount    : CARDINAL := LAST(CARDINAL);
    transitions : TransitionPickler.T;
    resetTime   : LONGREAL;
  END;

  TLB = OBJECT
    idx : CARDINAL;
    arr : REF ARRAY OF Transition.T;
  END;

  Result = TLB;

CONST Brand = "TransitionLeaderBoard";

END TransitionLeaderBoard.
