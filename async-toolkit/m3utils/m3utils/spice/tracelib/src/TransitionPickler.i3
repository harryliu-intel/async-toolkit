(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TransitionPickler;
IMPORT TraceOp;
IMPORT TransitionSeq;

TYPE
  T <: Public;

  Public = TraceOp.Pickle OBJECT
    idx                : CARDINAL;
    time, of           : TraceOp.Array;
    thresh, hysteresis : LONGREAL;
  END;

  (* will leave a result of type TransitionSeq.T in T.result *)
  Result = TransitionSeq.T;
  
CONST Brand = "TransitionPickler";

END TransitionPickler.
