(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TransitionSeq;
IMPORT SimpleTransitionSeq;
IMPORT V01X;

TYPE T = SimpleTransitionSeq.T OBJECT initVal : V01X.T END;

CONST Brand = "TransitionSeq";

END TransitionSeq.
