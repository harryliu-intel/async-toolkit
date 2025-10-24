(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VectorSeq;

IMPORT LRVectorSeq;
IMPORT Pathname;
IMPORT MultiEvalLRVector;

TYPE T = LRVectorSeq.T;

PROCEDURE ToMulti(seq        : T;
                  subdirPath : Pathname.T) : MultiEvalLRVector.Result;

END VectorSeq.
