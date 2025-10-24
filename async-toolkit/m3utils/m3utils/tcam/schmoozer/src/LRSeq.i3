(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LRSeq;
IMPORT LongRealSeq;
IMPORT Word;

TYPE T = LongRealSeq.T;

CONST Brand = "LRSeq";

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;

END LRSeq.
