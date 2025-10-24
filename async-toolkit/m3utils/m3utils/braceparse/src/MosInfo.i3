(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MosInfo;
IMPORT Atom, Word;
IMPORT Wx;

TYPE
  T = RECORD
    type  : Atom.T;
    len   : CARDINAL; (* in micro-microns = picometers *)
  END;

CONST Brand = "MosInfo";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE DebugOut(READONLY a : T; wx : Wx.T);

END MosInfo.
