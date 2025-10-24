(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE MosInfo;
IMPORT Word;
IMPORT Atom;
FROM Fmt IMPORT F, Int;
IMPORT Wx;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a.type = b.type AND a.len = b.len
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(Atom.Hash(a.type), a.len)
  END Hash;

PROCEDURE DebugOut(READONLY a : T; wx : Wx.T) =
  BEGIN
    Wx.PutText(wx,
               F("type %s length %s picometers ",
                 Atom.ToText(a.type),
                 Int(a.len)))
  END DebugOut;

BEGIN END MosInfo.
  
