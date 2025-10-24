(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertyDefine;
IMPORT LibertyComponentChildren;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write    := Write;
    children := Children;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx); 
    Wr.PutText(wr, "define(");
    t.s[0].write(wr, "");
    Wr.PutText(wr, ", ");
    t.s[1].write(wr, "");
    Wr.PutText(wr, ", ");
    t.s[2].write(wr, "");
    Wr.PutText(wr, ");");
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    RETURN SeqBuilder.BuildSeq(t.s[0], t.s[1], t.s[2])
  END Children;

BEGIN END LibertyDefine.

  
