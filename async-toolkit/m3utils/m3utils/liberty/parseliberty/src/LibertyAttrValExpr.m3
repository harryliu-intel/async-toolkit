(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertyAttrValExpr;
IMPORT LibertyComponentChildren;
IMPORT LibertyComponent;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;
IMPORT LibertyBoolean;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
    children := Children;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    TYPECASE t OF
      String(s) =>
      Wr.PutChar(wr, '"'); Wr.PutText(wr, s.val); Wr.PutChar(wr, '"')
    |
      Boolean(b) => LibertyBoolean.Write(b.val, wr, "")
    |
      Expr(x) => x.val.write(wr, "")
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    TYPECASE t OF
      Boolean    => RETURN SeqBuilder.BuildSeq()
    |
      Expr(x)    => RETURN SeqBuilder.BuildSeq(x.val)
    |
      String     => RETURN SeqBuilder.BuildSeq()
    ELSE
      <*ASSERT FALSE*>
    END
  END Children;

BEGIN END LibertyAttrValExpr.

  
