(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertySimpleAttr;
IMPORT LibertyComponentChildren;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
    children := Children;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    Wr.PutText(wr, t.ident);
    CASE t.syntax OF
      Syntax.ColonSemi, Syntax.Colon => Wr.PutText(wr, " : ")
    |
      Syntax.Eq                      => Wr.PutText(wr, " = ")
    END;
    t.attrValExpr.write(wr, "");
    CASE t.syntax OF
      Syntax.ColonSemi, Syntax.Eq    => Wr.PutText(wr, " ; ")
    |
      Syntax.Colon                   => (* skip *)
    END
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    RETURN SeqBuilder.BuildSeq(t.attrValExpr)
  END Children;

BEGIN END LibertySimpleAttr.
