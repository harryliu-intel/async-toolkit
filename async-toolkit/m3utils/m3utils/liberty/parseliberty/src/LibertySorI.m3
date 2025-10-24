(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertySorI;
IMPORT LibertyComponent;
IMPORT Wr;
IMPORT Thread;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    TYPECASE t OF
      String(s)=> Wr.PutChar(wr, '"'); Wr.PutText(wr, s.val); Wr.PutChar(wr, '"')
    |
      Ident(i) => Wr.PutText(wr, i.val)
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;

BEGIN END LibertySorI.
