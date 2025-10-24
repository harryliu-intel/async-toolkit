(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertyNumber;
IMPORT LibertyComponent;
IMPORT Thread;
IMPORT Fmt;
IMPORT Wr;

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
      Integer(i) => Wr.PutText(wr, Fmt.Int(i.val))
    |
      Floating(f) => Wr.PutText(wr, Fmt.LongReal(f.val)    )
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;
  
BEGIN END LibertyNumber.
