(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertyBoolean;
IMPORT Thread;
IMPORT Wr;


PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutText(wr, pfx);
    CASE t OF
      T.F => Wr.PutText(wr, "false")
    |
      T.T => Wr.PutText(wr, "true")
    END
  END Write;
  
BEGIN END LibertyBoolean.

  
