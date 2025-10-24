(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspCompiledProcess;
FROM Fmt IMPORT F, Int;
FROM Debug IMPORT UnNil;

VAR nextId      : CARDINAL := 0;
    nextFrameId : CARDINAL := 0;

PROCEDURE NextId() : CARDINAL =
  BEGIN
    TRY
      RETURN nextId
    FINALLY
      INC(nextId)
    END
  END NextId;

PROCEDURE NextFrameId() : CARDINAL =
  BEGIN
    TRY
      RETURN nextFrameId
    FINALLY
      INC(nextFrameId)
    END
  END NextFrameId;

PROCEDURE DebugClosure(cl : Closure) : TEXT =
  BEGIN
    IF cl = NIL THEN
      RETURN "NIL"
    ELSIF cl.fr = NIL THEN
      RETURN F("%s %s:%s",
               Int(cl.frameId),
               "NIL",
               UnNil(cl.name))
    ELSE
      RETURN F("%s %s:%s",
               Int(cl.frameId),
               cl.fr.name,
               UnNil(cl.name))
    END
  END DebugClosure;

BEGIN END CspCompiledProcess.
