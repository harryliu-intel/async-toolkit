MODULE CspCompiledProcess;
FROM Fmt IMPORT F, Int;

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
    ELSE
      RETURN F("%s %s:%s",
               Int(cl.frameId),
               cl.fr.name,
               cl.name)
    END
  END DebugClosure;

BEGIN END CspCompiledProcess.
