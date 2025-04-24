MODULE CspCompiledProcess;

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

BEGIN END CspCompiledProcess.
