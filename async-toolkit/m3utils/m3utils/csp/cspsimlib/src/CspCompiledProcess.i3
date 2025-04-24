INTERFACE CspCompiledProcess;
IMPORT Word;

TYPE
  Frame = BRANDED OBJECT
    id : CARDINAL;
  END;

  Closure = OBJECT
    id : CARDINAL;
    frameId : CARDINAL;
    scheduled : Word.T := -1; (* last time it was scheduled *)
  METHODS
    run();
  END;

CONST Brand = "CspCompiledProcess";

PROCEDURE NextId() : CARDINAL;

PROCEDURE NextFrameId() : CARDINAL;

      
END CspCompiledProcess.
