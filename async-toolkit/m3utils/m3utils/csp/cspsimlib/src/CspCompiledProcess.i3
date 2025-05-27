INTERFACE CspCompiledProcess;
IMPORT CspScheduler;
IMPORT Word;

TYPE
  Frame = BRANDED OBJECT
    name      : TEXT;
    typeName  : TEXT;
    id        : CARDINAL;
    affinity  : CspScheduler.T;
  METHODS
    start(); (* schedule start block to run *)
  END;

  Closure = OBJECT
    name      : TEXT;
    id        : CARDINAL;
    fr        : Frame; (* non-specific -- subtype includes subtyped Frame *)
    frameId   : CARDINAL;
    scheduled : Word.T := -1; (* last time it was scheduled *)
  METHODS
    run();
  END;

CONST Brand = "CspCompiledProcess";

PROCEDURE NextId() : CARDINAL;

PROCEDURE NextFrameId() : CARDINAL;

PROCEDURE DebugClosure(cl : Closure) : TEXT;

END CspCompiledProcess.
