INTERFACE LibertyComponent;
IMPORT Wr, Thread;

TYPE
  T = OBJECT METHODS
    write(wr : Wr.T; lineStart := "") RAISES { Wr.Failure, Thread.Alerted };
  END;

CONST Brand = "LibertyComponent";

END LibertyComponent.
