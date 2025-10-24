GENERIC INTERFACE GenViews(Tgt);
IMPORT OSError, Wr, Thread, Pathname;

TYPE T <: Tgt.Gen;

TYPE
  Compiler = Tgt.Public OBJECT
    gv : T;
  METHODS
    write(dirPath : Pathname.T; phase : Tgt.Phase)
      RAISES { OSError.E, Wr.Failure, Thread.Alerted };
  END;
     
END GenViews.
