GENERIC INTERFACE GenViews(Tgt);
IMPORT GenViews AS Super;
IMPORT OSError, Wr, Thread, Pathname;

TYPE T <: Super.T;

TYPE
  Compiler = Tgt.Public OBJECT METHODS
    write(dirPath : Pathname.T; phase : Tgt.Phase)
      RAISES { OSError.E, Wr.Failure, Thread.Alerted };
  END;
     
END GenViews.
