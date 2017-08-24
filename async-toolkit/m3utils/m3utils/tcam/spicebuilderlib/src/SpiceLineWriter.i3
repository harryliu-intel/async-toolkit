INTERFACE SpiceLineWriter;
IMPORT Wr, Thread;

CONST DefLineMax = 80;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(wr : Wr.T; max := DefLineMax) : T;
    word(word : TEXT) RAISES { Wr.Failure, Thread.Alerted };
    eol()             RAISES { Wr.Failure, Thread.Alerted };

    pl(txt : TEXT)    RAISES { Wr.Failure, Thread.Alerted }; 
    (* may override -- by default this method pushes to the wr in init *)
  END;

CONST Brand = "SpiceLineWriter";

END SpiceLineWriter.
