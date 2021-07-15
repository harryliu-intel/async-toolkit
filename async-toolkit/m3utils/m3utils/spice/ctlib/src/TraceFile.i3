INTERFACE TraceFile;
IMPORT Pathname;
IMPORT FileNamer;
IMPORT Rd, Wr, OSError, Thread;
IMPORT TempReader;

(* output aspice/aplot format trace file in efficient order *)

TYPE
  T <: Public;


  Public = OBJECT METHODS
    init(ofn    : Pathname.T;
         nFiles : CARDINAL;
         nNames : CARDINAL;
         fnr    : FileNamer.T) : T;

    write();

    writePll(wthreads : CARDINAL; writeTraceCmdPath : Pathname.T);
    
  END;

CONST Brand = "TraceFile";
  
PROCEDURE BlockWrite(wr            : Wr.T;
                     fr            : TempReader.T;
                     i             : CARDINAL;
                     dataStartByte : CARDINAL;
                     VAR buff      : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Wr.Failure, OSError.E, Thread.Alerted };

END TraceFile.
