INTERFACE TraceFile;
IMPORT Pathname;
IMPORT FileNamer;

TYPE
  T <: Public;


  Public = OBJECT METHODS
    init(ofn    : Pathname.T;
         nFiles : CARDINAL;
         nNames : CARDINAL;
         fnr    : FileNamer.T) : T;

    write();

    writePll(wthreads : CARDINAL);
    
  END;

CONST Brand = "TraceFile";
  
END TraceFile.
