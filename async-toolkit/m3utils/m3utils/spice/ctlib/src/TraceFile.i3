INTERFACE TraceFile;
IMPORT Pathname;
IMPORT FileNamer;
IMPORT Rd, Wr, OSError, Thread;
IMPORT TempReader;
IMPORT Word;
IMPORT Time;

(* output aspice/aplot format trace file in efficient order *)

TYPE
  T <: Public;


  Public = OBJECT METHODS
    init(ofn    : Pathname.T;
         nFiles : CARDINAL;
         nNames : CARDINAL;
         fnr    : FileNamer.T) : T;

    write(fmt : Version);

    writePll(wthreads : CARDINAL; writeTraceCmdPath : Pathname.T; fmt : Version);
    
  END;

  Version = { Unreordered, (* unreordered (node-major) for aspice *)
              Reordered,   (* reordered (time-major) from convert_trace, ct *)
              CompressedV1 (* compressed format V1 *)
  };
  
  Header = RECORD
    version : Version;   (* the version of the trace file *)
    ctime   : Time.T;    (* creation time                 *)
    nwaves  : CARDINAL;  (* number of nodes in the file   *)
  END;

  UInt32 = [ 0 .. Word.Shift(1, 32) - 1 ];
  
CONST VersionVals = ARRAY Version OF UInt32 { 0, 1, 256 };

      VersionNames = ARRAY Version OF TEXT { "Unreordered",
                                             "Reordered",
                                             "CompressedV1" };

      VersionSuffixes = ARRAY Version OF TEXT { "trace", "trace", "ztrace" };
      
PROCEDURE WriteHeader(wr : Wr.T; READONLY header : Header)
  RAISES { Wr.Failure, Thread.Alerted };

EXCEPTION FormatError;
          
PROCEDURE ReadHeader(rd : Rd.T) : Header
  RAISES { FormatError, Rd.Failure, Rd.EndOfFile, Thread.Alerted };

CONST Brand = "TraceFile";
  
PROCEDURE BlockWrite(wr            : Wr.T;
                     fr            : TempReader.T;
                     i             : CARDINAL;
                     dataStartByte : CARDINAL;
                     VAR buff      : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Wr.Failure, OSError.E, Thread.Alerted };
  (* why is this exported here? *)

END TraceFile.
