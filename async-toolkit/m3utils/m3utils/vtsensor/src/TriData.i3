INTERFACE TriData;
IMPORT Rd, DataPointSeq, Json;
IMPORT TriConfigSeq;

EXCEPTION SyntaxError;
          
PROCEDURE LoadJson(rd : Rd.T) : DataPointSeq.T RAISES { SyntaxError, Json.E };

PROCEDURE TraverseData(seq : DataPointSeq.T; corner : TEXT) : TriConfigSeq.T;



END TriData.
