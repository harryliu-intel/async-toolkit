MODULE StdfParser;
IMPORT StdfRecordHeader;
IMPORT StdfRecordObject, StdfRecordObjectSeq, StdfRecordObjectClass;
IMPORT Rd;
IMPORT StdfRecordTypes;
IMPORT StdfParseTable;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT StdfE, Thread;

VAR doDebug := Debug.DebugThis("StdfParser");
    
PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq.T
  RAISES { Rd.Failure, StdfE.E, Thread.Alerted } =
  (* hmm handling of Rd.EndOfFile is questionable
     this is really only sposed to be raised by a particular record type,
     or what?
  *)
  VAR
    seq := NEW(StdfRecordObjectSeq.T).init();
  BEGIN
    TRY
      LOOP
        VAR
          hdrlen : CARDINAL := StdfRecordHeader.Length;
          hdr : StdfRecordHeader.T;
          bdylen : CARDINAL;
          start := Rd.Index(rd);
        BEGIN
          StdfRecordHeader.Parse(rd, hdrlen, hdr);
          
          IF doDebug THEN
            Debug.Out(F("StdfParser.Parse Got StdfRecordHeader recTyp %s recSub %s len %s",
                        Int(hdr.recTyp), Int(hdr.recSub), Int(hdr.recLen)))
          END;
          
          bdylen := hdr.recLen;
          
          VAR
            recType : StdfRecordTypes.T;
            hadIt := StdfParseTable.Get(hdr.recTyp, hdr.recSub, recType);
            bdyLen : CARDINAL := hdr.recLen;
          BEGIN
            
            IF NOT hadIt THEN
              Debug.Error(F("No parser defined for recTyp %s recSub %s",
                            Int(hdr.recTyp), Int(hdr.recSub)))
            END;
            IF doDebug THEN
              Debug.Out(F("StdfParser.Parse idx %s parsing record of type %s len %s",
                          Int(Rd.Index(rd)), recType.nm, Int(hdr.recLen)))
            END;
            VAR
              o : StdfRecordObject.T;
            BEGIN
              o := recType.parser(rd, hdr, bdyLen);
              o.hdr := hdr;
              o.tag := recType.enum;
              
              IF doDebug THEN
                Debug.Out(F("StdfParser.Parse idx %s totlen %s got record of type %s :\n%s", Int(start), Int(Rd.Index(rd)-start), recType.nm, StdfRecordTypes.Formatters[o.tag](o)))
              END;
              
              seq.addhi(o)
            END
          END
        END
      END(*POOL*)
    EXCEPT
      Rd.EndOfFile => RETURN seq
    END
  END Parse;

BEGIN END StdfParser.
