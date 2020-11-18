MODULE StdfParser;
IMPORT StdfRecordHeader;
IMPORT StdfRecordObject, StdfRecordObjectSeq, StdfRecordObjectClass;
IMPORT Rd;
IMPORT StdfRecordTypes;
IMPORT StdfParseTable;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

VAR doDebug := TRUE;
    
PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq.T =
  VAR
    seq := NEW(StdfRecordObjectSeq.T).init();
  BEGIN
    LOOP
      VAR
        hdrlen : CARDINAL := StdfRecordHeader.Length;
        hdr : StdfRecordHeader.T;
        bdylen : CARDINAL;
        o : StdfRecordObject.T;
      BEGIN
        StdfRecordHeader.Parse(rd, hdrlen, hdr);

        IF doDebug THEN
          Debug.Out(F("Got StdfRecordHeader recTyp %s recSub %s len %s",
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
          WITH o = recType.parser(rd, bdyLen) DO
            o.hdr := hdr;
            o.tag := recType.enum;

            IF doDebug THEN
              Debug.Out("got record: " & StdfRecordTypes.Formatters[o.tag](o));
            END;
            
            seq.addhi(o)
          END
        END
      END
    END
  END Parse;

BEGIN END StdfParser.
