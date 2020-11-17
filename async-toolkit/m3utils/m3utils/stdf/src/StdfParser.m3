MODULE StdfParser;
IMPORT StdfRecordHeader;
IMPORT StdfRecordObject, StdfRecordObjectSeq;
IMPORT Rd;

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

        bdylen := hdr.recLen;

        WITH parsePrc = StdfParseTable.GetParser(hdr) DO
          parsePrc(rd, bdylen, o);
          o.hdr := hdr;
          seq.addhi(o)
        END
      END
    END
  END Parse;

BEGIN END StdfParser.
