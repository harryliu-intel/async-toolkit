MODULE StdfParser;
IMPORT Rd;

IMPORT StdfRecordHeader;

PROCEDURE Parse(rd : Rd.T) : StdfRecordObjectSeq.T =
  VAR
    seq := NEW(StdfRecordObjectSeq.T).init();
  BEGIN
    LOOP
      WITH c = Rd.GetChar(rd) DO (* block until there is data *)
        Rd.UnGetChar(rd, c)
      END;
      VAR
        hdrlen := StdfRecordHeader.Length;
        hdr : StdfRecordHeader.T;
        bdylen : CARDINAL;
        o : StdfRecordObject.T;
      BEGIN
        StdfRecordHeader.Parse(rd, hdrlen, hdr);

        bdylen := hdr.recLen;

        WITH parsePrc = StdfParseTable.GetParser(hdr) DO
          parsePrc(rd, bdylen, o);
          seq.addhi(o)
        END
      END
    END
  END Parse;

BEGIN END StdfParser.
