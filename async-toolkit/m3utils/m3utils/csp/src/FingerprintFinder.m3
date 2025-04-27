MODULE FingerprintFinder;

IMPORT Pathname, OSError, Rd;
IMPORT FileRd;
IMPORT TextReader;
IMPORT Text;
IMPORT Thread;

<*FATAL Thread.Alerted*>

PROCEDURE Find(pn : Pathname.T; key : TEXT) : TEXT
  RAISES { OSError.E, Rd.Failure } =
  VAR
    rd := FileRd.Open(pn);
    word : TEXT;
  BEGIN
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line),
             haveOne = reader.next(" ", word) DO
          IF haveOne AND Text.Equal(word, key) THEN
            RETURN reader.nextE("")
          END
        END
      END
    EXCEPT
      Rd.EndOfFile, TextReader.NoMore => RETURN NIL
    END
  END Find;

BEGIN END FingerprintFinder.
  
