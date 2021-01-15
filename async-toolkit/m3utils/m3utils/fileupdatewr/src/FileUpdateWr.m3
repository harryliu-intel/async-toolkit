MODULE FileUpdateWr;
IMPORT FileWr;
IMPORT Wr;
IMPORT WrClass;
IMPORT Pathname;
IMPORT OSError;
IMPORT Rd;
IMPORT FileRd;
IMPORT Thread;
IMPORT FS;

REVEAL
  T = FileWr.T BRANDED Brand OBJECT
    p       : Pathname.T;
    haveOld : BOOLEAN;
    suffix  : TEXT;
  OVERRIDES
    close := Close;
  END;

PROCEDURE Close(t : T)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    nfn := t.p;
    ofn := t.p & t.suffix;
  BEGIN
    FileWr.T.close(t);

    TRY
      IF t.haveOld THEN
        CONST
          BufSiz = 16 * 1024;
        VAR
          nRd := FileRd.Open(nfn);
          oRd := FileRd.Open(ofn);
          <*NOWARN*>nBuf, oBuf : ARRAY [0..BufSiz-1] OF CHAR;
        BEGIN
          LOOP
            WITH nRes = Rd.GetSub(nRd, nBuf),
                 oRes = Rd.GetSub(oRd, oBuf) DO
              IF nRes # oRes OR
                SUBARRAY(nBuf, 0, nRes) # SUBARRAY(oBuf, 0, oRes) THEN
                Rd.Close(nRd);
                Rd.Close(oRd);
                FS.DeleteFile(ofn);
                EXIT
              ELSIF nRes = 0 THEN
                (* new and old are the same --
                   close readers & copy old file over the new,
                   retaining mod. time *)
                
                Rd.Close(nRd);
                Rd.Close(oRd);
                FS.Rename(ofn, nfn); 
                EXIT
              END(*FI*)
            END(*HTIW*)
          END(*POOL*)
        END(*NIGEB*)
      END(*FI*)
    EXCEPT

    ELSE
      (* skip *)
    END
  END Close;
  
PROCEDURE Open(p : Pathname.T; suffix : TEXT) : T
  RAISES { OSError.E } =
  VAR
    res := NEW(T,
               p       := p,
               suffix  := suffix,
               haveOld := FALSE);
  BEGIN
    TRY
      FS.Rename(p, p & suffix);
      res.haveOld := TRUE;
    EXCEPT ELSE END;

    RETURN res.init(FS.OpenFile(p))
  END Open;

BEGIN END FileUpdateWr.
