MODULE TempReader;
IMPORT Rd, DataBlock;
IMPORT Pathname, OSError;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT FileRd;
IMPORT Thread;
IMPORT FileNamer;
IMPORT TempDataRep;

<*FATAL Thread.Alerted*>

VAR doDebug := Debug.DebugThis("TempReader");

CONST TE = Text.Equal;

PROCEDURE FileRd_Open(fn : Pathname.T) : Rd.T RAISES { OSError.E } =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("opening file \"%s\"", fn));
    END;
    RETURN FileRd.Open(fn)
  END FileRd_Open;
      
REVEAL
  T = Public BRANDED Brand OBJECT
    fnr  : FileNamer.T;
    dbRd : Rd.T       ;
    dbDb : DataBlock.T;
    dbFn : Pathname.T ;
  OVERRIDES
    init := InitM;

    readEntireFile := ReadEntireFileM;
  END;

PROCEDURE InitM(self : T; fnr : FileNamer.T) : T =
  BEGIN
    self.fnr  := fnr;
    self.dbRd := NIL;
    self.dbDb := NIL;
    self.dbFn := NIL;
    RETURN self
  END InitM;

PROCEDURE ReadEntireFileM(self     : T;
                          idx      : CARDINAL;
                          VAR data : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, OSError.E } =
  VAR
    fn := self.fnr.name(idx);
    ptr := 0;
    aLen := NUMBER(data);

  BEGIN

    (* 
       this routine is called sequentially by TraceFile.Write (or WritePll,
       but that is not a good idea)

       so it is called with idx = 0, 1, ... , nNodes - 1
    *)
    
    IF doDebug THEN
      Debug.Out(F("ConvertTrace.ReadEntireFileM: idx %s fn %s aLen %s",
                  Int(idx), fn, Int(aLen)))
    END;
    
    IF idx = 0 THEN
      TRY
        WITH rd  = FileRd_Open(fn),
             cnt = DataBlock.ReadData(rd, idx, data, fn) DO
          <*ASSERT cnt = NUMBER(data)*>
          Rd.Close(rd)
        END
      EXCEPT
        Rd.EndOfFile => <*ASSERT FALSE*> (* internal program error *)
      END
    ELSE
      (* 
         at this point, self.dbFn holds the filename of the 
         lately opened temp file.

         if self.dbRd is non-NIL, then that reader is still open and can
         be re-used.

         if on the other hand self.dbDb is NIL, we don't have a lately
         opened file open anymore and have to open it for sure.

         Also we obv. have to open the file if we lately had the WRONG
         file open! 

         While it is OK to run this multi-threaded, tests have shown that
         running it multi-threaded slows the program down (sometimes 
         dramatically) 
      *)
      IF self.dbDb = NIL OR NOT TE(fn, self.dbFn) THEN
        IF self.dbRd # NIL THEN
          Rd.Close(self.dbRd);
        END;
        self.dbFn := fn;
        self.dbRd := FileRd_Open(fn);
        self.dbDb := NEW(DataBlock.T).init(self.dbRd, aLen, fn)
      END;

      (* we now have the correct file open,
         the tag we seek should definitely be present in that file,
         if we have done everything right so far ... *)
      
      IF NOT self.dbDb.haveTag(idx) THEN
        Debug.Error(F("ConvertTrace.ReadEntireFileM: internal error: file %s doesn't contain tag %s", fn, Int(idx)))
      END;

      WITH blockType = self.dbDb.blockType(idx) DO
        CASE blockType OF
          DataBlock.BlockType.Array =>
          WITH readRecs = self.dbDb.readData(idx, data) DO
            IF readRecs # aLen THEN
              Debug.Warning(F("ConvertTrace.ReadEntireFileM: short read for node (%s) : %s # %s",
                              Int(idx), Int(ptr), Int(aLen)))
            END
          END
        |
          DataBlock.BlockType.Compressed =>
          VAR
            zdata := self.dbDb.readCompressed(idx);
            tempRep : TempDataRep.T;
          BEGIN
            TempDataRep.ReadFromTemp(zdata, tempRep);
            TempDataRep.Reconstruct(tempRep, data);
            
            (* expand to actual range *)
            WITH offset = tempRep.norm.min,
                 range  = tempRep.norm.max - tempRep.norm.min DO
              FOR i := FIRST(data) TO LAST(data) DO
                data[i] := data[i] * range + offset
              END
            END
          END
        END
      END
    END
  END ReadEntireFileM;

BEGIN END TempReader.
