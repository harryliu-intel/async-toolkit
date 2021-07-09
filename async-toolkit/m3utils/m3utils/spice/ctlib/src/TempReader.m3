MODULE TempReader;
IMPORT Rd, DataBlock;
IMPORT Pathname, OSError;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT FileRd;
IMPORT Thread;
IMPORT FileNamer;

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
      IF self.dbDb = NIL OR NOT TE(fn, self.dbFn) THEN
        IF self.dbRd # NIL THEN
          Rd.Close(self.dbRd);
        END;
        self.dbFn := fn;
        self.dbRd := FileRd_Open(fn);
        self.dbDb := NEW(DataBlock.T).init(self.dbRd, aLen, fn)
      END;
      
      IF NOT self.dbDb.haveTag(idx) THEN
        Debug.Error(F("ConvertTrace.ReadEntireFileM: internal error: file %s doesn't contain tag %s", fn, Int(idx)))
      END;
      
      WITH readRecs = self.dbDb.readData(idx, data) DO
        IF readRecs # aLen THEN
          Debug.Warning(F("ConvertTrace.ReadEntireFileM: short read for node (%s) : %s # %s",
                          Int(idx), Int(ptr), Int(aLen)))
        END
      END
    END
  END ReadEntireFileM;

BEGIN END TempReader.
