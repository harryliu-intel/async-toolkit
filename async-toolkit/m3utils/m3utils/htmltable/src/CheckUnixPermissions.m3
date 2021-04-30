MODULE CheckUnixPermissions;

PROCEDURE Check(path : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN GetReason(path).result
  END Check;

PROCEDURE GetReason(path : Pathname.T) : Reason =
  VAR
    cpath := M3toC.CopyTtoS(path);
    stat := NEW(Ustat.struct_stat_star);
  BEGIN
    TRY
      
      WITH sres = Ustat.stat(cpath, stat) DO
        IF sres < 0 THEN
          RETURN NIL (* not the right thing to do *)
        END
      END;
      
      RETURN Reason { FALSE, path };
    FINALLY
      M3toC.FreeCopiedS(cpath);
      DISPOSE(stat)
    END;
  END GetReason;
  
BEGIN END CheckUnixPermissions.
