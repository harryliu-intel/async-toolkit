UNSAFE MODULE UnixPermissions;
IMPORT Pathname;
IMPORT Ustat;
IMPORT M3toC;
FROM Cerrno IMPORT GetErrno;
FROM Socket IMPORT Unexpected;
IMPORT OSErrorPosix;
IMPORT AtomList;
IMPORT OSError;
IMPORT Atom;
IMPORT FS;
IMPORT Utypes;
IMPORT Word;

PROCEDURE AddPerm(VAR perms : Perms;
                  val       : Utypes.mode_t;
                  pt        : PT;
                  id        : R;
                  check     : Utypes.mode_t) =
  BEGIN
    IF Word.And(val, check) # 0 THEN
      perms[id] := perms[id] + P { pt }
    END
  END AddPerm;

PROCEDURE Get(path : Pathname.T) : T
  RAISES { OSError.E } =
  VAR
    realPath := FS.GetAbsolutePathname(path);
    cpath    := M3toC.CopyTtoS(realPath);
    stat     := NEW(Ustat.struct_stat_star);
    res      : T;
  BEGIN
    TRY
      
      WITH sres = Ustat.stat(cpath, stat) DO
        IF sres < 0 THEN
          IOError(Unexpected)
        END
      END;

      res.path     := path;
      res.realPath := realPath;
      res.uid      := stat.st_uid;
      res.gid      := stat.st_gid;
      
      VAR
        perms    := Perms { P { }, .. };
      BEGIN
        
        AddPerm(perms, stat.st_mode, PT.R, R.Owner, Ustat.S_IREAD);
        AddPerm(perms, stat.st_mode, PT.W, R.Owner, Ustat.S_IWRITE);
        AddPerm(perms, stat.st_mode, PT.X, R.Owner, Ustat.S_IEXEC);
        
        AddPerm(perms, stat.st_mode, PT.R, R.Group, Ustat.S_GREAD);
        AddPerm(perms, stat.st_mode, PT.W, R.Group, Ustat.S_GWRITE);
        AddPerm(perms, stat.st_mode, PT.X, R.Group, Ustat.S_GEXEC);
        
        AddPerm(perms, stat.st_mode, PT.R, R.Other, Ustat.S_OREAD);
        AddPerm(perms, stat.st_mode, PT.W, R.Other, Ustat.S_OWRITE);
        AddPerm(perms, stat.st_mode, PT.X, R.Other, Ustat.S_OEXEC);
        
        res.perms := perms
      END;

      RETURN res
      
    FINALLY
      M3toC.FreeCopiedS(cpath);
      DISPOSE(stat)
    END;
  END Get;

PROCEDURE IOError (a: Atom.T) RAISES {OSError.E} =
  VAR ec: AtomList.T := NIL;
  BEGIN
    IF (GetErrno() # 0) THEN
      ec := AtomList.List1 (OSErrorPosix.ErrnoAtom (GetErrno()));
    END;
    RAISE OSError.E (AtomList.Cons (a, ec));
  END IOError;

BEGIN END UnixPermissions.
