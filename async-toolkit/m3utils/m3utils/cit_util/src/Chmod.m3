(* $Id: Chmod.m3,v 1.1 2006/03/12 11:05:32 mika Exp $ *)

UNSAFE MODULE Chmod;
IMPORT M3toC, Unix, Pathname, Ctypes;

PROCEDURE chmod(path : Pathname.T; mode : CARDINAL) RAISES { Error } =
  VAR 
    res : INTEGER;
    s : Ctypes.char_star;
  BEGIN
    TRY
      s := M3toC.CopyTtoS(path);
      res := Unix.chmod(s,mode);
      IF res # 0 THEN RAISE Error END
    FINALLY
      M3toC.FreeCopiedS(s)
    END
  END chmod;

BEGIN END Chmod.
