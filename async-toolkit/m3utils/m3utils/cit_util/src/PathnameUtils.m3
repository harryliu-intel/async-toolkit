(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE PathnameUtils;
IMPORT Debug;
IMPORT FileWr;
IMPORT FS;
IMPORT OSError;
IMPORT Text;
IMPORT Pathname;
IMPORT Wr;

PROCEDURE CompleteE(t: T): T RAISES {OSError.E} =
  VAR
    base := Pathname.Prefix(t);
    iter := FS.Iterate(base);
    begin := Pathname.Last(t);
    name: TEXT;
  BEGIN
    WHILE iter.next(name) DO
      IF Text.Equal(Text.Sub(name, 0, Text.Length(begin)), begin) THEN
        RETURN SlashedPrefix(t) & name;
      END;
    END;
    RETURN t;
  END CompleteE;

PROCEDURE Complete(t: T): T =
  BEGIN
    TRY
      RETURN CompleteE(t);
    EXCEPT
      OSError.E => RETURN t;
    END;
  END Complete;

PROCEDURE Slashify(path: T): T =
  BEGIN
    IF Text.Length(path)#0 AND
      Text.GetChar(path,Text.Length(path)-1) # '/' THEN
      path := path & "/";
    END;
    RETURN path;
  END Slashify;

PROCEDURE SlashedPrefix(t: T): T =
  BEGIN
    RETURN Slashify(Pathname.Prefix(t));
  END SlashedPrefix;

PROCEDURE Join(pn, base: T; ext: TEXT := NIL): T =
  BEGIN
    IF NOT Pathname.Absolute(base) THEN
      RETURN Pathname.Join(pn, base, ext);
    ELSIF ext = NIL THEN
      RETURN base;
    ELSE
      RETURN base & ext;
    END;
  END Join;

PROCEDURE DirOf(pn: T): T =
  VAR
    res := Pathname.Prefix(pn);
  BEGIN
    IF Text.Equal(res, "") THEN RETURN "."; END;
    RETURN res;
  END DirOf;

PROCEDURE CreatedDir(pn: TEXT) =
  BEGIN
    Debug.S("created directory: "  & pn, 0);
  END CreatedDir;

PROCEDURE OpenMakingDirs(pn: T; verbose := FALSE): Wr.T RAISES {OSError.E} =
  VAR
    cur: TEXT;
    error: BOOLEAN;
  BEGIN
    TRY
      RETURN FileWr.Open(pn);
    EXCEPT OSError.E =>
    END;
    LOOP
      cur := DirOf(pn);
      TRY
        FS.CreateDirectory(cur);
        IF verbose THEN CreatedDir(cur); END;
        RETURN FileWr.Open(pn);
      EXCEPT OSError.E =>
      END;
      error := TRUE;
      WHILE Text.FindChar(cur, '/') # -1 DO
        cur := DirOf(cur);
        TRY
          FS.CreateDirectory(cur);
          IF verbose THEN CreatedDir(cur); END;
          error := FALSE;
          EXIT;
        EXCEPT OSError.E =>
        END;
      END;
      IF error THEN
        RAISE OSError.E(NIL);
      END;
    END;
  END OpenMakingDirs;


BEGIN
END PathnameUtils.
