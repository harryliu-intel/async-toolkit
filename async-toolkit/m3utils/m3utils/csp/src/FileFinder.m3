MODULE FileFinder;
IMPORT Pathname;
IMPORT SchemePair;
IMPORT RegEx;
IMPORT FS;
IMPORT SchemeUtils;
IMPORT SchemeString;
IMPORT OSError;

PROCEDURE Find(dirpath : Pathname.T; pattern : TEXT) : SchemePair.T
  RAISES { RegEx.Error, OSError.E } =
  VAR
    pat := RegEx.Compile(pattern);
    fsIter := FS.Iterate(dirpath);
    res : SchemePair.T := NIL;
    path : Pathname.T;
  BEGIN
    WHILE fsIter.next(path) DO
      IF RegEx.Execute(pat, path) # -1 THEN
        res := SchemeUtils.Cons(SchemeString.FromText(path),
                                res)
      END
    END;
    RETURN res
  END Find;
  
BEGIN END FileFinder.
