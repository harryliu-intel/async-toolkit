MODULE TechCleanup;
IMPORT Pathname;
IMPORT Debug;
IMPORT FS;
IMPORT CitTextUtils;
IMPORT TextWr;
FROM Fmt IMPORT F;
IMPORT ProcUtils;
IMPORT RegEx;
IMPORT OSError;

VAR Verbose := Debug.DebugThis("TechCleanup");
    
PROCEDURE DeleteMatching(dir     : Pathname.T;
                         pattern : TEXT) =
  VAR
    regEx := RegEx.Compile(pattern);
  BEGIN
    TRY
      VAR
        iter := FS.Iterate(dir);
        fn   : Pathname.T;
      BEGIN
        WHILE iter.next(fn) DO
          IF RegEx.Execute(regEx, fn) # -1 THEN
            TRY
              FS.DeleteFile(dir & "/" & fn)
            EXCEPT ELSE END
          END
        END
      END
    EXCEPT
    ELSE
    END
  END DeleteMatching;
                         

PROCEDURE DeleteRecursively(workdir, subdir : Pathname.T) =
  BEGIN
    TRY
      VAR
        dir  := workdir & "/" & subdir;
        iter := FS.Iterate(dir);
        fn   : Pathname.T;
      BEGIN
        WHILE iter.next(fn) DO
          WITH ffn = dir & "/" & fn DO
            IF Verbose THEN
              Debug.Out("Attempting to delete "& ffn)
            END;
            TRY FS.DeleteFile(ffn) EXCEPT ELSE END
          END
        END;
        IF Verbose THEN
          Debug.Out("Attempting to delete \""& dir & "\"");
        END;
        FS.DeleteDirectory(dir)
      END
    EXCEPT
      OSError.E => Debug.Out("Caught OSError.E (OK)")
    END
  END DeleteRecursively;

PROCEDURE CompressFilesWithExtension(dir : Pathname.T; ext : TEXT) =
  VAR
    iter := FS.Iterate(dir);
    bn : Pathname.T;
  BEGIN
    WHILE iter.next(bn) DO
      IF CitTextUtils.HaveSuffix(bn, ext) THEN
        CompressFile(dir & "/" & bn)
      END
    END    
  END CompressFilesWithExtension;

PROCEDURE CompressFile(fn : Pathname.T) =
  VAR
    wr             := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);
    cmd            := "gzip -9 " & fn;
  BEGIN
    WITH cm = ProcUtils.RunText(cmd,
                                stdout := stdout,
                                stderr := stderr,
                                stdin := NIL) DO
      TRY
        IF Verbose THEN
          Debug.Out("Compressing file \"" & fn & "\"")
        END;
        cm.wait()
      EXCEPT
        ProcUtils.ErrorExit(err) =>
        WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                     cmd,
                     TextWr.ToText(wr),
                     ProcUtils.FormatError(err)) DO
          Debug.Warning(msg)
        END
      END
    END
  END CompressFile;

BEGIN END TechCleanup.
