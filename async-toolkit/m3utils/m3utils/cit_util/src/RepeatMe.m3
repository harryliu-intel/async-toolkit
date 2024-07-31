MODULE RepeatMe;
IMPORT Process;
IMPORT Params;
IMPORT File;
IMPORT Thread;
IMPORT Time;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT OSError;
IMPORT AL;
IMPORT Wx;

PROCEDURE Do(execFlag               : TEXT;
             immediateQuit          : Process.ExitCode;
             READONLY addArgs       : ARRAY OF TEXT) : BOOLEAN =
  VAR
    stdin, stdout, stderr : File.T;
    params := NEW(REF ARRAY OF TEXT, Params.Count - 1 + NUMBER(addArgs) + 1);
    cmd := Params.Get(0);
  BEGIN
    (* copy the parameters over, but make params[0] the execFlag *)
    params[0] := execFlag;
    FOR i := FIRST(addArgs) TO LAST(addArgs) DO
      params[i + 1] := addArgs[i]
    END;
    FOR i := 1 TO Params.Count - 1 DO
      params[i + NUMBER(addArgs)] := Params.Get(i)
    END;
    
    Process.GetStandardFileHandles(stdin, stdout, stderr);

    TRY
      WITH proc     = Process.Create(cmd,
                                     params^,
                                     stdin := stdin,
                                     stdout := stdout,
                                     stderr := stderr),
           exitCode = Process.Wait(proc) DO

        IF exitCode # 0 THEN
          VAR
            wx := Wx.New();
          BEGIN
            Wx.PutText(wx, cmd);
            FOR i := FIRST(params^) TO LAST(params^) DO
              Wx.PutChar(wx, ' ');
              Wx.PutText(wx, params[i])
            END;
            Debug.Warning(F("RepeatMe.Do : subprocess exit code %s, failed:\n %s",
                            Int(exitCode), Wx.ToText(wx)))
          END
        END;
        
        IF exitCode = immediateQuit AND exitCode # 0 THEN
          Debug.Error("Aborting with exitCode " & Int(exitCode), exitCode := exitCode)
        END;
        
        RETURN exitCode = 0
      END
    EXCEPT
      OSError.E(x) => Debug.Warning("Process.Create failed : OSError.E : " & AL.Format(x));
      RETURN FALSE
    END
  END Do;
  
PROCEDURE Repeat(execFlag            : TEXT;
                 maxTimes            : CARDINAL;
                 delay               : Time.T;
                 immediateQuit       : Process.ExitCode;
                 READONLY addArgs    : ARRAY OF TEXT) =
  BEGIN
    FOR i := 1 TO maxTimes DO
      IF Do(execFlag, immediateQuit, addArgs) THEN
        Process.Exit(0)
      ELSE
        Debug.Warning("Process failed.  Re-executing!");
        Thread.Pause(delay)
      END
    END;
    Debug.Error(F("Process failed maxTimes(%s), quitting with error", Int(maxTimes)))
  END Repeat;

BEGIN END RepeatMe.
