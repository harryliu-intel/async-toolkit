MODULE RepeatMe;
IMPORT Process;
IMPORT Params;
IMPORT File;
IMPORT Thread;
IMPORT Time;

PROCEDURE Do(execFlag : TEXT) : BOOLEAN =
  VAR
    stdin, stdout, stderr : File.T;
    params := NEW(REF ARRAY OF TEXT, Params.Count - 1 + 1);
    cmd := Params.Get(0);
  BEGIN
    (* copy the parameters over, but make params[0] the execFlag *)
    params[0] := execFlag;
    FOR i := 1 TO Params.Count - 1 DO
      params[i] := Params.Get(i)
    END;
    
    Process.GetStandardFileHandles(stdin, stdout, stderr);

    WITH proc     = Process.Create(cmd,
                                   params^,
                                   stdin := stdin,
                                   stdout := stdout,
                                   stderr := stderr),
         exitCode = Process.Wait(proc) DO
      RETURN exitCode = 0
    END
  END Do;
  
PROCEDURE Repeat(execFlag : TEXT; maxTimes : CARDINAL; delay : Time.T) : BOOLEAN =
  BEGIN
    FOR i := 1 TO maxTimes DO
      IF Do(execFlag) THEN
        Process.Exit(0)
      ELSE
        Thread.Pause(delay)
      END
    END
  END Repeat;

BEGIN END RepeatMe.
