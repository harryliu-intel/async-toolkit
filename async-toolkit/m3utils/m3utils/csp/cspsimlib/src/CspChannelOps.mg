GENERIC MODULE CspChannelOps(CspDebug);
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT DebugClosure;
IMPORT Debug;
FROM Fmt IMPORT Int, F, FN, Bool;

CONST sendDebug = CspDebug.DebugSend;
CONST recvDebug = CspDebug.DebugRecv;
CONST probDebug = CspDebug.DebugProbe;
CONST seleDebug = CspDebug.DebugSelect;

TYPE TA = ARRAY OF TEXT;

PROCEDURE Lock  (c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug THEN
      Debug.Out(F("%s : %s Lock %s",
                  DebugClosure(cl), c.nm, 
                  cl.name))
    END;
    c.lockwr := c.wr; c.lockrd := c.rd
  END Lock;
  
PROCEDURE Unlock(c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug THEN
      Debug.Out(F("%s : %s Unlock %s",
                  DebugClosure(cl), c.nm, 
                  cl.name))
    END;

    (* NOP *)
  END Unlock;
  
PROCEDURE Ready (c : T; cl : Process.Closure) : BOOLEAN =
  BEGIN
    WITH res = c.lockwr # c.wr OR c.lockrd # c.rd DO
      IF seleDebug THEN
        Debug.Out(FN("%s : %s Ready %s : lockwr = %s wr = %s ; lockrd = %s rd = %s -> res = %s",
                    TA{DebugClosure(cl), c.nm, 
                       cl.name,
                       Int(c.lockwr), Int(c.wr),
                       Int(c.lockrd), Int(c.rd),
                       Bool(res)}
                       ))
      END;
      RETURN res
    END
  END Ready;
  
PROCEDURE Wait  (c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug THEN
      Debug.Out(F("%s : %s Wait %s",
                  DebugClosure(cl), c.nm, 
                  cl.name))
    END;
    c.waiter := cl
  END Wait;
  
PROCEDURE Unwait  (c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug THEN
      Debug.Out(F("%s : %s Unwait %s",
                  DebugClosure(cl), c.nm, 
                  cl.name))
    END;
    <*ASSERT c.waiter = NIL OR c.waiter = cl*> c.waiter := NIL
  END Unwait;


BEGIN END CspChannelOps.
