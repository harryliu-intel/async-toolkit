GENERIC MODULE CspChannelOps(CspDebug);
IMPORT CspChannelRep;
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT DebugClosure;
IMPORT Debug;
FROM Fmt IMPORT Int, F, FN, Bool;

CONST sendDebug = CspDebug.DebugSend;
CONST recvDebug = CspDebug.DebugRecv;
CONST probDebug = CspDebug.DebugProbe;
CONST seleDebug = CspDebug.DebugSelect;
CONST lockDebug = CspDebug.DebugLock;

TYPE TA = ARRAY OF TEXT;

PROCEDURE Lock  (c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug OR lockDebug THEN
      Debug.Out(F("%s : %s Lock %s",
                  DebugClosure(cl), c.nm, 
                  cl.name));
      <*ASSERT c.locker = NIL*>
      c.locker := cl
    END;
    c.lockwr := c.wr; c.lockrd := c.rd
  END Lock;
  
PROCEDURE Unlock(c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug OR lockDebug THEN
      Debug.Out(F("%s : %s Unlock %s",
                  DebugClosure(cl), c.nm, 
                  cl.name));
      <*ASSERT c.locker # NIL*>
      <*ASSERT c.locker.fr = cl.fr*>
      c.locker := NIL
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
    c.selecter := cl
  END Wait;
  
PROCEDURE Unwait  (c : T; cl : Process.Closure) =
  BEGIN
    IF seleDebug THEN
      Debug.Out(F("%s : %s Unwait %s",
                  DebugClosure(cl), c.nm, 
                  cl.name))
    END;
    <*ASSERT c.selecter = NIL OR c.selecter.fr = cl.fr*> c.selecter := NIL
  END Unwait;

BEGIN END CspChannelOps.
