GENERIC MODULE Channel(Type);
IMPORT Debug;
IMPORT CspCompiledProcess AS Process;
IMPORT CspCompiledScheduler AS Scheduler;
FROM Fmt IMPORT Int, F, Bool;

CONST doDebug = TRUE;

PROCEDURE SendProbe(VAR c : T; cl : Process.Closure) : BOOLEAN =
  BEGIN
    RETURN c.wr # c.rd OR c.waiter = cl
  END SendProbe;
  
PROCEDURE Send(VAR      c : T;
               READONLY x : Type.T;
               cl         : Process.Closure) : BOOLEAN =
  BEGIN
    (* the buffer is always big enough to write into 
       (it's one bigger than the slack)  *)
    
    c.data[c.wr] := x;
    
    IF c.wr = c.rd THEN
      (* channel is full, we check if the other end has caught up.

         If it has, we set ourselves as waiter, ask other end to
         proceed with receive.  Receiver will schedule with us as 
         the waiter.

         If we get here, and we are scheduled as waiter, that means
         the receiver has already copied out the data and is proceeding
         to bigger and better things.  We return TRUE because the
         handshake is complete.
      *)

      IF c.waiter = NIL THEN
        IF doDebug THEN Debug.Out(Int(cl.frameId) & " Send : wait") END;
        
        <*ASSERT c.waiter = NIL OR c.waiter = cl*>
        c.waiter := cl;
        RETURN FALSE
      ELSIF c.waiter # cl THEN
        (* tell reader to proceed *)
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) &
            " Send : schedule reader " &
            Int(c.waiter.id))
        END;
        Scheduler.Schedule(c.waiter);
        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* c.waiter.frame = cl *)
        INC(c.wr);
        IF c.wr = c.slack + 1 THEN c.wr := 0 END;
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " Send/full go : " & ChanDebug(c))
        END;

        c.waiter := NIL; (* end of handshake *)
        
        RETURN TRUE
      END
    ELSE
      (* write to non-full : update write pointer *)
      INC(c.wr);
      IF c.wr = c.slack + 1 THEN c.wr := 0 END;

      IF doDebug THEN
        Debug.Out(Int(cl.frameId) & " Send go : " & ChanDebug(c))
      END;

      (* if anyone is sleeping on the channel (i.e., the channel was
         empty and the receiver got here before us), wake them up *)
      IF c.waiter # NIL THEN
        <*ASSERT c.waiter.frameId = c.reader.id*>
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " Send : schedule " &
            Int(c.waiter.frameId))
        END;
        Scheduler.Schedule(c.waiter);
        c.waiter := NIL
      END;
      
      RETURN TRUE
    END
  END Send;

  (* 
     Schedule = we know it CAN run
     Release  = we know it MAY run
  *)
  
PROCEDURE RecvProbe(VAR c : T; cl : Process.Closure) : BOOLEAN =
  VAR
    nxtRd : CARDINAL;
  BEGIN
    
    IF c.rd = c.slack THEN
      nxtRd := 0
    ELSE
      nxtRd := c.rd + 1
    END;

    (* the difference between a regular receive and a select is that
       on a select, the sender cannot know whether it will succeed or
       not after sending ... 

       so the first time we get here, the waiter is NIL.  The select
       statement will set itself as the waiter.  So far so good.

       But what about on the second evaluation of the select?

       Let's say we return TRUE if the writing end is blocked 
       on the channel, too.
    *)
    WITH res = c.wr # nxtRd OR (c.waiter # NIL AND c.waiter.frameId = c.writer.id) DO
      
      IF doDebug THEN
        Debug.Out(Int(cl.frameId) & " RecvProbe : return " & Bool(res) & " state " & ChanDebug(c))
      END;

      RETURN res
    END

  END RecvProbe;
  
PROCEDURE Recv(VAR      c : T;
               VAR      x : Type.T;
               cl         : Process.Closure) : BOOLEAN =
  BEGIN
    VAR
      nxtRd : CARDINAL;
    BEGIN
      (* just compute the next pointer value.  Use an if rather than 
         an integer divide.  (Is that really better?) *)
      IF c.rd = c.slack THEN
        nxtRd := 0
      ELSE
        nxtRd := c.rd + 1
      END;
      
      IF c.wr = nxtRd AND c.waiter = NIL THEN
        (* channel is empty -- just block *)
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " Recv : wait " & ChanDebug(c))
        END;

        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* there is something in the channel, copy it in *)
        x := c.data[nxtRd];
        c.rd := nxtRd;

        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " Recv go : " & ChanDebug(c))
        END;

        IF c.waiter # NIL THEN
          (* 
             If someone was waiting, it must have been the other end. 
             Wake them up.
          *)
          <*ASSERT c.waiter.frameId = c.writer.id *>
          IF doDebug THEN
            Debug.Out(Int(cl.frameId) &
              " Recv : schedule " &
              Int(c.waiter.frameId)) END;
          Scheduler.Schedule(c.waiter);
        END;

        RETURN TRUE
      END
    END
  END Recv;

PROCEDURE ChanDebug(READONLY chan : T) : TEXT =
  VAR
    waiterStr : TEXT;
  BEGIN
    IF chan.waiter = NIL THEN
      waiterStr := "NIL"
    ELSIF chan.waiter.frameId = chan.writer.id THEN
      waiterStr := "writer"
    ELSIF chan.waiter.frameId = chan.reader.id THEN
      waiterStr := "reader"
    ELSE
      Debug.Error("Interloper waiting on channel")
    END;
    
    RETURN F("chan \"%s\" wr=%s rd=%s waiter=%s",
             chan.nm,
             Int(chan.wr),
             Int(chan.rd),
             waiterStr
    )
  END ChanDebug;

BEGIN END Channel.
