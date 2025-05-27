GENERIC MODULE Channel(Type, CspDebug);

IMPORT CspChannel;
IMPORT CspChannelRep;

IMPORT Debug;
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT DebugClosure;
IMPORT CspCompiledScheduler1 AS Scheduler;
FROM Fmt IMPORT Int, F, Bool;
IMPORT DynamicInt;
IMPORT Mpz;
IMPORT CspPortObject;

CONST sendDebug = CspDebug.DebugSend;
CONST recvDebug = CspDebug.DebugRecv;
CONST probDebug = CspDebug.DebugProbe;
(*CONST seleDebug = CspDebug.DebugSelect;*)

TYPE
  Buff     = ARRAY OF Item;         

REVEAL
  T = CspChannel.T BRANDED Brand OBJECT
    data           : REF Buff;       (* size slack + 1 *)
    surrog         : Surrogate := NIL;
  OVERRIDES
    makeSurrogate := MakeGenericSurrogate;
  END;

REVEAL
  (* under all conditions, the "real" channel is held at the receiving end *)
  Surrogate = T BRANDED Brand & " Surrogate" OBJECT
    target : T;
  END;

  (**********************************************************************)

PROCEDURE MakeGenericSurrogate(t : T) : CspPortObject.T =
  BEGIN
    RETURN MakeSurrogate(t)
  END MakeGenericSurrogate;

PROCEDURE MakeSurrogate(t : T) : Surrogate =
  (* making a surrogate is easy: simply duplicate the state *)
  VAR
    res := NEW(Surrogate,
               surrogate  := TRUE,       (* constant during lifetime *)

               slack      := t.slack,    (* constant during lifetime *)
               
               writer     := t.writer,   (* constant during lifetime *)
               reader     := t.reader,   (* constant during lifetime *)

               wr         := t.wr,       (* owned by writing end *)
               rd         := t.rd,       (* owned by reading end *)
 
               waiter     := NIL,        (* shared *)
               selecter   := NIL,        (* shared *)
               
               lockwr     := t.lockwr,   (* private to each end *)
               lockrd     := t.lockrd,   (* private to each end *)

               locker     := t.locker,   (* private to each end *)
               
               target     := t           (* constant during lifetime *)
    );
  BEGIN
    t.surrog := res;
    RETURN res
  END MakeSurrogate;

  (* updating routines in both direction s*)

PROCEDURE WriteSurrogate(s : Surrogate) =
  VAR
    t := s.target;
  BEGIN
    (* taking the write-end fields from the surrogate, 
       update them in the target *)
    t.wr := s.wr;
    IF s.waiter.fr = s.writer THEN
      <*ASSERT t.waiter.fr = s.writer OR t.waiter = NIL*>
      t.waiter := s.waiter
    END;
    IF s.selecter.fr = s.writer THEN
      <*ASSERT t.selecter.fr = s.writer OR t.selecter = NIL*>
      t.selecter := s.selecter
    END
  END WriteSurrogate;
  
PROCEDURE ReadSurrogate(t : T) =
  VAR
    s := t.surrog;
  BEGIN
    (* taking the write-end fields from the target, 
       update them in the surrogate *)
    s.rd := t.rd;
    IF t.waiter.fr = t.reader THEN
      <*ASSERT s.waiter.fr = t.reader OR s.waiter = NIL*>
      t.waiter := s.waiter
    END;
    IF t.selecter.fr = t.reader THEN
      <*ASSERT s.selecter.fr = t.reader OR s.selecter = NIL*>
      s.selecter := t.selecter
    END
  END ReadSurrogate;
  
PROCEDURE UnmakeSurrogate(s : Surrogate) : T =
  VAR
    t := s.target;
  BEGIN
    WriteSurrogate(s); (* do a final write before dumping the surrogate *)
    s.target := NIL;
    t.surrog := NIL;
    RETURN t
  END UnmakeSurrogate;

(**********************************************************************)
    
PROCEDURE SendProbe(c : T; cl : Process.Closure) : BOOLEAN =
  BEGIN
    WITH res = c.wr # c.rd OR c.waiter = cl DO
      IF probDebug THEN
        Debug.Out(F("%s : %s SendProbe : return %s state %s",
                    Int(cl.frameId), c.nm, 
                    Bool(res),
                    ChanDebug(c)))
      END;
      RETURN res
    END
  END SendProbe;

PROCEDURE Full(c : T) : BOOLEAN =
  BEGIN
    RETURN c.wr = c.rd
  END Full;
  
PROCEDURE Send(         c : T;
               READONLY x : Item;
               cl         : Process.Closure) : BOOLEAN =
  BEGIN
    (* the buffer is always big enough to write into 
       (it's one bigger than the slack)  *)
    IF sendDebug THEN
      Debug.Out(F("%s : %s Send called : %s",
                  DebugClosure(cl), c.nm,
                  ChanDebug(c)))
    END;
    
    c.data[c.wr] := x;

    IF c.selecter # NIL THEN
      IF sendDebug THEN
        Debug.Out(F("%s : %s Send unlock select : %s", DebugClosure(cl), c.nm, DebugClosure(c.selecter)))
      END;
     Scheduler.ScheduleOther(cl, c.selecter)
    END;
    
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
        IF sendDebug THEN
          Debug.Out(F("%s : %s Send wait", DebugClosure(cl), c.nm))
        END;
        
        <*ASSERT c.waiter = NIL OR c.waiter = cl*>
        c.waiter := cl;
        RETURN FALSE
      ELSIF c.waiter # cl THEN
        (* tell reader to proceed *)
        IF sendDebug THEN
          Debug.Out(
              F("%s : %s Send schedule reader %s",
                DebugClosure(cl), c.nm, 
                DebugClosure(c.waiter))
          )
        END;
        Scheduler.ScheduleOther(cl, c.waiter);
        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* c.waiter.frame = cl *)
        INC(c.wr);
        IF c.wr = c.slack + 1 THEN c.wr := 0 END;
        IF sendDebug THEN
          Debug.Out(F("%s : %s Send/full go : %s",
                      DebugClosure(cl), c.nm, 
                      ChanDebug(c)))
        END;

        c.waiter := NIL; (* end of handshake *)
        
        RETURN TRUE
      END
    ELSE
      (* write to non-full : update write pointer *)
      INC(c.wr);
      IF c.wr = c.slack + 1 THEN c.wr := 0 END;

      IF sendDebug THEN
          Debug.Out(F("%s : %s Send go : %s",
                      DebugClosure(cl), c.nm, 
                      ChanDebug(c)))
      END;

      (* if anyone is sleeping on the channel (i.e., the channel was
         empty and the receiver got here before us), wake them up *)
      IF c.waiter # NIL THEN
        <*ASSERT c.waiter.frameId = c.reader.id*>
        IF sendDebug THEN
          Debug.Out(F("%s : %s Send schedule : %s",
                      DebugClosure(cl), c.nm, 
                      DebugClosure(c.waiter)))
        END;
        Scheduler.ScheduleOther(cl, c.waiter);
        c.waiter := NIL
      END;
      
      RETURN TRUE
    END
  END Send;

  (* 
     Schedule = we know it CAN run
     Release  = we know it MAY run
  *)

PROCEDURE RecvProbe(c : T; cl : Process.Closure) : BOOLEAN =
  VAR
    nxtRd : CARDINAL;
  BEGIN
    IF probDebug THEN
      Debug.Out(F("%s : %s RecvProbe %s",
                  DebugClosure(cl), c.nm, 
                  ChanDebug(c)))
    END;

    <*ASSERT c.waiter = NIL OR c.waiter.frameId # c.reader.id*>
    
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
      
      IF probDebug THEN
        Debug.Out(F("%s : %s RecvProbe : return %s state %s",
                    DebugClosure(cl), c.nm, 
                    Bool(res),
                    ChanDebug(c)))
      END;

      RETURN res
    END
  END RecvProbe;
  
PROCEDURE Recv(     c : T;
               VAR      x : Item;
               cl         : Process.Closure) : BOOLEAN =
  BEGIN
    IF c.selecter # NIL THEN
      Debug.Out(F("%s : %s Recv unlock select : %s", DebugClosure(cl), c.nm, DebugClosure(c.selecter)));
      Scheduler.ScheduleOther(cl, c.selecter)
    END;
    
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

      IF c.writer = NIL THEN
        Debug.Error("Receiving on an unconnected channel : " & c.nm)
      END;
      
      IF c.wr = nxtRd AND c.waiter = NIL THEN
        (* channel is empty -- just block *)
        IF recvDebug THEN
          Debug.Out(F("%s : %s Recv : wait %s",
                      DebugClosure(cl), c.nm, 
                      ChanDebug(c)))
        END;

        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* there is something in the channel, copy it in *)
        x := c.data[nxtRd];
        c.rd := nxtRd;

        IF recvDebug THEN
          Debug.Out(F("%s : %s Recv go : %s",
                      DebugClosure(cl), c.nm, 
                      ChanDebug(c)))
        END;

        IF c.waiter # NIL THEN
          (* 
             If someone was waiting, it must have been the sender, 
             waiting for slack. 

             Wake him up.
          *)
          <*ASSERT c.waiter.frameId = c.writer.id *>
          IF recvDebug THEN
            Debug.Out(F("%s : %s Recv : schedule %s",
                        DebugClosure(cl), c.nm, 
                        DebugClosure(c.waiter)))
          END;
          Scheduler.ScheduleOther(cl, c.waiter);
          c.waiter := NIL 
        END;

        RETURN TRUE
      END
    END
  END Recv;

PROCEDURE ChanDebug(chan : T) : TEXT =
  VAR
    waiterStr : TEXT;
  BEGIN
    IF chan.waiter = NIL THEN
      waiterStr := "NIL"
    ELSIF chan.waiter.frameId = chan.writer.id THEN
      waiterStr := "writer " & chan.waiter.fr.name & ":" & chan.waiter.name
    ELSIF chan.waiter.frameId = chan.reader.id THEN
      waiterStr := "reader " & chan.waiter.fr.name & ":" & chan.waiter.name
    ELSE
      Debug.Error("Interloper waiting on channel : " & chan.waiter.fr.name)
    END;
    
    RETURN F("chan \"%s\" wr=%s rd=%s waiter=%s full=%s",
             chan.nm,
             Int(chan.wr),
             Int(chan.rd),
             waiterStr,
             Bool(Full(chan))
    )
  END ChanDebug;

PROCEDURE New(nm : TEXT; slack : CARDINAL) : Ref =
  BEGIN
    WITH res = NEW(Ref,
                   nm    := nm,
                   slack := slack,
                   wr    := 0,
                   rd    := slack,
                   data  := NEW(REF Buff, slack + 1)) DO
      Scheduler.RegisterEdge(res);
      RETURN res
    END
  END New;

PROCEDURE SendNative(c     : T;
                     x     : INTEGER;
                     cl    : Process.Closure) : BOOLEAN =
  VAR
    toSend := Item { 0, .. };
  BEGIN
    toSend[0] := x;
    IF x < 0 THEN
      <*NOWARN*>FOR i := 1 TO LAST(toSend) DO
        toSend[i] := -1
      END
    END;
    RETURN Send(c, toSend, cl)
  END SendNative;

PROCEDURE SendDynamic(c : T;
                      x     : DynamicInt.T;
                      cl    : Process.Closure) : BOOLEAN =
  VAR
    toSend : Item;
  BEGIN
    Mpz.Export(toSend, x);
    RETURN Send(c, toSend, cl)
  END SendDynamic;

PROCEDURE RecvNative(     c : T;
                     VAR      x : INTEGER;
                     cl         : Process.Closure) : BOOLEAN =
  VAR
    toRecv : Item;
  BEGIN
    IF Recv(c, toRecv, cl) THEN
      x := toRecv[0];
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END RecvNative;

PROCEDURE RecvDynamic(     c : T;
                      x(*OUT*)   : DynamicInt.T;
                      cl         : Process.Closure) : BOOLEAN =
  VAR
    toRecv : Item;
  BEGIN
    IF Recv(c, toRecv, cl) THEN
      Mpz.Import(x, toRecv);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END RecvDynamic;

BEGIN END Channel.
