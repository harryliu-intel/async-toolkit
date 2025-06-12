GENERIC MODULE Channel(Type, CspDebug);

IMPORT CspChannel;
FROM CspChannel IMPORT ReadUpdate, WriteUpdate;
IMPORT CspChannelRep;
FROM CspChannelRep IMPORT End;

IMPORT Debug;
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT DebugClosure;
IMPORT CspCompiledScheduler1 AS Scheduler;
FROM Fmt IMPORT Int, F, Bool;
IMPORT DynamicInt;
IMPORT Mpz;
IMPORT CspSim;
IMPORT Word;

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
    dirty          : BOOLEAN;
  OVERRIDES
    makeSurrogate    := GenericMakeSurrogate;
    readSurrogate    := ReadSurrogate;
    clean            := Clean;

    getReadUpdate    := GetReadUpdate;
    applyWriteUpdate := ApplyWriteUpdate;
  END;

REVEAL
  (* under all conditions, the "real" channel is held at the receiving end *)
  Surrogate = T BRANDED Brand & " Surrogate" OBJECT
    target : T;

    (* lastPwr:

       pickling for remote -- this is used to keep track of what data
       has not yet been transferred to the target, so that we may include
       it in the Update.

       At any given time, what has NOT been pickled is 

       [lastPwr , wr)

    *)
    
    lastPwr        : CARDINAL;
  OVERRIDES
    unmakeSurrogate  := GenericUnmakeSurrogate;
    writeSurrogate   := WriteSurrogate;
    getWriteUpdate   := GetWriteUpdate;
    applyReadUpdate  := ApplyReadUpdate;
  END;

PROCEDURE Clean(t : T) =
  BEGIN
    t.dirty        := FALSE
  END Clean;
  
  (**********************************************************************)

PROCEDURE GenericMakeSurrogate(t : T) : CspChannel.T =
  BEGIN
    RETURN MakeSurrogate(t)
  END GenericMakeSurrogate;

PROCEDURE GenericUnmakeSurrogate(s : Surrogate) : CspChannel.T =
  BEGIN
    RETURN UnmakeSurrogate(s)
  END GenericUnmakeSurrogate;

  (**********************************************************************)

PROCEDURE MakeSurrogate(t : T) : Surrogate =
  (* making a surrogate is easy: simply duplicate the state *)
  VAR
    res := NEW(Surrogate,
               surrogate  := TRUE,       (* constant during lifetime *)

               slack      := t.slack,    (* constant during lifetime *)
               
               writer     := t.writer,   (* constant during lifetime *)
               reader     := t.reader,   (* constant during lifetime *)
               width      := t.width,    (* constant during lifetime *)

               wr         := t.wr,       (* owned by writing end *)
               writes     := t.writes,   (* owned by writing end *)
               rd         := t.rd,       (* owned by reading end *)
 
               waiter     := NIL,        (* shared *)
               selecter   := NIL,        (* shared *)
               
               lockwr     := t.lockwr,   (* private to each end *)
               lockrd     := t.lockrd,   (* private to each end *)

               locker     := t.locker,   (* private to each end *)

               surrog     := NIL,        (* NIL for Surrogate *)
               
               target     := t,          (* constant during lifetime *)
               lastPwr    := 0           (* private to writing end *)
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
    t.wr     := s.wr;
    t.writes := s.writes;
    
    IF s.waiter.fr = s.writer THEN
      <*ASSERT t.waiter.fr = s.writer OR t.waiter = NIL*>
      t.waiter := s.waiter
    END;
    IF s.selecter.fr = s.writer THEN
      <*ASSERT t.selecter.fr = s.writer OR t.selecter = NIL*>
      t.selecter := s.selecter
    END
  END WriteSurrogate;

PROCEDURE GetWriteUpdate(s : Surrogate) : WriteUpdate =
  VAR
    res := NEW(WriteUpdate,
               id := s.id,
               wr := s.wr);
  BEGIN
    res.writes := s.writes;
    IF s.lastPwr # s.wr THEN
      (* if the write pointer has been changed since last time, we need
         to take the data that has been written, and copy it into the 
         Update *)
      WITH n   = NUMBER(s.data^),
           beg = s.lastPwr,
           del = (s.wr - beg) MOD n,
           wsz = del * NWords,
           dat = NEW(REF ARRAY OF Word.T, wsz) DO
        FOR i := 0 TO del - 1 DO
          SUBARRAY(dat^, i * NWords, NWords) := s.data[(beg + i) MOD n]
        END;

        res.data := dat
      END;
           
      s.lastPwr := s.wr
    END;
    IF s.waiter.fr = s.writer THEN
      res.waiter := End.Writer
    ELSE
      res.waiter := End.Unknown
    END;
    IF s.selecter.fr = s.writer THEN
      res.selecter := End.Writer
    ELSE
      res.selecter := End.Unknown
    END;
    RETURN res
  END GetWriteUpdate;

PROCEDURE ApplyWriteUpdate(t : T; u : WriteUpdate) =
  BEGIN
    t.wr     := u.wr;
    t.writes := u.writes;

    WITH n   = NUMBER(t.data^),    (* slack + 1 *)
         wsz = NUMBER(u.data^),    (* how much data is coming over the wire *)
         del = wsz DIV NWords,     (* how many items that is *)
         lim = t.wr,               (* this is the first item not written *)
         beg = (lim - del) MOD n   (* this is the starting index *)
     DO
      FOR i := 0 TO del - 1 DO
        t.data[(beg + i) MOD n] := SUBARRAY(u.data^, i * NWords, NWords)
      END
    END;
    IF u.waiter = End.Writer THEN
      <*ASSERT t.waiter.fr = t.writer OR t.waiter = NIL*>
      t.waiter := t.writer.dummy
    END;
    IF u.selecter = End.Writer THEN
      <*ASSERT t.selecter.fr = t.writer OR t.selecter = NIL*>
      t.selecter := t.writer.dummy
    END
  END ApplyWriteUpdate;
  
PROCEDURE ReadSurrogate(t : T) =
  VAR
    s := t.surrog;
  BEGIN
    (* taking the write-end fields from the target, 
       update them in the surrogate *)
    s.rd := t.rd;
    IF t.waiter.fr = t.reader THEN
      <*ASSERT s.waiter.fr = t.reader OR s.waiter = NIL*>
      s.waiter := t.waiter
    END;
    IF t.selecter.fr = t.reader THEN
      <*ASSERT s.selecter.fr = t.reader OR s.selecter = NIL*>
      s.selecter := t.selecter
    END
  END ReadSurrogate;

PROCEDURE GetReadUpdate(t : T) : ReadUpdate =
  VAR
    res := NEW(ReadUpdate,
               id := t.id,
               rd := t.rd);
  BEGIN
    IF t.waiter.fr = t.reader THEN
      res.waiter := End.Reader
    ELSE
      res.waiter := End.Unknown
    END;
    IF t.selecter.fr = t.reader THEN
      res.selecter := End.Reader
    ELSE
      res.selecter := End.Unknown
    END;
    RETURN res
  END GetReadUpdate;

PROCEDURE ApplyReadUpdate(s : Surrogate; u : ReadUpdate) =
  BEGIN
    s.rd := u.rd;
    IF u.waiter = End.Reader THEN
      <*ASSERT s.waiter.fr = s.reader OR s.waiter = NIL*>
      s.waiter := s.reader.dummy
    END;
    IF u.selecter = End.Reader THEN
      <*ASSERT s.selecter.fr = s.reader OR s.selecter = NIL*>
      s.selecter := s.reader.dummy
    END
  END ApplyReadUpdate;
  
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
    <*ASSERT c.surrog = NIL*>  (* if there is a surrogate, use that! *)
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
    <*ASSERT c.surrog = NIL*>  (* if there is a surrogate, use that! *)
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
        Debug.Out(F("%s : %s Send unlock select : %s", DebugClosure(cl),
                    c.nm,
                    DebugClosure(c.selecter)))
      END;
      Scheduler.ScheduleWait(cl, c.selecter)
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
        Scheduler.ScheduleComm(cl, c.waiter);
        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* c.waiter.frame = cl *)
        INC(c.wr);
        INC(c.writes);
        IF c.wr = c.slack + 1 THEN c.wr := 0 END;

        IF c.surrogate AND NOT c.dirty THEN
          Scheduler.WriteDirty(c, cl);
          c.dirty := TRUE
        END;
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
      INC(c.writes);
      IF c.wr = c.slack + 1 THEN c.wr := 0 END;

      IF c.surrogate AND NOT c.dirty THEN
        Scheduler.WriteDirty(c, cl);
        c.dirty := TRUE
      END;

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
        Scheduler.ScheduleComm(cl, c.waiter);
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
  
PROCEDURE Recv(         c : T;
               VAR      x : Item;
               cl         : Process.Closure) : BOOLEAN =
  BEGIN
    IF c.selecter # NIL THEN
      Debug.Out(F("%s : %s Recv unlock select : %s",
                  DebugClosure(cl),
                  c.nm,
                  DebugClosure(c.selecter)));
      Scheduler.ScheduleWait(cl, c.selecter)
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
        IF c.surrog # NIL AND NOT c.dirty THEN
          Scheduler.ReadDirty(c, cl);
          c.dirty := TRUE
        END;

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
          Scheduler.ScheduleComm(cl, c.waiter);
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

PROCEDURE New(nm : TEXT; id, slack : CARDINAL) : Ref =
  BEGIN
    WITH res = NEW(Ref,
                   nm     := nm,
                   id     := id,
                   width  := Type.Width,
                   slack  := slack,
                   wr     := 0,
                   writes := 0,
                   rd     := slack,
                   data   := NEW(REF Buff, slack + 1)) DO
      CspSim.RegisterEdge(res);
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

PROCEDURE RecvDynamic(         c : T;
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
