GENERIC MODULE Channel(Type, CspDebug);

IMPORT CspChannel;
FROM CspChannel IMPORT ReadUpdate, WriteUpdate;
IMPORT CspChannelRep;
FROM CspChannelRep IMPORT End;

IMPORT Debug; FROM Debug IMPORT UnNil;
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT DebugClosure;
IMPORT CspCompiledScheduler1 AS Scheduler;
FROM Fmt IMPORT Int, F, Bool, FN;
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
  TA       = ARRAY OF TEXT;

REVEAL
  T = CspChannel.T BRANDED Brand OBJECT
    data           : REF Buff;       (* size slack + 1 *)
    surrog         : Surrogate := NIL;
    dirty          : BOOLEAN;
    waiter         : Process.Closure;
  OVERRIDES
    makeSurrogate    := GenericMakeSurrogate;
    readSurrogate    := ReadSurrogate;
    clean            := Clean;

    getReadUpdate    := GetReadUpdate;
    applyWriteUpdate := ApplyWriteUpdate;

    markWriter       := MarkWriter;
    markReader       := MarkReader;
    isSurrogate      := ReturnFalse;

    isNilClosure     := MIsNilClosure;
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
    isSurrogate      := ReturnTrue;
  END;

PROCEDURE ReturnFalse(<*UNUSED*>t : T) : BOOLEAN =
  BEGIN RETURN FALSE END ReturnFalse;
  
PROCEDURE ReturnTrue(<*UNUSED*>t : T) : BOOLEAN =
  BEGIN RETURN TRUE END ReturnTrue;
  
PROCEDURE Clean(t : T) =
  BEGIN t.dirty        := FALSE END Clean;
  
  (**********************************************************************)

PROCEDURE MarkWriter(t : T; frame : Process.Frame) =
  BEGIN
    t.writer := frame
  END MarkWriter;

PROCEDURE MarkReader(t : T; frame : Process.Frame) =
  BEGIN
    t.reader := frame
  END MarkReader;

PROCEDURE GenericMakeSurrogate(t : T) : CspChannel.T =
  BEGIN
    RETURN MakeSurrogate(t)
  END GenericMakeSurrogate;

PROCEDURE GenericUnmakeSurrogate(s : Surrogate) : CspChannel.T =
  BEGIN
    RETURN UnmakeSurrogate(s)
  END GenericUnmakeSurrogate;

  (**********************************************************************)

  (* here we have the various "nils" supported by the endpoints.

     In a single thread, we could just make all these NIL.

     But in multiple threads, we sometimes need to track which end of
     a channel set the waiter or selecter to NIL so that we know when
     to send this information to the scheduler on the other end of the
     channel.

     Note that in a complex system, we will have many different "nils":
     each channel type implies three different ones, and each address
     space makes another set of copies.  In a distributed implementation,
     we can't use local pointers to communicate since those have no meaning
     outside of their native address space, and we instead convert all
     the Closures to instances of type CspChannelRep.End.

     The fact that each channel type has a different set of Nils is not
     used for any real purpose except debugging.  Certainly if they get
     mixed up, something is very wrong somewhere.
  *)
  
VAR
  ReaderNil := NEW(Process.Closure, name := "**READER-NIL("&Brand&")**"); 
  WriterNil := NEW(Process.Closure, name := "**WRITER-NIL("&Brand&")**"); 
  InitiaNil := NEW(Process.Closure, name := "**INITIA-NIL("&Brand&")**");

PROCEDURE IsNilClosure(cl : Process.Closure) : BOOLEAN =
  (* check whether a selecter or waiter is "Nil" in a way that we can tell
     who set it to that value *)
  BEGIN
    <*ASSERT cl # NIL*>
    RETURN cl = ReaderNil OR cl = WriterNil OR cl = InitiaNil
  END IsNilClosure;

PROCEDURE MIsNilClosure(<*UNUSED*>t : T; cl : Process.Closure) : BOOLEAN =
  BEGIN
    RETURN IsNilClosure(cl)
  END MIsNilClosure;
  
PROCEDURE MakeSurrogate(t : T) : Surrogate =
  (* making a surrogate is easy: simply duplicate the state *)
  VAR
    res := NEW(Surrogate,

               nm         := t.nm,
               id         := t.id,
               
               surrogate  := TRUE,       (* constant during lifetime *)

               slack      := t.slack,    (* constant during lifetime *)
               
               writer     := t.writer,   (* constant during lifetime *)
               reader     := t.reader,   (* constant during lifetime *)
               width      := t.width,    (* constant during lifetime *)

               readerNil  := ReaderNil,  (* constant *)
               writerNil  := WriterNil,  (* constant *)

               wr         := t.wr,       (* owned by writing end *)
               writes     := t.writes,   (* owned by writing end *)
               rd         := t.rd,       (* owned by reading end *)
 
               waiter     := InitiaNil,  (* shared *)
               selecter   := InitiaNil,  (* shared *)
               
               lockwr     := t.lockwr,   (* private to each end *)
               lockrd     := t.lockrd,   (* private to each end *)

               locker     := t.locker,   (* private to each end *)

               surrog     := NIL,        (* NIL for Surrogate *)
               
               target     := t,          (* constant during lifetime *)
               lastPwr    := 0,          (* private to writing end *)
               data       := t.data      (* share data on same machine *)
    );
  BEGIN
    t.surrog := res;
    RETURN res
  END MakeSurrogate;

  (* updating routines in both direction s
    
     I think the trickiest part is if the waiter or selecter is NIL.

     Then what?  

     How do we propagate the NIL-ing of the waiter and selecter?
  *)

PROCEDURE WriteSurrogate(s : Surrogate) =
  VAR
    t := s.target;
  BEGIN
    (* taking the write-end fields from the surrogate, 
       update them in the target *)
    t.wr     := s.wr;
    t.writes := s.writes;
    
    IF s.waiter = WriterNil OR s.waiter.fr = s.writer THEN
      <*ASSERT t.waiter.fr = s.writer OR IsNilClosure(t.waiter)*>
      t.waiter := s.waiter
    END;
    IF s.selecter = WriterNil OR s.selecter.fr = s.writer THEN
      <*ASSERT t.selecter.fr = s.writer OR IsNilClosure(t.selecter) *>
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
    IF    s.waiter = WriterNil THEN
      res.waiter := End.WriterNil
    ELSIF s.waiter.fr = s.writer THEN
      res.waiter := End.Writer
    ELSE
      res.waiter := End.Unknown
    END;
    IF s.selecter = WriterNil THEN
      res.selecter := End.WriterNil
    ELSIF s.selecter.fr = s.writer THEN
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
      <*ASSERT t.waiter.fr = t.writer OR IsNilClosure(t.waiter)*>
      t.waiter := t.writer.dummy
    ELSIF u.waiter = End.WriterNil THEN
      t.waiter := WriterNil
    END;
    IF u.selecter = End.Writer THEN
      <*ASSERT t.selecter.fr = t.writer OR IsNilClosure(t.selecter) *>
      t.selecter := t.writer.dummy
    ELSIF u.selecter = End.WriterNil THEN
      t.selecter := WriterNil
    END
  END ApplyWriteUpdate;
  
PROCEDURE ReadSurrogate(t : T) =
  VAR
    s := t.surrog;
  BEGIN
    (* taking the write-end fields from the target, 
       update them in the surrogate *)
    s.rd := t.rd;
    IF t.waiter = ReaderNil OR t.waiter.fr = t.reader THEN
      <*ASSERT s.waiter.fr = t.reader OR IsNilClosure(s.waiter)*>
      s.waiter := t.waiter
    END;
    IF t.selecter = ReaderNil OR t.selecter.fr = t.reader THEN
      <*ASSERT s.selecter.fr = t.reader OR IsNilClosure(s.selecter)*>
      s.selecter := t.selecter
    END
  END ReadSurrogate;

PROCEDURE GetReadUpdate(t : T) : ReadUpdate =
  VAR
    res := NEW(ReadUpdate,
               id := t.id,
               rd := t.rd);
  BEGIN
    IF t.waiter = ReaderNil THEN
    ELSIF t.waiter.fr = t.reader THEN
      res.waiter := End.Reader
    ELSE
      res.waiter := End.Unknown
    END;
    IF t.selecter = ReaderNil THEN
      res.selecter := End.ReaderNil
    ELSIF t.selecter.fr = t.reader THEN
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
      <*ASSERT s.waiter.fr = s.reader OR IsNilClosure(s.waiter)*>
      s.waiter := s.reader.dummy
    ELSIF u.waiter = End.ReaderNil THEN
      s.waiter := ReaderNil
    END;
    IF u.selecter = End.Reader THEN
      <*ASSERT s.selecter.fr = s.reader OR IsNilClosure(s.selecter) *>
      s.selecter := s.reader.dummy
    ELSIF u.selecter = End.ReaderNil THEN
      s.selecter := ReaderNil
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
    <*ASSERT cl # NIL*>
    IF sendDebug THEN
      Debug.Out(F("%s : %s Send called : %s",
                  DebugClosure(cl), UnNil(c.nm),
                  ChanDebug(c)))
    END;
    
    c.data[c.wr] := x;

    IF NOT IsNilClosure(c.selecter) THEN
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

      IF IsNilClosure(c.waiter) THEN
        IF sendDebug THEN
          Debug.Out(F("%s : %s Send wait", DebugClosure(cl), UnNil(c.nm)))
        END;
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

        c.waiter := WriterNil; (* end of handshake *)
        
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
                      DebugClosure(cl), UnNil(c.nm), 
                      ChanDebug(c)))
      END;

      (* if anyone is sleeping on the channel (i.e., the channel was
         empty and the receiver got here before us), wake them up *)
      IF NOT IsNilClosure(c.waiter) THEN
        <*ASSERT c.waiter.frameId = c.reader.id*>
        IF sendDebug THEN
          Debug.Out(F("%s : %s Send schedule : %s",
                      DebugClosure(cl), c.nm, 
                      DebugClosure(c.waiter)))
        END;
        Scheduler.ScheduleComm(cl, c.waiter);
        c.waiter := WriterNil
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

    <*ASSERT IsNilClosure(c.waiter) OR c.waiter.frameId # c.reader.id*>
    
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
    WITH res = c.wr # nxtRd OR (NOT IsNilClosure(c.waiter) AND c.waiter.frameId = c.writer.id) DO
      
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
    IF NOT IsNilClosure(c.selecter) THEN
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
      
      IF c.wr = nxtRd AND IsNilClosure(c.waiter) THEN
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

        IF NOT IsNilClosure(c.waiter) THEN
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
          c.waiter := ReaderNil 
        END;

        RETURN TRUE
      END
    END
  END Recv;

PROCEDURE ChanDebug(chan : T) : TEXT =
  VAR
    waiterStr : TEXT;
  BEGIN
    IF chan = NIL THEN
      RETURN "**ChanDebug(NIL)**"
    END;

    <*ASSERT chan.waiter # NIL*>
    <*ASSERT chan.selecter # NIL*>
    
    IF IsNilClosure(chan.waiter) THEN
      waiterStr := chan.waiter.name
    ELSIF chan.waiter.frameId = chan.writer.id THEN
      waiterStr := "writer " & chan.waiter.fr.name & ":" & chan.waiter.name
    ELSIF chan.waiter.frameId = chan.reader.id THEN
      waiterStr := "reader " & chan.waiter.fr.name & ":" & chan.waiter.name
    ELSE
      Debug.Error("Interloper waiting on channel : " & chan.waiter.fr.name)
    END;
    
    RETURN FN("chan \"%s\" surrogate=%s wr=%s rd=%s waiter=%s full=%s",
              TA {UnNil(chan.nm),
                  Bool(chan.surrogate),
                  Int(chan.wr),
                  Int(chan.rd),
                  waiterStr,
                  Bool(Full(chan))
    }
    )
  END ChanDebug;

PROCEDURE New(nm : TEXT; id, slack : CARDINAL) : T =
  BEGIN
    WITH res = NEW(T,
                   nm     := nm,
                   id     := id,
                   width  := Type.Width,
                   slack  := slack,
                   wr     := 0,
                   writes := 0,
                   rd     := slack,

                   writer    := NIL, (* NIL means not connected yet *)
                   reader    := NIL, (* NIL means not connected yet *)
                   
                   waiter    := InitiaNil,  
                   selecter  := InitiaNil,  

                   readerNil := ReaderNil,
                   writerNil := WriterNil,
                   
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
