MODULE first EXPORTS Main;
IMPORT Word;
IMPORT IO;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT Debug;

CONST
  doDebug = FALSE ;
      
CONST
  Slack = 1; (* JavaCSP has slack 1, minimum *)
  Width = 32;
(*  N     = 2;*)
  N     = 4 * 1024;
    
  
TYPE
  IntBuff =  ARRAY [0..Slack] OF INTEGER; (* N + 1 spaces *)

  IntChan = RECORD
    (* representing a CSP channel *)
    nm          : TEXT;
    slack       : CARDINAL;
    wr, rd      : CARDINAL;     (* write, read pointers *)
    data        : REF IntBuff;  (* indirect is so can be part of several IntChans *)

    waiter      : ProcClosure;

    writer, reader : ProcFrame; (* used ONLY for assertions! *)
  END;

  ProcFrame = OBJECT
    (* representing a CSP process instance *)
    id : CARDINAL;
  END;

  ProcClosure = OBJECT
    (* representing an executable fragment of a CSP process instance *)
    id        : CARDINAL;
    frameId   : CARDINAL; (* copy of frame id *)
    scheduled : Word.T := -1;   (* last time it was scheduled *)
  METHODS
    run();
  END;

(**********************************************************************)
  
  (* 
     channel buf rep

     empty:

             wr (next write)
             |
     .   .   .   .   .   .   .
         |
         rd (last read)

     non-empty:

                     wr (next write)
                     |
     .   .   .   X   .   .   .
             | 
             rd (last read)


     full:

                     wr (next write)
                     |
     X   X   X   X   .   X   X
                     |
                     rd (last read)
     
  *)

VAR nextId      : CARDINAL := 0;
    nextFrameId : CARDINAL := 0;

PROCEDURE NextId() : CARDINAL =
  BEGIN
    TRY
      RETURN nextId
    FINALLY
      INC(nextId)
    END
  END NextId;

PROCEDURE NextFrameId() : CARDINAL =
  BEGIN
    TRY
      RETURN nextFrameId
    FINALLY
      INC(nextFrameId)
    END
  END NextFrameId;

<*UNUSED*>
PROCEDURE IntSendProbe(VAR c : IntChan; cl : ProcClosure) : BOOLEAN =
  BEGIN
    RETURN c.wr # c.rd OR c.waiter = cl
  END IntSendProbe;
  
PROCEDURE IntSend(VAR      c : IntChan;
                  READONLY x : Word.T;
                  cl         : ProcClosure) : BOOLEAN =
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
        IF doDebug THEN Debug.Out(Int(cl.frameId) & " IntSend : wait") END;
        
        <*ASSERT c.waiter = NIL OR c.waiter = cl*>
        c.waiter := cl;
        RETURN FALSE
      ELSIF c.waiter # cl THEN
        (* tell reader to proceed *)
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " IntSend : schedule reader " & Int(c.waiter.id))
        END;
        Schedule(c.waiter);
        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* c.waiter.frame = cl *)
        INC(c.wr);
        IF c.wr = c.slack + 1 THEN c.wr := 0 END;
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " IntSend/full go : " & ChanDebug(c))
        END;

        c.waiter := NIL; (* end of handshake *)
        
        RETURN TRUE
      END
    ELSE
      (* write to non-full : update write pointer *)
      INC(c.wr);
      IF c.wr = c.slack + 1 THEN c.wr := 0 END;

      IF doDebug THEN
        Debug.Out(Int(cl.frameId) & " IntSend go : " & ChanDebug(c))
      END;

      (* if anyone is sleeping on the channel (i.e., the channel was
         empty and the receiver got here before us), wake them up *)
      IF c.waiter # NIL THEN
        <*ASSERT c.waiter.frameId = c.reader.id*>
        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " IntSend : schedule " &
            Int(c.waiter.frameId))
        END;
        Schedule(c.waiter);
        c.waiter := NIL
      END;
      
      RETURN TRUE
    END
  END IntSend;

  (* 
     Schedule = we know it CAN run
     Release  = we know it MAY run
  *)
  
PROCEDURE IntRecvProbe(VAR c : IntChan; cl : ProcClosure) : BOOLEAN =
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
        Debug.Out(Int(cl.frameId) & " IntRecvProbe : return " & Fmt.Bool(res) & " state " & ChanDebug(c))
      END;

      RETURN res
    END

  END IntRecvProbe;
  
PROCEDURE IntRecv(VAR      c : IntChan;
                  VAR      x : Word.T;
                  cl         : ProcClosure) : BOOLEAN =
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
          Debug.Out(Int(cl.frameId) & " IntRecv : wait " & ChanDebug(c))
        END;

        c.waiter := cl;
        RETURN FALSE
      ELSE
        (* there is something in the channel, copy it in *)
        x := c.data[nxtRd];
        c.rd := nxtRd;

        IF doDebug THEN
          Debug.Out(Int(cl.frameId) & " IntRecv go : " & ChanDebug(c))
        END;

        IF c.waiter # NIL THEN
          (* 
             If someone was waiting, it must have been the other end. 
             Wake them up.
          *)
          <*ASSERT c.waiter.frameId = c.writer.id *>
          IF doDebug THEN Debug.Out(Int(cl.frameId) & " IntRecv : schedule " & Int(c.waiter.frameId)) END;
          Schedule(c.waiter);
        END;

        RETURN TRUE
      END
    END
  END IntRecv;

PROCEDURE ChanDebug(READONLY chan : IntChan) : TEXT =
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


(**********************************************************************

IMPLEMENT first :

module first;

// cosimulate first.SYSTEM {csp,subcells}

int WIDTH = 32;
int N     = 4 * 1024;

define PROC(bool DOSTART)(bd(WIDTH) -L, +R)
{

  csp {
   int(WIDTH) i;
   [ DOSTART -> i=0; R!0 [] ~DOSTART -> skip ];
   
   *[ int(WIDTH) x;
      L?x ;

      [ DOSTART -> print("i = " + i + " x = " + x); i++ [] ~DOSTART -> skip ];
      
      y = x + 13;
      z = (y * 16807) % 2147483647;
      
      R!z
    ]
  }
}

define SYSTEM()()
{
  subcells {
    bd(WIDTH) chan[0..N-1];
  
    <i:N: PROC(i == 0) p[i] (chan[i],chan[(i+1)%N]); >
  } 
}

TYPE
  FirstFrame = ProcFrame OBJECT
    L, R          : REF IntChan;     (* interface *)
    i, x, y, z    : Word.T;          (* locals *)
    c0, c1, c2    : FirstClosure;    (* closures *)
  END;

  FirstBlock = PROCEDURE (cl : FirstClosure) : BOOLEAN;
  (* representing a fragment of CSP process text *)

  FirstClosure = ProcClosure OBJECT
    frame  : FirstFrame;  (* reference to process locals *)
    block  : FirstBlock;  (* program text of block *)
  OVERRIDES
    run := RunFirst;
  END;

PROCEDURE RunFirst(cl : FirstClosure) =
  BEGIN
    EVAL cl.block(cl)
  END RunFirst;
  
PROCEDURE First0_0(cl : FirstClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN Debug.Out("First0_0 : " & Int(cl.frame.id)) END;
    WITH frame = cl.frame DO
      frame.i := 0; (* ok, but maybe not really *)
      IF NOT IntSend(frame.R^, 0, cl) THEN
        RETURN FALSE
      END;
      
      Release(frame.c1);
      RETURN TRUE
    END
  END First0_0;

PROCEDURE First0_1(cl : FirstClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN Debug.Out("First0_1 : " & Int(cl.frame.id)) END;
    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L^, frame.x, cl) THEN
        RETURN FALSE
      END;
      
      IO.Put("i = " & Fmt.Int(frame.i) & " x = " & Fmt.Int(frame.x) & "\n");
      INC(frame.i);

      frame.y := Word.Plus(frame.x, 13);

      frame.z := Word.Mod(Word.Times(frame.y, 16807), 2147483647);
      (* this isn't quite right with signedness, is it? *)

      Release(frame.c2);
      RETURN TRUE
    END
  END First0_1;

PROCEDURE First1_1(cl : FirstClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN Debug.Out("First1_1 : " & Int(cl.frame.id)) END;
    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L^, frame.x, cl) THEN
        RETURN FALSE
      END;

      frame.y := Word.Plus(frame.x, 13);

      frame.z := Word.Mod(Word.Times(frame.y, 16807), 2147483647);
      (* this isn't quite right with signedness, is it? *)

      Release(frame.c2);
      RETURN TRUE
    END
  END First1_1;

PROCEDURE First_2(cl : FirstClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN Debug.Out("Proc2 : " & Int(cl.frame.id)) END;
    WITH frame = cl.frame DO
      CONST Mask = Word.Minus(Word.Shift(1, Width), 1);
      BEGIN
        IF NOT IntSend(frame.R^, Word.And(frame.z, Mask), cl) THEN
          RETURN FALSE
        END
      END;
      
      Release(frame.c1);
      RETURN TRUE
    END
  END First_2;

PROCEDURE NewIntChan(nm : TEXT) : REF IntChan =
  BEGIN
    RETURN NEW(REF IntChan,
               nm    := nm,
               slack := Slack,
               wr    := 0,
               rd    := Slack,
               data  := NEW(REF IntBuff))
  END NewIntChan;

PROCEDURE BuildFirst0(L, R : REF IntChan) =
  BEGIN
    WITH frame = NEW(FirstFrame,

                     id := NextFrameId(),
                     
                     (* CSP semantics *)
                     i := 0, x := 0, y := 0, z := 0,

                     L := L,
                     R := R
      )          DO

      <*ASSERT L.reader = NIL*>
      L.reader := frame;

      <*ASSERT R.writer = NIL*>
      R.writer := frame;

      frame.c0 := NEW(FirstClosure,
                      id    := NextId(),
                      frameId := frame.id,
                      frame := frame,
                      block := First0_0);
      
      frame.c1 := NEW(FirstClosure,
                      id    := NextId(),
                      frameId := frame.id,
                      frame := frame,
                      block := First0_1);
      
      frame.c2 := NEW(FirstClosure,
                      id    := NextId(),
                      frameId := frame.id,
                      frame := frame,
                      block := First_2);

      Schedule(frame.c0) (* load initial block *)
    END
  END BuildFirst0;

PROCEDURE BuildFirst1(L, R : REF IntChan) =
  BEGIN
    WITH frame = NEW(FirstFrame,

                     id := NextFrameId(),

                     (* CSP semantics *)
                     i := 0, x := 0, y := 0, z := 0,

                     L := L,
                     R := R
      )          DO

      <*ASSERT L.reader = NIL*>
      L.reader := frame;

      <*ASSERT R.writer = NIL*>
      R.writer := frame;
      
      frame.c1 := NEW(FirstClosure,
                      id    := NextId(),
                      frameId := frame.id,
                      frame := frame,
                      block := First1_1);
      
      frame.c2 := NEW(FirstClosure,
                      id    := NextId(),
                      frameId := frame.id,
                      frame := frame,
                      block := First_2);

      Schedule(frame.c1) (* load initial block *)
    END
  END BuildFirst1;

PROCEDURE BuildFirstSim() =
  VAR
    chan         := NEW(REF ARRAY OF REF IntChan, N);
  BEGIN

    (* init *)
    FOR i := 0 TO N - 1 DO
      chan[i] := NewIntChan("chan[" & Fmt.Int(i) & "]")
    END;
    
    FOR i := 0 TO N - 1 DO
      IF i = 0 THEN
        BuildFirst0(chan[i], chan[(i + 1) MOD N])
      ELSE
        BuildFirst1(chan[i], chan[(i + 1) MOD N])
      END
    END
  END BuildFirstSim;

(**********************************************************************

IMPLEMENT select :

module select;

// cosimulate first.SYSTEM {csp,subcells}

int WIDTH = 32;
int N     = 4 * 1024;

define DRIVE()(bd(WIDTH) +R0, +R1, -L)
{
  csp {
    int x;
    *[ R0!0 ; L?x ; R1!1 ; L?x ]
  }
}

define SELECT()(bd(WIDTH) -L0, -L1, +R)
{

   csp {
     int i, x;
     
     *[[  #L0 -> L0?x
       [] #L1 -> L1?x
       ];
       #[ i % 10000 == 0 -> print ("i/10000 = " + i/10000 + " x = " + x) ];
       i++;
       R!x
      ]
   }
}

define SYSTEM()()
{
  subcells {
    bd(WIDTH) c0, c1, cx;
  
    DRIVE() drive(c0, c1, cx);
    SELECT() select(c0, c1, cx);
  } 
}



**********************************************************************)


TYPE
  DriveFrame = ProcFrame OBJECT
    R0, R1, L : REF IntChan;
    x         : Word.T;
    c0, c1, c2, c3 : DriveClosure;
  END;

  DriveBlock = PROCEDURE (cl : DriveClosure) : BOOLEAN;

  DriveClosure = ProcClosure OBJECT
    frame : DriveFrame;
    block : DriveBlock;
  OVERRIDES
    run := RunDrive;
  END;
  
PROCEDURE RunDrive(cl : DriveClosure) =
  BEGIN
    EVAL cl.block(cl)
  END RunDrive;

PROCEDURE Drive_0(cl : DriveClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF NOT IntSend(frame.R0^, 0, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c1);
      RETURN TRUE
    END
  END Drive_0;

PROCEDURE Drive_1(cl : DriveClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L^, frame.x, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c2);
      RETURN TRUE
    END
  END Drive_1;

PROCEDURE Drive_2(cl : DriveClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF NOT IntSend(frame.R1^, 1, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c3);
      RETURN TRUE
    END
  END Drive_2;

PROCEDURE Drive_3(cl : DriveClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L^, frame.x, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c0);
      RETURN TRUE
    END
  END Drive_3;

PROCEDURE BuildDrive(R0, R1, L : REF IntChan) =
  BEGIN
    WITH frame = NEW(DriveFrame,

                     id := NextFrameId(),

                     x := 0,

                     R0 := R0, R1 := R1, L := L)
     DO
      L.reader := frame;
      R0.writer := frame;
      R1.writer := frame;

      frame.c0 := NEW(DriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Drive_0);
      frame.c1 := NEW(DriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Drive_1);
      frame.c2 := NEW(DriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Drive_2);
      frame.c3:= NEW(DriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Drive_3);

      Schedule(frame.c0)
    END
  END BuildDrive;

TYPE
  SelectFrame = ProcFrame OBJECT
    L0, L1, R          : REF IntChan;
    x, i               : Word.T;
    c0, c1, c2, c3, c4 : SelectClosure;
  END;

  SelectBlock = PROCEDURE (cl : SelectClosure) : BOOLEAN;

  SelectClosure = ProcClosure OBJECT
    frame : SelectFrame;
    block : SelectBlock;
  OVERRIDES
    run := RunSelect;
  END;

PROCEDURE RunSelect(cl : SelectClosure) =
  BEGIN
    EVAL cl.block(cl)
  END RunSelect;

PROCEDURE Select_0(cl : SelectClosure) : BOOLEAN =
  BEGIN
    (* this is the selection statement *)
    IF doDebug THEN
      Debug.Out("Select_0 " & Fmt.Int(cl.frameId))
    END;
    
    WITH frame = cl.frame DO

      PROCEDURE Unwait() =
        BEGIN
          IF frame.L0.waiter = cl THEN
            frame.L0.waiter := NIL
          END;
          IF frame.L1.waiter = cl THEN
            frame.L1.waiter := NIL
          END;
        END Unwait;

      PROCEDURE Wait() =
        BEGIN
          frame.L0.waiter := cl;
          frame.L1.waiter := cl
        END Wait;
        
      BEGIN        
        IF    IntRecvProbe(frame.L0^, cl) THEN
          Unwait();
          Release(frame.c1);
          RETURN TRUE
        ELSIF IntRecvProbe(frame.L1^, cl) THEN
          Unwait();
          Release(frame.c2);
          RETURN TRUE
        ELSE
          Wait();
          RETURN FALSE
        END
      END
    END
  END Select_0;

PROCEDURE Select_1(cl : SelectClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Select_1 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L0^, frame.x, cl) THEN
        RETURN FALSE
      END;
      
      Release(frame.c3);
      RETURN TRUE
    END
  END Select_1;

PROCEDURE Select_2(cl : SelectClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Select_2 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L1^, frame.x, cl) THEN
        RETURN FALSE
      END;
      
      Release(frame.c3);
      RETURN TRUE
    END
  END Select_2;

PROCEDURE Select_3(cl : SelectClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF frame.i MOD 10000 = 0 THEN
        IO.Put(" i/10000 = " & Fmt.Int(frame.i DIV 10000) & " x = " & Fmt.Int(frame.x) & "\n")
      END;
      INC(frame.i);
      Release(frame.c4);

      RETURN TRUE
    END
  END Select_3;

PROCEDURE Select_4(cl : SelectClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF NOT IntSend(frame.R^, frame.x, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c0);

      RETURN TRUE
    END
  END Select_4;

PROCEDURE BuildSelect(L0, L1, R : REF IntChan) =
  BEGIN
    WITH frame = NEW(SelectFrame,
                     id    := NextFrameId(),

                     x := 0, i := 0,
                     
                     L0 := L0, L1 := L1, R := R) DO
      L0.reader := frame;
      L1.reader := frame;
      R.writer := frame;

      frame.c0 := NEW(SelectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Select_0);
      frame.c1 := NEW(SelectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Select_1);
      frame.c2 := NEW(SelectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Select_2);
      frame.c3 := NEW(SelectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Select_3);
      frame.c4 := NEW(SelectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Select_4);

      Schedule(frame.c0)
    END
  END BuildSelect;

PROCEDURE BuildSelectSim() =
  VAR
    c0 := NewIntChan("c0");
    c1 := NewIntChan("c1");
    cx := NewIntChan("cx");
  BEGIN
    BuildDrive(c0, c1, cx);
    BuildSelect(c0, c1, cx)
  END BuildSelectSim;
 
(**********************************************************************
IMPLEMENT parallel :

module parallel;

// cosimulate first.SYSTEM {csp,subcells}

int WIDTH = 32;
int N     = 4 * 1024;

define PDRIVE()(bd(WIDTH) +R0, +R1, -L)
{
  csp {
    int x;
    *[ (R0!0, R1!1) ; L?x ; L?x ]
  }
}

define PSELECT()(bd(WIDTH) -L0, -L1, +R)
{

   csp {
     int i, x;
     
     *[[  #L0 -> L0?x
        : #L1 -> L1?x
       ];
       #[ i % 10000 == 0 -> print ("i/10000 = " + i/10000 + " x = " + x) ];
       i++;
       R!x
      ]
   }
}

define SYSTEM()()
{
  subcells {
    bd(WIDTH) c0, c1, cx;
  
    PDRIVE() pdrive(c0, c1, cx);
    PSELECT() pselect(c0, c1, cx);
  } 
}


**********************************************************************)

TYPE
  PdriveFrame = ProcFrame OBJECT
    R0, R1, L : REF IntChan;
    x         : Word.T;
    c00, c0, c1, c2, c3 : PdriveClosure;
    c00fork   : CARDINAL;
  END;

  PdriveBlock = PROCEDURE (cl : PdriveClosure) : BOOLEAN;

  PdriveClosure = ProcClosure OBJECT
    frame : PdriveFrame;
    block : PdriveBlock;
  OVERRIDES
    run := RunPdrive;
  END;
  
PROCEDURE RunPdrive(cl : PdriveClosure) =
  BEGIN
    EVAL cl.block(cl)
  END RunPdrive;

PROCEDURE Pdrive_0(cl : PdriveClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Pdrive_0 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntSend(frame.R0^, 0, cl) THEN
        RETURN FALSE
      END;
      DEC(cl.frame.c00fork);
      IF cl.frame.c00fork = 0 THEN
        Release(frame.c2)
      END;
      RETURN TRUE
    END
  END Pdrive_0;

PROCEDURE Pdrive_1(cl : PdriveClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Pdrive_1 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntSend(frame.R1^, 1, cl) THEN
        RETURN FALSE
      END;
      DEC(cl.frame.c00fork);
      IF cl.frame.c00fork = 0 THEN
        Release(frame.c2)
      END;
      RETURN TRUE
    END
  END Pdrive_1;

PROCEDURE Pdrive_2(cl : PdriveClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Pdrive_2 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L^, frame.x, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c3);
      RETURN TRUE
    END
  END Pdrive_2;

PROCEDURE Pdrive_3(cl : PdriveClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Pdrive_3 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L^, frame.x, cl) THEN
        RETURN FALSE
      END;

      (* the fork *)
      frame.c00fork := 2;
      Release(frame.c0);
      Release(frame.c1);

      RETURN TRUE
    END
  END Pdrive_3;

PROCEDURE BuildPdrive(R0, R1, L : REF IntChan) =
  BEGIN
    WITH frame = NEW(PdriveFrame,

                     id := NextFrameId(),

                     x := 0,

                     R0 := R0, R1 := R1, L := L,

                     c00fork := 0)
     DO
      L.reader := frame;
      R0.writer := frame;
      R1.writer := frame;

      
      frame.c0 := NEW(PdriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pdrive_0);
      frame.c1 := NEW(PdriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pdrive_1);
      frame.c2 := NEW(PdriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pdrive_2);
      frame.c3:= NEW(PdriveClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pdrive_3);

      frame.c00fork := 2;
      Schedule(frame.c0);
      Schedule(frame.c1);
    END
  END BuildPdrive;

TYPE
  PselectFrame = ProcFrame OBJECT
    L0, L1, R          : REF IntChan;
    x, i               : Word.T;
    c0, c1, c2, c3, c4 : PselectClosure;
  END;

  PselectBlock = PROCEDURE (cl : PselectClosure) : BOOLEAN;

  PselectClosure = ProcClosure OBJECT
    frame : PselectFrame;
    block : PselectBlock;
  OVERRIDES
    run := RunPselect;
  END;

PROCEDURE RunPselect(cl : PselectClosure) =
  BEGIN
    EVAL cl.block(cl)
  END RunPselect;

PROCEDURE Pselect_0(cl : PselectClosure) : BOOLEAN =
  BEGIN
    (* this is the pselection statement *)
    IF doDebug THEN
      Debug.Out("Pselect_0 " & Fmt.Int(cl.frameId))
    END;
    
    WITH frame = cl.frame DO

      PROCEDURE Unwait() =
        BEGIN
          IF frame.L0.waiter = cl THEN
            frame.L0.waiter := NIL
          END;
          IF frame.L1.waiter = cl THEN
            frame.L1.waiter := NIL
          END;
        END Unwait;

      PROCEDURE Wait() =
        BEGIN
          frame.L0.waiter := cl;
          frame.L1.waiter := cl
        END Wait;
        
      BEGIN        
        IF    IntRecvProbe(frame.L0^, cl) THEN
          Unwait();
          Release(frame.c1);
          RETURN TRUE
        ELSIF IntRecvProbe(frame.L1^, cl) THEN
          Unwait();
          Release(frame.c2);
          RETURN TRUE
        ELSE
          Wait();
          RETURN FALSE
        END
      END
    END
  END Pselect_0;

PROCEDURE Pselect_1(cl : PselectClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Pselect_1 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L0^, frame.x, cl) THEN
        RETURN FALSE
      END;
      
      Release(frame.c3);
      RETURN TRUE
    END
  END Pselect_1;

PROCEDURE Pselect_2(cl : PselectClosure) : BOOLEAN =
  BEGIN
    IF doDebug THEN
      Debug.Out("Pselect_2 " & Fmt.Int(cl.frameId))
    END;

    WITH frame = cl.frame DO
      IF NOT IntRecv(frame.L1^, frame.x, cl) THEN
        RETURN FALSE
      END;
      
      Release(frame.c3);
      RETURN TRUE
    END
  END Pselect_2;

PROCEDURE Pselect_3(cl : PselectClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF frame.i MOD 10000 = 0 THEN
        IO.Put(" i/10000 = " & Fmt.Int(frame.i DIV 10000) & " x = " & Fmt.Int(frame.x) & "\n")
      END;
      INC(frame.i);
      Release(frame.c4);

      RETURN TRUE
    END
  END Pselect_3;

PROCEDURE Pselect_4(cl : PselectClosure) : BOOLEAN =
  BEGIN
    WITH frame = cl.frame DO
      IF NOT IntSend(frame.R^, frame.x, cl) THEN
        RETURN FALSE
      END;
      Release(frame.c0);

      RETURN TRUE
    END
  END Pselect_4;

PROCEDURE BuildPselect(L0, L1, R : REF IntChan) =
  BEGIN
    WITH frame = NEW(PselectFrame,
                     id    := NextFrameId(),

                     x := 0, i := 0,
                     
                     L0 := L0, L1 := L1, R := R) DO
      L0.reader := frame;
      L1.reader := frame;
      R.writer := frame;

      frame.c0 := NEW(PselectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pselect_0);
      frame.c1 := NEW(PselectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pselect_1);
      frame.c2 := NEW(PselectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pselect_2);
      frame.c3 := NEW(PselectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pselect_3);
      frame.c4 := NEW(PselectClosure,
                      id      := NextId(),
                      frameId := frame.id,
                      frame   := frame,
                      block   := Pselect_4);

      Schedule(frame.c0)
    END
  END BuildPselect;

PROCEDURE BuildParallelSim() =
  VAR
    c0 := NewIntChan("c0");
    c1 := NewIntChan("c1");
    cx := NewIntChan("cx");
  BEGIN
    <*ASSERT Slack >= 1*>
    BuildPdrive(c0, c1, cx);
    BuildPselect(c0, c1, cx)
  END BuildParallelSim;
 
(**********************************************************************)
(**)
(* generic scheduling routines *)
(**)

PROCEDURE Schedule(closure : ProcClosure) =
  BEGIN
    <*ASSERT closure # NIL*>

    IF closure.scheduled = nexttime THEN
      IF doDebug THEN
        Debug.Out(Int(closure.id) & " already scheduled at " & Int(nexttime))
      END;
      RETURN 
    END;
    
    IF np > LAST(next^) THEN
      WITH new = NEW(REF ARRAY OF ProcClosure, NUMBER(next^) * 2) DO
        SUBARRAY(new^, 0, NUMBER(next^)) := next^;
        next := new
      END
    END;
    next[np] := closure;
    closure.scheduled := nexttime;
    INC(np)
  END Schedule;

CONST Release = Schedule;

VAR
  active, next := NEW(REF ARRAY OF ProcClosure, 1);
  ap, np := 0;
  nexttime : Word.T := 0;

PROCEDURE SchedulingLoop() =
  BEGIN
    (* run *)

    LOOP
      IF doDebug THEN Debug.Out("Scheduling loop : np = " & Fmt.Int(np)) END;

      IF np = 0 THEN
        Debug.Error("DEADLOCK!")
      END;
      
      VAR
        temp := active;
      BEGIN
        active := next;
        ap := np;
        next := temp;
        np := 0;
      END;

      INC(nexttime);

      FOR i := 0 TO ap - 1 DO
        WITH cl      = active[i] DO
          <*ASSERT cl # NIL*>
          cl.run();
          (*IF NOT success THEN Schedule(cl) END*)
        END
      END
    END
  END SchedulingLoop;
  
(**********************************************************************)

TYPE
  Simulation = { First, Select, Parallel };
  Build = PROCEDURE();
  
CONST
  Builder = ARRAY Simulation OF Build {
    BuildFirstSim,
    BuildSelectSim,
    BuildParallelSim (* this one needs slack >= 1 not to deadlock *)
  };

  theSimulation = Simulation.Parallel; (* can change this *)

BEGIN
  Builder[theSimulation]();
  
  SchedulingLoop()
END first.
