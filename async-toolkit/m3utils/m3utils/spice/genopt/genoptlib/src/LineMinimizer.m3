(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LineMinimizer;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT LineProblem;
IMPORT Thread;
IMPORT Debug;
IMPORT Compress;
FROM Fmt IMPORT F, LongReal;
FROM GenOptUtils IMPORT FmtP;

CONST LR = LongReal;

REVEAL
  T = Public BRANDED Brand OBJECT
    c       : Thread.Condition;
    
    (* semaphore *)
    done : BOOLEAN;
    
    (* input vars *)
    pp   : LRVector.T;
    dir  : LRVector.T;
    func : LRScalarField.T;
    rho  : LONGREAL;

    (* quit var *)
    doQuit : BOOLEAN;

    (* output var *)
    lps  : LineProblem.T;
  OVERRIDES
    init  := Init;
    apply := LinMinApply;
    start := Start;
    isDone:= IsDone;
    wait  := Wait;
    quit  := Quit;
  END;

VAR
  mu      := NEW(MUTEX);
  running := 0;
  doDebug := Debug.DebugThis("LineMinimizer");

PROCEDURE IsDone(cl : T) : BOOLEAN =
  BEGIN
    LOCK mu DO
      RETURN cl.done
    END
  END IsDone;
  
PROCEDURE LinMinApply(cl : T) : REFANY =
  (* call out to Brent *)
  BEGIN
    LOOP
      LOCK mu DO
        WHILE cl.done DO
          Thread.Wait(mu, cl.c)
        END
      END;

      IF doDebug THEN Debug.Out("LinMinApply : done FALSE.") END;

      (* NOT cl.done *)
      IF cl.doQuit THEN
        EXIT
      END;
        
      LOCK mu DO INC(running) END;
      
      WITH startp = LRVector.Copy(cl.pp),
           minval = Compress.LinMin(cl.pp,
                                    LRVector.Copy(cl.dir),
                                    cl.func,
                                    cl.rho,
                                    cl.rho / 10.0d0) DO
        
        IF doDebug THEN
          Debug.Out(F("LinMinApply minimization from %s [%s] dir %s : returned %s @ %s",
                      FmtP(startp),
                      LR(cl.func.eval(startp)),
                      FmtP(cl.dir),
                      LR(minval),
                      FmtP(cl.pp)))
        END;
        
        LOCK mu DO
          cl.lps := LineProblem.T { cl.dir, cl.pp, minval };
          cl.done := TRUE;
          DEC(running);
          Thread.Signal(cl.c)
        END
      END
    END;
    <*ASSERT FALSE*>
  END LinMinApply;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.c := NEW(Thread.Condition);
    t.done := TRUE;
    t.doQuit := FALSE;
    EVAL Thread.Fork(t);
    RETURN t
  END Init;

PROCEDURE Start(t    : T;
                pp   : LRVector.T;
                dir  : LRVector.T;
                func : LRScalarField.T;
                rho  : LONGREAL) =
  BEGIN
    LOCK mu DO
      <*ASSERT t.done = TRUE*>
      t.pp   := pp;
      t.dir  := dir;
      t.func := func;
      t.rho  := rho;
      t.done := FALSE;
      Thread.Signal(t.c) 
    END
  END Start;

PROCEDURE Wait(t : T) : LineProblem.T =
  BEGIN
    LOCK mu DO
      WHILE NOT t.done DO
        Thread.Wait(mu, t.c);
      END;
      RETURN t.lps
    END
  END Wait;

PROCEDURE Quit(t : T) =
  BEGIN
    LOCK mu DO
      t.doQuit := TRUE;
      Thread.Signal(t.c)
    END
  END Quit;

PROCEDURE Running() : CARDINAL =
  BEGIN LOCK mu DO RETURN running END END Running;

BEGIN END LineMinimizer.
