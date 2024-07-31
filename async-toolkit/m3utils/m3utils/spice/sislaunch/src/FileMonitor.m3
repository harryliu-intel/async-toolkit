MODULE FileMonitor;
IMPORT Pathname;
IMPORT Time;
IMPORT Thread;
IMPORT FileRd;
IMPORT Scan;
IMPORT Rd;
IMPORT RefList;
IMPORT OSError;
IMPORT FloatMode;
IMPORT Lex;

REVEAL
  T = Public BRANDED Brand OBJECT
    mu        : MUTEX;
    path      : Pathname.T;
    interval  : Time.T;
    lastVal   : [ DefValue .. LAST(ReturnType) ];
    callbacks : RefList.T;
  OVERRIDES
    init             := Init;
    registerCallback := RegisterCallback;
  END;

PROCEDURE Init(t        : T;
               path     : Pathname.T;
               cb       : Callback;
               interval : Time.T;
               defValue : INTEGER) : T =
  BEGIN
    t.path     := path;
    t.interval := interval;
    t.mu       := NEW(MUTEX);
    t.lastVal  := defValue;
    IF cb # NIL THEN
      t.callbacks := RefList.List1(cb)
    ELSE
      t.callbacks := NIL
    END;
    WITH cl = NEW(Closure, t := t) DO
      EVAL Thread.Fork(cl)
    END;
    RETURN t
  END Init;

TYPE
  Closure = Thread.Closure OBJECT
    t : T;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  BEGIN
    LOOP
      TRY
        WITH rd   = FileRd.Open(cl.t.path),
             line = Rd.GetLine(rd),
             num  = Scan.Int(line) DO
          IF num >= FIRST(ReturnType) AND num <= LAST(ReturnType) THEN
            IF num # cl.t.lastVal THEN
              LOCK cl.t.mu DO
                cl.t.lastVal := num;
                
                VAR p := cl.t.callbacks; BEGIN
                  WHILE p # NIL DO
                    NARROW(p.head, Callback).changed(cl.t.lastVal);
                    p := p.tail
                  END
                END
              END
            END
          END
        END
      EXCEPT
        OSError.E, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
        FloatMode.Trap, Lex.Error => (* skip *)
      END;
      Thread.Pause(cl.t.interval)
    END
  END Apply;

PROCEDURE RegisterCallback(t : T; cb : Callback) =
  BEGIN
    LOCK t.mu DO
      t.callbacks := RefList.Cons(cb, t.callbacks)
    END
  END RegisterCallback;
  
BEGIN END FileMonitor.
