(* $Id$ *)

UNSAFE MODULE TZ;
IMPORT Utime, UtimeR, Date, XTime AS Time;
FROM M3toC IMPORT CopyTtoS, FreeCopiedS, CopyStoT;
IMPORT CTZ, Text;
IMPORT Debug, Fmt;
IMPORT OSError;
IMPORT FS, Env;
FROM Ctypes IMPORT long_star;
IMPORT Word;
IMPORT Thread;

REVEAL
  T = Public BRANDED Brand OBJECT
    tz : TEXT;
    hashV : Word.T;
    cache : Cache;
  OVERRIDES
    init := Init;
    mktime := Mktime;
    localtime := Localtime;
    name := Name;
  END;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b OR a.tz = b.tz OR Text.Equal(a.tz, b.tz) 
  END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hashV END Hash;

TYPE Cache = RECORD itime : INTEGER (* TRUNC(t : Time.T) *); d : Date.T END;

PROCEDURE Name(t : T) : TEXT = BEGIN RETURN t.tz END Name;

PROCEDURE Init(t : T; tz : TEXT; disableChecking : BOOLEAN) : T RAISES { OSError.E } = 
  BEGIN 
    IF doChecking AND NOT disableChecking THEN
      VAR
        fn := tz;
      BEGIN
        IF Text.GetChar(fn,0) = ':' THEN fn := Text.Sub(fn,1) END;
        EVAL FS.Status(tzDir & "/" & fn); (* RAISEs OSError.E *)
      END
    END;

    t.tz := tz; 
    t.hashV := Text.Hash(tz);
    t.cache.itime := -1; (* impossible key *)
    RETURN t 
  END Init;

PROCEDURE SetCurTZ(to : TEXT) =
  (* mu must be locked *)
  BEGIN
    IF NOT Text.Equal(to,CurTZ) THEN
      WITH s = CopyTtoS(to) DO
        EVAL CTZ.setenv(TZTZ,s,1);
        FreeCopiedS(s)
      END;
      CurTZ := to
    END
  END SetCurTZ;

PROCEDURE GetOldTZ() : TEXT =
  (* mu must be locked *)
  BEGIN
    WITH res = CopyStoT(CTZ.getenv(TZTZ)) DO
      IF Debug.GetLevel() > 30 THEN 
        Thread.Release(mu); (* avoid re-entrant locking (Debug uses TZ) *)
        TRY
          Debug.Out("TZ.GetOldTZ: " & res) 
        FINALLY
          Thread.Acquire(mu)
        END
      END;
      RETURN res
    END
  END GetOldTZ;

PROCEDURE Localtime(t : T; timeArg : Time.T) : Date.T =
  VAR
    time := timeArg - UnixEpoch; (* on Unix, UnixEpoch is 0 *)
    itime := TRUNC(time); (* note: not ROUND! *)

    d : Date.T;
  BEGIN
    (* first of all, see if the conversion is in the same minute
       as we just converted.  If so, modify return value accordingly
       and return it, saving system calls, etc. *)
    LOCK mu DO

      (* tricky note.

         We might think we can use a t-specific mutex here.
         That is so, and we used to do it this way, by calling it 
         a field in t.  However, this won't work if we ever want
         to Pickle a T and transfer that Pickle between Win32 and
         POSIX.

         If we really, really want per-T locks, we need to do 
         something with tables.
       *)
      
      WITH newSecond = t.cache.d.second + (itime - t.cache.itime) DO
        IF newSecond >= 0 AND newSecond <= 59 THEN
          VAR 
            newD := t.cache.d;
          BEGIN
            newD.second := newSecond;
            RETURN newD
          END
        END
      END
    END;
    
    VAR
      tms := NEW(UNTRACED REF Utime.struct_tm);
      clock := NEW(long_star);
      oldTZ : TEXT;
    BEGIN
      TRY
        LOCK mu DO
          oldTZ := GetOldTZ();
          TRY
            SetCurTZ(t.tz);
            
            IF Debug.GetLevel() >= 20 THEN
              Debug.Out("TZ.Localtime: time=" & Fmt.LongReal(time))
            END;
            
            clock^ := itime;
            
            (* the following code should match what is in DateBsd.m3 ... *)
            (* the main difference is the use of localtime_r rather than
               localtime, in order to eliminate static storage depedencies *)
            
            WITH tm = UtimeR.localtime_r(clock,tms) DO
              d.second := MIN(tm.tm_sec,59); (* leap seconds!? *)
              d.minute := tm.tm_min;
              d.hour := tm.tm_hour;
              d.day := tm.tm_mday;
              d.month := VAL(tm.tm_mon,Date.Month);
              d.year := tm.tm_year + 1900;
              d.weekDay := VAL(tm.tm_wday,Date.WeekDay);
              
              d.offset := -(tm.tm_gmtoff);
              d.zone := CopyStoT(tm.tm_zone)
            END
          FINALLY
            SetCurTZ(oldTZ)
          END
        END;
        t.cache.itime := itime; t.cache.d := d;
        RETURN d
        
      FINALLY
        DISPOSE(clock);
        DISPOSE(tms)
      END
    END
  END Localtime;

PROCEDURE Mktime(t : T; d : Date.T) : Time.T =
  BEGIN
    LOCK mu DO
      SetCurTZ(t.tz);
      
      VAR
        tm := NEW(UNTRACED REF Utime.struct_tm);
        now := NEW(long_star);
      BEGIN
        now^ := SomeTimeT;

        tm := UtimeR.localtime_r(now,tm);
        tm.tm_sec := d.second;
        tm.tm_min := d.minute;
        tm.tm_hour := d.hour;
        tm.tm_mday := d.day;
        tm.tm_mon := ORD(d.month);
        tm.tm_year := d.year-1900;
        tm.tm_isdst := -1;
        tm.tm_gmtoff := 0;
        tm.tm_wday := 0; (* ignored *)
        tm.tm_yday := 0;  (* ignored *)
        WITH res = Utime.mktime(tm) DO
          <* ASSERT res >= 0 *>
          DISPOSE(tm);
          DISPOSE(now);
          RETURN FLOAT(res,LONGREAL)
        END
      END
    END
  END Mktime;

PROCEDURE DisableChecking() = BEGIN doChecking := FALSE END DisableChecking;

VAR doChecking := TRUE;

VAR CurTZ := "";
VAR mu := NEW(MUTEX);

VAR TZTZ := CopyTtoS("TZ");
VAR SomeTimeT := ROUND(Time.Now());

VAR
  tzDir := DefaultTZRoot;
BEGIN 
  WITH env = Env.Get("TZROOT") DO
    IF env # NIL THEN tzDir := env END
  END;

  WITH s = CopyTtoS(CurTZ) DO
    EVAL CTZ.setenv(TZTZ,s,1);
    FreeCopiedS(s)
  END
END TZ.
