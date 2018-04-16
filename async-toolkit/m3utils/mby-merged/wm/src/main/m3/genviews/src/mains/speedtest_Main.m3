MODULE Main;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT IO;
IMPORT CompAddr, CompRange;
IMPORT Fmt;
IMPORT Thread;
IMPORT Debug;
IMPORT Time;
IMPORT Word;

VAR
  x : Map.T;
  y : MapAddr.A;
CONST
  n = NUMBER(x.Sched.RxqStorageData);

PROCEDURE P(READONLY z : Map.T; READONLY a : MapAddr.A) =
  BEGIN
    IO.Put("Hi!\n")
  END P;
  
CONST Writes = 10 * 1000  * 1000;

BEGIN
  P(x,y);
  VAR
    map := NEW(MapAddr.H).init(CompAddr.T { 0, 0 });
    start1, stop1 : Time.T;
    start2, stop2 : Time.T;
    start3, stop3 : Time.T;
    qq : Word.T := 0;
  BEGIN
    IO.Put(Fmt.Int(CompAddr.initCount) & " fields have been address initialized.\n");
    <*ASSERT map # NIL*>
    <*ASSERT map.update.Sched.RxqStorageData[1234].TailCsumLen # NIL*>

    Debug.Out("writing @ " & CompRange.Format(map.a.Sched.RxqStorageData[1234].TailCsumLen));

    map.update.Sched.RxqStorageData[1234].TailCsumLen.u(0);
    <*ASSERT map.read.Sched.RxqStorageData[1234].TailCsumLen=0*>
    map.update.Sched.RxqStorageData[1234].TailCsumLen.u(16_c0ed);
    <*ASSERT map.read.Sched.RxqStorageData[1234].TailCsumLen=16_c0ed*>

    Debug.Out("Start test");
    start1 := Time.Now();
    FOR i := 1 TO Writes DO 
       WITH ii = i MOD NUMBER(map.update.Sched.RxqStorageData) DO
         map.update.Sched.RxqStorageData[ii].TailCsumLen.u(i MOD 257);
        <*ASSERT map.read.Sched.RxqStorageData[ii].TailCsumLen=i MOD 257*>
       END
    END;
    stop1 := Time.Now();
    Debug.Out("Test complete");
    Debug.Out(Fmt.F("%s writes per second", Fmt.LongReal(FLOAT(Writes,LONGREAL)/(stop1-start1), prec := 6)));

    Debug.Out("Start test");
    start2 := Time.Now();
    FOR i := 1 TO 10*Writes DO 
       WITH ii = i MOD NUMBER(map.update.Sched.RxqStorageData) DO
         qq := map.read.Sched.RxqStorageData[ii].TailCsumLen + n - 1 + ii + qq
       END
    END;
    stop2 := Time.Now();
    Debug.Out("Test complete " & Fmt.Int(qq));
    Debug.Out(Fmt.F("%s reads per second", Fmt.LongReal(FLOAT(10*Writes,LONGREAL)/(stop2-start2), prec := 6)));


    Debug.Out("Start test");
    start3 := Time.Now();
    FOR i := 1 TO 10*Writes DO 
       WITH ii = i MOD NUMBER(map.update.Sched.RxqStorageData) DO
         qq := map.read.Sched.RxqStorageData[ii].TailCsumLen +  map.read.Sched.RxqStorageData[n-ii-1].TailCsumLen + qq
       END
    END;
    stop3 := Time.Now();
    Debug.Out("Test complete " & Fmt.Int(qq));
    Debug.Out(Fmt.F("%s reads per second", Fmt.LongReal(FLOAT(10*Writes,LONGREAL)/(stop3-start3), prec := 6)));

    Debug.Out(Fmt.F("%s reads per second (raw read speed)", Fmt.LongReal(FLOAT(10*Writes,LONGREAL)/(stop3-start3-(stop2-start2)), prec := 6)))

  END;
  Thread.Pause(0.0d0);
END Main.
