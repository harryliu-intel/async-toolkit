MODULE Main;
IMPORT IO, Fmt;
FROM Fmt IMPORT F, Int;
FROM Math IMPORT log;

PROCEDURE LR(z : LONGREAL; prec := 12) : TEXT =
  BEGIN
    RETURN Fmt.LongReal(z, prec := prec)
  END LR;
      
PROCEDURE Avail(fit, mttr : LONGREAL; VAR unavail, avail : LONGREAL) =
  VAR
    fps   := fit / 1.0d9 / 3600.0d0; (* failures per second *)
    mtbf  := 1.0d0/fps;
  BEGIN
    unavail := mttr / (mttr + mtbf);
    avail := mtbf / (mttr + mtbf); 
  END Avail;

TYPE
  Case = RECORD
    lb    : TEXT;
    fit   : LONGREAL;
    recov : LONGREAL;
  END;

CONST
  Cases1 = ARRAY OF Case {
  Case { "non FW arrays",  500.0d0, 0.800d0 },
  Case { "TCAM         ", 1500.0d0, 0.020d0 },
  Case { "FW arrays    ",  200.0d0, 1.500d0 }
  };

  Cases2 = ARRAY OF Case {
    Case { "IP           ", 1500.0d0, 240.0d0 }
  };

PROCEDURE DoCases(header : TEXT; READONLY a : ARRAY OF Case) =
  VAR
    avail, unavail : LONGREAL;
    log10 : LONGREAL;
    nines : CARDINAL;
    totUnavail := 0.0d0;
    sumFIT := 0.0d0;
    
  PROCEDURE SPY(ua : LONGREAL) : LONGREAL =
    CONST SecondsPerYear = 365.22d0*86400.0d0;
    BEGIN RETURN ua * SecondsPerYear END SPY;
    
  BEGIN
    IO.Put(F("%-17s FIT  MTTR     AVAIL           UNAVAIL              log           NINES DOWN-s/yr\n",header));
    FOR i := FIRST(a) TO LAST(a) DO
      WITH z = a[i] DO
        Avail(z.fit, z.recov, unavail, avail);
        sumFIT := sumFIT + z.fit;
        log10 := Log10(unavail);
        nines := FLOOR(-log10);
        
        IO.Put(F("%s %8s %-8s %-15s %-20s",
                 z.lb, LR(z.fit), LR(z.recov), LR(avail), LR(unavail)));
        IO.Put(F(" %-15s %3s %s\n",
                 LR(log10,2), Int(nines), LR(SPY(unavail),3)   ));
        totUnavail := totUnavail + unavail
      END
    END;
    IO.Put(F("Total         %8s          %-15s %-20s                 %3s %s\n",
             LR(sumFIT),
             LR(1.0d0-totUnavail),LR(totUnavail,2), 
             Int(FLOOR(-Log10(totUnavail))), LR(SPY(totUnavail), 3)   ));
    IO.Put("\n");
  END DoCases;

PROCEDURE Log10(z : LONGREAL) : LONGREAL =
  BEGIN RETURN log(z)/log(10.0d0) END Log10;
    
BEGIN
  DoCases("fast IP", Cases1);
  DoCases("slow IP", Cases2);
END Main.
