UNSAFE MODULE RTRefStats;
(* $Id$ *)

FROM Debug IMPORT S;
IMPORT Fmt;
IMPORT RTHeapRep;
IMPORT RTType;
IMPORT RTBrand;
FROM RT0 IMPORT Typecode;
IMPORT IntPQ;

TYPE
  R = RECORD
    count, size := 0;
  END;
  T = RTHeapRep.RefVisitor OBJECT
    x: REF ARRAY OF R;
  OVERRIDES
    visit := Visit;
  END;

PROCEDURE Visit(self: T; tc: Typecode;
                <*UNUSED*>r: REFANY; size: CARDINAL): BOOLEAN =
  BEGIN
    INC(self.x[tc].count);
    INC(self.x[tc].size, size);
    RETURN TRUE;
  END Visit;

TYPE
  E = IntPQ.Elt OBJECT tc, count: INTEGER; END;

PROCEDURE ReportReachable() =
  VAR
    n := RTType.MaxTypecode();
    self := NEW(T, x := NEW(REF ARRAY OF R, n));
  BEGIN
    S("visiting references...", 0);
    RTHeapRep.VisitAllRefs(self);
    S("sorting stats by size...", 0);
    VAR
      q := NEW(IntPQ.Default).init();
      e: E;
      brand: TEXT;
    BEGIN
      FOR tc := 0 TO n-1 DO
        IF self.x[tc].count # 0 THEN
          q.insert(NEW(E,
                       priority := self.x[tc].size,
                       count := self.x[tc].count,
                       tc := tc));
        END;
      END;
      S("   TC  Brand                                    totalSize   count  avgSize",0);
      TRY
        LOOP
          e := q.deleteMin();
          TRY
            brand := RTBrand.GetByTC(e.tc);
          EXCEPT RTBrand.NotBranded =>
            brand := "??";
          END;
          S(Fmt.Pad(Fmt.Int(e.tc),5) & "  " &
            Fmt.Pad(brand,40,' ',Fmt.Align.Left) &
            Fmt.Pad(Fmt.Int(e.priority),10) &
            Fmt.Pad(Fmt.Int(e.count),8) &
            Fmt.Pad(Fmt.Int(e.priority DIV e.count),9),
            0);
        END;
      EXCEPT IntPQ.Empty =>
      END;
    END;
  END ReportReachable;

BEGIN
END RTRefStats.
