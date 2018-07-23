MODULE MbyDoRealignKeys;
FROM MbyParserToMapper IMPORT PaKeys;
IMPORT Word;
IMPORT MbyPaKeys AS PK;
IMPORT MbyRealignKeys AS RK;
FROM WmUtils IMPORT GetUnnamedField, ModfyUnnamedField;

(* these should be moved... wm_support ? *)

PROCEDURE  RealignKeys(READONLY isIpV4, isIpV6    : ARRAY [0..1] OF BOOLEAN;
                       READONLY pk (*paKeys*)     : PaKeys;
                       VAR rk  (*realignedKeys*)  : RaKeys;
                       VAR ihlOk                  : BOOLEAN;
                       VAR ihlFits                : BOOLEAN) =

  TYPE PKI = [FIRST(PaKeys)..LAST(PaKeys)];
       RKI = [FIRST(RaKeys)..LAST(RaKeys)];

  PROCEDURE CopyDefault() =
    BEGIN rk := SUBARRAY(pk, 0, NUMBER(rk)) END CopyDefault;

  PROCEDURE RA1(from : PKI;
                to   : RKI;
                foff, toff : INTEGER := 0) =
    BEGIN
      rk[  to + toff].k := pk[from + foff].k;
      rk[from + foff].v := FALSE
    END RA1;

  PROCEDURE GetUnnamed(from : PKI; sb, len : CARDINAL) : Word.T =
    BEGIN
      RETURN GetUnnamedField(pk[from].k, sb, len)
    END GetUnnamed;

  PROCEDURE SetUnnamed(to : RKI; sb, len : CARDINAL; value : Word.T) =
    BEGIN
      <*ASSERT value < Word.LeftShift(1,len)*>
      rk[to].k := ModfyUnnamedField(rk[to].k, sb, len, value);
    END SetUnnamed;
    
  PROCEDURE Realign4(foff, toff := 0) =

    PROCEDURE C(f : PKI; t : RKI) =
      BEGIN RA1(f, t, foff, toff) END C;

    PROCEDURE G(f : PKI; sb, len : CARDINAL) : Word.T =
      BEGIN RETURN GetUnnamed(f + foff, sb, len) END G;

    PROCEDURE S(t : RKI; sb, len : CARDINAL; value : Word.T) =
      BEGIN
        SetUnnamed(t + toff, sb, len, value)
      END S;
      
    BEGIN
      C(PK.InnerIpHdr+4,RK.InnerIpTtlProt);
      WITH un0 = G(PK.InnerIpHdr, 0, 8) DO
        S(RK.InnerIpDsFlow, 8, 8, un0)
      END;
      WITH innerIhlNot5 = G(PK.InnerIpHdr, 8, 4) # 5 DO
        S(RK.InnerIpDsFlow, 4, 1, ORD(innerIhlNot5))
      END;
      WITH un1 = G(PK.InnerIpHdr+3, 14, 1) DO
        S(RK.InnerIpDsFlow, 5, 1, un1)
      END;
      WITH innerHf = G(PK.InnerIpHdr+3, 0, 13) = 0 DO
        S(RK.InnerIpDsFlow, 6, 1, ORD(innerHf))
      END;
      WITH un2 = G(PK.InnerIpHdr+3, 13, 1) DO
        S(RK.InnerIpDsFlow, 7, 1, un2)
      END;
      rk[RK.InnerIpDsFlow].v := pk[PK.InnerIpHdr].v;

      S(RK.InnerIpDsFlow, 0, 4, 0);
      rk[RK.InnerIpFlow].k := 0;
      rk[RK.InnerIpFlow].v := FALSE;
      rk[PK.InnerIpHdr + 4].v := FALSE;
      rk[PK.InnerIpHdr + 5].v := FALSE;

      C(PK.InnerSipdip + 2, RK.InnerDip    );
      C(PK.InnerSipdip + 3, RK.InnerDip + 1);
    END Realign4;

  PROCEDURE RealignInner4() = BEGIN Realign4() END RealignInner4;

  PROCEDURE RealignOuter(realign : PROCEDURE(foff, toff := 0)) =
    BEGIN
      WITH
        toff = RK.OuterIpTtlProt - RK.InnerIpTtlProt, (* 6 *)
        foff = PK.OuterIpHdr - PK.InnerIpHdr          (* 6 *) DO
        <*ASSERT foff = toff AND foff = 6*>
        realign(foff,toff)
      END;
    END RealignOuter;                                       
    
  PROCEDURE RealignOuter4() =
    BEGIN
      RealignOuter(Realign4);
      WITH ihl = GetUnnamed(PK.OuterIpHdr, 8, 4) DO
        ihlOk := ihl >= 5;
        ihlFits := rk[RK.OuterIpLen].k >= 4*ihl
      END
    END RealignOuter4;

  PROCEDURE Realign6(foff, toff := 0) =

    PROCEDURE C(f : PKI; t : RKI) =
      BEGIN RA1(f, t, foff, toff) END C;

    PROCEDURE G(f : PKI; sb, len : CARDINAL) : Word.T =
      BEGIN RETURN GetUnnamed(f + foff, sb, len) END G;

    PROCEDURE S(t : RKI; sb, len : CARDINAL; value : Word.T) =
      BEGIN
        SetUnnamed(t + toff, sb, len, value)
      END S;
      
    BEGIN
      WITH un0 = G(PK.InnerIpHdr + 5, 0, 8) DO
        S(RK.InnerIpTtlProt, 8, 8, un0)
      END;
      VAR
        uo1 : [1..5];
        un1 : [0..16_ff];
      BEGIN
        IF pk[PK.InnerIpHdr + 1].v THEN
          uo1 := 1
        ELSE
          uo1 := 5
        END;
        un1 := G(PK.InnerIpHdr + uo1, 8, 8);
        S(RK.InnerIpTtlProt, 0, 8, un1);
        rk[RK.InnerIpTtlProt].v := pk[PK.InnerIpHdr + 5].v;
        rk[PK.InnerIpHdr + 5].v := FALSE
      END;

      C(PK.InnerIpHdr + 4, RK.InnerIpLen);

      WITH un2 = G(PK.InnerIpHdr + 2, 4, 8) DO
        S(RK.InnerIpDsFlow, 8, 8, un2)
      END;

      S(RK.InnerIpDsFlow, 4, 4, 0);
      rk[RK.InnerIpDsFlow].v := pk[PK.InnerIpHdr + 2].v;

      WITH un3 = G(PK.InnerIpHdr + 2, 0, 4) DO
        S(RK.InnerIpDsFlow, 0, 4, un3)
      END;

      rk[RK.InnerIpFlow] := pk[PK.InnerIpHdr + 3]
    END Realign6;
    
  PROCEDURE RealignInner6() = BEGIN Realign6() END RealignInner6;
    
  PROCEDURE RealignOuter6() =
    BEGIN
      RealignOuter(Realign6)
    END RealignOuter6;
    
  BEGIN
    CopyDefault();

    IF isIpV4[1] THEN RealignInner4() END;

    IF isIpV4[0] THEN RealignOuter4() END;
    
    IF isIpV6[1] THEN RealignInner6() END;

    IF isIpV6[0] THEN RealignOuter6() END;
    
  END RealignKeys;

BEGIN END MbyDoRealignKeys.
