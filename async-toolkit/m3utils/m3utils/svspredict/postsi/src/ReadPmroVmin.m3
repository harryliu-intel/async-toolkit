MODULE ReadPmroVmin;
IMPORT Rd, Lex, FloatMode;
IMPORT LongRealSeq AS LRSeq;
IMPORT PMRO;
FROM ReadStats IMPORT DoStats, ReadFile;
FROM JBay IMPORT WeightLkgSigma;
FROM N7Tech IMPORT Transistor;

PROCEDURE Read(rd                     : Rd.T;
               Unit                   : LONGREAL;
               READONLY pmro          : ARRAY OF PMRO.T;
               VAR n                  : CARDINAL;
               VAR meanVmin, sdevVmin : LONGREAL;
               VAR meanPmro, sdevPmro : LONGREAL)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure } =

  CONST
    Columns = NUMBER(Transistor) + 1;
      
  VAR
    seqs := ARRAY [ 0.. Columns- 1 ] OF LRSeq.T { NEW(LRSeq.T).init(), .. };
    dummy : TEXT;

    sigmaSeq := NEW(LRSeq.T).init();
    
  BEGIN
    ReadFile(rd, seqs, dummy);

    FOR i := 0 TO seqs[0].size() - 1 DO
      VAR
        a : ARRAY Transistor OF LONGREAL;
      BEGIN
        FOR j := FIRST(a) TO LAST(a) DO
          a[j] := seqs[ORD(j)].get(i)
        END;
        sigmaSeq.addhi(WeightLkgSigma(a))
      END
    END;

    DoStats(sigmaSeq, n, meanPmro, sdevPmro);
    VAR
      m, s : LONGREAL;
    BEGIN
      DoStats(seqs[LAST(seqs)], n, m, s);
      meanVmin := m * Unit;
      sdevVmin := s * Unit;
    END
  END Read;

BEGIN END ReadPmroVmin.
