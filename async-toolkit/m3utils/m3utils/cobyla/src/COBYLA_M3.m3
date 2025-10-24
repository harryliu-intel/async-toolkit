(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE MODULE COBYLA_M3;
IMPORT LRVector, LRVectorField;
IMPORT COBYLA;
IMPORT Word;

CONST Tag = 16_c0edbeef;

TYPE
  T = RECORD
    tag  : Word.T;
    func : LRVectorField.T;
    x    : LRVector.T;
    n, m : CARDINAL;
    lastf : LONGREAL;
    lastcon : LRVector.T;
  END;

PROCEDURE Minimize((*INOUT*)p       : LRVector.T;
                   m                : CARDINAL;
                   func             : LRVectorField.T;
                   rhobeg, rhoend   : LONGREAL;
                   maxfun           : CARDINAL;
                   iprint           : INTEGER;
                   ) : Result =
  VAR
    N, M              : ADDRESS;  n       : INTEGER;
    X                 : ADDRESS;
    RHOBEG, RHOEND    : ADDRESS;
    IPRINT            : ADDRESS;
    MAXFUN            : ADDRESS;
    W                 : ADDRESS;  w       : REF ARRAY OF LONGREAL;
    IACT              : ADDRESS;  iact    : REF ARRAY OF INTEGER;
    CALCFC            : COBYLA.Func;
    ITAG              : ADDRESS;  t       : T;
  BEGIN
    <*ASSERT p # NIL*>
    <*ASSERT func # NIL*>
    n      := NUMBER(p^);
    w      := NEW(REF ARRAY OF LONGREAL, n * (3 * n + 2 * m + 11) + 4 * m + 6);
    iact   := NEW(REF ARRAY OF INTEGER , m + 1);
    t      := T { tag := Tag,
                  func := func,
                  x := p,
                  n := n,
                  m := m,
                  lastf := FIRST(LONGREAL),
                  lastcon := NEW(LRVector.T, m)};

    N      := ADR(n);
    M      := ADR(m);
    X      := ADR(p[0]);
    RHOBEG := ADR(rhobeg);
    RHOEND := ADR(rhoend);
    IPRINT := ADR(iprint);
    MAXFUN := ADR(maxfun);
    W      := ADR(w[0]);
    IACT   := ADR(iact[0]);
    CALCFC := Wrapper;
    ITAG   := ADR(t);

    VAR u := LOOPHOLE(ITAG, REF T);
    BEGIN
      <*ASSERT u.tag = Tag*>
    END;

    COBYLA.Call(N, M,
                X,
                RHOBEG, RHOEND,
                IPRINT,
                MAXFUN,
                W,
                IACT,
                CALCFC,
                ITAG);

    WITH res = Result { t.lastf, t.lastcon } DO
      RETURN res
    END
  END Minimize;

PROCEDURE Wrapper(N : ADDRESS; M : ADDRESS; X : ADDRESS; CON : ADDRESS; ITAG : ADDRESS) : LONGREAL =
  VAR
    t   := LOOPHOLE(ITAG, REF T);
    n   := LOOPHOLE(N, REF INTEGER)^;
    m   := LOOPHOLE(M, REF INTEGER)^;
  BEGIN
    (* make sure we passed through the Fortran OK *)
    <*ASSERT t.tag = Tag*>
    <*ASSERT X = ADR(t.x[0])*>
    <*ASSERT n = NUMBER(t.x^)*>
    <*ASSERT n = t.n*>
    <*ASSERT m = t.m*>
    WITH res = t.func.eval(t.x) DO
      t.lastcon^ := SUBARRAY(res^, 1, m);
      FOR i := 1 TO m DO
        WITH adr = CON + (i - 1) * BYTESIZE(LONGREAL) DO
          LOOPHOLE(adr, REF LONGREAL)^ := res[i]
        END
      END;
      t.lastf := res[0];
      RETURN res[0]
    END
  END Wrapper;

BEGIN END COBYLA_M3.
