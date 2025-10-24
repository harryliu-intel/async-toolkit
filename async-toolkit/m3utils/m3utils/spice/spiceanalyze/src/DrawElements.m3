(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DrawElements;

IMPORT PicPoint;
IMPORT PicCoord;
IMPORT PicSegment;
IMPORT PicCircle;

IMPORT PicSegments;

CONST Big   = 50.0d0;
      Small = 30.0d0;

TYPE P = PicPoint.T;
TYPE PArray = ARRAY OF P;

PROCEDURE Mid(READONLY p, q : P) : P =
  BEGIN
    RETURN PicPoint.Times(0.5d0, PicPoint.Plus(p, q))
  END Mid;

PROCEDURE Step(READONLY from : P;
               big, small    : CARDINAL;
               dx, dy        : [ -1 .. +1 ] ) : P =
  VAR
    deltax := FLOAT(dx * big, LONGREAL) * Big +
              FLOAT(dx * small, LONGREAL) * Small;
    deltay := FLOAT(dy * big, LONGREAL) * Big +
              FLOAT(dy * small, LONGREAL) * Small;
  BEGIN
    RETURN PicPoint.Plus(from, P { deltax, deltay })
  END Step;

PROCEDURE Up(READONLY f : P; 
             big        : CARDINAL := 1;
             small      : CARDINAL := 0) : P =
  BEGIN RETURN Step(f, big, small,  0,  1) END Up;
      
PROCEDURE Down(READONLY f : P; 
               big        : CARDINAL := 1;
               small      : CARDINAL := 0) : P =
  BEGIN RETURN Step(f, big, small,  0, -1) END Down;
  
PROCEDURE Right(READONLY f : P;
                big        : CARDINAL := 1;
                small      : CARDINAL := 0) : P =
  BEGIN RETURN Step(f, big, small,  1,  0) END Right;
      
PROCEDURE Left(READONLY f : P;
               big        : CARDINAL := 1;
               small      : CARDINAL := 0) : P =
  BEGIN RETURN Step(f, big, small, -1,  0) END Left;

  (**********************************************************************)
  
PROCEDURE Fet(to     : PicSegments.T;
                  type   : FetType;
                  doBody : BOOLEAN) =
  VAR
    (* main bit of transistor *)
    p1  := PicPoint.Zero;    (* source/drain *)
    p2  := Up(p1);           
    p3  := Left(p2);         (* bottom of channel *)
    p4  := Up(p3, 2);
    pc3 := Up(p3);           (* middle of channel *)
    p5  := Right(p4);
    p6  := Up(p5);           (* drain/source *)

    q1  := Left(p3, 0, 1);   (* gate bottom *)
    q2  := Up(q1, 2);        (* gate top *)
    qc  := Up(q1);           (* where bubble touches gate *)

    cp1 := Left(qc, 0, 1);   (* bubble left *)

    bm  := Mid(qc, cp1);

    dp1 := cp1;              (* line from bubble left, P transistor *)
    dn1 := qc;               (* line from gate left, N transistor *)
    d2  := Left(pc3, 2);
 
    r1  := pc3;              (* body *)
    r2  := Right(pc3, 2);   
  BEGIN
    OpenPath(to, PArray { p1, p2, p3, p4, p5, p6 });
    OpenPath(to, PArray { q1, q2 });

    CASE type OF
      Nfet =>
      OpenPath(to, PArray { dn1, d2 })
    |
      Pfet =>
      to.addCircle(PicCircle.T { bm, Small / 2.0d0 });
      OpenPath(to, PArray { dp1, d2 })
    END;
    IF doBody THEN
      OpenPath(to, PArray { r1, r2 })
    END
  END Fet;

PROCEDURE Res(to : PicSegments.T) =
  VAR
    t0 := PicPoint.Zero;
    t1 := Up(t0, 1);

    u0 := Up(t1, 2);
    u1 := Up(u0, 1);
  BEGIN
    OpenPath(to, PArray { t0, t1 });
    Rect(to, t1, 2.0d0 * Small, 2.0d0 * Big);
    OpenPath(to, PArray { u0, u1 });
  END Res;

PROCEDURE Dio(to : PicSegments.T) =
  VAR
    t0 := PicPoint.Zero;

    u1 := Up(t0, 4);
    u0 := Down(t1, 1, 1);

    t1 := Down(u0, 1);

    v0 := Left(u0, 1); (* cathode *)
    v1 := Right(u0, 1);

    w0 := Down(v0, 1); (* anode *)
    w1 := Down(v1, 1);
  BEGIN
    OpenPath(to, PArray { t0, t1 });
    OpenPath(to, PArray { u0, u1 });
    OpenPath(to, PArray { v0, v1 });
    ClosedPath(to, PArray { u0, w0, w1 })
  END Dio;

PROCEDURE Cap(to : PicSegments.T) =
  VAR
    t0 := PicPoint.Zero;
    t1 := Up(t0, 0, 3);

    u1 := Up(t0, 4);
    u0 := Down(u1, 0, 3);
    
    v0 := Left(t1, 1);    (* lower plate *)
    v1 := Right(t1, 1);
    w0 := Left(u0, 1);    (* upper plate *)
    w1 := Right(u0, 1);
  BEGIN
    OpenPath(to, PArray { t0, t1 });
    OpenPath(to, PArray { u0, u1 });
    OpenPath(to, PArray { v0, v1 });
    OpenPath(to, PArray { w0, w1 });
  END Cap;

PROCEDURE Vdd(to : PicSegments.T) =
  VAR
    t0 := Left(PicPoint.Zero);
    t1 := Right(PicPoint.Zero);
  BEGIN
    OpenPath(to, PArray { t0, t1 });
  END Vdd;
    
PROCEDURE Gnd(to : PicSegments.T) =
  VAR
    t0 := Left(PicPoint.Zero, 0, 1);
    t1 := Right(PicPoint.Zero, 0, 1);
    t2 := Down(PicPoint.Zero);
  BEGIN
    ClosedPath(to, PArray { t0, t1, t2 })
  END Gnd;

  (**********************************************************************)

PROCEDURE Rect(to   : PicSegments.T;
                   at   : P;
                   w, h : PicCoord.T) =
  VAR
    ll := PicPoint.Minus(at, P { -w / 2.0d0, 0.0d0 });
    ur := PicPoint.Plus (at, P { w, h });
    lr := P { ur.x, ll.y };
    ul := P { ur.y, ll.x };
  BEGIN
    ClosedPath(to, PArray { ll, lr, ur, ul });
  END Rect;


PROCEDURE ClosedPath(to           : PicSegments.T;
                        READONLY lst : PArray) =
  BEGIN
    WITH n = NUMBER(lst) DO
      FOR i := FIRST(lst) TO LAST(lst) DO
        to.addSegment( PicSegment.T { lst[i], lst[(i + 1) MOD n] } )
      END
    END
  END ClosedPath;

PROCEDURE OpenPath(to           : PicSegments.T;
                      READONLY lst : PArray) =
  BEGIN
    FOR i := FIRST(lst) TO LAST(lst) - 1 DO
      to.addSegment(PicSegment.T { lst[i], lst[i + 1] })
    END
  END OpenPath;

BEGIN END DrawElements.
