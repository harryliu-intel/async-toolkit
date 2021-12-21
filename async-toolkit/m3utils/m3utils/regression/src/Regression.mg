(* $Id: Regression.mg,v 1.4 2008/02/06 10:10:02 mika Exp $ *)

GENERIC MODULE Regression (Matrix2, LU, M2M3);
IMPORT Debug, Fmt;
IMPORT drdist;
FROM Matrix IMPORT Singular;

<* FATAL Singular *>

REVEAL
  Recycler = BRANDED Brand & " Recycler" OBJECT
    indx: REF ARRAY OF INTEGER;
    q, b, temp : REF Matrix2.M;
    bC : REF Matrix2.V;
  END;

PROCEDURE Run(x, y : REF Matrix2.M; 
              (* OUT *) VAR yHat : REF Matrix2.M; 
              debug : BOOLEAN;
              data : T;
              h : Matrix2.Base) =
  VAR
    dim := Matrix2.GetDim(x^);
    dimT := Matrix2.Dim { dim.cols, dim.rows };
    q := Matrix2.NewM(dimT);
    indx := NEW(REF ARRAY OF INTEGER, dim.cols);
    temp := Matrix2.NewM(Matrix2.Dim { dim.cols, dim.cols } );
    b := Matrix2.NewM(Matrix2.Dim { dim.cols, Matrix2.GetDim(y^).cols });
    yHat_c := Matrix2.NewM(Matrix2.Dim { dim.rows, Matrix2.GetDim(y^).cols });
    rec := NEW(Recycler, indx := indx, q := q, b := b, temp := temp);
  BEGIN
    RunR(x,y,yHat_c,debug,data, rec, h);
    yHat := yHat_c;
  END Run;

PROCEDURE RunR(x, y         : REF Matrix2.M; 
               yHat_c       : REF Matrix2.M; 
               debug        : BOOLEAN;
               data         : T;
               VAR recycler : Recycler;
               h            : Matrix2.Base
  ) =
  VAR
    n := NUMBER(x^);
    k := NUMBER(x[0]) - 1;
    
    sum_sq_y, sum_sq_yhat, df_deviations, sum_sq_deviations : Matrix2.Base;
    mean_sq_deviations, my, myHat, R_sq : Matrix2.Base;
    df_regression, mean_sq_regression, p : Matrix2.Base;

    arg : INTEGER;

  BEGIN

    IF recycler = NIL OR 
       Matrix2.GetDim(recycler.q^).cols # Matrix2.GetDim(x^).rows OR 
       Matrix2.GetDim(recycler.q^).rows # Matrix2.GetDim(x^).cols THEN
      VAR
        dim := Matrix2.GetDim(x^);
        dimT := Matrix2.Dim { dim.cols, dim.rows };
        q := Matrix2.NewM(dimT);
        indx := NEW(REF ARRAY OF INTEGER, dim.cols);
        temp := Matrix2.NewM(Matrix2.Dim { dim.cols, dim.cols } );
        b := Matrix2.NewM(Matrix2.Dim { dim.cols, Matrix2.GetDim(y^).cols });
      BEGIN
        recycler := NEW(Recycler, indx := indx, q := q, b := b, temp := temp)
      END
    END;

    (****************************************)

    RidgeRegress(x^, h,
                 recycler.q^, recycler.indx, recycler.temp^, debug);

    (****************************************)

    Matrix2.MulMM(recycler.q^, y^, recycler.b^);

    Matrix2.MulMM(x^, recycler.b^, yHat_c^);

    (* the following will probably overflow at some point.
       rescale matrices first instead... *)
    sum_sq_y := Matrix2.SumSqM(y^);
    sum_sq_yhat := Matrix2.SumSqM(yHat_c^);

    df_deviations := FLOAT(n-k, Matrix2.Base);
    sum_sq_deviations := Matrix2.SumDiffSqM(y^, yHat_c^);
    mean_sq_deviations := sum_sq_deviations /  df_deviations;

    my := Matrix2.MeanM(y^);
    myHat  := Matrix2.MeanM(yHat_c^);
    R_sq := (sum_sq_yhat - FLOAT(n,Matrix2.Base)*myHat*myHat) / (sum_sq_y - FLOAT(n,Matrix2.Base)*my*my);

    df_regression := FLOAT(k,Matrix2.Base);


    (* the following are suspect when yhat is not zero-centered... *)
    
    mean_sq_regression := sum_sq_yhat / df_regression;
    
    VAR
      F := FLOAT(mean_sq_regression / mean_sq_deviations,LONGREAL);
    BEGIN
      arg := n-k-1;
      p := FLOAT(1.0d0 - drdist.Fish(F, k, arg),Matrix2.Base);

      data.b := recycler.b;
      
      data.R_sq := R_sq;
      data.F := FLOAT(F,Matrix2.Base);
    END;
    data.mean_sq_y := sum_sq_y / FLOAT(n-1,Matrix2.Base);
    data.s_sq_dev:= mean_sq_deviations
  END RunR;

PROCEDURE RunR1(READONLY x : Matrix2.M;
                READONLY y : Matrix2.V;
                VAR yHat_c : Matrix2.V; 
                debug : BOOLEAN;
                data : T;
                VAR recycler : Recycler;
                h : Matrix2.Base) =
  VAR
    n := NUMBER(x);
    k := NUMBER(x[0]) - 1;
    
    sum_sq_y, sum_sq_yhat, df_deviations, sum_sq_deviations : Matrix2.Base;
    mean_sq_deviations, my, myHat, R_sq : Matrix2.Base;
    df_regression, mean_sq_regression, p : Matrix2.Base;

    arg : INTEGER;

  BEGIN

    IF recycler = NIL OR 
       Matrix2.GetDim(recycler.q^).cols # Matrix2.GetDim(x).rows OR 
       Matrix2.GetDim(recycler.q^).rows # Matrix2.GetDim(x).cols THEN
      VAR
        dim := Matrix2.GetDim(x);
        dimT := Matrix2.Dim { dim.cols, dim.rows };
        q := Matrix2.NewM(dimT);
        indx := NEW(REF ARRAY OF INTEGER, dim.cols);
        temp := Matrix2.NewM(Matrix2.Dim { dim.cols, dim.cols } );
        b := Matrix2.NewM(Matrix2.Dim { dim.cols, 1 });
        bC := Matrix2.NewV(dim.cols);
      BEGIN
        recycler := NEW(Recycler, indx := indx, q := q, b := b, temp := temp,
                        bC := bC)
      END
    END;

    (****************************************)

    RidgeRegress(x, h,
                 recycler.q^, recycler.indx, recycler.temp^, debug);

    (****************************************)
    (* this flip-flopping between b and bC is annoying, should be 
       eliminated *)

    (*Matrix2.ExtractColAsVector(recycler.b^, 0, recycler.bC^);*) (*uninit'd*)

    Matrix2.MulMV(recycler.q^, y, recycler.bC^);

(*
    WITH c2 = Matrix2.NewV(NUMBER(recycler.bC^)) DO
      M2M3.MulMV(recycler.q^, y, c2^);
      
      Debug.Out("c2[0]/bC[0] = " & Matrix2.Format(c2[0]/recycler.bC[0]));
    END;
*)

    Matrix2.MulMV(x, recycler.bC^, yHat_c);

    Matrix2.SetCol(recycler.b^, 0, recycler.bC^);

    (* the following will probably overflow at some point.
       rescale matrices first instead... *)
    sum_sq_y := Matrix2.SumSqV(y);
    sum_sq_yhat := Matrix2.SumSqV(yHat_c);

    df_deviations := FLOAT(n-k, Matrix2.Base);
    sum_sq_deviations := Matrix2.SumDiffSqV(y, yHat_c);
    mean_sq_deviations := sum_sq_deviations /  df_deviations;

    my := Matrix2.MeanV(y);
    myHat  := Matrix2.MeanV(yHat_c);
    R_sq := (sum_sq_yhat - FLOAT(n,Matrix2.Base)*myHat*myHat) / (sum_sq_y - FLOAT(n,Matrix2.Base)*my*my);

    df_regression := FLOAT(k,Matrix2.Base);


    (* the following are suspect when yhat is not zero-centered... *)
    
    mean_sq_regression := sum_sq_yhat / df_regression;
    
    VAR
      F := FLOAT(mean_sq_regression / mean_sq_deviations,LONGREAL);
    BEGIN
      arg := n-k-1;
      p := FLOAT(1.0d0 - drdist.Fish(F, k, arg),Matrix2.Base);

      data.b := recycler.b;
      
      data.R_sq := R_sq;
      data.F := FLOAT(F,Matrix2.Base);
    END;
    data.mean_sq_y := sum_sq_y / FLOAT(n-1,Matrix2.Base);
    data.s_sq_dev:= mean_sq_deviations
  END RunR1;

PROCEDURE RidgeRegress(READONLY x : Matrix2.M; 
                       h          : Matrix2.Base;
                       VAR res    : Matrix2.M (* OUT : ((xTx + h^2 I)^-1)(xT) *);
                       indx       : REF ARRAY OF INTEGER;
                       VAR xTx    : Matrix2.M; (* size cols of x, square *)
                       debug := FALSE)   =
  VAR
    det_xTx : Matrix2.Base;
    d : Matrix2.Base;
  BEGIN
    Matrix2.MulTransposeMM(x, x, xTx);
    det_xTx := Matrix2.Det(xTx);
    Matrix2.AddToDiagonal(xTx, h*h*det_xTx);

    IF debug AND Debug.GetLevel() > 999 THEN
      Debug.Out("\nxTx + h^2 I =\n" & Matrix2.FormatM(xTx));
    END;

    WITH vv = NEW(REF Matrix2.V, NUMBER(xTx)) DO
      LU.DecomposeR(xTx, vv^, indx, d)
    END;

    (* xTx_plus_hsqi is LU decomp here *)

    IF debug AND Debug.GetLevel() > 999 THEN
      Debug.Out("\nLU(xTx + h^2 I) =\n" & Matrix2.FormatM(xTx));
    END;

    VAR
      col := NEW(REF Matrix2.V, Matrix2.GetDim(x).cols);
    BEGIN
      RidgeRegressStep2(x, xTx, col^,res, indx)
    END;

    IF debug AND Debug.GetLevel() > 999 THEN
      Debug.Out("\n((xTx + h^2 I)^-1)(xT) =\n" & Matrix2.FormatM(res))
    END
  END RidgeRegress;

PROCEDURE RidgeRegressStep2(READONLY x, xTx : Matrix2.M;
                            VAR col : Matrix2.V;
                            VAR res : Matrix2.M;
                            indx : REF ARRAY OF INTEGER) =
  (* this routine, sans debugging statements, to be provided in Fortran... *)
  CONST debug = FALSE;
  BEGIN
    (* important special case: col has a single entry *)
    IF NUMBER(col) = 1 THEN
      VAR
        t := xTx[0,0];
      BEGIN
        FOR c := 0 TO Matrix2.GetDim(x).rows - 1 DO
          res[0,c] := x[c,0]/t
        END
      END
    ELSE
      FOR c := 0 TO Matrix2.GetDim(x).rows - 1 DO
        Matrix2.ExtractRowAsVector(x, c, col);
        
        IF debug AND Debug.GetLevel() > 999 THEN 
          Debug.Out("\ncol. " & Fmt.Int(c) & " of xT =\n" & Matrix2.FormatV(col))
        END;
        LU.BackSubstitute(xTx, indx, col);
        IF debug AND Debug.GetLevel() > 999 THEN 
          Debug.Out("\ncol. " & Fmt.Int(c) & " of (xTx + h^2 I)^-1 xT =\n" & Matrix2.FormatV(col))
        END;
        
        (* store in result matrix *)
        Matrix2.SetCol(res, c, col)
      END
    END
  END RidgeRegressStep2;

BEGIN END Regression.

