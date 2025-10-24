(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ResponseModel;

IMPORT SchemeSymbol;
IMPORT SurfaceRep;
IMPORT LRMatrix2 AS M;
IMPORT LRVector;
IMPORT Wx;

TYPE
  Order = { Constant, Linear, Quadratic };

  T = RECORD
    symbol : SchemeSymbol.T;
    order  : Order;
  END;

CONST OrderNames = ARRAY Order OF TEXT { "Constant", "Linear", "Quadratic" };

CONST Brand = "ResponseModel";

TYPE DofFunc = PROCEDURE (n : CARDINAL) : CARDINAL;

CONST Dofs = ARRAY Order OF DofFunc { SurfaceRep.Cdofs,
                                    SurfaceRep.Ldofs,
                                    SurfaceRep.Qdofs };

TYPE M2QFunc = PROCEDURE(n : CARDINAL; READONLY b : M.M) : REF M.M;
     
CONST M2Q = ARRAY Order OF M2QFunc { SurfaceRep.C2Q,
                                    SurfaceRep.L2Q,
                                    SurfaceRep.Q2Q };

TYPE IndepsFunc = PROCEDURE(p             : LRVector.T;
                            row           : CARDINAL;
                            VAR(*OUT*) x  : M.M);

CONST Indeps = ARRAY Order OF IndepsFunc { SurfaceRep.ComputeIndepsC, 
                                          SurfaceRep.ComputeIndepsL,
                                          SurfaceRep.ComputeIndepsQ };

TYPE FmtProc = PROCEDURE(n : CARDINAL; b : REF M.M) : TEXT;

CONST Fmt = ARRAY Order OF FmtProc {
  SurfaceRep.FmtC, SurfaceRep.FmtL, SurfaceRep.FmtQ
  };
  
TYPE ComputeProc = PROCEDURE(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL;

CONST Compute =  ARRAY Order OF ComputeProc {
  SurfaceRep.ComputeC, SurfaceRep.ComputeL, SurfaceRep.ComputeQ
  };


END ResponseModel.
    
