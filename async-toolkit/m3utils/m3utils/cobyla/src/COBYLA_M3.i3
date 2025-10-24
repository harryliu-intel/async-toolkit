(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE COBYLA_M3;
IMPORT LRVector, LRVectorField;

TYPE
  Result = RECORD
    f   : LONGREAL;
    con : REF ARRAY OF LONGREAL;
  END;
  
PROCEDURE Minimize((*INOUT*)p       : LRVector.T;
                   m                : CARDINAL;
                   func             : LRVectorField.T;
                   rhobeg, rhoend   : LONGREAL;
                   maxfun           : CARDINAL;
                   iprint         := 0;
                   ) : Result;

  (* 
     We must have that m = NUMBER(func) - 1 
     func[0] is the minimization function
     func[1 .. LAST(func)] are >= constraints
  *)

END COBYLA_M3.
