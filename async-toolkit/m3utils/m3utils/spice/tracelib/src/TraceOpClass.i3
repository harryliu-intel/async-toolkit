(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TraceOpClass;
IMPORT TraceOp;
IMPORT Rd;
IMPORT Trace;

REVEAL
  TraceOp.T <: Private;
  TraceOp.Array <: PrivateArray;
  TraceOp.Pickle <: PrivatePickle;

  (* 
     implementers of concrete subtypes of TraceOp.T should
     provide an eval() method as declared below

     eval() should write its result to the result field of the
     object.
  *)
  
TYPE
  Private = OBJECT
    trace  : Trace.T;
  METHODS
    eval() RAISES { Rd.EndOfFile, Rd.Failure };
  END;
  
  PrivateArray = TraceOp.PublicArray OBJECT
    result : REF ARRAY OF LONGREAL;
  END;

  PrivatePickle = TraceOp.PublicPickle OBJECT
    result : REFANY;
  END;

END TraceOpClass.
