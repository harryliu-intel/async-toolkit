(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TransitionFinder;
IMPORT TransitionSeq;
IMPORT Trace;
IMPORT Rd;
IMPORT Transition;
IMPORT V01X;

TYPE
  T <: Public;

  Public = OBJECT METHODS (* memoizer object on id *)
    init(trace : Trace.T; thres, hysteresis : LONGREAL) : T;

    forNode(id : CARDINAL; doSlew := FALSE) : TransitionSeq.T
      RAISES { Rd.EndOfFile, Rd.Failure } ;

  END;


PROCEDURE Find(READONLY timea, nodea : ARRAY OF LONGREAL;
               thres, hysteresis     : LONGREAL;
               doSlew                := FALSE) : TransitionSeq.T;

TYPE
  Index = [ -1 .. LAST(CARDINAL) ];
  
PROCEDURE FindFloorIdx(seq  : TransitionSeq.T;
                       time : LONGREAL) : Index;
  (* find the index of last transition that occurred no later than time time in
     sequence -- -1 if the first transition occurred after the sought time *)

PROCEDURE FindValueAt(seq : TransitionSeq.T; time : LONGREAL) : V01X.T;
  
CONST Brand = "TransitionFinder";

PROCEDURE FilterDir(seq : TransitionSeq.T;
                    dir : Transition.Dir) : TransitionSeq.T;

PROCEDURE FilterTime(seq : TransitionSeq.T;
                     lo, hi : LONGREAL) : TransitionSeq.T;
  (* transitions in range [lo, hi) *)
  
END TransitionFinder.
