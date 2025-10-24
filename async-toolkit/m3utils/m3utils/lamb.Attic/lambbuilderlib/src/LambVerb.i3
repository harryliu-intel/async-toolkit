(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LambVerb;

(* Radr, Wadr, and Wdata are pseudo-verbs! *)

TYPE
  T =   {  Nop , Read ,  Writ ,  RdWr, Radr , Wadr,  Wdata   };

CONST Names = ARRAY T OF TEXT                        
    { "Nop", "Read", "Writ", "RdWr", "Radr", "Wadr", "Wdata" };
  Nops = ARRAY T OF [-1..LAST(CARDINAL)]
    {    0 ,     .. };

CONST Brand = "LambVerb";

END LambVerb.
