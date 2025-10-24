(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE N7PMRO;
IMPORT N7Tech;
IMPORT PMRO;

TYPE
  T = ARRAY N7Tech.Transistor OF PMRO.T;

  (* from PMRO data sheet *)
  
CONST
  V = T {
  PMRO.T { 394, 450, 512 }, (* ulvt *)
  PMRO.T { 310, 367, 430 }, (* lvt  *)
  PMRO.T { 242, 292, 348 }  (* svt  *)
  };

END N7PMRO.
