(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ReorderUdpAdapter;
IMPORT NestedUdpAdapter;
IMPORT Random;

TYPE
  T <: Public;

  Public = NestedUdpAdapter.T OBJECT METHODS
    setParams(reorderProb, dropProb, dupProb := 0.0d0;
              rand       : Random.T := NIL) : T;
    (* returns itself *)
  END;

  (* init, underlying is the underlying UDP implementation.
     if underlying is NIL, a new UdpAdapter.Default will be
     allocated *)
  
CONST Brand = "ReorderUdpAdapter";

END ReorderUdpAdapter.
