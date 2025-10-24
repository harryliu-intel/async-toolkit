(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE UdpAdapter;
IMPORT UDP;

TYPE
  T = UDP.T OBJECT METHODS
  END;

  Default <: T;
  (* an adapter wrapped around a regular UDP.T *)
  
CONST Brand = "UdpAdapter";

END UdpAdapter.
      
