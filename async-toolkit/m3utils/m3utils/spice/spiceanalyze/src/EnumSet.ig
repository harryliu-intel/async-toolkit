(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE EnumSet(Enum);

TYPE P = Enum.T;
     
TYPE T = SET OF P;

CONST Brand = "EnumSet(" & Enum.Brand & ")";

CONST
  Empty        = T {};
  
END EnumSet.
