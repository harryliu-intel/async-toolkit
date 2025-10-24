(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Sortable;

TYPE
  CompRes = [ -1 .. +1 ];
  
  T = OBJECT METHODS
    compare(with : T) : CompRes;
  END;

CONST Brand = "Sortable";

PROCEDURE Equal(a, b : T) : BOOLEAN; (* pointer comparison *)

PROCEDURE Compare(a, b : T) : CompRes; (* calls a.compare(b) *)

END Sortable.
