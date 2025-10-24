(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE TopoSort(Elem, ElemSeq);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;

    addDependency(pred, succ : Elem.T);

    sort() : ElemSeq.T;
  END;

CONST Brand = "TopoSort(" & Elem.Brand & ")";

END TopoSort.
    
