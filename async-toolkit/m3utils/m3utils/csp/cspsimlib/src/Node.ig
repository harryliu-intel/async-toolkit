(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE Node(Type);
IMPORT CspNode;

TYPE
  Ref = T;
  
  T = CspNode.T OBJECT
    data : Type.T;
  END;

CONST Brand = "Node(" & Type.Brand & ")";
      
END Node.
