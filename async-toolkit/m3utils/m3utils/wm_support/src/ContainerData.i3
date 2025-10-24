(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ContainerData;
IMPORT AddrVisitor;

TYPE Neg = [ FIRST(INTEGER) .. -1 ];
     Pos = CARDINAL;

TYPE
  T =  AddrVisitor.Internal OBJECT
    id : Neg;
    up : T;
  END;

CONST Brand = "ContainerData";

END ContainerData.
