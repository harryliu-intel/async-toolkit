(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FieldData;
FROM ContainerData IMPORT Pos, Neg;
IMPORT Word; 

TYPE
  T = RECORD
    id   : Pos;
    up   : Neg;
    byte : Word.T;  (* limits us to 2^64 bytes *)
    lsb  : [0..8-1];
    wid  : CARDINAL; 
  END;

  AP = REF ARRAY OF T;

PROCEDURE ArrayGet(a : AP; idx : CARDINAL) : T;
PROCEDURE ArraySize(a : AP) : CARDINAL;
  
CONST Brand = "FieldData";

END FieldData.
