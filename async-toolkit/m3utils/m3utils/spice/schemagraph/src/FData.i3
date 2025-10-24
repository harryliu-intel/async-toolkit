(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FData;
IMPORT Field;

TYPE
  T =  RECORD
    type    : Field.T;
    name    : TEXT;
    formula : TEXT;
  END;
  
CONST Brand = "FData";

PROCEDURE Format(READONLY t : T) : TEXT;
      
END FData.
