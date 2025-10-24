(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FieldData;

PROCEDURE ArrayGet(a : AP; idx : CARDINAL) : T =
  BEGIN RETURN a[idx] END ArrayGet;
  
PROCEDURE ArraySize(a : AP) : CARDINAL =
  BEGIN RETURN NUMBER(a^) END ArraySize;

BEGIN END FieldData.
