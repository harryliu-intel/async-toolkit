(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FinInfo;

PROCEDURE Add(READONLY a, b : T) : T =
  VAR
    res : T;
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := a[i] + b[i]
    END;
    RETURN res
  END Add;

BEGIN END FinInfo.
    
