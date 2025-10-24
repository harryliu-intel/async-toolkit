(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NodeMeasurement;
IMPORT Word, ProbeType;

TYPE
  T = RECORD
    nm : TEXT;
    quantity : ProbeType.T;
  END;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Brand = "NodeMeasurement";

END NodeMeasurement.
