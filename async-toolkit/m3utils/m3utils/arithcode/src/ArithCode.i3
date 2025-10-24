(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ArithCode;
IMPORT ArithCoder;
IMPORT FreqTable;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(READONLY frequencies : FreqTable.T) : T;
    newEncoder() : ArithCoder.T;
    newDecoder() : ArithCoder.T;
  END;

CONST Brand = "ArithCode";

END ArithCode.
