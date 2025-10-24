(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE AreaSpec;
IMPORT Entry;

TYPE
  T = RECORD
    col    : Entry.CsvCols;
    colVal : TEXT;
    area   : LONGREAL;
  END;

CONST Brand = "AreaSpec";

END AreaSpec.
