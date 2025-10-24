(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE DatabaseClass;
FROM Database IMPORT Table, Vector, Matrix;
IMPORT DatabaseTable;

TYPE 
  PrivateTable = DatabaseTable.Public OBJECT
    fieldNames : Vector;
    data       : Matrix;
  END;

REVEAL Table <: PrivateTable;

END DatabaseClass.
