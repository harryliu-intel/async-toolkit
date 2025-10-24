(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyCsv;
IMPORT SchemePair;

PROCEDURE ToList(csvString : TEXT) : SchemePair.T;

PROCEDURE ToCsv(lst : SchemePair.T) : TEXT;

CONST Brand = "LibertyCsv";

END LibertyCsv.
