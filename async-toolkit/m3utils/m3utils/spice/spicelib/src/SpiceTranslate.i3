(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceTranslate;

(* translate names in a spice deck from GDS to CAST *)

IMPORT SpiceFormat;

PROCEDURE Translate(spice : SpiceFormat.T);

END SpiceTranslate.

  

