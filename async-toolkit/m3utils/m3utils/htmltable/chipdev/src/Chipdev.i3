(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE Chipdev;
IMPORT DBerr;

PROCEDURE InitDB();
  (* call this right after initializing database *)

PROCEDURE StartURL() : TEXT;

END Chipdev.
