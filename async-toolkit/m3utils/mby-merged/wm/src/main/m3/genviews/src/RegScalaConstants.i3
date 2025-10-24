(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegScalaConstants;

CONST PathSep = "__";

CONST Brand = "Scala";

PROCEDURE IdiomName(txt : TEXT; debug : BOOLEAN := TRUE) : TEXT;
      
END RegScalaConstants.
