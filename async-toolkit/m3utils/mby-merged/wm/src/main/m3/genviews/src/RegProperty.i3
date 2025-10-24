(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyRvalueKeyword;
IMPORT ParseError;

PROCEDURE GetKw(q : RdlPropertyAssignRhs.Const) : RdlPropertyRvalueKeyword.T;

PROCEDURE GetNumeric(q : RdlPropertyAssignRhs.Const) : INTEGER;

PROCEDURE Unquote(str : TEXT) : TEXT RAISES { ParseError.E };

CONST Brand = "RegProperty";
      
END RegProperty.
