(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE URL;

IMPORT FloatMode, Lex;

PROCEDURE PlusToSpace(this : TEXT) : TEXT;
PROCEDURE Unescape(this : TEXT) : TEXT 
  RAISES { FloatMode.Trap, Lex.Error };

END URL.
