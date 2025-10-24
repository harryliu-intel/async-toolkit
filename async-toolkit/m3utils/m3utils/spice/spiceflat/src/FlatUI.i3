(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FlatUI;
IMPORT TextSpiceInstanceSetTbl, TextTextSetTbl, TextTextTbl;

PROCEDURE REPL(assocs : TextSpiceInstanceSetTbl.T;
               symTab : TextTextSetTbl.T;
               canonTbl : TextTextTbl.T);

END FlatUI.
