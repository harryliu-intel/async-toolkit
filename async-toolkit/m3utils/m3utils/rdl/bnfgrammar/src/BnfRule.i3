(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BnfRule;
IMPORT BnfType AS Bnf;
IMPORT SyntaxType;

TYPE
  T = RECORD
    t    : TEXT;
    b    : Bnf.T;
    type : SyntaxType.T;
  END;

CONST Brand = "TextBnf";

END BnfRule.
