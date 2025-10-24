(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MyRegEx;
IMPORT RegEx;
IMPORT Refany;

TYPE T = RegEx.Pattern;

CONST Equal = Refany.Equal;

CONST Brand = "MyRegEx";

END MyRegEx.
