(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SourceTextSTRING;
IMPORT source_text_lexExt;

TYPE T = TEXT;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "SourceTextString";

END SourceTextSTRING.
