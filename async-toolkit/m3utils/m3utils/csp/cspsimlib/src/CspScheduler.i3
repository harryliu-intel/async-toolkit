(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspScheduler;
IMPORT Word;

TYPE T = BRANDED OBJECT END;

VAR GetTime : PROCEDURE () : Word.T;

END CspScheduler.
    
