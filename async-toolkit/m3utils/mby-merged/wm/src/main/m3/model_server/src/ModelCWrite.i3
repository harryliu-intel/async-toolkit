(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ModelCWrite;
IMPORT Word;

<*EXTERNAL set_model_c2m3callback*>
PROCEDURE SetCallback(cb : CbProc);

TYPE CbProc = PROCEDURE(addr : ADDRESS; val : Word.T);

END ModelCWrite.
