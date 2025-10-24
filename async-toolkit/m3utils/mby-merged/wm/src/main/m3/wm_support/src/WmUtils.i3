(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE WmUtils;
IMPORT Word;

PROCEDURE GetUnnamedField(rvalue     : Word.T;
                          start, len : CARDINAL) : Word.T;

PROCEDURE ModfyUnnamedField(rvalue : Word.T;
                            start, len : CARDINAL;
                            value : Word.T) : Word.T;

END WmUtils.
