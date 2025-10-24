(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE UnixGid;
IMPORT Word;

PROCEDURE Hash(t : T) : Word.T = BEGIN RETURN t END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

BEGIN END UnixGid.
