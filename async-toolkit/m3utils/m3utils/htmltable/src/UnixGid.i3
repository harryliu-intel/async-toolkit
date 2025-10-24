(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE UnixGid;
IMPORT Utypes;
IMPORT Word;

TYPE T = Utypes.gid_t;

PROCEDURE Hash(t : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "UnixGid";

END UnixGid.
