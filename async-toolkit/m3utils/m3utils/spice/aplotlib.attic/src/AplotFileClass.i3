(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE AplotFileClass;
IMPORT AplotFile;

REVEAL
  AplotFile.Default <: Class;

TYPE
  Class = AplotFile.T OBJECT METHODS
  END;

CONST Brand = "AplotFileClass";

END AplotFileClass.
