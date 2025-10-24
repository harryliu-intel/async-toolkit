(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FileNamer;
IMPORT Pathname;

(* compute name of temp files *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(wd : Pathname.T; nFiles : CARDINAL; nNames : CARDINAL) : T;
    name(idx : CARDINAL) : Pathname.T;
    getWd() : Pathname.T;
  END;

CONST Brand = "FileNamer";

END FileNamer.
