(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FileFinder;
IMPORT Pathname;
IMPORT SchemePair;
IMPORT RegEx;
IMPORT OSError;

PROCEDURE Find(dirpath : Pathname.T; pattern : TEXT) : SchemePair.T
  RAISES { RegEx.Error, OSError.E };

  

END FileFinder.
