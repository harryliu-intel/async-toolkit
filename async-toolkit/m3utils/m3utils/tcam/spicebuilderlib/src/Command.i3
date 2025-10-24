(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Command;
IMPORT Verb;

TYPE
  T = RECORD
    v      : Verb.T;
    p0, p1 : INTEGER := LAST(INTEGER); (* should probably be "Integer.T" *)
  END;

CONST Brand = "Command";

END Command.

