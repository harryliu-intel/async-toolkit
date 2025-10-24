(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechKill;
IMPORT Pathname, Rd, OSError;

PROCEDURE FromLogFile(logFn : Pathname.T)
  RAISES { Rd.Failure, OSError.E };

END TechKill.
