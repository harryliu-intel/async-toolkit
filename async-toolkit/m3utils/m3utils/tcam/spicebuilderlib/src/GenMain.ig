(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE GenMain();

IMPORT ParseParams, Wr, OSError;

PROCEDURE DoIt(pp : ParseParams.T) RAISES { ParseParams.Error, Wr.Failure, OSError.E };

END GenMain.
