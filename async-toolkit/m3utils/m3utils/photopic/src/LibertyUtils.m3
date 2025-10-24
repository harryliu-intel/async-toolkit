(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertyUtils;
IMPORT ParseParams;
IMPORT Stdio;

PROCEDURE DoParseParams() : ParseParams.T =
  BEGIN
    RETURN NEW(ParseParams.T).init(Stdio.stderr)
  END DoParseParams;

BEGIN END LibertyUtils.
