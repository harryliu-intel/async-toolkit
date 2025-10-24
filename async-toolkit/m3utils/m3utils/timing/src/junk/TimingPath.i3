(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)


INTERFACE TimingPath;

TYPE
  T = RECORD
    startPoint : TEXT;
    startParen : TEXT;
    endPoint   : TEXT;
    endParen   : TEXT;
    scenario   : TEXT;
    pathGroup  : TEXT;
    pathType   : TEXT;
    transData  : TransPathData.T;
  END;

CONST Brand = "TimingPath";

END TimingPath.
