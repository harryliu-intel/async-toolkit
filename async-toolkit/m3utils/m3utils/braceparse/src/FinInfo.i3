(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FinInfo;

TYPE Info = { MosCnt, FinCnt, FinXLength, DrawnWidth, DrawnWidthXLength };
     
TYPE T = ARRAY Info OF CARDINAL;

CONST ColName = ARRAY Info OF TEXT { "count", "fins", "fins*pm", "drawn", "drawn*pm" };

CONST Brand = "FinInfo";

PROCEDURE Add(READONLY a, b : T) : T;

END FinInfo.
