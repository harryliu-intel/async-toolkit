(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlPropertyDefault;
IMPORT RdlStr, RdlNum;

TYPE
  T = ROOT BRANDED OBJECT END;

  Str = T OBJECT
    str : RdlStr.T;
  END;

  Num = T OBJECT
    num : RdlNum.T;
  END;

  Boolean = T OBJECT
    val : BOOLEAN;
  END;

CONST Brand = "RdlPropertyDefault";

END RdlPropertyDefault.
