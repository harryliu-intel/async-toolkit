(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyAttrVal;
IMPORT LibertyComponent;
IMPORT LibertyBoolean;
IMPORT LibertyNumber;
IMPORT LibertySorI;

TYPE
  T <: LibertyComponent.T;

  Num = T OBJECT
    val : LibertyNumber.T;
  END;

  SorI = T OBJECT
    val : LibertySorI.T;
  END;

  Colon = T OBJECT
    x, y : LibertySorI.T;
  END;

  Boolean = T OBJECT
    val : LibertyBoolean.T;
  END;

CONST Brand = "LibertyAttrVal";

END LibertyAttrVal.
