(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlArray;
IMPORT RdlNum;

TYPE
  T = ROOT BRANDED OBJECT END;

  Single = T BRANDED OBJECT n : RdlNum.T END;

  Range = T BRANDED OBJECT frm, to : RdlNum.T END;

CONST Brand = "RdlArray";

END RdlArray.
