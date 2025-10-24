(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE PicMerge;
IMPORT PicList;

REVEAL
  T = Public BRANDED Brand OBJECT
    lst : PicList.T;
  OVERRIDES
  END;

BEGIN END PicMerge;
