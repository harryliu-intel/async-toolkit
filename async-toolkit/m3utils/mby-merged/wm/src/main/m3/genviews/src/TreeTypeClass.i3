(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TreeTypeClass;
IMPORT TreeType;
IMPORT TreeTypeSeq;
IMPORT TreeTypeArraySeq;

REVEAL
  TreeType.Struct = TreeType.T BRANDED OBJECT
    fields : TreeTypeSeq.T;
  END;

PROCEDURE GetArrays(t : TreeType.T; seq : TreeTypeArraySeq.T);

END TreeTypeClass.
