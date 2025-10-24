(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE VectorUtils;
IMPORT LRVector;

PROCEDURE Orthogonalize(READONLY da : ARRAY OF LRVector.T);
  (* orthogonalizes (orthonormalizes) the first N elements of da;
     doesnt touch da[0] *)

END VectorUtils.
