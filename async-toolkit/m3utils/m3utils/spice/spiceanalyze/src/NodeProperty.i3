(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NodeProperty;

TYPE
  T = { IsNfetSourceDrain,
        IsPfetSourceDrain,
        IsNfetBody,
        IsPfetBody,
        IsNfetGate,
        IsPfetGate,
        IsVdd,
        IsGnd,
        IsCellBoundaryNode,
        IsDiodeCathode,
        IsDiodeAnode
  };

CONST Brand = "NodeProperty";

END NodeProperty.
