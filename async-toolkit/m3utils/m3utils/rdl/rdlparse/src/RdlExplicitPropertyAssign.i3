(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlExplicitPropertyAssign;
IMPORT RdlProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyModifier;

TYPE
  T = OBJECT
    haveModifier : BOOLEAN;
    modifier     : RdlPropertyModifier.T;
    default      : BOOLEAN;
    property     : RdlProperty.T;
    rhs          : RdlPropertyAssignRhs.T;
  END;

CONST Brand = "RdlExplicitPropertyAssign";

PROCEDURE Format(t : T) : TEXT;

END RdlExplicitPropertyAssign.
