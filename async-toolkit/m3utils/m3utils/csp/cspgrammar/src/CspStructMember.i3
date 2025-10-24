(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspStructMember;
IMPORT CspType;

TYPE
  T = RECORD
    name : TEXT; (* should this be Atom.T ? *)
    type : CspType.T;
  END;

CONST Brand = "CspStructMember";

END CspStructMember.
    
