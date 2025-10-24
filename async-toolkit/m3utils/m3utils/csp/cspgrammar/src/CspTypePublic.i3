(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspTypePublic;
IMPORT CspType;
IMPORT CspStructMemberSeq;
IMPORT CspExpression;
IMPORT CspInterval;

REVEAL
  CspType.ChannelStructure = CspType.T BRANDED CspType.Brand & " ChannelStructure" OBJECT
    members : CspStructMemberSeq.T;
  END;

  CspType.Integer <: CspType.MayBeConst OBJECT
    isSigned : BOOLEAN;
    dw                : CspExpression.T;
    hasInterval       : BOOLEAN;
    interval          : CspInterval.T;
  END;
  
END CspTypePublic.
