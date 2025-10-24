(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ModelVar;
IMPORT SchemeSymbol;
IMPORT StatComponent;
IMPORT ResponseModel;

TYPE
  T = RECORD
    nm      : SchemeSymbol.T;
    orders  : ARRAY StatComponent.T OF ResponseModel.Order;
  END;

PROCEDURE Format(READONLY t : T) : TEXT;
  
CONST Brand = "ModelVar";

END ModelVar.
