(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyHead;
IMPORT LibertyComponent;
IMPORT LibertyParamList;
IMPORT LibertyAttrValSeq;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    ident  : TEXT;
    sep    : TEXT;
    params : LibertyAttrValSeq.T;
  END;

PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T;

CONST Brand = "LibertyHead";

END LibertyHead.
