(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GenCUtils;
IMPORT TextSeq, RdlArray, RegCGenState;
IMPORT RegComponent, RegGenState;

PROCEDURE FmtConstant(xDecls : TextSeq.T; val : TEXT; nm, sfx : TEXT);

PROCEDURE FmtArrSz(xDecls : TextSeq.T; a : RdlArray.Single; nm : TEXT);

PROCEDURE PutXDecls(gs : RegCGenState.T; xDecls : TextSeq.T);

TYPE
  Variant = RECORD ptr : FieldType;  sfx : TEXT END;
  FieldType = { UInt, Pointer, Id };

PROCEDURE FmtFieldType(baseType : TEXT; ft : FieldType) : TEXT;
  
PROCEDURE FmtFieldModifier(ft : FieldType) : TEXT;

PROCEDURE ArrSz(a : RdlArray.Single) : CARDINAL;

PROCEDURE ComponentTypeName(c : RegComponent.T; gs : RegGenState.T) : TEXT;
  
END GenCUtils.
