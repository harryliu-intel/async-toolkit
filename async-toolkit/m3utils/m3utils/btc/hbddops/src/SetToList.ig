(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: SetToList.ig,v 1.1 2014/02/09 11:16:08 mika Exp $ *)

GENERIC INTERFACE SetToList(Base, BaseSet);
IMPORT SchemePair;

PROCEDURE SetToList(set : BaseSet.T) : SchemePair.T;

PROCEDURE SetToArr(set : BaseSet.T) : REF ARRAY OF Base.T;

CONST Brand = "SetToList(" & BaseSet.Brand &")";

END SetToList.
