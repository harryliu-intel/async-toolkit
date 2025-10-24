(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DecoratedComponentDef;
IMPORT RdlComponentDef, RdlComponentDefClass;
IMPORT RegComponent;

REVEAL
  T = Public BRANDED Brand OBJECT 
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(dcd  : T;
               old  : RdlComponentDef.T;
               comp : RegComponent.T) : T =
  BEGIN
    dcd.type := old.type;
    dcd.id   := old.id;
    dcd.list := old.list;
    dcd.anonInstElems := old.anonInstElems;

    dcd.comp := comp;

    RETURN dcd
  END Init;

BEGIN END DecoratedComponentDef.
