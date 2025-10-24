(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlComponentDefClass;
IMPORT RdlComponentDef;
IMPORT RdlRootElem;
IMPORT RdlComponentDefType;
IMPORT RdlComponentDefLstRecord;
IMPORT RdlAnonComponentInstElems;

REVEAL
  RdlComponentDef.T = RdlRootElem.T BRANDED RdlComponentDef.Brand OBJECT
    type          : RdlComponentDefType.T;
    id            : TEXT;
    list          : RdlComponentDefLstRecord.T;
    anonInstElems : RdlAnonComponentInstElems.T;
  END;

END RdlComponentDefClass.
