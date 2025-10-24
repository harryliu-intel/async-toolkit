(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GenViews;
IMPORT RdlComponentDef;
IMPORT DecoratedComponentDef;
IMPORT RegAddrmap;
IMPORT Pathname;
IMPORT ParseError;

TYPE
  T = OBJECT METHODS
    decorate(def       : RdlComponentDef.T;
             path      : TEXT             ;
             hierName  : TEXT              ) : DecoratedComponentDef.T
    RAISES { ParseError.E };
    gen     (tgtmap    : RegAddrmap.T;
             outDir    : Pathname.T       );
  END;

CONST Brand = "GenViews";

END GenViews.
