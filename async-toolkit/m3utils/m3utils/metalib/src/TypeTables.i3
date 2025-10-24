(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TypeTables;
IMPORT NameNameTbl, NameNameListTbl, Dsim, NameRefTbl;

TYPE 
  Tables = RECORD 
    instanceTypes : NameNameTbl.T;
    typeInstances : NameNameListTbl.T;
  END;

PROCEDURE Make(root : Dsim.Define; types : NameRefTbl.T) : Tables;

END TypeTables.
       
