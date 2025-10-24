(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FileNamer;
IMPORT Pathname;
FROM NameControl IMPORT FileIndex;
FROM Fmt IMPORT F, Int;

REVEAL
  T = Public BRANDED Brand OBJECT
    wd     : Pathname.T;
    nFiles : CARDINAL;
    nNames : CARDINAL;
  OVERRIDES
    init := Init;
    name := Name;
    getWd := GetWd;
  END;

PROCEDURE Init(t : T; wd : Pathname.T; nFiles : CARDINAL; nNames : CARDINAL) : T=
  BEGIN
    t.wd := wd; t.nFiles := nFiles; t.nNames := nNames;
    RETURN t
  END Init;

PROCEDURE GetWd(t : T) : Pathname.T = BEGIN RETURN t.wd END GetWd;

PROCEDURE Name(t : T; idx : CARDINAL) : Pathname.T =
  BEGIN
    RETURN t.wd & "/" & FormatFN(FileIndex(t.nFiles, t.nNames, idx))
  END Name;
  
PROCEDURE FormatFN(i : CARDINAL) : TEXT =
  BEGIN RETURN F("%08s", Int(i)) END FormatFN;

BEGIN END FileNamer.
