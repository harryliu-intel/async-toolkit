(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE INTERFACE UnsafeUpdater;
IMPORT CompPath;
IMPORT Updater;

TYPE
  T <: Public;

  Public = Updater.T OBJECT METHODS
    init(base : REFANY; fieldAddr : ADDRESS; width : CARDINAL; nm : CompPath.T) : T;
    getWidth() : CARDINAL;
    getNm() : CompPath.T;
  END;

CONST Brand = "UnsafeUpdater";

VAR doDebug : BOOLEAN;

END UnsafeUpdater.
     
  
