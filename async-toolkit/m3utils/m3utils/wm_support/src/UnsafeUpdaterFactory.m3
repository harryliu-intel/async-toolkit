(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE MODULE UnsafeUpdaterFactory;
IMPORT UpdaterFactory;
IMPORT UnsafeUpdater;
IMPORT Updater;

REVEAL
  T = UpdaterFactory.T BRANDED Brand OBJECT
  OVERRIDES
    buildT := BuildT;
  END;

PROCEDURE  BuildT(t : T) : Updater.T =
  BEGIN RETURN NEW(UnsafeUpdater.T) END BuildT;

BEGIN END UnsafeUpdaterFactory.
