(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Updater;
IMPORT Word;

TYPE
  T = OBJECT
    doSync := FALSE; (* do we call sync() on every CSR write from SBIOSF *)
  METHODS
    update(to : Word.T);
    value() : Word.T;
    sync(); (* call to synchronize alternate representations *)
  END;

CONST Brand = "Updater";

END Updater.
