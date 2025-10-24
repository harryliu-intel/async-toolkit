(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE GenViews(Tgt);
IMPORT OSError, Wr, Thread, Pathname;

TYPE T <: Tgt.Gen;

TYPE
  Compiler = Tgt.Public OBJECT
    gv : T;
  METHODS
    write(dirPath : Pathname.T; phase : Tgt.Phase)
      RAISES { OSError.E, Wr.Failure, Thread.Alerted };
  END;
     
END GenViews.
