(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HTMLTextArea;
IMPORT HTML;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    init(name, contents: TEXT := NIL; rows, cols := -1) : T;
  END;

END HTMLTextArea.
