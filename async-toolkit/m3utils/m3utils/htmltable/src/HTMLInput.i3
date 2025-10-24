(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE HTMLInput;
IMPORT HTML;

TYPE
  T <: Public;

  Type = { button, checkbox, file, hidden, image, password, radio,
           reset, submit, text };
  

  Public = HTML.T OBJECT
  METHODS
    init(type : Type; name, value, src, style, onkeypress, onkeyup : TEXT := NIL) : T;
  END;

END HTMLInput.
