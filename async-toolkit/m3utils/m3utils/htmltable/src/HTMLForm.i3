(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)
INTERFACE HTMLForm;
IMPORT HTML,Request;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    add(stuff : HTML.Stuff);
    init(toURL : TEXT; request : Request.T) : T;
  END;

END HTMLForm.
