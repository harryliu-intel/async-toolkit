(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DrawnWidth;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(defString : TEXT) : T;
    (* specify formula for the width.  formula must be purely functional *)
    
    eval(fins : [1..LAST(CARDINAL)]) : CARDINAL;
    (* convert width in fins to width in nanometers *)
  END;

CONST Brand = "DrawnWidth";

END DrawnWidth.
