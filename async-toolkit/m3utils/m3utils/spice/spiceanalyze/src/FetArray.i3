(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FetArray;
IMPORT CktElement;
IMPORT CktElementSeq;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;

    addToRow(e : CktElement.T; row : CARDINAL);
    (* 0 is the row nearest the output (drain end of network) --
       increases towards the power supply *)

    nrows() : CARDINAL;
    nelems() : CARDINAL;
    
    getRow(row : CARDINAL) : Row;
  END;

  Row = CktElementSeq.T;

CONST Brand = "FetArray";

END FetArray.
