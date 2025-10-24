(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RegField;
IMPORT RdlNum;
IMPORT RegComponent;
IMPORT RegFieldAccess;

CONST
  Unspecified = LAST(CARDINAL);
  
TYPE
  T = RegComponent.T OBJECT
    width, lsb := Unspecified;
    defVal : RdlNum.T := NIL;
    reserved : BOOLEAN;
    access : RegFieldAccess.T;
  METHODS
    name(debug := TRUE) : TEXT;
    (* if debug is set to TRUE the identifier shall include a comment
       (for the target language) showing its original form;
       if debug is set to FALSE only the identifier shall be returned *)
  END;

PROCEDURE Compare(a, b : T) : [-1..1];
  (* sort by lsb *)
  
CONST
  Brand = "RegField";

END RegField.
