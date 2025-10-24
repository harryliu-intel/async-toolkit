(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE HTMLTextArea;
IMPORT Fmt;

REVEAL
  T = Public BRANDED "HTML TextArea" OBJECT
    name, contents : TEXT := NIL; 
    rows := 10;
    cols := 40;
  OVERRIDES
    init := Init;
    format := Format;
  END;

(* XXX unfinished *)
PROCEDURE Init(self : T;
               name : TEXT; contents : TEXT; rows, cols : INTEGER) : T =
  BEGIN
    (* basically validate the input *)
    self.name := name;
    self.contents := contents;
    IF rows > 0 THEN self.rows := rows END;
    IF cols > 0 THEN self.cols := cols END;
    RETURN self
  END Init;

PROCEDURE Format(self : T) : TEXT =
  VAR
    res := "<textarea wrap=physical name=\"" & self.name & "\" "&
               "rows=" & Fmt.Int(self.rows) & 
               " cols=" & Fmt.Int(self.cols) & ">";
  BEGIN
    
    IF self.contents # NIL THEN res := res & self.contents END;
    res := res & "</textarea>";
    RETURN res
  END Format;

BEGIN END HTMLTextArea.
