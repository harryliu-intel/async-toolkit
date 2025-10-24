(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FetArray;
IMPORT CktGraph;
IMPORT CktElement;
IMPORT ElementPropertySet AS EtProps;
IMPORT ElementProperty;
IMPORT CktElementSeq AS ElemSeq;
IMPORT CktElementSeqSeq AS ElemSeqSeq;

REVEAL
  T = Public BRANDED Brand OBJECT
    rep : ElemSeqSeq.T;
  OVERRIDES
    init     := Init;
    addToRow := AddToRow;
    nelems   := Nelems;
    nrows    := Nrows;
    getRow   := GetRow;
  END;

PROCEDURE Nelems(t : T) : CARDINAL =
  VAR
    sz : CARDINAL := 0;
  BEGIN
    FOR i := 0 TO t.rep.size() - 1 DO
      INC(sz, t.rep.get(i).size())
    END;
    RETURN sz
  END Nelems;

PROCEDURE Nrows(t : T) : CARDINAL =
  BEGIN RETURN t.rep.size() END Nrows;

PROCEDURE GetRow(t : T; row : CARDINAL) : ElemSeq.T =
  BEGIN
    RETURN t.rep.get(row)
  END GetRow;
  
TYPE EtProp = ElementProperty.T;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.rep := NEW(ElemSeqSeq.T).init();
    RETURN t
  END Init;

PROCEDURE AddToRow(t : T; fet : CktElement.T; row : CARDINAL) =
  CONST
    IsFet = EtProps.T { EtProp.IsNfet, EtProp.IsPfet };
  BEGIN
    <*ASSERT fet.props * IsFet # EtProps.Empty*>
    WHILE row >= t.rep.size() DO
      t.rep.addhi(NEW(ElemSeq.T).init())
    END;
    t.rep.get(row).addhi(fet)
  END AddToRow;

BEGIN END FetArray.
  
