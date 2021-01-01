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
    init := Init;
    addToRow := AddToRow;
  END;

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
  END AddToRow;

BEGIN END FetArray.
  
