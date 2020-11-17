MODULE StdfParseTable;
IMPORT CardPair, StdfCardPairRecTbl;
IMPORT StdfRecordTypes;

PROCEDURE Get(recTyp, recSub : CARDINAL; VAR res : StdfRecordTypes.T) : BOOLEAN=
  BEGIN
    RETURN tbl.get(CardPair.T { recTyp, recSub }, res)
  END Get;

VAR
  tbl := NEW(StdfCardPairRecTbl.Default).init();
  
BEGIN
  FOR i := FIRST(StdfRecordTypes.Types) TO LAST(StdfRecordTypes.Types) DO
    WITH rec = StdfRecordTypes.Types[i] DO
      EVAL tbl.put(CardPair.T { rec.recTyp, rec.recSub },
                   rec)
    END
  END
END StdfParseTable.
