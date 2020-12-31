MODULE SpiceAnalyze;
IMPORT Debug;
IMPORT SpiceCircuit;
IMPORT TextSpiceObjectTbl;
FROM Fmt IMPORT F, Int;
IMPORT CktGraph;
IMPORT CktCell;
IMPORT TextCktCellTbl;
IMPORT SpiceObject;
IMPORT TextSpiceCircuitTbl;

REVEAL
  CktCell.T = BRANDED OBJECT
    typeNm   : TEXT;
    subcells : TextCktCellTbl.T;
  END;

PROCEDURE Cell(nm      : TEXT;
               ckt     : SpiceCircuit.T;
               power   : PowerSets;
               hierTbl : TextCktCellTbl.T;
               subCkts : TextSpiceCircuitTbl.T) =
  VAR
    symtab := NEW(TextSpiceObjectTbl.Default).init();

  BEGIN
    Debug.Out("Working on " & nm);
    Debug.Out(F("name %s", ckt.name));
    FOR i := 0 TO ckt.params.size() - 1 DO
      Debug.Out(F("param[%s] %s", Int(i), ckt.params.get(i)))
    END;
    FOR i := 0 TO ckt.elements.size() - 1 DO
      WITH obj = ckt.elements.get(i) DO
        <*ASSERT obj.name # NIL*>
        Debug.Out(F("elem[%s] %s", Int(i), obj.name));
        EVAL symtab.put(obj.name, obj)
      END
    END;

    VAR
      hier := BuildCellStructure(ckt, symtab, hierTbl);
    BEGIN
      Debug.Out(F("hier.name %s subcells %s",
                  hier.typeNm,
                  Int(hier.subcells.size())));
      EVAL hierTbl.put(ckt.name, hier);
    END

  END Cell;

PROCEDURE BuildCellStructure(ckt    : SpiceCircuit.T;
                             symtab : TextSpiceObjectTbl.T;
                             typeTbl : TextCktCellTbl.T) : CktCell.T  =
  VAR
    res := NEW(CktCell.T,
               typeNm := ckt.name,
               subcells := NEW(TextCktCellTbl.Default).init());
    
    subc : CktCell.T;
  BEGIN
    FOR i := 0 TO ckt.elements.size() - 1 DO
      TYPECASE ckt.elements.get(i) OF
        SpiceObject.X(e) =>
        WITH hadIt = typeTbl.get(e.type, subc) DO
          IF NOT hadIt THEN
            Debug.Error(F("Working on %s : subcell type %s not found",
                          ckt.name, e.type))
          END;
          WITH hadIt2 = res.subcells.put(e.name, subc) DO
            IF hadIt2 THEN
              Debug.Error(F("Working on %s : subcell instance %s multiply defined",
                            ckt.name, e.name))
            END
          END
        END
      ELSE
      END
    END;
    RETURN res
  END BuildCellStructure;

BEGIN END SpiceAnalyze.
