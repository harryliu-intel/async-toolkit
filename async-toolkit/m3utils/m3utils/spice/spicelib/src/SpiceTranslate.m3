MODULE SpiceTranslate;

IMPORT SpiceFormat;
IMPORT Gds2Cast;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT TextSpiceCircuitTbl;
IMPORT TextSeq;

PROCEDURE Translate(spice : SpiceFormat.T) =
  VAR
    t : TEXT;
    ckt : SpiceCircuit.T;
  BEGIN
    (* no need to xlate topCkt *)
    WITH iter = spice.subCkts.iterate(),
         new  = NEW(TextSpiceCircuitTbl.Default).init()
     DO
      WHILE iter.next(t, ckt) DO
        Circuit(ckt);
        WITH hadIt = new.put(Gds2Cast.Gds2Cast(t), ckt) DO
          <*ASSERT NOT hadIt*>
        END
      END;
      spice.subCkts := new
    END;

    Seq(spice.subCktNames)
  END Translate;

PROCEDURE Circuit(ckt : SpiceCircuit.T) =
  BEGIN
    IF ckt.name # NIL THEN
      ckt.name := Gds2Cast.Gds2Cast(ckt.name)
    END;
    FOR i := 0 TO ckt.elements.size() - 1 DO
      Object(ckt.elements.get(i))
    END;
    (* bindings is not for us *)
    Seq(ckt.probes)
  END Circuit;

PROCEDURE Object(obj : SpiceObject.T) =
  BEGIN
    obj.name := Gds2Cast.Gds2Cast(obj.name);
    Seq(obj.terminals)
  END Object;

PROCEDURE Seq(seq : TextSeq.T) =
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH orig = seq.get(i),
           tran = Gds2Cast.Gds2Cast(orig) DO
        seq.put(i, tran)
      END
    END
  END Seq;

BEGIN END SpiceTranslate.
