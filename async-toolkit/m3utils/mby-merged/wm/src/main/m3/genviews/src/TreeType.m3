MODULE TreeType EXPORTS TreeType, TreeTypeClass;
IMPORT RegComponentTypeTbl;
IMPORT RegContainer;
IMPORT RegComponent, RegChild, RegReg, Debug, RegField, BigInt, RdlArray;
IMPORT TreeTypeSeq;
FROM Fmt IMPORT F, Int;

PROCEDURE To(c : RegComponent.T; tbl : RegComponentTypeTbl.T) : T =
  BEGIN
    Debug.Out("TreeType.To " & c.nm);
    TYPECASE c OF
      RegContainer.T => RETURN Container(c, tbl)
    |
      RegReg.T       => RETURN Reg(c, tbl)
    |
      RegField.T     => <*ASSERT FALSE*> (* right? *)
    ELSE
      <*ASSERT FALSE*>
    END
  END To;

PROCEDURE Container(c : RegContainer.T; tbl : RegComponentTypeTbl.T) : T =
  VAR
    skipArc := c.skipArc();
  BEGIN
    IF skipArc THEN
      <*ASSERT c.children.size() = 1*>
      VAR
        chld := c.children.get(0);
        down := Child(chld, tbl);
        n := BigInt.ToInteger(NARROW(chld.array,RdlArray.Single).n.x);
      BEGIN
        WITH arr = NEW(Array,
                       comp := c,
                       n    := n,
                       sz   := n * down.sz,
                       elem := down) DO
          EVAL tbl.put(c, arr);
          RETURN arr
        END
      END
    ELSE
      VAR
        seq := NEW(TreeTypeSeq.T).init();
        sz := 0;
      BEGIN
        FOR i := 0 TO c.children.size()-1 DO
          VAR
            chld := c.children.get(i);
            ct := Child(chld, tbl);
          BEGIN
            IF chld.array # NIL THEN
              WITH n = BigInt.ToInteger(
                           NARROW(chld.array,RdlArray.Single).n.x) DO
                ct := NEW(Array,
                          comp := chld.comp,
                          n  := n,
                          sz :=  n * ct.sz,
                          elem := ct)
              END;
              EVAL tbl.put(chld.comp, ct)
            END;
            seq.addhi(ct);
            INC(sz, ct.sz);
          END
        END(*FOR*);

        WITH res = NEW(Struct, sz := sz, comp := c, fields := seq) DO
          EVAL tbl.put(c, res);
          RETURN res
        END
      END
    END;
  END Container;

PROCEDURE Child(c : RegChild.T; tbl : RegComponentTypeTbl.T) : T =
  BEGIN
    <*ASSERT NOT ISTYPE(c.comp, RegField.T)*>
    RETURN To(c.comp, tbl)
  END Child;

PROCEDURE Reg(c : RegReg.T; tbl : RegComponentTypeTbl.T) : T =
  VAR
    seq := NEW(TreeTypeSeq.T).init();
  BEGIN
    FOR i := 0 TO c.fields.size()-1 DO
      seq.addhi(NEW(Field, sz := 1, comp := c.fields.get(i)))
    END;
    
    WITH res = NEW(Struct,
                   sz      := c.fields.size(),
                   comp    := c,
                   fields  := seq) DO
      EVAL tbl.put(c, res);
      RETURN res
    END
  END Reg;

PROCEDURE Format(type : T) : TEXT =
  BEGIN
    TYPECASE type OF
      Array(a) =>
      RETURN F("Array sz %s", Int(a.sz))
    |
      Struct(s) =>
      RETURN F("Struct sz %s fields %s", Int(s.sz), Int(s.fields.size()))
    ELSE
      RETURN F("Unknown sz %s", Int(type.sz))
    END
  END Format;

BEGIN END TreeType.
