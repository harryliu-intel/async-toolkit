MODULE TreeType EXPORTS TreeType, TreeTypeClass;
IMPORT RegComponentTypeTbl;
IMPORT RegContainer;
IMPORT RegComponent, RegChild, RegReg, Debug, RegField, BigInt, RdlArray;
IMPORT TreeTypeSeq;
FROM Fmt IMPORT F, Int, Unsigned;

PROCEDURE To(c : RegComponent.T; tbl : RegComponentTypeTbl.T) : T =
  VAR
    res : T;
  BEGIN
    Debug.Out("TreeType.To " & c.nm);
    TYPECASE c OF
      RegContainer.T => res := MarkOffset(Container(c, tbl))
    |
      RegReg.T       => res := MarkOffset(Reg(c, tbl))
    |
      RegField.T     => <*ASSERT FALSE*> (* right? *)
    ELSE
      <*ASSERT FALSE*>
    END;
    res.tag := "ROOT";
    RETURN res
  END To;

PROCEDURE MarkOffset(t : T) : T =
  BEGIN
    TYPECASE t OF
      Array(a) => a.elem.offset := 0
    |
      Struct(s) =>
      VAR off := 0; BEGIN
        FOR i := 0 TO s.fields.size()-1 DO
          WITH z = s.fields.get(i) DO
            z.offset := off;
            INC(off, z.sz)
          END
        END
      END
    |
      Field => <*ASSERT FALSE*>
    ELSE
      <*ASSERT FALSE*>
    END;
    RETURN t
  END MarkOffset;

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
                       tag  := chld.nm & "[skip]",
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
            ct.tag := chld.nm;
            IF chld.array # NIL THEN
              WITH n = BigInt.ToInteger(
                           NARROW(chld.array,RdlArray.Single).n.x) DO
                ct := NEW(Array,
                          tag    := chld.nm, 
                          comp   := chld.comp,
                          n      := n,
                          sz     := n * ct.sz,
                          elem   := ct)
              END;
              EVAL tbl.put(chld.comp, ct)
            END;
            seq.addhi(ct);
            INC(sz, ct.sz);
          END
        END(*FOR*);

        WITH res = NEW(Struct,
                       sz     := sz,
                       comp   := c,
                       fields := seq) DO
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
      WITH f = c.fields.get(i) DO
        seq.addhi(NEW(Field,
                      tag  := f.nm,
                      sz   := 1,
                      comp := f))
      END
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
  VAR
    res : TEXT;
  BEGIN
    TYPECASE type OF
      Array(a) =>
      res := F("Array n %s stride %s strideb %s",
               Int(a.n), Int(a.stride), Unsigned(a.strideBits))
    |
      Struct(s) =>
      res := F("Struct fields %s", Int(s.fields.size()))
    ELSE
      res := F("Unknown")
    END;
    res := res & F(" \"%s\"", type.tag);
    res := res &
               F(" sz %s off %s addr %s addrb %s",
                 Int(type.sz), Int(type.offset), Int(type.address),
                 Unsigned(type.addrBits));
    RETURN res
  END Format;

PROCEDURE ComputeAddresses(tree : T; base : CARDINAL; ac : AddressConverter) =
  BEGIN
    tree.address := base;
    tree.addrBits := ac.field2bit(tree.address);
    TYPECASE tree OF
      Array(a) =>
      ComputeAddresses(a.elem, base, ac);
      a.stride := a.elem.sz;
      IF a.n <= 1 THEN
        a.strideBits := 0
      ELSE
        a.strideBits := ac.field2bit(a.address + a.stride) - a.addrBits
      END
    |
      Struct(s) =>
      FOR i := 0 TO s.fields.size()-1 DO
        WITH f = s.fields.get(i) DO
          ComputeAddresses(f, base, ac);
          INC(base, f.sz)
        END
      END
    |
      Field => (* skip -- no children *)
    ELSE
      <*ASSERT FALSE*>
    END;
    Debug.Out(Format(tree))
  END ComputeAddresses;
  
BEGIN END TreeType.
