(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TreeType EXPORTS TreeType, TreeTypeClass;
IMPORT RegContainer;
IMPORT RegComponent, RegChild, RegReg, Debug, RegField, BigInt, RdlArray;
IMPORT TreeTypeSeq;
FROM Fmt IMPORT F, Int, Unsigned;
IMPORT TreeTypeArraySeq;

PROCEDURE To(c : RegComponent.T) : T =
  VAR
    res : T;
  BEGIN
    Debug.Out("TreeType.To " & c.nm);
    TYPECASE c OF
      RegContainer.T => res := MarkOffset(Container(c))
    |
      RegReg.T       => res := MarkOffset(Reg(c))
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

PROCEDURE Container(c : RegContainer.T) : T =
  VAR
    skipArc := c.skipArc();
  BEGIN
    IF skipArc THEN
      <*ASSERT c.children.size() = 1*>
      VAR
        chld := c.children.get(0);
        down := Child(chld);
        n := BigInt.ToInteger(NARROW(chld.array,RdlArray.Single).n.x);
      BEGIN
        WITH arr = NEW(Array,
                       tag  := chld.nm & "[skip]",
                       comp := c,
                       n    := n,
                       sz   := n * down.sz,
                       elem := down) DO
          down.up := arr;
          RETURN arr
        END
      END
    ELSE (* NOT skipArc *)
      VAR
        seq := NEW(TreeTypeSeq.T).init();
        res := NEW(Struct,
                   comp   := c,
                   sz     := 0,
                   fields := seq);
      BEGIN
        FOR i := 0 TO c.children.size()-1 DO
          VAR
            chld := c.children.get(i);
            ct := Child(chld);
          BEGIN
            ct.tag := chld.nm;
            IF chld.array = NIL THEN
              ct.up := res
            ELSE
              WITH n = BigInt.ToInteger(
                           NARROW(chld.array,RdlArray.Single).n.x) DO
                WITH a = NEW(Array,
                          tag    := chld.nm, 
                          comp   := chld.comp,
                          n      := n,
                          sz     := n * ct.sz,
                          elem   := ct,
                          up     := res) DO
                  ct.up := a;
                  ct := a
                END
              END;
            END;
            seq.addhi(ct);
            INC(res.sz, ct.sz);
          END
        END(*FOR*);

        RETURN res
      END
    END;
  END Container;

PROCEDURE Child(c : RegChild.T) : T = 
  BEGIN
    <*ASSERT NOT ISTYPE(c.comp, RegField.T)*>
    RETURN To(c.comp)
  END Child;

PROCEDURE Reg(c : RegReg.T) : T = 
  VAR
    seq := NEW(TreeTypeSeq.T).init();
    res := NEW(Struct,
                   sz      := c.fields.size(),
                   comp    := c,
                   fields  := seq);
  BEGIN
    FOR i := 0 TO c.fields.size()-1 DO
      WITH f = c.fields.get(i) DO
        seq.addhi(NEW(Field,
                      tag  := f.nm,
                      sz   := 1,
                      comp := f,
                      up   := res))
      END
    END;
    
    RETURN res
  END Reg;

PROCEDURE Format(type : T) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    TYPECASE type OF
      Array(a) =>
      res := F("Array n %s stride %s strideb 16_%s strideB %s",
               Int(a.n), Int(a.stride), Unsigned(a.strideBits),
               Int(a.strideBits DIV 8))
    |
      Struct(s) =>
      res := F("Struct fields %s", Int(s.fields.size()))
    |
      Field => res := "Field"
    ELSE
      res := F("Unknown")
    END;
    res := res & F(" \"%s\"", type.tag);
    res := res &
               F(" sz %s off %s addr %s addrb 16_%s",
                 Int(type.sz), Int(type.offset), Int(type.address),
                 Unsigned(type.addrBits));
    RETURN res
  END Format;

PROCEDURE ComputeAddresses(tree : T; base : CARDINAL; ac : AddressConverter) =
  BEGIN
    Debug.Out("ComputeAddresses " & tree.tag);
    tree.address := base;
    tree.addrBits := ac.field2bit(tree.address);
    TYPECASE tree OF
      Array(a) =>
      ComputeAddresses(a.elem, base, ac);
      a.stride := a.elem.sz;
      IF a.n <= 1 THEN
        a.strideBits := 0
      ELSE
        WITH e0 = a.addrBits,
             f1 = a.address + a.stride,
             e1 = ac.field2bit(f1),
             sb = e1 - e0 DO
          Debug.Out("stride: " & tree.tag);
          Debug.Out(F("stride: e0 %s f1 %s e1 %s sb %s mod8 %s",
                      Int(e0),Int(f1),Int(e1),Int(sb), Int(sb MOD 8)));
          a.strideBits := sb
        END
      END;
      <*ASSERT a.strideBits MOD 8 = 0*>
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

PROCEDURE GetArrays(t : T; seq : TreeTypeArraySeq.T) =
  BEGIN
    TYPECASE t OF
      NULL => RETURN
    |
      Array(a) => seq.addhi(t)
    ELSE
      (* skip *)
    END;
    GetArrays(t.up, seq)
  END GetArrays;

BEGIN END TreeType.
