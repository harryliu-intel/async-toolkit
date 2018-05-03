MODULE CompMemory;
IMPORT CompRange, CsrOp;
IMPORT CsrAccessStatus;
IMPORT CompAddr;
IMPORT Word;
IMPORT Debug;
IMPORT Fmt;
IMPORT CompMemoryListener, CompMemoryListenerList;
IMPORT CompMemoryListenerSetDef;

REVEAL
  T = Public BRANDED Brand OBJECT
    base : CompAddr.T;
    (* base of the memory range.
       memory range is Word aligned, regardless of the base.
       if the memory range is requested to be unaligned at the base,
       the first bits of the 0th Word are not used *)

    aBase : CompAddr.T;
    (* base of allocation above *)

    top : CompAddr.T;
    (* first invalid bit *)
    
    mem : REF ARRAY OF Word.T;
    (* the actual memory itself *)

    listeners : REF ARRAY OF CompMemoryListenerList.T;
    (* a sledgehammer approach to callbacks *)
    
  OVERRIDES
    init             := Init;
    csrOp            := DoCsrOp;
    registerListener := RegisterListener;
  END;

PROCEDURE Init(t : T; range : CompRange.T) : T =
  VAR
    base := range.pos;
    aBase := CompAddr.T { base.word, 0 };
    top := CompRange.Lim(range);
    nwords : CARDINAL;
  BEGIN
    IF top.bit = 0 THEN
      nwords := top.word - aBase.word
    ELSE
      nwords := top.word - aBase.word + 1
    END;

    t.base := range.pos;
    t.aBase := aBase;
    t.top := top;
    t.mem := NEW(REF ARRAY OF Word.T, nwords);
    t.listeners := NEW(REF ARRAY OF CompMemoryListenerList.T, nwords);

    FOR i := FIRST(t.listeners^) TO LAST(t.listeners^) DO
      t.listeners[i] := NIL
    END;
    
    Debug.Out("CompMemory.Init: allocated " & Fmt.Int(nwords) & " machine words.");
    RETURN t
  END Init;

PROCEDURE DoCsrOp(t : T; op : CsrOp.T) : CsrAccessStatus.T =
  BEGIN
    WITH waddr = op.at-t.aBase.word DO
      IF op.data = NIL AND op.fv = 0 AND op.lv = BITSIZE(Word.T)-1 THEN
        (* fast path, full word *)

        CASE op.rw OF
          CsrOp.RW.W =>
          t.mem[waddr] := op.single;
          VAR p := t.listeners[waddr]; BEGIN
            WHILE p # NIL DO
              p.head.callback(op);
              p := p.tail
            END
          END
        |
          CsrOp.RW.R =>
          op.single := t.mem[waddr]
        END;
        RETURN CsrAccessStatus.T.OK
        
      ELSIF op.data = NIL THEN
        WITH loMask   = Word.LeftShift(1,op.fv)-1,         (* lo ignore *)
             hiMask   = Word.Not(Word.Shift(1,op.lv+1)-1), (* hi ignore *)
             
             fullMask = Word.Or(loMask,hiMask)    (* mask valid for old *)
         DO    
          CASE op.rw OF
            CsrOp.RW.W =>
            CopyWithMask(t.mem[waddr], op.single, fullMask);
            
            VAR p := t.listeners[waddr]; BEGIN
              WHILE p # NIL DO
                p.head.callback(op);
                p := p.tail
              END
            END;
            
            RETURN CsrAccessStatus.T.OK
          |
            CsrOp.RW.R =>
            CopyWithMask(op.single, t.mem[waddr], fullMask);
            RETURN CsrAccessStatus.T.OK
          END
        END(*WITH*)
      ELSE
        CASE op.rw OF
          CsrOp.RW.W =>
          RETURN DoCsrWriteMulti(t, op)
        |
          CsrOp.RW.R =>
          RETURN DoCsrReadMulti(t, op)
        END
      END
    END(*WITH*)
  END DoCsrOp;

PROCEDURE CopyWithMask(VAR      lhs      : Word.T; (* dest *)
                       READONLY rhs      : Word.T; (* src  *)
                       READONLY fullMask : Word.T  (* bits NOT to modify *)
  ) =
  BEGIN
    WITH notMask = Word.Not(fullMask) DO
      lhs := Word.Or(Word.And(fullMask, lhs), Word.And(notMask, rhs))
    END
  END CopyWithMask;

PROCEDURE DoCsrWriteMulti(t : T; op : CsrOp.T) : CsrAccessStatus.T =
  BEGIN
    WITH waddr  = op.at-t.aBase.word,
         lwaddr = MIN(waddr + NUMBER(op.data^) - 1, LAST(t.mem^)),
         loMask = Word.LeftShift(1,op.fv)-1,
         hiMask = Word.Not(Word.LeftShift(1,op.lv+1)-1)
     DO
      (* special case, single word *)
      IF waddr = lwaddr THEN
        WITH fullMask = Word.Or(loMask, hiMask) DO
          CopyWithMask(t.mem[waddr], op.data[0], fullMask)
        END
      ELSE
        (* first word *)
        CopyWithMask(t.mem[waddr],op.data[0],loMask);

        FOR i := 1 TO NUMBER(op.data^)-2 DO
          t.mem[i+waddr] := op.data[i]
        END;

        (* last word *)
        CopyWithMask(t.mem[lwaddr], op.data[LAST(op.data^)], hiMask)
      END;

      WITH s = NEW(CompMemoryListenerSetDef.T).init() DO
        FOR i := waddr TO lwaddr DO
          VAR p := t.listeners[i]; BEGIN
            WHILE p # NIL DO EVAL s.insert(p.head); p := p.tail END
          END
        END;
        
        VAR
          iter := s.iterate();
          l : CompMemoryListener.T;
        BEGIN
          WHILE iter.next(l) DO l.callback(op) END
        END
      END;
      
      RETURN CsrAccessStatus.T.OK
    END
  END DoCsrWriteMulti;
    
PROCEDURE DoCsrReadMulti(t : T; op : CsrOp.T) : CsrAccessStatus.T =
  BEGIN
    WITH waddr  = op.at-t.aBase.word,
         lwaddr = MIN(waddr + NUMBER(op.data^) - 1, LAST(t.mem^)),
         loMask = Word.LeftShift(1,op.fv)-1,
         hiMask = Word.Not(Word.LeftShift(1,op.lv+1)-1)
     DO
      (* special case, single word *)
      IF waddr = lwaddr THEN
        WITH fullMask = Word.Or(loMask, hiMask) DO
          CopyWithMask(op.data[0], t.mem[waddr], fullMask)
        END
      ELSE
        (* first word *)
        CopyWithMask(op.data[0],t.mem[waddr],loMask);

        FOR i := 1 TO NUMBER(op.data^)-2 DO
          op.data[i] := t.mem[i+waddr]
        END;

        (* last word *)
        CopyWithMask(op.data[LAST(op.data^)], t.mem[lwaddr], hiMask)
      END;

      RETURN CsrAccessStatus.T.OK
    END
  END DoCsrReadMulti;
    
PROCEDURE RegisterListener(t        : T;
                           range    : CompRange.T;
                           listener : CompMemoryListener.T) =
  BEGIN
    FOR w := range.pos.word TO
             CompAddr.Minus(CompRange.Lim(range),CompAddr.FromBits(1)).word
     DO
      t.listeners[w] := CompMemoryListenerList.Cons(listener, t.listeners[w])
    END
  END RegisterListener;

BEGIN END CompMemory.
