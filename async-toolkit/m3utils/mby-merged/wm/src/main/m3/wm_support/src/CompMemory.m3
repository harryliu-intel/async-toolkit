MODULE CompMemory;
IMPORT CompRange, CsrOp;
IMPORT CsrAccessStatus;
IMPORT CompAddr;
IMPORT Word;
IMPORT Debug;
IMPORT Fmt;
IMPORT CompMemoryListener, CompMemoryListenerList;

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
    top := CompRange.Hi(range);
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
    CASE op.rw OF
      CsrOp.RW.W =>
      IF op.data = NIL THEN
        (* single write *)
        IF op.fv = 0 AND op.lv = BITSIZE(Word.T)-1 THEN
          (* fast path, full word *)
          t.mem[op.at-t.aBase.word] := op.single;
          RETURN CsrAccessStatus.T.OK
        ELSE
          WITH loMask = Word.LeftShift(1,op.fv)-1,   (* lo to ignore *)
               hiMask = Word.LeftShift(1,op.lv+1)-1, (* hi to ignore *)
               fullMask = Word.Or(loMask,hiMask),    (* mask valid for old *)
               notMask  = Word.Not(fullMask),        (* mask valid for new *)
               waddr = op.at-t.aBase.word
           DO    
            t.mem[waddr] :=
                Word.Or(
                    Word.And(fullMask,t.mem[waddr]),
                    Word.And(notMask, op.single));

            VAR p := t.listeners[waddr]; BEGIN
              WHILE p # NIL DO
                p.head.callback(op);
                p := p.tail
              END
            END;
            
            RETURN CsrAccessStatus.T.OK
          END
        END
      ELSE
        <*ASSERT FALSE*>
      END
    |
      CsrOp.RW.R =>
      <*ASSERT FALSE*>
    END
  END DoCsrOp;

PROCEDURE RegisterListener(t        : T;
                           range    : CompRange.T;
                           listener : CompMemoryListener.T) =
  BEGIN
    FOR w := range.pos.word TO
             CompAddr.Minus(CompRange.Hi(range),CompAddr.FromBits(1)).word
     DO
      t.listeners[w] := CompMemoryListenerList.Cons(listener, t.listeners[w])
    END
  END RegisterListener;

BEGIN END CompMemory.
