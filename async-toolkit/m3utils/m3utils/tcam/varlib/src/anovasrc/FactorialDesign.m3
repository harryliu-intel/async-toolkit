(* $Id: FactorialDesign.m3,v 1.11 2010/04/22 20:10:05 mika Exp $ *)

MODULE FactorialDesign;
IMPORT FactorialBindings;
IMPORT Text;
IMPORT Fmt;
FROM FactorialBindings IMPORT NoSuchVar, Var, Type, VarIterator, PublicVarIterator;
IMPORT Wr, FileWr, OSError;
IMPORT FactorialValues AS Values;
IMPORT FactorialSuper;
IMPORT TextSet, TextSetDef, IntSet, IntSetDef;
IMPORT FactorialIndex;

TYPE B = FactorialBindings.T;

REVEAL
  T = Public BRANDED Brand OBJECT
    samplesPerSlot : CARDINAL;

    directory : TEXT;
    wr : Wr.T;
  OVERRIDES
    init := Init;
    initWr := InitWr;
    iterate := Iterate;
    getAllTextBindings := GetAllTextBindings;
    getAllIntBindings := GetAllIntBindings;
    recordResults := RecordResults;
    close := Close;
    openFile := OpenFile;
    filePath := FilePath;
    size := Size;
    getIndexedBindings := GetIndexedBindings;
    getSamplesPerSlot := GetSamplesPerSlot;
  END;

  Iterator = PublicIterator BRANDED Brand & " Iterator" OBJECT
    t : T;
    nextV : REF ARRAY OF CARDINAL; (* index into values.v *)
    nextSample : CARDINAL;
    finished := FALSE;
  OVERRIDES
    next := INext;
  END;

  B = FactorialBindings.Public BRANDED FactorialBindings.Brand OBJECT
    t : T;
    this : REF ARRAY OF CARDINAL; (* same as Iterator.next *)
    used : Used;
    sampleIndex : CARDINAL;
  OVERRIDES
    lookup := Lookup;
    getBool := GetBool;
    getInt := GetInt;
    getLR := GetLR;
    getText := GetText;

    getIndex := GetIndex;

    format := FormatBindings;
    formatValues := FormatValueBindings;

    iterateUnused := IterateUnused;
    iterateAll := IterateAll;
    initCopy := InitCopy;
  END;

TYPE Used = REF ARRAY OF BOOLEAN;

(* DESIGN METHODS *)

PROCEDURE GetSamplesPerSlot(t : T) : CARDINAL =
  BEGIN RETURN t.samplesPerSlot END GetSamplesPerSlot;

PROCEDURE OpenFile(t : T) RAISES { OSError.E } = 
  BEGIN
    t.wroteHeader := FALSE;
    t.wr := FileWr.Open(t.filePath())
  END OpenFile;

PROCEDURE FilePath(t : T) : TEXT = 
  BEGIN RETURN t.directory & "/" & t.name END FilePath;

PROCEDURE Init(t : T; name : TEXT; directory : TEXT; samplesPerSlot : CARDINAL;
               READONLY responses : ARRAY OF TEXT;
               doOpenFile : BOOLEAN) : T RAISES { OSError.E } =
  VAR
    wr : Wr.T := NIL;
  BEGIN
    IF doOpenFile THEN
      wr := FileWr.Open(directory & "/" & name)
    END;

    RETURN t.initWr(wr, name, directory, samplesPerSlot, responses)
  END Init;

PROCEDURE InitWr(t : T; wr : Wr.T;
                 name, directory : TEXT;
                 samplesPerSlot : CARDINAL;
                 READONLY responses : ARRAY OF TEXT) : T =
  BEGIN
    t.name := name;

    t.responses := NEW(REF ARRAY OF TEXT, NUMBER(responses));
    t.responses^ := responses;

    t.directory := directory;
    t.samplesPerSlot := samplesPerSlot;
    t.values := NEW(REF ARRAY OF Values.T, 0);

    t.wr := wr;

    RETURN t
  END InitWr;

PROCEDURE Size(t : T) : CARDINAL =
  VAR
    res :=  t.samplesPerSlot;
  BEGIN
    FOR i := FIRST(t.values^) TO LAST(t.values^) DO
      res := res*t.values[i].n
    END;
    RETURN res
  END Size;

PROCEDURE Iterate(t : T) : Iterator =

  PROCEDURE Zeros() : REF ARRAY OF CARDINAL =
    VAR
      res := NEW(REF ARRAY OF CARDINAL, NUMBER(t.values^));
    BEGIN
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := 0 
      END;
      RETURN res
    END Zeros;

  BEGIN
    RETURN NEW(Iterator, t := t, nextV := Zeros(), nextSample := 0)
  END Iterate;

PROCEDURE GetAllTextBindings(t : T; 
                             var : TEXT;
                             res : TextSet.T) : TextSet.T RAISES { NoSuchVar }=
  VAR
    iter := t.iterate();
    b : B;
  BEGIN
    IF res = NIL THEN
      res := NEW(TextSetDef.T).init()
    END;

    WHILE iter.next(b) DO
      EVAL res.insert(b.getText(var))
    END;
    RETURN res
  END GetAllTextBindings;

PROCEDURE GetAllIntBindings(t : T; 
                             var : TEXT;
                             res : IntSet.T) : IntSet.T RAISES { NoSuchVar }=
  VAR
    iter := t.iterate();
    b : B;
  BEGIN
    IF res = NIL THEN
      res := NEW(IntSetDef.T).init()
    END;

    WHILE iter.next(b) DO
      EVAL res.insert(b.getInt(var))
    END;
    RETURN res
  END GetAllIntBindings;

(***********************************************************************)

PROCEDURE GetIndexedBindings(t : T; 
                             idx : FactorialIndex.T) : FactorialBindings.T =
  VAR
    res := NEW(B, t := t);
  BEGIN
    res.sampleIndex := 0; (* wrong! *)
    res.this := NEW(REF ARRAY OF CARDINAL, NUMBER(t.values^));

    res.used := NEW(Used, NUMBER(t.values^));

    FOR i := FIRST(res.used^) TO LAST(res.used^) DO
      res.used[i] := FALSE
    END;
    FOR i := FIRST(res.this^) TO LAST(res.this^) DO
      res.this[i] := idx.index(i)
    END;
    RETURN res
  END GetIndexedBindings;

(***********************************************************************)

(* ITERATOR METHODS *)

PROCEDURE INext(i : Iterator; VAR x : FactorialBindings.T) : BOOLEAN =
  BEGIN
    IF i.finished THEN RETURN FALSE END;

    x := NEW(B, t := i.t);
    x.sampleIndex := i.nextSample;
    x.this := NEW(REF ARRAY OF CARDINAL, NUMBER(i.nextV^));

    x.used := NEW(Used, NUMBER(i.nextV^));
    FOR i := FIRST(x.used^) TO LAST(x.used^) DO
      x.used[i] := FALSE
    END;

    x.this^ := i.nextV^;

    IF i.nextSample < i.t.samplesPerSlot-1 THEN
      INC(i.nextSample);
    ELSE 
      i.finished := TRUE;
      FOR k := FIRST(i.t.values^) TO LAST(i.t.values^) DO
        IF i.nextV[k] < i.t.values[k].n - 1 THEN
          INC(i.nextV[k]);
          FOR l := FIRST(i.t.values^) TO k-1 DO
            i.nextV[l] := 0
          END;
          i.nextSample := 0;
          i.finished := FALSE;
          EXIT
        END
      END
    END;

    RETURN TRUE
  END INext;

(***********************************************************************)

(* BINDINGS METHODS *)

PROCEDURE Lookup(b : B; named : TEXT) : CARDINAL RAISES { NoSuchVar } =
  BEGIN
    FOR i := FIRST(b.t.values^) TO LAST(b.t.values^) DO
      IF Text.Equal(b.t.values[i].named,named) THEN
        RETURN i
      END
    END;
    RAISE NoSuchVar(Var { named, Type.Any })
  END Lookup;

PROCEDURE GetBool(b : B; named : TEXT) : BOOLEAN RAISES { NoSuchVar } =
  VAR 
    i := b.lookup(named);
  BEGIN
    b.used[i] := TRUE;
      
    IF NOT ISTYPE(b.t.values[i],Values.Bool) THEN
      RAISE NoSuchVar(Var { named, Type.Boolean })
    END;
      
    RETURN NARROW(b.t.values[i],Values.Bool).v[b.this[i]]
  END GetBool;

PROCEDURE GetText(b : B; named : TEXT) : TEXT RAISES { NoSuchVar } =
  VAR
    i := b.lookup(named);
  BEGIN
    b.used[i] := TRUE;
      
    IF NOT ISTYPE(b.t.values[i],Values.TX) THEN
      RAISE NoSuchVar(Var { named, Type.Text })
    END;
      
    RETURN NARROW(b.t.values[i],Values.TX).v[b.this[i]]
  END GetText;

PROCEDURE GetInt(b : B; named : TEXT) : INTEGER RAISES { NoSuchVar } =
  VAR
    i := b.lookup(named);
  BEGIN
    b.used[i] := TRUE;
    
    IF NOT ISTYPE(b.t.values[i],Values.Int) THEN
      RAISE NoSuchVar(Var { named, Type.Integer })
    END;
    
    RETURN NARROW(b.t.values[i],Values.Int).v[b.this[i]]
  END GetInt;

PROCEDURE GetIndex(b : B; named : TEXT) : CARDINAL RAISES { NoSuchVar } =
  VAR
    i := b.lookup(named);
  BEGIN
    b.used[i] := TRUE;
      
    RETURN b.this[i]
  END GetIndex;

PROCEDURE GetLR(b : B; named : TEXT) : LONGREAL RAISES { NoSuchVar } = 
  VAR
    i := b.lookup(named);
  BEGIN
    b.used[i] := TRUE;
    
    IF NOT ISTYPE(b.t.values[i],Values.LR) THEN
      RAISE NoSuchVar(Var { named, Type.LongReal })
    END;
    
    RETURN NARROW(b.t.values[i],Values.LR).v[b.this[i]]
  END GetLR;

(**********************************************************************)

PROCEDURE FormatBindings(b : B) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(b.this^) TO LAST(b.this^) DO
      res := res & Fmt.Int(b.this[i]);
      IF i < LAST(b.this^) THEN res := res & "_" END
    END;
    res := res & "_SAMPLE" & Fmt.Pad(Fmt.Int(b.sampleIndex),
                                     length := 5, padChar:='0');
    RETURN res
  END FormatBindings;

PROCEDURE FormatValueBindings(b : B; 
                              betweenString : TEXT;
                              equalsString : TEXT;
                              afterString : TEXT) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(b.this^) TO LAST(b.this^) DO
      res := res & b.t.values[i].named & equalsString & 
                   b.t.values[i].formatV(b.this[i]) & afterString;
      IF i < LAST(b.this^) THEN res := res & betweenString END
    END;
    RETURN res
  END FormatValueBindings;


PROCEDURE RecordResults(t : T; 
                        bindings : B; READONLY data : ARRAY OF LONGREAL) RAISES { Wr.Failure } =
  BEGIN
    t.writeResults(t.wr, bindings, data)
  END RecordResults;

REVEAL
  VarIterator = PublicVarIterator BRANDED FactorialBindings.Brand & " VarIterator" OBJECT
    b : B;
    p := 0;
  END;

PROCEDURE VIUNext(vi : VarIterator; VAR var : Var) : BOOLEAN =
  BEGIN
    FOR i := vi.p TO LAST(vi.b.used^) DO
      IF vi.b.used[i] = FALSE THEN
        var := Var { vi.b.t.values[i].named, vi.b.t.typeOf(i) };
        vi.p := i+1;
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END VIUNext;

PROCEDURE VINext(vi : VarIterator; VAR var : Var) : BOOLEAN =
  BEGIN
    FOR i := vi.p TO LAST(vi.b.used^) DO
      var := Var { vi.b.t.values[i].named, vi.b.t.typeOf(i) };
      vi.p := i+1;
      RETURN TRUE
    END;
    RETURN FALSE
  END VINext;

PROCEDURE IterateUnused(b : B) : VarIterator =
  BEGIN RETURN NEW(VarIterator, b := b, next := VIUNext) END IterateUnused;

PROCEDURE IterateAll(b : B) : VarIterator =
  BEGIN RETURN NEW(VarIterator, b := b, next := VINext) END IterateAll;

PROCEDURE InitCopy(b : B; old : B) : B =
  BEGIN
    b.t := old.t;
    b.used := old.used; (* we init this by reference, so that a "use"
                           via the copy looks like it's been used in the
                           original, too *)

    b.this := NEW(REF ARRAY OF CARDINAL, NUMBER(old.this^)); 
    b.this^ := old.this^;

    RETURN b
  END InitCopy;

PROCEDURE Close(t : T) RAISES { Wr.Failure } = 
  BEGIN
    FactorialSuper.T.close(t,t.wr) 
  END Close;

BEGIN END FactorialDesign.




