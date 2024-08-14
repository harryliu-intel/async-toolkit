MODULE SchemaGraph;

(* 
   Flexible graphing program 
*)

IMPORT Debug; FROM Debug IMPORT UnNil;
IMPORT Text;
IMPORT CardSeq;
IMPORT Rd;
IMPORT CitTextUtils;
IMPORT Pathname;
IMPORT Stdio;
IMPORT FileRd;
IMPORT TextReader;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Params;
IMPORT TextSeq;
IMPORT Thread;
IMPORT OSError;
IMPORT Field;
IMPORT FData;
IMPORT FDataSeq;
IMPORT RefSeq;
IMPORT Scheme;
IMPORT TextLongRealTbl AS TextLRTbl;
IMPORT TextTextTbl;
IMPORT FloatMode;
IMPORT Lex;
IMPORT Scan;
IMPORT SchemeSymbol;
IMPORT SchemeString;
IMPORT SchemeLongReal;
IMPORT SchemeUtils;
IMPORT SchemaEntry;
IMPORT SchemaEntrySeq;
IMPORT TextSchemaEntrySeqTbl;
IMPORT SchemaEntryArraySort;
IMPORT FileWr;
IMPORT Wr;
IMPORT Schema;

<*FATAL Thread.Alerted*>

CONST LR = LongReal;

PROCEDURE RemoveLeadingSpaces(VAR field : TEXT) =
  BEGIN
    WHILE Text.Length(field) # 0 AND Text.GetChar(field, 0) = ' ' DO
      field := Text.Sub(field, 1)
    END
  END RemoveLeadingSpaces;

PROCEDURE NextToken(VAR buff : TEXT) : TEXT =
  VAR
    idx : INTEGER;
  BEGIN
    Debug.Out(F("NextToken(\"%s\")", buff));
    
    RemoveLeadingSpaces(buff);
    idx := Text.FindChar(buff, ' ');

    IF idx = -1 THEN
      (* if no space, then the whole thing is a token (or nothing) *)
      idx := Text.Length(buff)
    END;

    IF idx = 0 THEN
      (* empty string *)
      buff := "";
      RETURN NIL
    END;
    
    TRY
      RETURN Text.Sub(buff, 0, idx)
    FINALLY
      buff := Text.Sub(buff, idx);
      RemoveLeadingSpaces(buff)
    END
  END NextToken;

PROCEDURE ReadSchema(spn : Pathname.T) : Schema.T =
  VAR
    rd           := FileRd.Open(spn);
    text         := "";
    res : Schema.T;
  BEGIN
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd) DO
          IF Text.Length(line) # 0 AND Text.GetChar(line, 0) # '#' THEN
            (* active line *)
            
            text := text & line & " ";
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(rd)
    END;

    VAR
      reader := NEW(TextReader.T).init(text);
      flds   := reader.shatter(",", "", skipNulls := FALSE);
      p      := flds;
      count  := ARRAY Field.T OF CARDINAL { 0, .. };
      types  := NEW(CardSeq.T).init();
      i      := 0;
    BEGIN
      res := Schema.T { nfields := 0,
                        dfields := 0,
                        fdata   := NEW(FDataSeq.T).init() };
      
      WHILE p # NIL DO
        (* p.head points to a field *)
        VAR
          field := p.head;
          type : Field.T;
          name : TEXT;
          formula : TEXT;
        BEGIN
          RemoveLeadingSpaces(field);
          
          Debug.Out(F("Parsing field \"%s\"", field));
          
          IF Text.Length(field) = 0 THEN
            type := Field.T.Ignore
          ELSE
            VAR
              found := FALSE;
            BEGIN
              FOR i := FIRST(Field.T) TO LAST(Field.T) DO
                IF CitTextUtils.HavePrefix(field, Field.Names[i]) THEN
                  found := TRUE;
                  type := i;
                  field := CitTextUtils.RemovePrefix(field, Field.Names[i]);
                  RemoveLeadingSpaces(field);
                  EXIT
                END
              END;
              IF NOT found THEN
                Debug.Error(F("Incomprehensible schema field \"%s\"", field))
              END
            END
          END;
          
          (* type is set *)
          Debug.Out(F("Got field type %s", Field.Names[type]));
          INC(count[type]);
          types.addhi(ORD(type));
          
          IF Text.Length(field) = 0 THEN
            Debug.Out(F("Field is now empty"));
            name := NIL;
            formula := NIL
          ELSIF Text.GetChar(field, 0) # '(' THEN
            Debug.Out(F("Field not starting with paren, so this is a name: \"%s\"", field));
            (* what we have is either empty or a name *)
            name := NextToken(field)
          END;
          
          IF Text.Length(field) # 0 AND Text.GetChar(field, 0) = '(' THEN
            Debug.Out(F("Remainder must be formula: \"%s\"", field));
            formula := field
          END;
          
          (* got a field *)
          Debug.Out(F("Field %s name %s type %s (count %s) formula %s",
                      Int(i),
                      UnNil(name),
                      Field.Names[type],
                      Int(count[type]),
                      UnNil(formula)));
          
          res.fdata.addhi( FData.T { type, name, formula });
          
          INC(res.nfields);
          IF type # Field.T.Formula THEN
            INC(res.dfields);
          END;
          
        END;
        p := p.tail;
        INC(i)
      END
    END;
    RETURN res
  END ReadSchema;

PROCEDURE ReadData(schema : Schema.T; files : TextSeq.T) : RefSeq.T =
  VAR
    res := NEW(RefSeq.T).init();
  BEGIN
    FOR i := 0 TO files.size() - 1 DO
      WITH nm = files.get(i) DO
        WITH rd     = FileRd.Open(nm),
             line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line),
             data   = NEW(TextSeq.T).init() DO

          VAR
            p := reader.shatter(",", "", skipNulls := FALSE);
          BEGIN
            WHILE p # NIL DO
              data.addhi(p.head);
              p := p.tail
            END;
            IF data.size() # schema.dfields THEN
              Debug.Warning(F("Schema has %s data fields, data in \"%s\" has %s",
                              Int(schema.dfields),
                              nm,
                              Int(data.size())))
            END
          END;
          res.addhi(data);
          Rd.Close(rd)
        END
      END
    END;
    Debug.Out(F("Loaded data from %s files", Int(files.size())));
    RETURN res
  END ReadData;

PROCEDURE EvalFormula(scm     : Scheme.T;
                      symtabN : TextLRTbl.T;
                      symtabS : TextTextTbl.T;

                      formula : TEXT) : TEXT =
  VAR
    sym  : TEXT;
    valS : TEXT;
    valN : LONGREAL;
  BEGIN
    WITH iter = symtabS.iterate() DO
      WHILE iter.next(sym, valS) DO
        WITH ssym = SchemeSymbol.FromText(sym),
             sval = SchemeString.FromText(valS) DO
          Debug.Out(F("Defining %s <- \"%s\"", sym, valS));
          scm.defineInGlobalEnv(ssym, sval)
        END
      END
    END;

    WITH iter = symtabN.iterate() DO
      WHILE iter.next(sym, valN) DO
        WITH ssym = SchemeSymbol.FromText(sym),
             sval = SchemeLongReal.FromLR(valN) DO
          Debug.Out(F("Defining %s <- %s", sym, LR(valN)));
          scm.defineInGlobalEnv(ssym, sval)
        END
      END
    END;

    TRY
      WITH resultObj = scm.loadEvalText(formula),
           res       = SchemeUtils.Stringify(resultObj) DO
        Debug.Out(F("Evaluating %s to %s", formula, res));
        RETURN res
      END
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when evaluating %s",
                    x, formula));
      <*ASSERT FALSE*>
    END
  END EvalFormula;
  
PROCEDURE EvalFormulas(scm : Scheme.T; schema : Schema.T; data : RefSeq.T) =
  TYPE
    FT = Field.T;
  VAR
    val : TEXT;
  BEGIN
    FOR i := 0 TO data.size() - 1 DO
      WITH row     = NARROW(data.get(i), TextSeq.T),
           symtabN = NEW(TextLRTbl.Default).init(),
           symtabS = NEW(TextTextTbl.Default).init() DO
        WHILE row.size() < schema.fdata.size() DO
          row.addhi("")
        END;
        <*ASSERT row.size() >= schema.fdata.size()*>
        FOR fi := 0 TO schema.fdata.size() - 1 DO
          WITH fd  = schema.fdata.get(fi) DO
            val := row.get(fi);

            (* compute value of field, if a formula *)
            IF fd.type = FT.Formula THEN
              val := EvalFormula(scm, symtabN, symtabS, fd.formula)
            END;

            (* store back *)
            row.put(fi, val);

            (* bind in symbol table --

               we had a little bug here.

               Every formula got defined in the Scheme environment,
               except the last one.  So we have to write the result not only into the
               symtabs, but also into the Scheme interpreter.
               
               Question is whether we still need the symtabs!
            *)
            IF fd.name # NIL THEN
              Debug.Out("Binding : " & fd.name);
              WITH snm = fd.name & ":string",
                   ssnm = SchemeSymbol.FromText(snm),
                   sval = SchemeString.FromText(val) DO
                EVAL symtabS.put(snm, val);
                scm.defineInGlobalEnv(ssnm, sval)
              END;
              
              TRY
                VAR dummy : LONGREAL;
                BEGIN
                  EVAL symtabN.delete(fd.name, dummy);
                  EVAL symtabN.delete(fd.name  & ":real", dummy)
                END;
                WITH numval = Scan.LongReal(val) DO
                  WITH nnm = fd.name & ":real",
                       snnm = SchemeSymbol.FromText(nnm),
                       nm = fd.name,
                       snm = SchemeSymbol.FromText(nm),
                       snumval = SchemeLongReal.FromLR(numval) DO
                    EVAL symtabN.put(nm, numval);
                    EVAL symtabN.put(nnm, numval);
                    scm.defineInGlobalEnv(snnm, snumval);
                    scm.defineInGlobalEnv(snm, snumval)
                  END
                END
              EXCEPT
                Lex.Error, FloatMode.Trap => (* skip *)
              END
            END
            
          END
        END
      END
    END
  END EvalFormulas;
    
PROCEDURE DoOneSweep(targDir : Pathname.T;
                     schema : Schema.T; data : RefSeq.T; idx : CARDINAL;
                     doLabels : BOOLEAN) =

  PROCEDURE AddSchemaEntry(label : TEXT; entry : SchemaEntry.T) =
    VAR
      seq : SchemaEntrySeq.T;
    BEGIN
      IF NOT allData.get(label, seq) THEN
        seq := NEW(SchemaEntrySeq.T).init();
      END;
      seq.addhi(entry);
      EVAL allData.put(label,seq)
    END AddSchemaEntry;
    
  VAR
    allData := NEW(TextSchemaEntrySeqTbl.Default).init();
    cols    := NEW(TextSeq.T).init();
  BEGIN
    
    WITH fd = schema.fdata.get(idx) DO
      Debug.Out(F("Sweeping field %s", FData.Format(fd)));

      cols.addhi(FData.Format(fd));
      
      FOR j := 0 TO schema.fdata.size() - 1 DO
        IF j # idx THEN
          WITH fd = schema.fdata.get(j) DO
            CASE fd.type OF
              Field.T.Ignore,
              Field.T.Collate,
              Field.T.Sweep =>(* skip *)
            |
              Field.T.Report,
              Field.T.Formula =>
              cols.addhi(FData.Format(fd))
            END
          END
        END(*FI*)
      END(*ROF*);

          (* work through the data entries *)
      FOR i := 0 TO data.size() - 1 DO

        PROCEDURE Report(field : TEXT) =
          BEGIN
            <*ASSERT entry.report # NIL*>
            entry.report.addhi(field)
          END Report;

        PROCEDURE Collate(field : TEXT) =
          BEGIN
            <*ASSERT label # NIL*>
            <*ASSERT field # NIL*>
            label := label & field & "_"
          END Collate;

        TYPE
          CA = ARRAY OF CHAR;
        VAR
          row   := NARROW(data.get(i), TextSeq.T);
          entry := SchemaEntry.T { x      := LAST(LONGREAL),
                             report := NEW(TextSeq.T).init() };
          label := "";
        BEGIN
          (* now walk the fields *)
          
          
          FOR j := 0 TO schema.fdata.size() - 1 DO
            IF j = idx THEN
              entry.x := Scan.LongReal(row.get(j))
            ELSE
              WITH fdata = schema.fdata.get(j) DO
                CASE fdata.type OF
                  Field.T.Ignore => (* skip *)
                |
                  Field.T.Collate => Collate(row.get(j))
                |
                  Field.T.Report, Field.T.Formula => Report(row.get(j))
                |
                  Field.T.Sweep =>
                  WITH easyName = CitTextUtils.Tr(row.get(j),
                                                  CA { '-', '.' },
                                                  CA { 'm', 'p' }) DO
                                                  
                    Collate(fdata.name & "=" & easyName)
                  END
                END
              END
            END(*FI*)
          END(*ROF*);

          AddSchemaEntry(label, entry)
          
        END(*HTIW*)
      END
    END;

    (* dump out all the files *)
    Debug.Out("Writing sweep files...");
    VAR
      iter := allData.iterate();
      label : TEXT;
      seq   : SchemaEntrySeq.T;
    BEGIN
      WHILE iter.next(label, seq) DO
        Debug.Out("Sweep label is \"" & label & "\"");
        WITH arr = NEW(REF ARRAY OF SchemaEntry.T, seq.size()) DO
          FOR i := FIRST(arr^) TO LAST(arr^) DO
            arr[i] := seq.get(i)
          END;
          SchemaEntryArraySort.Sort(arr^);

          (* write main .dat file for this combo *)
          WITH root = CitTextUtils.RemoveSuffix(label, "_") DO

            (* record the columns *)
            WITH fn   = root & ".cols",
                 wr   = FileWr.Open(targDir & "/" & fn) DO
              FOR i := 0 TO cols.size() - 1 DO
                Wr.PutText(wr, F("$%s=%s ", Int(i + 1), cols.get(i)))
              END;
              Wr.PutChar(wr, '\n');
              Wr.Close(wr)
            END;
            
            (* the main dat file *)
            WITH fn   = root & ".dat",
                 wr   = FileWr.Open(targDir & "/" & fn) DO
              FOR i := FIRST(arr^) TO LAST(arr^) DO
                WITH e = arr[i] DO
                  Wr.PutText(wr, LR(e.x));
                  FOR j := 0 TO e.report.size() - 1 DO
                    Wr.PutChar(wr, ' ');
                    Wr.PutText(wr, e.report.get(j))
                  END
                END;
                Wr.PutChar(wr, '\n')
              END(*ROF*);
              Wr.Close(wr)
            END;

            (* now write labels *)
            IF doLabels AND NUMBER(arr^) # 0 THEN
              WITH nCol = arr[0].report.size() DO
                FOR i := 1 TO nCol - 1 DO
                  FOR j := 1 TO nCol - 1 DO
                    IF i # j THEN
                      WITH fn = F("%s_labels_%s_%s.src",
                                  root,
                                  Int(i + 2),
                                  Int(j + 2)),
                           wr = FileWr.Open(targDir & "/" & fn) DO
                        FOR k := FIRST(arr^) TO LAST(arr^) DO
                          WITH e = arr[k] DO
                            Wr.PutText(wr,
                                       F("set label \"%s\" at %s, %s\n",
                                         LR(e.x),
                                         e.report.get(i),
                                         e.report.get(j)))
                          END
                        END(*ROF*);
                        Wr.Close(wr)
                      END
                    END
                  END
                END
              END
            END(*FI*)
          END
        END    
      END
    END
  END DoOneSweep;
  
PROCEDURE DoSweeps(targDir : Pathname.T;schema : Schema.T; data : RefSeq.T; doLabels : BOOLEAN) =
  BEGIN
    FOR i := 0 TO schema.fdata.size() - 1 DO
      WITH fd = schema.fdata.get(i) DO
        IF fd.type = Field.T.Sweep THEN
          DoOneSweep(targDir, schema, data, i, doLabels)
        END
      END
    END
  END DoSweeps;
  
BEGIN END SchemaGraph.
