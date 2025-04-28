MODULE CspCompilerDriver;
IMPORT Pathname;
IMPORT TextCspPortSeqTbl;
IMPORT OSError, Rd;
IMPORT FileRd;
IMPORT TextReader;
IMPORT TextSet, TextSetDef;
IMPORT Wx;
IMPORT Text;
IMPORT TextSeq;
IMPORT TextList;
IMPORT Debug;
IMPORT RefSeq;
IMPORT Thread;
IMPORT CspPort;
IMPORT CspPortSeq;
FROM Fmt IMPORT F, Int, Bool;
IMPORT Atom;
IMPORT TextAtomTbl;
IMPORT M3Ident;

<*FATAL Thread.Alerted*>

CONST doDebug = FALSE;

REVEAL
  T = Public BRANDED Brand OBJECT
    ptypeEset : TextSet.T;
    ports     : TextCspPortSeqTbl.T;
    insts     : RefSeq.T;
  OVERRIDES
    init            := Init;
    getProcTypes    := GetProcTypes;
    setProcessPorts := SetProcessPorts;
    genBuilder      := GenBuilder;
  END;

TYPE
  Instance = RECORD
    name, ptype, ptypeE : TEXT;
    chans                : TextList.T;
  END;
  
PROCEDURE Init(t : T; procGraphPath : Pathname.T) : T 
  RAISES { OSError.E, Rd.Failure } =
  VAR
    rd := FileRd.Open(procGraphPath);
  BEGIN
    t.ptypeEset := NEW(TextSetDef.T).init();
    t.insts     := NEW(RefSeq.T).init();
    
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line),
             
             name    = reader.get(),
             ptype   = reader.get(),
             ptypeE  = reader.get(), (* this is the name of the Java output *)
             chans   = reader.shatter(" \t", "", TRUE)
         DO
          EVAL t.ptypeEset.insert(ptypeE);

          t.insts.addhi(
                NEW(REF Instance,
                    name   := name,
                    ptype  := ptype,
                    ptypeE := ptypeE,
                    chans  := chans)
          )
        END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(rd) (* ok *)
    |
      TextReader.NoMore => RAISE SyntaxError
    END;
    RETURN t
  END Init;

PROCEDURE GetProcTypes(t : T) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
    txt : TEXT;
  BEGIN
    WITH iter = t.ptypeEset.iterate() DO
      WHILE iter.next(txt) DO res.addhi(txt) END
    END;
    RETURN res
  END GetProcTypes;

PROCEDURE SetProcessPorts(t : T; tbl : TextCspPortSeqTbl.T) =
  BEGIN
    t.ports := tbl;
  END SetProcessPorts;

PROCEDURE GenBuilder(t : T; builderName : TEXT) : TEXT =

  VAR chanTbl := NEW(TextAtomTbl.Default).init();
      chanTypes := NEW(TextSetDef.T).init();
      
  PROCEDURE ExtractChannelInfo() =
    BEGIN
      FOR i := 0 TO t.insts.size() - 1 DO
        WITH inst = NARROW(t.insts.get(i), REF Instance)^ DO
          VAR
            p     := inst.chans;
            pSeq  : CspPortSeq.T;
            found : BOOLEAN;
            port  : CspPort.T;
          BEGIN
            found := t.ports.get(inst.ptypeE, pSeq);
            IF doDebug THEN
              Debug.Out(F("INSTANCE %s : ptypeE=%s found=%s",
                          inst.name, inst.ptypeE, Bool(found)))
            END;
            <*ASSERT found*>
            WHILE p # NIL DO
              IF doDebug THEN Debug.Out(F("channel assign : %s", p.head)) END;
              WITH reader = NEW(TextReader.T).init(p.head),
                   portNm = reader.nextE("="),
                   patom  = Atom.FromText(portNm),
                   global = reader.nextE("") DO
                IF doDebug THEN Debug.Out(F("port %s global %s", portNm, global)) END;

                found := FALSE;
                FOR i := 0 TO pSeq.size() - 1 DO
                  WITH prec = pSeq.get(i) DO
                    IF patom = prec.name THEN
                      port  := prec;
                      found := TRUE
                    END
                  END
                END;
                
                IF NOT found THEN
                  Debug.Error(F("%s : port %s not found : instance %s of type %s",
                                global, portNm, inst.name, inst.ptypeE))
                END;

                VAR
                  ntypeTxt := F("%s%sChan",
                                CspPort.ClassTypeNames[port.class],
                                Int(port.width));
                  ntype : Atom.T := Atom.FromText(ntypeTxt);
                  ptype : Atom.T;
                BEGIN
                  IF chanTbl.get(global, ptype) THEN
                    IF ptype # ntype THEN
                      Debug.Error(F("%s : port mismatch : %s # %s [%s]",
                                    global,
                                    Atom.ToText(ptype),
                                    Atom.ToText(port.typeName)))
                    END
                  ELSE
                    EVAL chanTbl.put(global, ntype);
                    EVAL chanTypes.insert(ntypeTxt)
                  END
                END;
                
              END(*WITH*);
              p := p.tail
            END
          END
        END
      END
    END ExtractChannelInfo;
  
  PROCEDURE P(txt : TEXT) =
    BEGIN Wx.PutText(wx, txt) END P;

  PROCEDURE WriteImports() =
    VAR
      iter := t.ptypeEset.iterate();
      ptype, ctype : TEXT;
    BEGIN
      WHILE iter.next(ptype) DO
        P(F("IMPORT m3__%s;\n", ptype))
      END;

      P("\n");
      
      iter := chanTypes.iterate();
      WHILE iter.next(ctype) DO
        P(F("IMPORT %s;\n", ctype))
      END        
    END WriteImports;

  PROCEDURE WriteChannelCreation() =
    VAR
      iter := chanTbl.iterate();
      cname : TEXT;
      ctype : Atom.T;
    BEGIN
      WHILE iter.next(cname, ctype) DO
        P(F(
                           "    %s := %s.New(\"%s\");\n",
                           M3Ident.Escape(cname),
                           Atom.ToText(ctype),
                           cname)
        )

      END
      
    END WriteChannelCreation;

  PROCEDURE WriteProcessCreation() =
    BEGIN
      FOR i := 0 TO t.insts.size() - 1 DO
        WITH inst = NARROW(t.insts.get(i), REF Instance)^ DO
          P(F(
                "    m3__%s.Build(\"%s\" \n",
                (* 
                   this is a hack!
                   we need to map the names the same way that Java
                   does it---for sure!

                   Really, the Scheme part should set up a mapping table
                   from Java escaped types (used for the input .scm files)
                   to the M3 escaped types (used for the output .i3/m3 files)

                   But for now, we are just guessing!
                *)

                             inst.ptypeE,
                             M3Ident.Escape(inst.name))
          );

          VAR p := inst.chans; BEGIN
            WHILE p # NIL DO
              WITH reader = NEW(TextReader.T).init(p.head),
                   portNm = reader.nextE("="),
                   patom  = Atom.FromText(portNm),
                   global = reader.nextE("") DO
                P(F(
        "      , %s := %s\n",                                   
        M3Ident.Escape(portNm), M3Ident.Escape(global)));
                p := p.tail
              END
            END
          END(*VAR*);

          (* now hook up the Node ports too *)

          Wx.PutText(wx,
       "    );\n"                     
          );
          
        END          
      END
    END WriteProcessCreation;
    
  VAR
    wx := Wx.New();
  BEGIN
    ExtractChannelInfo();
    WriteImports();

    P(F("\n\nPROCEDURE %s() =\n", builderName));
    P(F("  VAR\n"));
    WriteChannelCreation();
    P(F("  BEGIN\n"));
    WriteProcessCreation();
    P(F("  END %s;\n", builderName));
    RETURN Wx.ToText(wx);
  END GenBuilder;

BEGIN END CspCompilerDriver.
