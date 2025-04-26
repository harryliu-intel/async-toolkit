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

CONST doDebug = TRUE;

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
             ptypeE  = reader.get(),
             chans   = reader.shatter(" \t", "", TRUE)
         DO
          EVAL t.ptypeEset.insert(ptypeE);

          t.insts.addhi(
                NEW(REF Instance,
                    name  := name,
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
            Debug.Out(F("INSTANCE %s : ptypeE=%s found=%s",
                        inst.name, inst.ptypeE, Bool(found)));
            <*ASSERT found*>
            WHILE p # NIL DO
              Debug.Out(F("channel assign : %s", p.head));
              WITH reader = NEW(TextReader.T).init(p.head),
                   portNm = reader.nextE("="),
                   patom  = Atom.FromText(portNm),
                   global = reader.nextE("") DO
                Debug.Out(F("port %s global %s", portNm, global));

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
                  ptype : Atom.T;
                BEGIN
                  IF chanTbl.get(global, ptype) THEN
                    IF ptype # port.typeName THEN
                      Debug.Error(F("%s : port mismatch : %s # %s",
                                    Atom.ToText(ptype),
                                    Atom.ToText(port.typeName)))
                    END
                  ELSE
                    EVAL chanTbl.put(global, port.typeName)
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

  PROCEDURE WriteChannelCreation() =
    VAR
      iter := chanTbl.iterate();
      cname : TEXT;
      ctype : Atom.T;
    BEGIN
      WHILE iter.next(cname, ctype) DO
        Wx.PutText(wx, F(
                           "    %s := %s.New(\"%s\")\n",
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
          Wx.PutText(wx, F(
        "    %s.Build(\"%s\", \n",
        M3Ident.Escape(inst.name),
        inst.ptypeE)
          );

          VAR p := inst.chans; BEGIN
            WHILE p # NIL DO
              WITH reader = NEW(TextReader.T).init(p.head),
                   portNm = reader.nextE("="),
                   patom  = Atom.FromText(portNm),
                   global = reader.nextE("") DO
                Wx.PutText(wx, F(
        "    %s := %s,\n",                                   
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

    Wx.PutText(wx, F("PROCEDURE %s() =\n", builderName));
    Wx.PutText(wx, F("  VAR\n"));
    WriteChannelCreation();
    Wx.PutText(wx, F("  BEGIN\n"));
    WriteProcessCreation();
    Wx.PutText(wx, F("  END %s;\n", builderName));
    RETURN Wx.ToText(wx);
  END GenBuilder;

BEGIN END CspCompilerDriver.
