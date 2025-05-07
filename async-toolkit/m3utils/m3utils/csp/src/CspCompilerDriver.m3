MODULE CspCompilerDriver;
IMPORT Pathname;
IMPORT TextCspPortSeqTbl;
IMPORT OSError, Rd;
IMPORT FileRd;
IMPORT TextReader;
IMPORT TextSet, TextSetDef;
IMPORT Wx;
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
IMPORT Text;
IMPORT TextTextTbl;

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

PROCEDURE Unarray(designator : TEXT) : TEXT =
  (* return only the ID part of ID[..][..] *)
  BEGIN
    WITH pos = Text.FindChar(designator, '[', 1) DO
      IF pos = -1 THEN
        RETURN designator
      ELSE
        RETURN Text.Sub(designator, 0, pos)
      END
    END
  END Unarray;

PROCEDURE Unid(designator : TEXT) : TEXT =
  (* return only the [..][..] part of ID[..][..] *)
  BEGIN
    WITH pos = Text.FindChar(designator, '[', 1) DO
      IF pos = -1 THEN
        RETURN ""
      ELSE
        RETURN Text.Sub(designator, pos)
      END
    END
  END Unid;

PROCEDURE MapFailed(map : TextTextTbl.T; what : TEXT) =
  VAR
    existing := "";
    iter := map.iterate();
    k, v : TEXT;
  BEGIN
    WHILE iter.next(k, v) DO
      existing := existing & F("%s -> %s\n", k, v)
    END;
    Debug.Error(F("Not found : %s, existing=\n%s",
                  what,
                  existing))
  END MapFailed;
  
PROCEDURE DumpMap(map : TextTextTbl.T) =
  VAR
    existing := "";
    iter := map.iterate();
    k, v : TEXT;
  BEGIN
    WHILE iter.next(k, v) DO
      existing := existing & F("%s -> %s\n", k, v)
    END;
    Debug.Out(F("MAP : \n%s", existing))
  END DumpMap;
  
PROCEDURE BuildPortBinding(lhs  : TEXT;
                           port : CspPort.T;
                           map  : TextTextTbl.T (* map R[0][1] -> chanxyz *)
  ) : TEXT =
  VAR
    g : TEXT;
    chanName := M3Ident.Escape(Atom.ToText(port.name));
  BEGIN
    IF ISTYPE(port.def, CspPort.Scalar) THEN
      WITH found = map.get(chanName, g) DO
        IF NOT found THEN
          RETURN lhs & F("NIL (* not found : %s *)", chanName)
        END;
        RETURN lhs & g
      END
    ELSE
      RETURN lhs & DefBinding(port.def, map, chanName)
    END
  END BuildPortBinding;

PROCEDURE DefBinding(def    : CspPort.Channel;
                     map    : TextTextTbl.T;
                     chan   : TEXT) : TEXT =

  PROCEDURE Recurse(def    : CspPort.Channel;
                    prefix : TEXT) : TEXT =
    BEGIN
      IF ISTYPE(def, CspPort.Scalar) THEN
        VAR
          g : TEXT;
          search := chan & prefix & "]";
          found := map.get(search, g);
        BEGIN
          IF NOT found THEN
            MapFailed(map, search)
          END;
          RETURN g
        END
      ELSE
        WITH arr  = NARROW(def, CspPort.Array),
             decl = CspPort.M3ChanDecl(def),
             wx   = Wx.New() DO
          Wx.PutText(wx, F("%s { ", decl));
          FOR i := arr.range.min TO arr.range.max DO
            VAR
              newPrefix := prefix & F("%s", Int(i));
            BEGIN
              IF ISTYPE(arr.elem, CspPort.Array) THEN
                newPrefix := newPrefix & ","
              END;
              
              Wx.PutText(wx, Recurse(arr.elem, newPrefix));
              IF i # arr.range.max THEN Wx.PutText(wx, ", ") END
            END
          END;
          Wx.PutText(wx, " }");
          RETURN Wx.ToText(wx)
        END
      END
    END Recurse;

  BEGIN
    RETURN Recurse(def, "[")
  END DefBinding;
  
PROCEDURE GenBuilder(t : T; builderName : TEXT; defSlack : CARDINAL) : TEXT =

  VAR chanTbl := NEW(TextAtomTbl.Default).init();
      chanTypes := NEW(TextSetDef.T).init();
      
  PROCEDURE ExtractChannelInfo() =
    (* this takes all the channels mentioned in the .procs file and
       figures out the type of each channel, so that we may proceed
       with channel creation *)
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
                   portNm = Unarray(reader.nextE("=")),
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
                  baseChanType := CspPort.BaseChanType(port.def);
                  
                  ntypeTxt := F("%s%sChan",
                                CspPort.ClassTypeNames[baseChanType.class],
                                Int(baseChanType.width));
                  ntype : Atom.T := Atom.FromText(ntypeTxt);
                  ptype : Atom.T;
                BEGIN
                  IF chanTbl.get(global, ptype) THEN
                    IF ptype # ntype THEN
                      Debug.Error(F("%s : port mismatch : %s # %s [%s]",
                                    global,
                                    Atom.ToText(ptype),
                                    Atom.ToText(baseChanType.typeName)))
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
                           "    %s := %s.New(\"%s\", slack := %s);\n",
                           M3Ident.Escape(cname),
                           Atom.ToText(ctype),
                           cname,
                           Int(defSlack))
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
                             inst.name) (* the name should NOT be escaped *)
          );

          VAR
            p    := inst.chans;
            map  := NEW(TextTextTbl.Default).init();
            pSeq : CspPortSeq.T;
            found := t.ports.get(inst.ptypeE, pSeq);
          BEGIN
            <*ASSERT found*>
            WHILE p # NIL DO
              WITH reader  = NEW(TextReader.T).init(p.head),
                   portNm  = reader.nextE("="),
                   portId  = Unarray(portNm),
                   portIdx = Unid(portNm),
                   global  = reader.nextE("") DO
                EVAL map.put(M3Ident.Escape(portId) & portIdx,
                             M3Ident.Escape(global));
                p := p.tail
              END;
              
            END;(*WHILE*)

            DumpMap(map);

            VAR
              iter := map.iterate();
              p, g : TEXT;
            BEGIN
              (* start with the channels *)
              FOR i := 0 TO pSeq.size() - 1 DO
                WITH port = pSeq.get(i) DO
                  WITH lhs = F(", %s := ",
                               M3Ident.Escape(Atom.ToText(port.name))) DO
                    Wx.PutText(wx, F("      %s\n",
                                     BuildPortBinding(lhs, port, map)))
                  END;
                  
                  Wx.PutText(wx, F("      (* port %s *)\n",
                                   Atom.ToText(port.name)))
                END
              END;
              
              (* now hook up the Node ports too *)
              
              Wx.PutText(wx,
                         "    );\n"                     
              )
            END;
            
          END(*VAR*)
          
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
