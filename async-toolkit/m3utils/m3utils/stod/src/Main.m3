MODULE Main;
IMPORT Params;
IMPORT Rd, FileRd, OSError, Pathname;
IMPORT TextReader;
IMPORT TextList, TextRefTbl, Scan, Text;
IMPORT Debug; FROM Debug IMPORT UnNil;
IMPORT Thread;
IMPORT TextLongRealTbl, TextRd;
IMPORT ProcUtils;
IMPORT Lex;
IMPORT TextTextTbl;
FROM Fmt IMPORT F, Int, LongReal, Pad;
IMPORT RegEx, TextUtils;
IMPORT CardSetDef;
IMPORT Wr, FileWr;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

TYPE
  Rec = REF RECORD
    fn    : Pathname.T;
    q     : LONGREAL;
    idsid : TEXT;
    line  : CARDINAL;
  END;


CONST WS         = "\t ";
      MyPrefix   = "hlp_";
      MyDigits   = 4;
      Cell       = "sc";
      Project    = "HLP";
      Group      = "hlp";
      Script     = "stod.script";
      (*Sudo     = "sudo -u srodaadm ";*)
      Sudo       = "";
(*
      fileServer = "sccfsv03a";
      volName    = "aggr1_sccfs03n04a_H";
*)
      fileServer = "sccfsv03a";
      volName    = "aggr2_sccfs03n04a_H";

VAR pattern : RegEx.Pattern;

PROCEDURE MakePattern() =
  VAR tp := MyPrefix;
  BEGIN
    FOR i := 1 TO MyDigits DO
      tp := tp & "[0-9]"
    END;
    tp := tp & "$";
    Debug.Out("tp \"" & tp & "\"");
    pattern := RegEx.Compile(tp)
  END MakePattern;

VAR ids := NEW(CardSetDef.T).init();

PROCEDURE CheckUB(anm : TEXT) : [-1..LAST(CARDINAL)] =
  BEGIN
    WITH lst = Pathname.Last(anm),
         mat = RegEx.Execute(pattern, anm) # -1 DO
      Debug.Out(F("CheckUB(%s) %s",anm,lst),100);
      IF mat THEN
        WITH num   = TextUtils.RemovePrefix(lst, MyPrefix),
             n     = Scan.Int(num),
             hadIt = ids.insert(n) DO
          <*ASSERT NOT hadIt*>
          RETURN n
        END
      END
    END;
    RETURN -1
  END CheckUB;

PROCEDURE Load(fn : Pathname.T) 
  RAISES { OSError.E, Rd.Failure } = 
  VAR
    rd := FileRd.Open(fn);
    def := 0.0d0;
    toks : TextList.T;
    lNo := 0;
  BEGIN
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line) DO
          INC(lNo);
          IF Text.Length(line) # 0 AND Text.GetChar(line, 0) # '#' THEN
            toks := reader.shatter(WS, "", skipNulls := TRUE);
            IF TE(toks.head, "DEFAULT") THEN
              toks := toks.tail;
              def := Scan.LongReal(toks.head)
            ELSIF TE(toks.head, "USER") THEN
              VAR
                new := NEW(Rec, fn := fn, line := lNo, q := def);
              BEGIN
                toks := toks.tail;
                new.idsid := toks.head;
                toks := toks.tail;
                IF toks # NIL THEN
                  new.q := Scan.LongReal(toks.head)
                END;
                WITH hadIt = tbl.put(new.idsid, new) DO
                  IF hadIt THEN
                    Debug.Error("Duplicate entry for idsid \"" & 
                      new.idsid & "\"")
                  END
                END
              END
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(rd)
    END
  END Load;

PROCEDURE GetAreas() =
  VAR
    txt := ProcUtils.ToText("stodstatus areas");
    rd := NEW(TextRd.T).init(txt);
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd),
             toks = NEW(TextReader.T).init(line).shatter(WS,
                                                         "",
                                                         skipNulls:= TRUE) DO
          Debug.Out("GetAreas/line=" & line, 100);

          IF Text.Length(line) >= 116 THEN
            TRY
              WITH apth = Extract(line,  73, 114),
                   szt  = Extract(line, 114, 121),
                   sz   = Scan.LongReal(szt) DO
                IF NOT TE(apth,"") AND CheckUB(apth) # -1 THEN
                  Debug.Out(F("szTbl.put(%s,%s)", apth, LongReal(sz)),100);
                  WITH hadIt = szTbl.put(apth, sz) DO
                    <*ASSERT NOT hadIt*>
                  END;
                END
              END
            EXCEPT
              Lex.Error => (* whatever *)
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END GetAreas;

PROCEDURE Extract(txt : TEXT; start, lim : CARDINAL) : TEXT =
  CONST
    WS = SET OF CHAR { ' ', '\t' };
  VAR
    p := start;
    q := MIN(lim,Text.Length(txt));
  BEGIN
    WHILE Text.GetChar(txt, p)   IN WS DO INC(p) END;
    WHILE Text.GetChar(txt, q-1) IN WS DO DEC(q) END;

    IF p > q THEN RETURN "" ELSE RETURN Text.Sub(txt, p, q-p) END
  END Extract;

PROCEDURE GetLinks() =
  VAR
    txt := ProcUtils.ToText("ls -l /p/hlp");
    rd := NEW(TextRd.T).init(txt);
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd),
             toks = NEW(TextReader.T).init(line).shatter(WS,
                                                         "",
                                                         skipNulls:= TRUE) DO
          Debug.Out("GetLinks/line=" & line,100);
          IF TextList.Length(toks) = 10 AND TE(TextList.Nth(toks,8),"->") THEN
            (* a link *)
            EVAL linkTbl.put(TextList.Nth(toks,7),
                             TextList.Nth(toks,9))

          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END GetLinks;

VAR
  tbl     := NEW(TextRefTbl.Default).init();
  szTbl   := NEW(TextLongRealTbl.Default).init();
  linkTbl := NEW(TextTextTbl.Default).init();

PROCEDURE Do() RAISES { OSError.E, Wr.Failure } =
  VAR 
    iter := tbl.iterate();
    idsid : TEXT;
    r     : REFANY;
    tgt   : TEXT;
    sWr := FileWr.Open(Script);
  BEGIN
    Debug.Out(F("Creating script \"%s\"", Script), 0);
    Debug.Out(F("%s users, %s areas, %s links, %s ids", 
                Int(tbl.size()), Int(szTbl.size()), Int(linkTbl.size()), Int(ids.size())),0);

    Wr.PutText(sWr,   "#!/bin/sh -x\n");
    Wr.PutText(sWr, F("# generated by %s\n", Params.Get(0)));
    Wr.PutText(sWr,   "if [ \"`whoami`\" != \"srodaadm\" ]; then\n");
    Wr.PutText(sWr,   "  echo \"?sudo this script\"\n");    
    Wr.PutText(sWr,   "  exit 1\n");
    Wr.PutText(sWr,   "fi\n");

    WHILE iter.next(idsid, r) DO
      tgt := NIL;
      WITH rec = NARROW(r, Rec),
           haveLink = linkTbl.get(idsid, tgt) DO
        Debug.Out(F("idsid %10s desired %4s tgt %s", 
                    idsid, 
                    LongReal(rec.q),
                    UnNil(tgt)));
        IF tgt = NIL THEN
          Debug.Out("Must create area for " & idsid);
          EmitCreate(sWr, rec);
          emittedAny := TRUE
        ELSE
          VAR
            pfx := Pathname.Prefix(tgt);
            sz := FIRST(LONGREAL);
          BEGIN
            WITH haveSz = szTbl.get(pfx, sz) DO
              Debug.Out(F("pfx %s sz %s", pfx, LongReal(sz)));
              IF NOT TE(tgt, pfx & "/" & idsid) THEN
                Debug.Warning(F("Area for idsid %s malformed : %s (SKIPPING)",
                                idsid, tgt))
              ELSIF haveSz THEN
                IF sz # rec.q THEN 
                  EmitSizeChange(sWr, rec, pfx, sz); 
                  emittedAny := TRUE
                END
              ELSE
                Debug.Warning(F("Couldnt find a size for disk %s, idsid %s (SKIPPING)", pfx, idsid));
              END
            END
          END
        END
      END
    END;
    Wr.Close(sWr);

    EVAL ProcUtils.ToText(F("chmod +x %s", Script))
  END Do;

VAR emittedComments := FALSE;
VAR emittedAny      := FALSE;

PROCEDURE EmitSizeChange(wr : Wr.T; rec : Rec; pfx : TEXT; oldSize : LONGREAL) 
  RAISES { Wr.Failure } =
  VAR 
    prt : TEXT;
  BEGIN
    IF oldSize > rec.q THEN 
      Debug.Warning(F("requesting reduced size for idsid %s disk %s : %s GB -> %s GB",
                    rec.idsid, pfx, LongReal(oldSize), LongReal(rec.q)));
      Debug.Out("will comment out in script, edit manually if you want this!",0);
      emittedComments := TRUE;
      prt := "#"
    ELSE
      prt := ""
    END;
    prt := prt & F("stod resize --path %s --size %sG", pfx, LongReal(rec.q));

    Wr.PutText(wr, F("# resize disk for %s / %s GB -> %s GB\n", rec.idsid, LongReal(oldSize), LongReal(rec.q)));
    Wr.PutText(wr, prt); Wr.PutChar(wr, '\n')
  END EmitSizeChange;

PROCEDURE EmitCreate(wr : Wr.T; rec : Rec) RAISES { Wr.Failure } =
  VAR
    id := 1; (* reserve id 0 *)
    prt : TEXT;
    diskName : TEXT;
  BEGIN
    WHILE ids.member(id) DO
      INC(id);
      <*ASSERT Text.Length(Int(id)) <= MyDigits*>
    END;

    WITH hadIt = ids.insert(id) DO <*ASSERT NOT hadIt*> END;

    Wr.PutText(wr, F("# creating disk for %s\n", rec.idsid));
    diskName := MyPrefix & Pad(Int(id),padChar:= '0', length := MyDigits);

    WITH req      = F("--requirements \"fileserver='%s'&&disk=='%s'\"",
                      fileServer, volName),
         diskPath = F("/nfs/%s/disks/%s", Cell, diskName),
         homePath = diskPath & "/" & rec.idsid DO
      prt := F("stod create --verbose --cell %s --duration nolimit --immediate --wait-for-mount --owner srodaadm --project %s --business-group DCG %s --permissions 'owner:read,write,execute primary_group:read,execute sticky all:'",
               Cell, Project, req);
      prt := prt & F(" --size %sG --group %s --path %s\n", Int(CEILING(rec.q)), Group, diskPath);

      Wr.PutText(wr, prt); 

      prt := F(Sudo & "mkdir %s\n", homePath);
      Wr.PutText(wr, prt); 
    
      prt := F(Sudo & "stodfs chown --options %s --path %s\n", rec.idsid, homePath);
      Wr.PutText(wr, prt); 

      prt := F(Sudo & "ln -s %s /p/%s/%s\n", homePath, Group, rec.idsid);
      Wr.PutText(wr, prt); 
    END
    
  END EmitCreate;

BEGIN
  MakePattern();

  GetLinks();

  GetAreas();

  FOR i := 1 TO Params.Count-1 DO
    WITH fn = Params.Get(i) DO
      Load(fn)
    END
  END;

  Do();

  IF emittedComments THEN
    Debug.Warning("Certain commands are commented out, please check and edit " & Script)
  END;

  IF emittedAny THEN
    Debug.Out("When satisfied, please run the script " & Script, 0)
  ELSE
    Debug.Out("all configurations up to date --- no changes requested.", 0)
  END
END Main.
