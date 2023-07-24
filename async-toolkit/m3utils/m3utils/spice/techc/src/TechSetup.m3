MODULE TechSetup EXPORTS TechSetup, TechTemplate;
IMPORT TechConfig;
IMPORT TextTextTbl;
IMPORT TextSeq;
IMPORT OSError;
IMPORT Rd;
IMPORT Debug;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT AL;
IMPORT Pathname;
IMPORT Text;
IMPORT Wr;
IMPORT FileRd;
IMPORT CitTextUtils;
IMPORT FileWr;

FROM TechConfig IMPORT Corp, Mode, Gate;
FROM TechConfig IMPORT TranNames, GateNames, TechNames, TechCorp, TemplateNames;

FROM TechTechs IMPORT Techs;
FROM TechConfig IMPORT Gate1;
IMPORT Env;
IMPORT Thread;

<*FATAL Thread.Alerted*>

TYPE Config = TechConfig.T;
CONST TE = Text.Equal;
      LR = Fmt.LongReal;

      Verbose = TRUE;

VAR
  DoMcFileOnly := Env.Get("SETUP_MC_FILE_ONLY") # NIL;
  (* set this env. var to generate the .mc0 file to get the names of the
     varying parameters *)
  
CONST    
  StdPlugText = "vcc vssx";

PROCEDURE DoSetup(READONLY c : Config) =
  VAR
    SimFile := c.simRoot & ".sp";
    map     := NEW(TextTextTbl.Default).init();
    template : TextSeq.T;
    templatePath := c.templateDir & "/" & TemplateNames[c.gate];
  BEGIN
    MapCommon(c, map);

    TRY
      template := LoadTemplate(templatePath);
    EXCEPT
      OSError.E(e) => Debug.Error(F("Couldn't open template file \"%s\" : OSError.E : %s",
                                    templatePath, AL.Format(e)))
    |
      Rd.Failure(e) =>
      Debug.Error(F("Couldn't read template file \"%s\" : Rd.Failure : %s",
                                    templatePath, AL.Format(e)))
    END;

    ModifyTemplate(template, map);

    TRY
      WriteTemplate(template, SimFile)
    EXCEPT
      OSError.E(e) => Debug.Error(F("Couldn't write simulation file \"%s\" : OSError.E : %s",
                                    SimFile, AL.Format(e)))
    |
      Wr.Failure(e) =>
      Debug.Error(F("Couldn't write simulation file \"%s\" : Wr.Failure : %s",
                    SimFile, AL.Format(e)))
    END
  END DoSetup;

PROCEDURE MapCommon(READONLY c : Config; map : TextTextTbl.T)=
  VAR
    iter : TextTextTbl.Iterator;
    k, v : TEXT;
    tech := Techs[c.tech];
  BEGIN
    EVAL map.put("@HSPICE_MODEL_ROOT@", c.hspiceModelRoot);
    EVAL map.put("@HSPICE_MODEL@", c.hspiceModel);
    EVAL map.put("@TEMP@", LR(c.temp));
    EVAL map.put("@VOLT@", LR(c.volt));
    EVAL map.put("@FANOUT@", Int(c.fanout));

    (* gate terminals *)
    CASE c.gate OF
      Gate.Buf =>
      EVAL map.put("@T0A@", "in");
      EVAL map.put("@T0B@", "");
      EVAL map.put("@T0C@", "");

      EVAL map.put("@T1A@", "xi");
      EVAL map.put("@T1B@", "");
      EVAL map.put("@T1C@", "");
    |
      Gate.Xor =>
      EVAL map.put("@T0A@", "in");
      EVAL map.put("@T0B@", "vcc");
      EVAL map.put("@T0C@", "");

      EVAL map.put("@T1A@", "vcc");
      EVAL map.put("@T1B@", "xi");
      EVAL map.put("@T1C@", "");
    |
      Gate.XorAlt =>
      EVAL map.put("@T0A@", "in");
      EVAL map.put("@T0B@", "vcc");
      EVAL map.put("@T0C@", "");

      EVAL map.put("@T1A@", "vcc");
      EVAL map.put("@T1B@", "xi");
      EVAL map.put("@T1C@", "");
    |
      Gate.Aoi,
      Gate.Aoi_Z1_0p0sigma, Gate.Aoi_Z1_5p3sigma,
      Gate.Aoi_Z2_0p0sigma, Gate.Aoi_Z2_5p3sigma
      =>
      (* Intel terminal order is a b c  where b & c are the symmetric inputs
         TSMC terminal order is A1 A2 B *)
      CASE TechCorp[c.tech] OF
        Corp.Intc =>
        EVAL map.put("@T0A@", "vssx");
        EVAL map.put("@T0B@", "in");
        EVAL map.put("@T0C@", "in");
        
        EVAL map.put("@T1A@", "vcc");
        EVAL map.put("@T1B@", "xi");
        EVAL map.put("@T1C@", "xi");
      |
        Corp.Tsmc =>
        EVAL map.put("@T0A@", "in");
        EVAL map.put("@T0B@", "in");
        EVAL map.put("@T0C@", "vssx");
        
        EVAL map.put("@T1A@", "xi");
        EVAL map.put("@T1B@", "xi");
        EVAL map.put("@T1C@", "vcc");
      END        
    |
      Gate.Oai,
      Gate.Oai_Z1_0p0sigma, Gate.Oai_Z1_5p3sigma,
      Gate.Oai_Z2_0p0sigma, Gate.Oai_Z2_5p3sigma =>
      <*ASSERT FALSE*>
    |
      Gate.Xor_Z1_0p0sigma, Gate.Xor_Z1_5p3sigma,
      Gate.Xor_Z2_0p0sigma, Gate.Xor_Z2_5p3sigma
      =>
      EVAL map.put("@T0A@", "in");
      EVAL map.put("@T0B@", "vcc");
      EVAL map.put("@T0C@", "");

      EVAL map.put("@T1A@", "vcc");
      EVAL map.put("@T1B@", "xi");
      EVAL map.put("@T1C@", "")
    END;

    IF DoMcFileOnly THEN
      EVAL map.put("@MC_OPTIONS@", ".option mc_file_only=yes");
      EVAL map.put("@MC_SPEC@", "sweep monte=2");
    ELSE
      EVAL map.put("@MC_OPTIONS@", "");

      CASE c.gate OF
        Gate.Xor_Z1_0p0sigma, Gate.Xor_Z2_0p0sigma,
        Gate.Aoi_Z1_0p0sigma, Gate.Aoi_Z2_0p0sigma =>
        EVAL map.put("@MC_SPEC@", "sweep monte=list(1:1)");
      |
        Gate.Xor_Z1_5p3sigma, Gate.Xor_Z2_5p3sigma,
        Gate.Aoi_Z1_5p3sigma, Gate.Aoi_Z2_5p3sigma =>
        EVAL map.put("@MC_SPEC@", "sweep monte=list(2:2)");
      ELSE
        (* skip *)
      END
    END;

    CASE c.gate OF
      Gate.Xor_Z1_0p0sigma, Gate.Xor_Z1_5p3sigma,
      Gate.Aoi_Z1_0p0sigma, Gate.Aoi_Z1_5p3sigma =>
      EVAL map.put("@Z@", "1");
    |
      Gate.Xor_Z2_0p0sigma, Gate.Xor_Z2_5p3sigma,
      Gate.Aoi_Z2_0p0sigma, Gate.Aoi_Z2_5p3sigma =>
      EVAL map.put("@Z@", "2");
    ELSE
      (* skip *)
    END;
      
    EVAL map.put("@THRESH@", TranNames[c.tran]);
    
    (* parasitic or not *)
    IF c.para THEN
      WITH gate1     = Gate1[c.gate],
           cellname0 = tech.cellNames[c.gate][c.tran],
           cellname1 = tech.cellNames[gate1 ][c.tran] DO
        <*ASSERT cellname0 # NIL*>
        <*ASSERT cellname1 # NIL*>
        EVAL map.put("@CELLNAME0@", cellname0);
        EVAL map.put("@CELLNAME1@", cellname1)
      END;
      EVAL map.put("@PLUGTEXT@", tech.plugText);
      WITH p0 =        tech.cellPaths[c.gate][c.tran],
           g1 = Gate1[c.gate],
           p1 =        tech.cellPaths[g1    ][c.tran] DO
        IF TE(p0, p1) THEN
          EVAL map.put("@INCLUDELIB@", F(".include \"%s\"\n",
                                         p0));
        ELSE
          EVAL map.put("@INCLUDELIB@", F(".include \"%s\"\n.include \"%s\"\n",
                                         p0, p1));
        END
      END
        
    ELSE
      (* not parasitic *)
      WITH gate1     = Gate1[c.gate],
           cellname0 = GateNames[c.gate] & "_gate",
           cellname1 = GateNames[gate1]  & "_gate" DO
        <*ASSERT cellname0 # NIL*>
        <*ASSERT cellname1 # NIL*>
        EVAL map.put("@CELLNAME0@", cellname0);
        EVAL map.put("@CELLNAME1@", cellname1)
      END;
      EVAL map.put("@CELLNAME0@", GateNames[c.gate] & "_gate");
      EVAL map.put("@CELLNAME1@", GateNames[c.gate] & "_gate");
      EVAL map.put("@PLUGTEXT@", StdPlugText);
      EVAL map.put("@INCLUDELIB@", "");
      EVAL map.put("@OPTVCC@", "vcc"); (*  vcc input for XOR *)
    END;
    
    EVAL map.put("@NANOSECONDS@", Int(CEILING(c.nanoseconds)));
    EVAL map.put("@TIMESTEP@", Int(ROUND(c.timestep / 1.0d-12)) & "ps");
    EVAL map.put("@OPTIONS@", SimOptions[c.simu]);
    EVAL map.put("@CORNER@", tech.cornNames[c.corn]);

    CASE c.mode OF
      Mode.Dyn =>
      EVAL map.put("@RESET_SOURCE@",
                   "Vres _RESET 0 DC=0 PWL 0 0 10ns 0 10.1ns vtrue")
    |
      Mode.Leak =>
      EVAL map.put("@RESET_SOURCE@", "Vres _RESET 0 DC=0")
    END;

    WITH sufx = tech.tranSufxs[c.tran] DO
      IF sufx = NIL THEN
        Debug.Error(F("No mapping for %s in %s",
                      TranNames[c.tran],
                      TechNames[c.tech]))
      END;
      EVAL map.put("@TRANSUFX@", sufx)
    END;

    EVAL map.put("@TRANSIZE@", tech.tranSize);

    iter := extraMap.iterate();
    WHILE iter.next(k, v) DO
      WITH key = F("@%s@", k),
           hadIt = map.put(key, v) DO
        IF hadIt THEN
          Debug.Error("Duplicate mapping for key " & key)
        END
      END
    END;

    iter := overrideMap.iterate();
    WHILE iter.next(k, v) DO
      WITH key   = F("@%s@", k),
           <*NOWARN*>hadIt = map.put(key, v) DO
        (* skip *)
      END
    END
  END MapCommon;

PROCEDURE LoadTemplate(path : Pathname.T) : TextSeq.T
  RAISES { OSError.E, Rd.Failure } =
  VAR
    rd := FileRd.Open(path);
    res := NEW(TextSeq.T).init();
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          res.addhi(line)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    Rd.Close(rd);
    RETURN res
  END LoadTemplate;

PROCEDURE WriteTemplate(template : TextSeq.T; path : Pathname.T)
  RAISES { OSError.E, Wr.Failure } =
  VAR
    wr := FileWr.Open(path);
  BEGIN
    FOR i := 0 TO template.size() - 1 DO
      Wr.PutText(wr, template.get(i));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END WriteTemplate;

PROCEDURE ModifyTemplate(template : TextSeq.T; map : TextTextTbl.T) =
  VAR
    k, v, line : TEXT;
    iter := map.iterate();
  BEGIN
    WHILE iter.next(k, v) DO
      IF Verbose THEN
        Debug.Out(F("k %s -> v %s", k, Debug.UnNil(v)))
      END
    END;
    
    FOR i := 0 TO template.size() - 1 DO
      line := template.get(i);
      iter := map.iterate();
      WHILE iter.next(k, v) DO
        IF v = NIL THEN
          Debug.Error("NIL mapping for " & k)
        END;
        line := CitTextUtils.Replace(line, k, v)
      END;
      template.put(i, line)
    END
  END ModifyTemplate;

BEGIN
  extraMap    := NEW(TextTextTbl.Default).init();
  overrideMap := NEW(TextTextTbl.Default).init();


END TechSetup.

