MODULE TestVar EXPORTS Main;
IMPORT Variation_p1274_0x1r1 AS TheVariation;
IMPORT Wr, FileWr;
IMPORT Variation; FROM Variation IMPORT SetP;
IMPORT HspVariation;
IMPORT Variation_p1274_3x2r1;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

TYPE TA = ARRAY OF TEXT;
     LA = ARRAY OF LONGREAL;

PROCEDURE TheBasicTest() =
  VAR
    geo : ARRAY [0..NUMBER(TheVariation.Geo)-1] OF Variation.Param;
    dis : ARRAY [0..NUMBER(TheVariation.Var)-1] OF Variation.Param;
    params : ARRAY [0..NUMBER(geo)+NUMBER(dis)-1] OF Variation.Param;
  BEGIN
    FOR i := FIRST(geo) TO LAST(geo) DO geo[i].nm := TheVariation.Geo[i] END;
    FOR i := FIRST(dis) TO LAST(dis) DO dis[i].nm := TheVariation.Var[i] END;
    
    WITH var = TheVariation.New(),
         wr  = FileWr.Open("test.lib") DO
      SUBARRAY(params, 0, NUMBER(geo)) := geo;
      SUBARRAY(params, NUMBER(geo), NUMBER(dis)) := dis;
      
      SetP(params,
           TA {   "M", "WdrawnUm", "LdrawnUm", "lermat", "vtsingle",   "NF" },
           LA { 1.0d0,    42.0d-3,    28.0d-3,    1.0d0,     -1.0d0,  1.0d0 });
      
      var.wrModel(wr,
                  fromLib          := "tttt",
                  baseTranTypeName := "n",
                  newTranTypeName  := "n_var01",
                  params           := params);
      Wr.Close(wr)
    END
  END TheBasicTest;

PROCEDURE TheDynamicTest() =
  VAR
    variation : HspVariation.T :=
        NEW(HspVariation.T).init(Variation_p1274_3x2r1.HspName);
    wr := FileWr.Open("testvar.var");
    params : REF ARRAY OF Variation.Param;
    vars := variation.getVars("pfff", "nsvt");
  BEGIN
    variation.setGlobalParam("widtnflag", 1.0d0);
    variation.setGlobalParam("scale", 1.0d0);
    
    FOR i := FIRST(vars^) TO LAST(vars^) DO
      Debug.Out(F("vars[%s] : %s", Int(i), vars[i]))
    END;
    params := NEW(REF ARRAY OF Variation.Param,
                  NUMBER(TheVariation.Geo) + NUMBER(vars^));
    FOR i := FIRST(HspVariation.GeoVars) TO LAST(HspVariation.GeoVars) DO
      params[i].nm := HspVariation.GeoVars[i]
    END;
    FOR i := NUMBER(TheVariation.Geo) TO LAST(params^) DO
      params[i].nm := vars[i-NUMBER(TheVariation.Geo)]
    END;
    SetP(params^,
         TA { "M", "Wdrawn", "Ldrawn", "NF", "lermat", "vtsingle", "noiv" },
         LA { 1.0d0, 34.0d-9, 20.0d-9, 1.0d0, 6.0d0, 0.0d0, 0.0d0 } );
    variation.wrModel(wr,
                      "tttt",
                      "nsvt",
                      "nsvt_var00",
                      params^);
    SetP(params^,
         TA { "M", "Wdrawn", "Ldrawn", "NF", "lermat", "vtsingle", "noiv" },
         LA { 1.0d0, 34.0d-9, 20.0d-9, 1.0d0, 0.0d0, 6.0d0, 0.0d0 } );
    variation.wrModel(wr,
                      "tttt",
                      "nsvt",
                      "nsvt_var01",
                      params^);
    SetP(params^,
         TA { "M", "Wdrawn", "Ldrawn", "NF", "lermat", "vtsingle", "noiv" },
         LA { 1.0d0, 34.0d-9, 20.0d-9, 1.0d0, 0.0d0, 0.0d0, 6.0d0 } );
    variation.wrModel(wr,
                      "tttt",
                      "nsvt",
                      "nsvt_var02",
                      params^);
    SetP(params^,
         TA { "M", "Wdrawn", "Ldrawn", "NF", "lermat", "vtsingle", "noiv" },
         LA { 1.0d0, 34.0d-9, 20.0d-9, 1.0d0,-6.0d0, 0.0d0, 0.0d0 } );
    variation.wrModel(wr,
                      "tttt",
                      "nsvt",
                      "nsvt_var03",
                      params^);
    SetP(params^,
         TA { "M", "Wdrawn", "Ldrawn", "NF", "lermat", "vtsingle", "noiv" },
         LA { 1.0d0, 34.0d-9, 20.0d-9, 1.0d0, 0.0d0,-6.0d0, 0.0d0 } );
    variation.wrModel(wr,
                      "tttt",
                      "nsvt",
                      "nsvt_var04",
                      params^);
    SetP(params^,
         TA { "M", "Wdrawn", "Ldrawn", "NF", "lermat", "vtsingle", "noiv" },
         LA { 1.0d0, 34.0d-9, 20.0d-9, 1.0d0, 0.0d0, 0.0d0,-6.0d0 } );
    variation.wrModel(wr,
                      "tttt",
                      "nsvt",
                      "nsvt_var05",
                      params^);
    Wr.Close(wr)
  END TheDynamicTest;
    
BEGIN
  TheDynamicTest()
END TestVar.
