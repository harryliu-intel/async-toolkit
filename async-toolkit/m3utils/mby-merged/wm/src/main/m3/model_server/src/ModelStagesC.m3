UNSAFE MODULE ModelStagesC;
IMPORT Text;
IMPORT M3toC;
IMPORT Debug;
FROM Fmt IMPORT F;

CONST TE = Text.Equal;
      
PROCEDURE Lookup(top_map_name, stage_name : TEXT; VAR info : Info) : BOOLEAN =
  VAR
    p := model_stages;
  BEGIN
    WHILE p # NIL DO
      WITH top_map_name_txt = M3toC.StoT(p.top_map_name),
           stage_name_txt   = M3toC.StoT(p.stage_name) DO
        Debug.Out(F("ModelStagesC.Lookup : Looking for %s/%s checking %s/%s",
                    top_map_name, stage_name,
                    top_map_name_txt, stage_name_txt));
        
        IF TE(top_map_name, top_map_name_txt) AND
           TE(stage_name, stage_name_txt) THEN
          info := p^;
          Debug.Out("ModelStagesC.Lookup : Found it!");
          RETURN TRUE
        END
      END;
      p := p.next
    END;
    RETURN FALSE
  END Lookup;

PROCEDURE CallStage(READONLY info : Info) =
  BEGIN
  END CallStage;
  
BEGIN END ModelStagesC.
