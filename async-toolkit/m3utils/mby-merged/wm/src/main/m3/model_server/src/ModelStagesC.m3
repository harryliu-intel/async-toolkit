UNSAFE MODULE ModelStagesC;
IMPORT Text;
IMPORT M3toC;
IMPORT Debug;
FROM Fmt IMPORT F;
IMPORT Byte;

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

PROCEDURE CallStage(READONLY info    : Info;
                    r                : ADDRESS;
                    w                : ADDRESS;
                    READONLY in      : ARRAY OF Byte.T;
                    VAR      out     : REF ARRAY OF Byte.T;
                    READONLY rxDataP : ARRAY OF Byte.T;
                    VAR      txDataP : REF ARRAY OF Byte.T)
  RAISES { Error } =
  VAR
    rxData, txData := NEW(UNTRACED REF Varchar);
  BEGIN
    IF NUMBER(in)  # info.in_size THEN RAISE Error("in size mismatch") END;

    IF NUMBER(rxDataP) = 0 THEN
      rxData^.data   := NIL;
    ELSE
      rxData^.data   := ADR(rxDataP[0]);
    END;
    rxData^.length := NUMBER(rxDataP);

    out := NEW(REF ARRAY OF Byte.T, info.out_size);
    
    info.stage_func(r, w, ADR(in[0]), ADR(out[0]), rxData, txData);

    txDataP := NEW(REF ARRAY OF Byte.T, txData.length);
    FOR i := 0 TO txData.length-1 DO
      txDataP[i] := LOOPHOLE(txData.data + i, UNTRACED REF Byte.T)^
    END;

    DISPOSE(rxData);
    DISPOSE(txData)
  END CallStage;
  
BEGIN END ModelStagesC.
