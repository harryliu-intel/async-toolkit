MODULE Search;
IMPORT Port;

PROCEDURE Search(READONLY ports     : ARRAY OF Port.T;
                 checker            : Checker;
                 nslots             : CARDINAL) : BOOLEAN =

  PROCEDURE Recurse(at                  : CARDINAL) : BOOLEAN =

    PROCEDURE InitialPlacement(): SinglePlacement =
      BEGIN 
        IF at = 0 THEN 
          RETURN FIRST(SinglePlacement)
        ELSE 
          WITH prev = placement[at-1] DO
            IF prev = LAST(ports) THEN 
              RETURN FIRST(SinglePlacement) 
            ELSE
              RETURN prev + 1
            END
          END
        END
      END InitialPlacement;
      
    PROCEDURE Next(VAR pl : SinglePlacement) : BOOLEAN =
      VAR new : SinglePlacement;
      BEGIN
        IF pl = LAST(ports) THEN 
          new := -1 
        ELSE
          new := pl + 1
        END;
        IF new = initPlacement THEN
          RETURN FALSE
        ELSE
          pl := new; 
          RETURN TRUE
        END
      END Next;
      
    VAR
      initPlacement := InitialPlacement();
      thisPlacement := initPlacement;
    BEGIN
      LOOP
        placement[at] := thisPlacement;        
        pSeq[thisPlacement].addhi(at);
        WITH thunk = checker.makePlacement(SUBARRAY(placement^, 0, at+1)) DO
          IF checker.placementOK(placement^, pSeq^) THEN
            IF Recurse(at+1) THEN RETURN TRUE END
          END;
          checker.unmakePlacement(placement^, pSeq^, thunk);
          WITH hi = pSeq[thisPlacement].remhi() DO
            <*ASSERT hi = at *>
          END
        END;
        IF NOT Next(thisPlacement) THEN RETURN FALSE END
      END
    END Recurse;

  VAR placement := NEW(REF Placement, nslots);
      pSeq      := NEW(REF RevPlacement, NUMBER(ports));
  BEGIN 
    RETURN Recurse(0) 
  END Search;

BEGIN END Search.
