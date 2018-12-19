MODULE RegContainer;
IMPORT RegRegfile;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    skipArc := SkipArc;
  END;

PROCEDURE SkipArc(t : T) : BOOLEAN =
  BEGIN RETURN ISTYPE(t, RegRegfile.T) AND t.children.size() = 1 END SkipArc;

BEGIN END RegContainer.
