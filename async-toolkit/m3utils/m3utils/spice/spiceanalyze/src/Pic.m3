MODULE Pic;
IMPORT PicExtent;

REVEAL
  T = Public BRANDED Brand OBJECT
    reqExtent : PicExtent.T;
  OVERRIDES
    init := Init;
    setExtent := SetExtent;
    curExtent := CurExtent;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    RETURN t
  END Init;

PROCEDURE SetExtent(t : T; READONLY extent : PicExtent.T) =
  BEGIN t.reqExtent := extent END SetExtent;

PROCEDURE CurExtent(t : T) : PicExtent.T =
  BEGIN RETURN t.reqExtent END CurExtent;
  
BEGIN END Pic.
