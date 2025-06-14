MODULE CspRemote;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    gid2wid := Gid2Wid; (* global to worker *)
    gid2sid := Gid2Sid; (* global to scheduler *)
  END;

PROCEDURE Gid2Wid(t : T; gid : CARDINAL) : CARDINAL =
  BEGIN
    (* LSBs are the thread within the worker *)
    RETURN gid DIV t.mt
  END Gid2Wid;

PROCEDURE Gid2Sid(t : T; gid : CARDINAL) : CARDINAL =
  BEGIN
    (* LSBs are the thread within the worker *)
    RETURN gid MOD t.mt
  END Gid2Sid;

BEGIN END CspRemote.
