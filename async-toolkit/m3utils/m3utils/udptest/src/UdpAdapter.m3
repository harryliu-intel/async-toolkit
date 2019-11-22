MODULE UdpAdapter;

REVEAL
  Default = T BRANDED Brand & " Default" OBJECT
  OVERRIDES
    eot := NopEot;
  END;

PROCEDURE NopEot(<*UNUSED*>t : T) =
  BEGIN
    (* skip *)
  END NopEot;

BEGIN END UdpAdapter.
