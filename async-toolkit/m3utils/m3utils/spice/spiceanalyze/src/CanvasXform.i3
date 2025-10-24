INTERFACE CanvasXform;
IMPORT PicPoint, PicCoord;

TYPE
  (* we could use a 4x4 here, really, but that's getting fancy.. *)
  T = RECORD
    translateOrigin : PicPoint.T;
    (* old origin in output coordinate system *)
    
    scale           : PicCoord.T;
    (* scale in input coordinate system *)
  END;

  (* scale is applied first, translate second

     -- so that the old origin will be in the location requested in the
     new system, NOT in the scaled location!  (new origin is not scaled)
  *)

CONST Brand = "CanvasXform";

END CanvasXform.

  
