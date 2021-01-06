INTERFACE PicCanvas;

(* a canvas drawing into a PicSegments.T *)

IMPORT PicSegments;
IMPORT Canvas;

TYPE
  T <: Public;

  Public = Canvas.T OBJECT METHODS
    init(tgt : PicSegments.T) : T;
  END;

CONST Brand = "PicCanvas";

END PicCanvas.
