INTERFACE SvgCanvas;
IMPORT Canvas;
IMPORT Wr;

TYPE
  T <: Public;

  Public = Canvas.T OBJECT METHODS
    init() : T;

    write(wr : Wr.T) RAISES { Wr.Failure };
  END;

CONST Brand = "SvgCanvas";

END SvgCanvas.
