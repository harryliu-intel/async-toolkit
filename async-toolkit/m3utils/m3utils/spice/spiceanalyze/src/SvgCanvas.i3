INTERFACE SvgCanvas;
IMPORT Canvas;
IMPORT Wr;
IMPORT SvgColor;
IMPORT Thread;

TYPE
  T <: Public;

  Public = Canvas.T OBJECT METHODS
    init(defColor       := SvgColor.Black;
         defStrokeWidth := 2.0d0) : T;

    write(wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted };
  END;

CONST Brand = "SvgCanvas";

END SvgCanvas.
