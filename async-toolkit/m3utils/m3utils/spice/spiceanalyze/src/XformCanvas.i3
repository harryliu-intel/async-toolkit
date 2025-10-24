INTERFACE XformCanvas;
IMPORT Canvas, CanvasXform;
IMPORT PicPoint, PicCoord;

TYPE

  T <: Public;

  Public = Canvas.T OBJECT METHODS
    init(target : Canvas.T) : T;

    (* override the following *)
    xformPoint(p : PicPoint.T) : PicPoint.T; 
    xformLength(p : PicCoord.T) : PicCoord.T;
  END;

  Default <: PublicDefault;

  PublicDefault = T OBJECT METHODS
    init(target : Canvas.T; xform : CanvasXform.T) : T;
  END;
  
CONST Brand = "XformCanvas";

END XformCanvas.
