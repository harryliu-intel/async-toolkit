INTERFACE GenViews;
IMPORT RdlComponentDef;
IMPORT DecoratedComponentDef;
IMPORT RegAddrmap;
IMPORT Pathname;

TYPE
  T = OBJECT METHODS
    decorate(def       : RdlComponentDef.T;
             path      : TEXT             ) : DecoratedComponentDef.T;
    gen     (tgtmap    : RegAddrmap.T;
             outDir    : Pathname.T       );
  END;

CONST Brand = "GenViews";

END GenViews.
