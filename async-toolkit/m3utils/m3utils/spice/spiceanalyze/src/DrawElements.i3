INTERFACE DrawElements;
IMPORT PicSegments;
IMPORT SpiceAnalyze;

CONST Nfet = SpiceAnalyze.TransistorType.N;
      Pfet = SpiceAnalyze.TransistorType.P;
      
TYPE FetType = [ Nfet .. Pfet ];
     
PROCEDURE Fet(to     : PicSegments.T;
              type   : FetType;
              doBody : BOOLEAN);

PROCEDURE Res(to : PicSegments.T);

PROCEDURE Dio(to : PicSegments.T);

PROCEDURE Cap(to : PicSegments.T);

PROCEDURE Vdd(to : PicSegments.T);

PROCEDURE Gnd(to : PicSegments.T);

END DrawElements.
