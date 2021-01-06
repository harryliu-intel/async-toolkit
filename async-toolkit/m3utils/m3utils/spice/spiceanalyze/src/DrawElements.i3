INTERFACE DrawElements;
IMPORT PicSegments;

TYPE FetType = { N, P };
     
PROCEDURE DrawFet(to     : PicSegments.T;
                  type   : FetType;
                  doBody : BOOLEAN);

PROCEDURE DrawRes(to : PicSegments.T);

PROCEDURE DrawDio(to : PicSegments.T);

PROCEDURE DrawCap(to : PicSegments.T);

PROCEDURE DrawVdd(to : PicSegments.T);

PROCEDURE DrawGnd(to : PicSegments.T);

END DrawElements.
