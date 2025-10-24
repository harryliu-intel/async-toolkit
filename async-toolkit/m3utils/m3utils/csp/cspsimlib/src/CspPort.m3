MODULE CspPort;
IMPORT Atom;
IMPORT CspDirection;
FROM Fmt IMPORT F, Int;

PROCEDURE  New( name     : Atom.T;
                dir      : CspDirection.T;
                def      : Channel
              ) : T =
  BEGIN
    RETURN
      NEW(T,
          name     := name     ,
          dir      := dir      ,
          def      := def)
  END New;

PROCEDURE NewScalar(class    : Class;
                    width    : CARDINAL;
                    typeName : Atom.T) : Scalar =
  BEGIN
    RETURN NEW(Scalar, class := class, width := width, typeName := typeName)
  END NewScalar;

PROCEDURE NewArray(range : Range;
                   elem  : Channel) : Array =
  BEGIN
    RETURN
      NEW(Array, range := range, elem := elem)
  END NewArray;

PROCEDURE NewRange(min, max : INTEGER) : Range =
  BEGIN
    RETURN Range { min, max }
  END NewRange;

PROCEDURE BaseChanType(chan : Channel) : Scalar =
  BEGIN
    TYPECASE chan OF
      Array(arr) => RETURN BaseChanType(arr.elem)
    ELSE
      RETURN chan
    END
  END BaseChanType;

PROCEDURE M3ChanDecl(chan : Channel) : TEXT =
  BEGIN
    TYPECASE chan OF
      Scalar(s) =>
      RETURN F("%s%sChan.Ref", ClassTypeNames[s.class], Int(s.width))
    |
      Array(a) =>
      RETURN F("ARRAY [ 16_%s .. 16_%s ] OF %s",
               Int(a.range.min, base := 16),
               Int(a.range.max, base := 16),
               M3ChanDecl(a.elem))
    ELSE
      <*ASSERT FALSE*>
    END
  END M3ChanDecl;
  
BEGIN END CspPort.
