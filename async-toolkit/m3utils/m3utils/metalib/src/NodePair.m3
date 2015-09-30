MODULE NodePair;
IMPORT Name, Word;
IMPORT Fmt;
IMPORT Dsim;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = 
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN RETURN Word.Plus(Word.Plus(Name.Hash(a.fanout),Name.Hash(a.fanin)),
                         ORD(a.outDir)+ORD(a.inDir)*2)
  END Hash;

PROCEDURE Format(t : T) : TEXT =
  TYPE DS = Dsim.Sense;
  PROCEDURE S(s : DS) : TEXT =
    BEGIN CASE s OF DS.Up => RETURN "+" | DS.Down => RETURN "-" END END S;

  BEGIN 
    RETURN Fmt.F("%s%s;%s%s",Name.Format(t.fanin),S(t.inDir),
                             Name.Format(t.fanout),S(t.outDir))
  END Format;

BEGIN END NodePair.
