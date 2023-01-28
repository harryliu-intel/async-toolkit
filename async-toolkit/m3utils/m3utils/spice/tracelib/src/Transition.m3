MODULE Transition;
IMPORT LongrealType AS LR;
IMPORT Word;

PROCEDURE CompareByTime(READONLY a, b : T) : CompareResult =
  BEGIN RETURN LR.Compare(a.at, b.at) END CompareByTime;

PROCEDURE CompareBySlew(READONLY a, b : T) : CompareResult =
  BEGIN RETURN LR.Compare(a.slew, b.slew) END CompareBySlew;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  (* it's generally not interesting to have more than one transition at a 
     given time since these objects are not keyed by waveform id *)
  BEGIN RETURN LR.Hash(a.at) END Hash;

BEGIN END Transition.
