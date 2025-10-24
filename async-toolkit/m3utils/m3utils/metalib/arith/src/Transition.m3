MODULE Transition;
IMPORT PRS, Boolean, Name, Word;

PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN Word.Plus(Name.Hash(a.name),Boolean.Hash(a.newValue)) END Hash;

BEGIN END Transition.
