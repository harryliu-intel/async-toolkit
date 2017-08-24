MODULE SpiceInstance;
IMPORT Word;
IMPORT Text;
IMPORT SpiceObject;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T; flatName : TEXT; obj : SpiceObject.T; parent : T) : T =
  BEGIN
    t.flatName := flatName;
    t.obj := obj;
    t.parent := parent;
    RETURN t
  END Init;
      
PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    IF TE(a.flatName, b.flatName) THEN
      <*ASSERT a.obj=b.obj*>
    END;
    RETURN TE(a.flatName, b.flatName)
  END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Text.Hash(a.flatName) END Hash;

BEGIN END SpiceInstance.
