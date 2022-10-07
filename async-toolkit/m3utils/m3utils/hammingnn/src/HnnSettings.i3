INTERFACE HnnSettings;
IMPORT Hnn;
IMPORT Word;

REVEAL
  Hnn.T <: Settings;

CONST
  MaxS = 32;

TYPE
  Settings = Hnn.Public OBJECT METHODS
    setS(s : [ 1 .. MaxS ]);
  END;

END HnnSettings.

