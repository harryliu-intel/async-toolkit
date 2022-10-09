INTERFACE HnnSettings;
IMPORT Hnn;
IMPORT Word;

REVEAL
  Hnn.T <: Settings;

CONST
  MaxS = 20; (* hmm... *)

TYPE
  Settings = Hnn.Public OBJECT METHODS
    setS(s : [ 1 .. MaxS ]);
  END;

END HnnSettings.

