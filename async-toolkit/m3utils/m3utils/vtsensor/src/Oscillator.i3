INTERFACE Oscillator;
IMPORT OscillatorTemp;

TYPE
  T = OBJECT
    temps : REF ARRAY OF OscillatorTemp.T;
  END;
  
CONST Brand = "Oscillator";

END Oscillator.
