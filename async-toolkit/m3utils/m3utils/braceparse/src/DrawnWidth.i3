INTERFACE DrawnWidth;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(defString : TEXT) : T;
    (* specify formula for the width.  formula must be purely functional *)
    
    eval(fins : CARDINAL) : CARDINAL;
    (* convert width in fins to width in nanometers *)
  END;

CONST Brand = "DrawnWidth";

END DrawnWidth.
