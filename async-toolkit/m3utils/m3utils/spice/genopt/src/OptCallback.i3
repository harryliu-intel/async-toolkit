INTERFACE OptCallback;

TYPE
  T = OBJECT METHODS
    command(samples : CARDINAL := 0) : TEXT;
  END;

CONST Brand = "OptCallback";

END OptCallback.
