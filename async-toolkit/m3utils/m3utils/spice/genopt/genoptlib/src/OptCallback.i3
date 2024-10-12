INTERFACE OptCallback;

TYPE
  T = OBJECT METHODS
    command(samples : [-1..LAST(CARDINAL) ] := 0) : TEXT;
  END;
  (* 
     samples=0 means a single sample  
     
     samples=-1 means a single nominal evaluation
  *)

CONST Brand = "OptCallback";

END OptCallback.
