INTERFACE ArithCallback;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    setErrorCb(ecb : T);
    newError(msg : TEXT);

    newByte(byte : CHAR); (* abstract *)
    newEof();             (* abstract *) 
  END;

CONST Brand = "ArithCallback";

END ArithCallback.
