MODULE UpdateTracing;
IMPORT Env;

BEGIN
  Enabled := Env.Get("UPDATE_TRACING") # NIL;
END UpdateTracing.
