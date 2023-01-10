INTERFACE Blackbody;

PROCEDURE PlanckRadiance(T, nu : LONGREAL) : LONGREAL;
  (* spectral radiance in watts per square meter per steradian per hertz at 
     given temperature in kelvin and frequency in hertz *)

END Blackbody.
