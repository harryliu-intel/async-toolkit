MODULE Blackbody;
IMPORT Math;

CONST
  h = 6.62607015d-34; (* Planck's constant / [m^2 kg / s] *)
  k =   1.380649d-23; (* Boltzmann's constant / [m^2 kg / s / K] *)
  c =  299792458.0d0; (* speed of light / [m/s] *)
  
PROCEDURE PlanckRadiance(T, nu : LONGREAL) : LONGREAL =
  BEGIN
    (* Planck's Law *)
    WITH t1 = 2.0d0 * nu * nu / c / c,
         ex = Math.exp(h * nu / k / T),
         t2 = h * nu / (ex - 1.0d0) DO
      RETURN t1 * t2
    END
  END PlanckRadiance;

BEGIN END Blackbody.
