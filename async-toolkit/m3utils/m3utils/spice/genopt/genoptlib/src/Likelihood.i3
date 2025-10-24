INTERFACE Likelihood;

PROCEDURE LogLikelihood(x, mu, sigma : LONGREAL) : LONGREAL;
  (* likelihood (density) of x given it is ~N(mu, sigma) *)

END Likelihood.
