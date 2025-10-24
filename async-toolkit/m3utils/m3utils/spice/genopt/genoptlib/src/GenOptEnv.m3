MODULE GenOptEnv;
IMPORT Env;

BEGIN
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  NbOpts  := Env.Get("NBRUNOPTIONS");
END GenOptEnv.

