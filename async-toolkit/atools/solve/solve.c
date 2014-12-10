#include "solve.h"

/*** main **/
int main(int argc, char *argv[]) {
  /*** setup ***/
  setlinebuf(stdout);
  setlinebuf(stderr);
  output = stdout;
  init_solve();

  /*** parse commands ***/
  parse(stdin,argc,argv);
  fflush(stdout);
  fflush(stderr);
  if (output!=stdout) fclose(output);

  /*** free and check for memory leaks ***/
  free_solve();
  leak_check();
  return ERROR;
}
