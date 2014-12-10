#include "auto.h"

/*** read a command like argument ***/
char *arg(LEX *lex, int argc, char *argv[])
  {
  int i;
  if (lex_eatif_sym(lex,"$"))
    {
    i=lex_eat_integer(lex);
    if (i<argc) return argv[i];
    }
  lex_warning(lex,"command line argument expected.");
  lex_eat_until(lex,"\n");
  return "";
  }

/*** either get a quoted filename, or use command line arguments ***/
char *lex_eat_quote_arg(LEX *lex, int argc, char *argv[])
  {
  if (lex_is_quote(lex)) return lex_eat_quote(lex);
  else return arg(lex,argc,argv);
  }

/*** an int or a command line argument ***/
int lex_eat_integer_arg(LEX *lex, int argc, char *argv[])
  {
  if (lex_is_integer(lex)) return lex_eat_integer(lex);
  else return atoi(arg(lex,argc,argv));
  }

/*** an double or command line argument ***/
double lex_eat_real_arg(LEX *lex, int argc, char *argv[])
  {
  if (lex_is_real(lex)) return lex_eat_real(lex);
  else return atof(arg(lex,argc,argv));
  }
