/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** data structures ***/

#ifdef NEWLEX
typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    char *c;
    struct _position *pos;
    } p;
  } LIST;

typedef struct _position
  {
  int position,column,line;
  } POSITION;

typedef struct _lex
  {
  FILE *file;
  POSITION position;
  LIST *position_stack,*buffer;
  int min,max,nested_comments;
  char *whitespace,*begin_comment,*end_comment,*token,*filename,*string;
  } LEX;
#else
typedef void LEX; /*** Users should mind their own damn business ***/
#endif

/*** function prototypes ***/

LEX *lex_file(FILE *file);
LEX *lex_file_with_name(FILE *file, char *filename);
LEX *lex_string(char *string);
void lex_free(LEX *lex);
int lex_line(LEX *lex);
char *lex_get_filename(LEX *lex);
void lex_set_line(LEX *lex, int line);
void lex_set_filename(LEX *lex, char *filename);
int lex_column(LEX *lex);
void lex_push_position(LEX *lex);
void lex_restore_position(LEX *lex);
void lex_pop_position(LEX *lex);
char *lex_get_token(LEX *lex);
char lex_char(LEX *lex);
char lex_eat_char(LEX *lex);
void lex_message(LEX *lex, char *pre, char *msg);
void lex_error(LEX *lex, char *expecting);
void lex_warning(LEX *lex, char *expecting);
void lex_file_error(LEX *lex, char *file);
void lex_file_warning(LEX *lex, char *file);
void lex_error_pos(LEX *lex, const char *err_tp);
void lex_define_whitespace(LEX *lex, char *whitespace);
void lex_define_comments(LEX *lex, char *begin, char *end, int nested);

void lex_finish_comment(LEX *lex);
int lex_do_whitespace(LEX *lex);
int lex_do_id(LEX *lex);
int lex_do_sym(LEX *lex, char *symbol);
int lex_do_integer(LEX *lex);
int lex_do_real(LEX *lex);
int lex_do_quote(LEX *lex);
int lex_do_keyword(LEX *lex, char *keyword);

int lex_is_eof(LEX *lex);
int lex_is_whitespace(LEX *lex);
char *lex_whitespace(LEX *lex);
char *lex_eat_whitespace(LEX *lex);
int lex_is_id(LEX *lex);
int lex_is_keyword(LEX *lex, char *keyword);
void lex_eat_keyword(LEX *lex, char *keyword);
int lex_eatif_keyword(LEX *lex, char *keyword);
char *lex_id(LEX *lex);
char *lex_eat_id(LEX *lex);
char *lex_eatif_id(LEX *lex);
int lex_is_sym(LEX *lex, char *sym);
void lex_eat_sym(LEX *lex, char *sym);
int lex_eatif_sym(LEX *lex, char *sym);
int lex_is_integer(LEX *lex);
int lex_integer(LEX *lex);
int lex_eat_integer(LEX *lex);
int lex_is_quote(LEX *lex);
char *lex_quote(LEX *lex);
char *lex_eat_quote(LEX *lex);
int lex_is_real(LEX *lex);
double lex_real(LEX *lex);
double lex_eat_real(LEX *lex);
char *lex_eat_until(LEX *lex, char *term);
char *lex_eat_until_whitespace(LEX *lex);
