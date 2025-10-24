/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#define NEWLEX
#include "newlex.h"
#include "list.h"
#include "misc.h"
#include "leak.h"

/*** start lexing the given file or string ***/

LEX *lex_create()
  {
  LEX *lex;
  lex=(LEX *) leak_malloc(sizeof(LEX));
  lex->file=NULL;
  lex->string=NULL;
  lex->position.position=0;
  lex->position.line=1;
  lex->position.column=1;
  lex->min=0;
  lex->max=0;
  lex->position_stack=list_create(sizeof(POSITION));
  lex->position_stack->shrink=0;
  lex->buffer=list_create(sizeof(char));
  lex->buffer->shrink=0; /*** only grows bigger! ***/
  lex->token=leak_strdup("");
  lex->begin_comment=leak_strdup("/*");
  lex->end_comment=leak_strdup("*/");
  lex->nested_comments=1;
  lex->whitespace=leak_strdup(" \n\t");
  lex->filename=NULL;
  return lex;
  }

LEX *lex_file(FILE *file)
  {
  LEX *lex;
  lex=lex_create();
  lex->file=file;
  return lex;
  }

LEX *lex_file_with_name(FILE *file, char *filename)
  {
  LEX *lex;
  lex=lex_create();
  lex->file=file;
  lex->filename=leak_strdup(filename);
  return lex;
  }

LEX *lex_string(char *string)
  {
  LEX *lex;
  lex=lex_create();
  lex->string=string;
  return lex;
  }

/*** stop lexing and free lex structure ***/

void lex_free(LEX *lex)
  {
  list_free(lex->position_stack);
  list_free(lex->buffer);
  leak_free(lex->token);
  leak_free(lex->begin_comment);
  leak_free(lex->end_comment);
  leak_free(lex->whitespace);
  if (lex->filename!=NULL) leak_free(lex->filename);
  leak_free(lex);
  }

/*** returns/sets lex debugging info ***/

int lex_line(LEX *lex)
  {
  return lex->position.line;
  }

int lex_column(LEX *lex)
  {
  return lex->position.column;
  }

char *lex_get_filename(LEX *lex) 
  {
  return lex->filename;
  }

void lex_set_line(LEX *lex, int line)
  {
  lex->position.line=line;
  }

void lex_set_filename(LEX *lex, char *filename)
  {
  if (lex->filename!=NULL) leak_free(lex->filename);
  lex->filename=leak_strdup(filename);
  }

/*** save current position on a stack ***/

void lex_push_position(LEX *lex)
  {
  list_append_element(lex->position_stack,&lex->position);
  }

/*** pop position off stack and move to it ***/

void lex_restore_position(LEX *lex)
  {
  assert(lex->position_stack->max>0);
  lex->position=lex->position_stack->p.pos[lex->position_stack->max-1];
  list_remove_element(lex->position_stack,lex->position_stack->max-1);
  }

/*** pop position off stack and discard it ***/

void lex_pop_position(LEX *lex)
  {
  int cut;
  assert(lex->position_stack->max>0);
  if (lex->position_stack->max==1)
    {
    cut=lex->position_stack->p.pos[0].position-lex->min;
    list_remove_list(lex->buffer,0,cut);
    lex->min+=cut;
    }
  list_remove_element(lex->position_stack,lex->position_stack->max-1);
  }

/*
 * copies lex->buffer from the tos position to the current position to
 * lex->token
 */

char *lex_get_token(LEX *lex)
  {
  int length,start;
  leak_free(lex->token);
  if (lex->position_stack->max==0) lex->token=leak_strdup("");
  else
    {
    start=lex->position_stack->p.pos[lex->position_stack->max-1].position-lex->min;
    length=lex->position.position - lex->position_stack->p.pos[lex->position_stack->max-1].position;
    lex->token=(char *) leak_malloc(length+1);
    strncpy(lex->token,lex->buffer->p.c+start,length);
    lex->token[length]=0;
    }
  return lex->token;
  }

/*** return current character ***/

char lex_char(LEX *lex)
  {
  char c;
  assert(lex->position.position-lex->min>=0);
  if (lex->position.position-lex->max>=0)
    {
    if      (lex->file!=NULL) c=fgetc(lex->file);
    else if (lex->string!=NULL) {c=*lex->string; if (c==0) c=EOF; else (lex->string)++;}
    else c=EOF;
    list_append_element(lex->buffer,&c);
    if (lex->buffer->max>STRMAX) lex_error(lex,"smaller parse window");
    lex->max++;
    }
  c=lex->buffer->p.c[lex->position.position-lex->min];
  return c;
  }

/*** return current character and advance position ***/

char lex_eat_char(LEX *lex)
  {
  char c;
  c=lex_char(lex);
  if (c==EOF) return c;
  lex->position.position++;
  lex->position.column++;
  if (c=='\n') {lex->position.line++; lex->position.column=1;}
  if (lex->position_stack->max==0)
    {
    list_remove_element(lex->buffer,0);
    lex->min++;
    }
  return c;
  }


/*** Print err_tp followed by file, line, column, pending token ***/

void lex_error_pos(LEX *lex, const char *err_tp)
  {
  int i, len = strlen(err_tp) + 2;
  fprintf(stderr, "%s: ", err_tp);
  if (lex->filename!=NULL)
    {
    fprintf(stderr,"file=%s,\n",lex->filename);
    for (i = 0; i < len; i++) putc(' ', stderr);
    }
  fprintf(stderr,"line=%d, column=%d",lex->position.line,lex->position.column);
  if (lex->token[0]!=0) fprintf(stderr,", token=%s",lex->token);
  fprintf(stderr,"\n");
  for (i = 0; i < len; i++) putc(' ', stderr);
  }

/*
 * Print error position, next 10 characters on the line, then an
 * arbitraty message.
 */

void lex_message(LEX *lex, char *err_tp, char *msg)
  {
  int i;
  leak_free(lex->token);
  lex->token=(char *) leak_malloc(11);
  lex_push_position(lex);
  for (i=0; i<11; i++)
    {
    lex->token[i]=lex_eat_char(lex);
    if ((i==10)||(lex->token[i]==EOF)||(lex->token[i]=='\n'))
      {lex->token[i]=0; break;}
    }
  lex_restore_position(lex);
  lex_error_pos(lex,err_tp);
  fprintf(stderr,msg);
  }

/*** Print error position then "expecting ..." then exit ***/

void lex_error(LEX *lex, char *expecting)
  {
  char msg[STRMAX];
  safe_sprintf(msg,"expecting %s\n",expecting);
  lex_message(lex,"ERROR",msg);
  exit(1);
  }

/*** Print warning position then "expecting ..." ***/

void lex_warning(LEX *lex, char *expecting)
  {
  char msg[STRMAX];
  safe_sprintf(msg,"expecting %s\n",expecting);
  lex_message(lex,"WARNING",msg);
  }

/*** Print error position then "file ... not found" then exit ***/

void lex_file_error(LEX *lex, char *file)
  {
  char msg[STRMAX];
  safe_sprintf(msg,"file %s not found\n",file);
  lex_message(lex,"ERROR",msg);
  exit(1);
  }
  
/*** Print error position then "file ... not found" ***/

void lex_file_warning(LEX *lex, char *file)
  {
  char msg[STRMAX];
  safe_sprintf(msg,"file %s not found\n",file);
  lex_message(lex,"WARNING",msg);
  }
  
/*** define the characters in whitespace ***/

void lex_define_whitespace(LEX *lex, char *whitespace)
  {
  leak_free(lex->whitespace);
  lex->whitespace=leak_strdup(whitespace);
  }

/*** define the types of comments to use ***/

void lex_define_comments(LEX *lex, char *begin, char *end, int nested)
  {
  leak_free(lex->begin_comment);
  leak_free(lex->end_comment);
  lex->begin_comment=leak_strdup(begin);
  lex->end_comment=leak_strdup(end);
  lex->nested_comments=nested;
  }

/*** internal multicharacter parsing functions ***/
/*** if sucessful, advance the position and return 1, else return 0 ***/

void lex_finish_comment(LEX *lex)
  {
  while (1)
    {
    if (lex_char(lex)==EOF) break;
    else if (lex_do_sym(lex,lex->end_comment)) break;
    else if (lex->nested_comments&&lex_do_sym(lex,lex->begin_comment))
      lex_finish_comment(lex);
    else lex_eat_char(lex);
    }
  }

int lex_do_whitespace(LEX *lex)
  {
  int i,v=0;
  char c;
  while (1)
    {
    c=lex_char(lex);
    if (c==EOF) break;
    for (i=0; i<strlen(lex->whitespace); i++) if (lex->whitespace[i]==c) break;
    if (i<strlen(lex->whitespace)) {lex_eat_char(lex); v=1;}
    else if ((strlen(lex->begin_comment)>0)&&lex_do_sym(lex,lex->begin_comment))
      {lex_finish_comment(lex); v=1;}
    else break;
    }
  return v;
  }

int lex_do_id(LEX *lex)
  {
  int first=1;
  char c;
  while (1)
    {
    c=lex_char(lex);
    if (isalpha(c)||(c=='_')||(isdigit(c)&&!first)) lex_eat_char(lex);
    else break;
    first=0;
    }
  return (!first);
  }

int lex_do_sym(LEX *lex, char *symbol)
  {
  int i;
  lex_push_position(lex);
  for (i=0; i<strlen(symbol); i++) if (lex_eat_char(lex)!=symbol[i]) break;
  if (i<strlen(symbol)) {lex_restore_position(lex); return 0;}
  else                  {lex_pop_position(lex);     return 1;}
  }

int lex_do_quote(LEX *lex)
  {
  int v=0;
  char c;
  lex_push_position(lex);
  if (lex_eat_char(lex)=='"') while(1)
    {
    c=lex_char(lex);
    if (c==EOF) break;
    else if (c=='"') {lex_eat_char(lex); v=1; break;}
    else lex_eat_char(lex);
    }
  if (v) {lex_pop_position(lex); return 1;}
  else   {lex_restore_position(lex); return 0;}
  }

int lex_do_integer(LEX *lex)
  {
  char c;
  lex_push_position(lex);
  if ((lex_char(lex)=='-')||(lex_char(lex)=='+')) lex_eat_char(lex);
  c = lex_char(lex);
  if (isdigit(c))
    {
    lex_eat_char(lex);
    if (c == '0' && (lex_char(lex) == 'x' || lex_char(lex) == 'X'))
      {
      lex_eat_char(lex);
      while ((c=lex_char(lex))=='_' || isalnum(c)) lex_eat_char(lex);
      }
    else
      {
      while ((c=lex_char(lex))=='_' || isdigit(c)) lex_eat_char(lex);
#ifdef ADA_NUMBERS
      /* Ada (and VHDL) convention for arbitrary base numbers */
      if (lex_char(lex) == '#')
	{
	lex_eat_char(lex);
	while ((c=lex_char(lex))=='_' || isalnum(c)) lex_eat_char(lex);
	if (c != '#')
	  {lex_restore_position(lex); return 0;}
	lex_eat_char(lex);
	}
#endif
      }
    }
  else {lex_restore_position(lex); return 0;}
  lex_pop_position(lex); return 1;
  }

/*** Parse real numbers or standard decimal integer ***/

int lex_do_real(LEX *lex)
  {
  char c;
  lex_push_position(lex);
  if ((lex_char(lex)=='+')||(lex_char(lex)=='-')) lex_eat_char(lex);
  c=lex_char(lex);
  if (isdigit(c))
    {
    lex_eat_char(lex);
    if (c == '0' && (lex_char(lex) == 'x' || lex_char(lex) == 'X'))
      { lex_restore_position(lex); return 0; }
    }
  else {lex_restore_position(lex); return 0;}
  while (isdigit(lex_char(lex))) lex_eat_char(lex);
#ifdef ADA_NUMBERS
  if (lex_char(lex)=='#' || lex_char(lex)=='_')
    {lex_restore_position(lex); return 0;}
#else
  if (lex_char(lex)=='_')
    {lex_restore_position(lex); return 0;}
#endif
#if 0
  /* do not recognize pure integers as reals: */
  if (lex_char(lex)!='.' && lex_char(lex)!='e' && lex_char(lex)!='E')
    {lex_restore_position(lex); return 0;}
#endif
  if (lex_char(lex)=='.')
    {
    lex_eat_char(lex);
    if (isdigit(lex_char(lex))) lex_eat_char(lex);
    else {lex_restore_position(lex); return 0;}
    while (isdigit(lex_char(lex))) lex_eat_char(lex);
    }
  if ((lex_char(lex)=='e')||(lex_char(lex)=='E'))
    {
    lex_eat_char(lex);
    if ((lex_char(lex)=='+')||(lex_char(lex)=='-')) lex_eat_char(lex);
    if (isdigit(lex_char(lex))) lex_eat_char(lex);
    else {lex_restore_position(lex); return 0;}
    while (isdigit(lex_char(lex))) lex_eat_char(lex);
    }
  lex_pop_position(lex);
  return 1;
  }

/*** user interface routines ***/

int lex_is_eof(LEX *lex)
  {
  lex_do_whitespace(lex);
  return (lex_char(lex)==EOF);
  } 

int lex_is_whitespace(LEX *lex)
  {
  int v;
  lex_push_position(lex);
  v=lex_do_whitespace(lex);
  lex_restore_position(lex);
  return v;
  }

char *lex_whitespace(LEX *lex)
  {
  lex_push_position(lex);
  if (!lex_do_whitespace(lex)) lex_error(lex,"whitespace");
  lex_get_token(lex);
  lex_restore_position(lex);
  return lex->token;
  }

char *lex_eat_whitespace(LEX *lex)
  {
  lex_push_position(lex);
  if (!lex_do_whitespace(lex)) lex_error(lex,"whitespace");
  lex_get_token(lex);
  lex_pop_position(lex);
  return lex->token;
  }

int lex_is_id(LEX *lex)
  {
  int v;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  v=lex_do_id(lex);
  lex_restore_position(lex);
  return v;
  }

char *lex_id(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_id(lex)) lex_error(lex,"identifier");
  lex_get_token(lex);
  lex_restore_position(lex);
  return lex->token;
  }

char *lex_eat_id(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_id(lex)) lex_error(lex,"identifier");
  lex_get_token(lex);
  lex_pop_position(lex);
  return lex->token;
  }

char *lex_eatif_id(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  lex_do_id(lex);
  lex_get_token(lex);
  lex_pop_position(lex);
  return lex->token;
  }

int lex_is_sym(LEX *lex, char *sym)
  {
  int v;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  v=lex_do_sym(lex,sym);
  lex_restore_position(lex);
  return v;
  }

void lex_eat_sym(LEX *lex, char *sym)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_sym(lex,sym)) lex_error(lex,sym);
  lex_pop_position(lex);
  }

int lex_eatif_sym(LEX *lex, char *sym)
  {
  lex_do_whitespace(lex);
  return lex_do_sym(lex,sym);
  } 

int lex_is_keyword(LEX *lex, char *keyword)
  {
  int v;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  v=lex_do_id(lex);
  lex_get_token(lex);
  lex_restore_position(lex);
  return (v&&(strcmp(lex->token,keyword)==0));
  }

int lex_do_keyword(LEX *lex, char *keyword)
  {
  int v;
  lex_push_position(lex);
  v=lex_do_id(lex);
  lex_get_token(lex);
  v=v&&(strcmp(lex->token,keyword)==0);
  if (v) lex_pop_position(lex);
  else lex_restore_position(lex);
  return v;
  }

void lex_eat_keyword(LEX *lex, char *keyword)
  {
  lex_do_whitespace(lex);
  if (!lex_do_keyword(lex,keyword)) lex_error(lex,"keyword");
  }

int lex_eatif_keyword(LEX *lex, char *keyword)
  {
  lex_do_whitespace(lex);
  return lex_do_keyword(lex,keyword);
  }

int lex_is_quote(LEX *lex)
  {
  int v;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  v=lex_do_quote(lex);
  lex_restore_position(lex);
  return v;
  }

char *lex_quote(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_quote(lex)) lex_error(lex,"quote");
  lex_get_token(lex);
  lex_restore_position(lex);
  lex->token[strlen(lex->token)-1]=0;
  return lex->token+1;
  }

char *lex_eat_quote(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_quote(lex)) lex_error(lex,"quote");
  lex_get_token(lex);
  lex->token[strlen(lex->token)-1]=0;
  lex_pop_position(lex);
  return lex->token+1;
  }

int lex_is_integer(LEX *lex)
  {
  int v;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  v=lex_do_integer(lex);
  lex_restore_position(lex);
  return v;
  }

static int based_int_value(const char *c, const char **end_ptr, int base)
  {
  int v=0, d;
  do {
     if (*c == '_') { c++; continue; }
     d = isdigit(*c) ? *c-'0' : 10 + (islower(*c) ? *c-'a' : *c-'A');
     if (d < 0 || base <= d) break;
     v = v * base + d;
     c++;
  } while (*c);
  if (end_ptr) *end_ptr = c;
  return v;
  }

/*
 * Pre: assumes lex->token has correct syntax for a number.
 * Syntax: optional sign followed by decimal, C-style hex (0x..., 0X...),
 * or Ada style integer (base#...#). Underscores are allowed, but number
 * must start with a digit.
 */

static int int_value(LEX *lex)
  {
  int neg=0, v=0;
  const char *c = lex->token;
  if (*c == '-') { neg=1; c++; }
  else if (*c == '+') c++;
  if (*c == '0' && (*(c+1) == 'x' || *(c+1) == 'X'))
    {
    v = based_int_value(c+2, &c, 16);
    if (*c) lex_error(lex, "hex number (found illegal digit)");
    }
  else
    {
      v = based_int_value(c, &c, 10);
#ifdef ADA_NUMBERS
      if (*c == '#')
	{
	if (v < 2 || 36 < v) lex_error(lex, "base 2..36");
	v = based_int_value(c+1, &c, v);
	if (*c != '#') lex_error(lex, "based number (found illegal digit)");
	}
#endif
    }
    if (neg) v = -v;
    return v;
  }

int lex_integer(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_integer(lex)) lex_error(lex,"integer");
  lex_get_token(lex);
  lex_restore_position(lex);
  return int_value(lex);
  }

int lex_eat_integer(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_integer(lex)) lex_error(lex,"integer");
  lex_get_token(lex);
  lex_pop_position(lex);
  return int_value(lex);
  }

int lex_is_real(LEX *lex)
  {
  int v;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  v=lex_do_real(lex);
  lex_restore_position(lex);
  return v;
  }

double lex_real(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_real(lex)) lex_error(lex,"real");
  lex_get_token(lex);
  lex_restore_position(lex);
  return atof(lex->token);
  }

double lex_eat_real(LEX *lex)
  {
  lex_do_whitespace(lex);
  lex_push_position(lex);
  if (!lex_do_real(lex)) lex_error(lex,"real");
  lex_get_token(lex);
  lex_pop_position(lex);
  return atof(lex->token);
  }

char *lex_eat_until(LEX *lex, char *term)
  {
  char c,*p;
  int i,l;

  l=strlen(term);
  lex_push_position(lex);
  while (1)
    {
    c=lex_char(lex);
    for (i=0; i<l; i++) if (c==term[i]) break;
    if ((i<l)||(c==EOF)) break;
    lex_eat_char(lex);
    }
  p=lex_get_token(lex);
  lex_pop_position(lex);
  return p;
  }

char *lex_eat_until_whitespace(LEX *lex) {
  lex_do_whitespace(lex);
  return lex_eat_until(lex,lex->whitespace);
}
