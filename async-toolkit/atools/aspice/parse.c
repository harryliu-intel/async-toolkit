/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

int noResistors=0; /* global to convert resistors into wires */
int unique_include=0; /* only include files once */

/** define action type strings ***/
char *TYPE_EXCLLO = "excllo";
char *TYPE_EXCLHI = "exclhi";
char *TYPE_EXCLCC = "exclcc";
char *TYPE_NOCC = "nocc";
char *TYPE_RES = "res";
char *TYPE_CAP = "cap";
char *TYPE_N_TRANSISTOR = "n_transistor";
char *TYPE_P_TRANSISTOR = "p_transistor";
char *TYPE_N_DIODE = "n_diode";
char *TYPE_P_DIODE = "p_diode";
char *TYPE_CURRENT_SOURCE = "current_source";
char *TYPE_VOLTAGE_SOURCE = "voltage_source";
char *TYPE_CORNER = "corner";
char *TYPE_BSIM4 = "bsim4";

/** malloc unique strings only **/
LIST *strings=NULL;

/** include files only once **/
LIST *included_files=NULL;

void alloc_super_strings()
  {
  strings = list_create(sizeof(char *));
  }

void free_super_strings()
  {
  free_string_list(strings);
  }

char *super_strdup(char *a)
  {
  int j;
  char *str;
  j = find_element_lazy_sort(strings,&a,&pstrcmp);
  if (j<0) 
    {
    str = leak_strdup(a);
    list_insert_element_lazy_sort(strings,&str,&pstrcmp);
    }
  else str = strings->p.pc[j];
  return str;
  }

MODIFIER *create_modifier()
  {
  MODIFIER *pm;
  pm = leak_malloc(sizeof(MODIFIER));
  pm->type=NULL;
  pm->name=NULL;
  pm->env=0;
  pm->skip=0;
  pm->applied=0;
  return pm;
  }

void do_alnum(LEX *lex)
  {
  char c;
  while(1)
    {
    c=lex_char(lex);
    if (isalnum(c)||c=='_') lex_eat_char(lex);
    else break;
    }
  }

void do_array(LEX *lex)
  {
  while (lex_do_sym(lex,"["))
    {
    do lex_eat_integer(lex); while (lex_do_sym(lex,","));
    lex_eat_sym(lex,"]");
    }
  }

char *parse_nodename(LEX *lex)
  {
  char *str;
  lex_do_whitespace(lex);
  if (lex_is_quote(lex)) {str=leak_strdup(lex_eat_quote(lex)); return str;} /* quotes always ok */
  lex_push_position(lex);
  lex_do_sym(lex,"$"); /* unnamed node */
  do {do_alnum(lex); do_array(lex);} while (lex_do_sym(lex,".")||lex_do_sym(lex,":"));
  if      (lex_do_sym(lex,"#")); /* unnamed internal node */
  str=leak_strdup(lex_get_token(lex));
  lex_pop_position(lex);
  if (str[0]==0) lex_error(lex,"node name");
  return str;
  }

char *super_parse_nodename(LEX *lex)
  {
  char *str;
  lex_do_whitespace(lex);
  if (lex_is_quote(lex)) {str=super_strdup(lex_eat_quote(lex)); return str;} /* quotes always ok */
  lex_push_position(lex);
  lex_do_sym(lex,"$"); /* unnamed node */
  do {do_alnum(lex); do_array(lex);} while (lex_do_sym(lex,".")||lex_do_sym(lex,":"));
  if      (lex_do_sym(lex,"#")); /* unnamed internal node */
  str=super_strdup(lex_get_token(lex));
  lex_pop_position(lex);
  if (str[0]==0) lex_error(lex,"node name");
  return str;
  }

char *parse_cellname(LEX *lex)
  {
  char *str;
  lex_do_whitespace(lex);
  if (lex_is_quote(lex)) {str=super_strdup(lex_eat_quote(lex)); return str;} /* quotes ok */
  lex_push_position(lex);
  do {do_alnum(lex); do_array(lex);} while (lex_do_sym(lex,"."));
  str=super_strdup(lex_get_token(lex));
  lex_pop_position(lex);
  if (str[0]==0) lex_error(lex,"cell name");
  return str;
  }

char *parse_typename(LEX *lex)
  {
  char *str;
  lex_do_whitespace(lex);
  if (lex_is_quote(lex)) {str=super_strdup(lex_eat_quote(lex)); return str;} /* quotes ok */
  lex_push_position(lex);
  do {do_alnum(lex); do_array(lex);}
  while (lex_do_sym(lex,"/")||lex_do_sym(lex,"-")||lex_do_sym(lex,"#")||lex_do_sym(lex,"."));
  str=super_strdup(lex_get_token(lex));
  lex_pop_position(lex);
  if (str[0]==0) lex_error(lex,"type name");
  return str;
  }

LIST *parse_nodes(LEX *lex)
  {
  char *pc;
  LIST *nodes;

  nodes=list_create(sizeof(char *));
  if (lex_is_sym(lex,"("))
    {
    lex_eat_sym(lex,"(");
    while (!lex_is_sym(lex,")"))
      {
      pc=super_parse_nodename(lex);
      list_append_element(nodes,&pc);
      if (lex_is_sym(lex,")")) break;
      lex_eat_sym(lex,",");
      }
    lex_eat_sym(lex,")");
    }
  return nodes;
  }

LIST *parse_parms(LEX *lex)
  {
  FMLA fmla;
  LIST *parms;

  parms=list_create(sizeof(FMLA));
  if (lex_is_sym(lex,"("))
    {
    lex_eat_sym(lex,"(");
    while (!lex_is_sym(lex,")"))
      {
      fmla=create_fmla();
      parse_expression(lex,fmla.ops,fmla.nodes,0);
      list_append_element(parms,&fmla);
      if (lex_is_sym(lex,")")) break;
      lex_eat_sym(lex,",");
      }
    lex_eat_sym(lex,")");
    }
  return parms;
  }

LIST *parse_dummies(LEX *lex)
  {
  LIST *parms;
  char *pc;
  
  parms=list_create(sizeof(char *));
  if (lex_is_sym(lex,"("))
    {
    lex_eat_sym(lex,"(");
    while (!lex_is_sym(lex,")"))
      {
      pc=super_strdup(lex_eat_id(lex));
      list_append_element(parms,&pc);
      if (lex_is_sym(lex,")")) break;
      lex_eat_sym(lex,",");
      }
    lex_eat_sym(lex,")");
    }
  return parms;
  }

/*** actiontype actionname (nodes) (parms); ***/
/*** for WIRE, DEVICE, INSTANCE, EXCLUSION actions ***/
void parse_action(LEX *lex, LIST *actions, int action)
  {
  PARSEACTION act;
  char name[STRMAX]; 

  act.line=lex_line(lex);
  act.action=action;
  act.name=NULL;
  act.type=NULL;
  act.nodes=NULL;
  act.parms=NULL;
  if (action==ACTION_EXCLUSIVE)
    {
    if      (lex_eatif_keyword(lex,TYPE_EXCLLO)) act.type=TYPE_EXCLLO;
    else if (lex_eatif_keyword(lex,TYPE_EXCLHI)) act.type=TYPE_EXCLHI;
    else if (lex_eatif_keyword(lex,TYPE_EXCLCC)) act.type=TYPE_EXCLCC;
    else if (lex_eatif_keyword(lex,TYPE_NOCC))   act.type=TYPE_NOCC;
    else lex_error(lex,"excllo OR exclhi OR exclcc OR nocc");
    }
  else if (action==ACTION_DEVICE) 
    {
    if      (lex_eatif_keyword(lex,TYPE_RES))          act.type=TYPE_RES;
    else if (lex_eatif_keyword(lex,TYPE_CAP))          act.type=TYPE_CAP;
    else if (lex_eatif_keyword(lex,TYPE_N_TRANSISTOR)) act.type=TYPE_N_TRANSISTOR;
    else if (lex_eatif_keyword(lex,TYPE_P_TRANSISTOR)) act.type=TYPE_P_TRANSISTOR;
    else if (lex_eatif_keyword(lex,TYPE_N_DIODE))      act.type=TYPE_N_DIODE;
    else if (lex_eatif_keyword(lex,TYPE_P_DIODE))      act.type=TYPE_P_DIODE;
    else lex_error(lex,"device type");
    if (lex_is_id(lex)||lex_is_quote(lex)) act.name=parse_cellname(lex);
    }
  else if (action==ACTION_INSTANCE)
    {
    act.type=parse_typename(lex);
    act.unnamed=0;
    if (lex_is_id(lex)||lex_is_quote(lex)) act.name=parse_cellname(lex);
    else 
      {
      // unnamed instance, give it a unique number
      safe_sprintf(name,"%d",actions->max);
      act.name=super_strdup(name);
      act.unnamed=1;
      }
    }
  else if (action==ACTION_GROUP)
    {
    lex_eat_id(lex);
    act.name=super_parse_nodename(lex);
    }
  else lex_eat_id(lex);
  act.nodes=parse_nodes(lex);
  if ((action==ACTION_DEVICE) || (action==ACTION_INSTANCE))
    act.parms=parse_parms(lex);
  lex_eatif_sym(lex,";");
  list_append_element(actions,&act);
  }

/*** parse a set of current sources ***/
void parse_source(LEX *lex, LIST *actions, char *type)
  {
  PARSEACTION act;
  char *str;
  act.type=type;
  lex_eat_sym(lex,"{");
  while(!lex_is_sym(lex,"}"))
    {
    act.line=lex_line(lex);
    act.action=ACTION_FMLA;
    act.nodes=list_create(sizeof(char *));
    act.parms=list_create(sizeof(OP));
    str="";
    list_append_element(act.nodes,&str);
    parse_expression(lex,act.parms,act.nodes,0);
    lex_eat_sym(lex,"->");
    act.nodes->p.pc[0]=super_parse_nodename(lex);
    if (lex_eatif_sym(lex,",")) act.name=parse_cellname(lex);
    else act.name=NULL;
    list_append_element(actions,&act);
    }
  lex_eat_sym(lex,"}");
  }

/**
 * Parse flags for PRS/GMA's.  Returns 1 if an after delay was given.
 * Will produce parse errors on conflicting flags.
 **/
int parse_flags(LEX *lex, PARSEACTION *pact)
  {
  int instant=0;
  pact->flags=0;
  if (lex_eatif_keyword(lex,"env"))
    pact->flags |= FLAG_ENV;
  if (lex_eatif_keyword(lex,"instant"))
    {
    instant=1;
    pact->flags |= FLAG_INSTANT;
    }
  else if (lex_eatif_keyword(lex,"timed")) 
    pact->flags |= FLAG_TIMED;
  else if (lex_eatif_keyword(lex,"isochronic"))
    pact->flags |= FLAG_ISOCHRONIC;
  if (lex_eatif_keyword(lex,"unstab"))
    pact->flags |= FLAG_UNSTAB;
  else if (lex_eatif_keyword(lex,"metastab"))
    pact->flags |= FLAG_METASTAB;
  if (!instant && lex_eatif_keyword(lex,"after"))
    return 1;
  else if (!instant && lex_eatif_keyword(lex,"after_ps"))
    {
    pact->flags |= FLAG_REALTIME;
    return 1;
    }
  return 0; // no delay specified
  }

/*** parse a real digital prs in disjunctive normal form ***/
void parse_prs(LEX *lex, LIST *actions)
  {
  PARSEACTION act;
  int s;
  char *name;
  lex_eat_sym(lex,"{");
  while(!lex_eatif_sym(lex,"}"))
    {
    act.line=lex_line(lex);
    act.action=ACTION_PRS;
    act.type=NULL;
    act.name=NULL;
    act.nodes=list_create(sizeof(char *));
    act.parms=list_create(sizeof(int));
    if (parse_flags(lex,&act)) act.delay=lex_eat_integer(lex);
    else act.delay=100;
    do
      {
      s=!lex_eatif_sym(lex,"~");
      list_append_element(act.parms,&s);
      name=super_parse_nodename(lex);
      list_append_element(act.nodes,&name);
      } while (lex_eatif_sym(lex,"&"));
    lex_eat_sym(lex,"->");
    name=super_parse_nodename(lex);
    list_append_element(act.nodes,&name);
    if      (lex_eatif_sym(lex,"+")) s=1;
    else if (lex_eatif_sym(lex,"-")) s=0;
    else lex_error(lex,"+ or -");
    list_append_element(act.parms,&s);
    list_append_element(actions,&act);
    if (act.nodes->max>256) lex_error(lex,"less than 256 literals in prs guard");
    }
  }

/*** parse a guarded-multiple-assignment body ***/
void parse_gma(LEX *lex, LIST *actions)
  {
  PARSEACTION act;
  LIST *expr;
  BIOP op;
  char *str;
  lex_eat_sym(lex,"{");
  while(!lex_eatif_sym(lex,"}"))
    {
    act.line=lex_line(lex);
    act.action=ACTION_GMA;
    act.type=NULL;
    act.name=NULL;
    act.nodes=list_create(sizeof(char *));
    act.parms=list_create(sizeof(LIST *));

    // get guard
    expr = parse_bigint_expression(lex,parse_nodename);
    list_append_element(act.parms,&expr);
    lex_eat_sym(lex,"->");

    // multiple assignments
    do 
      {
      // get delay and flags
      if (parse_flags(lex,&act))
        expr=parse_bigint_expression(lex,parse_nodename);
      else
        {
        expr=list_create(sizeof(BIOP));
        op.type=BIOP_CONST;
        op.value=bigint_from_int(0);
        list_append_element(expr,&op);
        }
      list_append_element(act.parms,&expr);

      // save flags
      expr=list_create(sizeof(BIOP));
      op.type=BIOP_CONST;
      op.value=bigint_from_int(act.flags);
      list_append_element(expr,&op);
      list_append_element(act.parms,&expr);

      // get target node or group name with index
      str = super_parse_nodename(lex);
      list_append_element(act.nodes,&str);
      if (!lex_is_whitespace(lex) && lex_eatif_sym(lex,"("))
        {
        // index expression for group lvalues
        expr=parse_bigint_expression(lex,parse_nodename);
        lex_eatif_sym(lex,")");
        }
      else expr=list_create(sizeof(BIOP)); // empty expression for node lvalues
      list_append_element(act.parms,&expr);

      // get expression
      lex_eat_sym(lex,"=");
      expr = parse_bigint_expression(lex,parse_nodename);
      list_append_element(act.parms,&expr);
      } while (lex_eatif_sym(lex,","));
    list_append_element(actions,&act);
    }
  }

/*** .var=expression; ***/
void parse_assignment(LEX *lex, LIST *actions)
  {
  PARSEACTION act;

  act.line=lex_line(lex);
  act.action=ACTION_ASSIGNMENT;
  act.name=super_strdup(lex_eat_id(lex));
  lex_eat_sym(lex,"=");
  act.nodes=list_create(sizeof(char *));
  act.parms=list_create(sizeof(OP));
  parse_expression(lex,act.parms,act.nodes,0);
  list_append_element(actions,&act);
  }

void free_parse_cell(PARSECELL *pcell)
  {
  PARSEACTION *pact;
  int k,l;
  for (k=0; k<pcell->actions->max; k++)
    {
    pact=&pcell->actions->p.parseaction[k];
    if      (pact->action==ACTION_WIRE)
      {
      list_free(pact->nodes);
      }
    else if (pact->action==ACTION_EXCLUSIVE)
      {
      list_free(pact->nodes);
      }
    else if (pact->action==ACTION_DEVICE)
      {
      list_free(pact->nodes);
      free_fmla_list(pact->parms);
      }
    else if (pact->action==ACTION_INSTANCE)
      {
      list_free(pact->nodes);
      free_fmla_list(pact->parms);
      }
    else if (pact->action==ACTION_PRS)
      {
      list_free(pact->nodes);
      list_free(pact->parms);
      }
    else if (pact->action==ACTION_GMA)
      {
      list_free(pact->nodes);
      for (l=0; l<pact->parms->max; l++)
        {
        LIST *expr=pact->parms->p.list[l];
        free_bigint_expression(expr,1,1,1,1,0,0); // free VAR/VARSTR/USR/USRSTR's
        }
      list_free(pact->parms);
      }
    else if (pact->action==ACTION_GROUP)
      {
      list_free(pact->nodes);
      }
    else if (pact->action==ACTION_FMLA)
      {
      list_free(pact->nodes);
      list_free(pact->parms);
      }
    else if (pact->action==ACTION_ASSIGNMENT) 
      {
      list_free(pact->nodes);
      list_free(pact->parms);
      }
    else if (pact->action==ACTION_MODEL)
      {
      if (pact->parms!=NULL) list_free(pact->parms);
      if (pact->nodes!=NULL) list_free(pact->nodes);
      }
    }
  list_free(pcell->nodes);
  list_free(pcell->parms);
  list_free(pcell->actions);
  leak_free(pcell);
  }

/***
 * Dummy parser to skip the contents of a cell.  Just looks for
 * comments, quotes, and matching braces.
 ***/
void skip_body(LEX *lex)
  {
  int depth=0;
  while (!lex_is_eof(lex))
    {
    lex_do_whitespace(lex);
    if      (lex_is_quote(lex)) lex_eat_quote(lex); 
    else if (lex_eatif_sym(lex,"{")) depth++;
    else if ((depth==0) && lex_is_sym(lex,"}")) break;
    else if (lex_eatif_sym(lex,"}")) depth--;
    else lex_eat_char(lex);
    }
  }

/*** Parse contents of a cell or the top level file. ***/
int parse_body(LEX *lex, PARSECELL *pcell, LIST *cells,
               LIST *globals, LIST *criticalnets,
               LIST *modifiers, int top, int *parsedLines)
  {
  FILE *file;
  LEX *newlex;
  PARSEACTION act;
  char *pc;
  int resistor;
  int instances=0;

  /*** parse body ***/
  while ((!((!top)&&lex_is_sym(lex,"}")))&&(!lex_is_eof(lex)))
    {
    if (*parsedLines==0 || lex_line(lex)>=*parsedLines+NUM_NOTIFY)
      {
      *parsedLines=lex_line(lex);
      printf("Parsing %s:%d...\n",lex_get_filename(lex),*parsedLines);
      }
    resistor= lex_is_keyword(lex,TYPE_RES);
    if ((resistor && (noResistors==0))||
	lex_is_keyword(lex,TYPE_CAP)||
        lex_is_keyword(lex,TYPE_N_TRANSISTOR)||
        lex_is_keyword(lex,TYPE_P_TRANSISTOR)||
        lex_is_keyword(lex,TYPE_N_DIODE)||
        lex_is_keyword(lex,TYPE_P_DIODE))
      parse_action(lex,pcell->actions,ACTION_DEVICE);
    else if (lex_is_keyword(lex,"wire"))
      parse_action(lex,pcell->actions,ACTION_WIRE);
    else if (resistor && (noResistors==1))
      {
      parse_action(lex,pcell->actions,ACTION_WIRE);
      free_fmla_list(parse_parms(lex));
      lex_eatif_sym(lex,";");
      }
    else if (lex_is_keyword(lex,"group"))
      parse_action(lex,pcell->actions,ACTION_GROUP);
    else if (lex_is_keyword(lex,TYPE_EXCLLO)||
             lex_is_keyword(lex,TYPE_EXCLHI)||
             lex_is_keyword(lex,TYPE_EXCLCC)||
             lex_is_keyword(lex,TYPE_NOCC))
      parse_action(lex,pcell->actions,ACTION_EXCLUSIVE);
    else if (lex_eatif_keyword(lex,"dsim") || lex_eatif_keyword(lex,"prs"))
      parse_prs(lex,pcell->actions);
    else if (lex_eatif_keyword(lex,"gma"))
      parse_gma(lex,pcell->actions);
    else if (lex_eatif_keyword(lex,"source") || lex_eatif_keyword(lex,TYPE_CURRENT_SOURCE))
      parse_source(lex,pcell->actions,TYPE_CURRENT_SOURCE);
    else if (lex_eatif_keyword(lex,TYPE_VOLTAGE_SOURCE))
      parse_source(lex,pcell->actions,TYPE_VOLTAGE_SOURCE);
    else if (lex_eatif_keyword(lex,"define"))
      {
      MODIFIER *pm;
      PARSECELL *pnewcell;
      if (!top) lex_error(lex,"define at top level only");
      pnewcell=leak_malloc(sizeof(PARSECELL));
      pnewcell->name=parse_typename(lex);
      pnewcell->actions=list_create(sizeof(PARSEACTION));
      pnewcell->nodes=parse_nodes(lex);
      pnewcell->parms=parse_dummies(lex);
      lex_eat_sym(lex,"{");
      pm = get_modifier(modifiers,pnewcell->name,NULL); // look for type-only modifier
      if (find_element_lazy_sort(cells,&pnewcell,&pcellcmp)>=0)
        {
        // ignore duplicate cell definition
        printf("duplicate %s\n",pnewcell->name);
        skip_body(lex);
        free_parse_cell(pnewcell);
        }
      else if ((pm!=NULL) && pm->skip)
        {
        // skip parsing this cell due to a modify skip statement
        printf("skip %s\n",pnewcell->name);
        skip_body(lex);
        free_parse_cell(pnewcell);
        }
      else
        {
        // parse a new cell definition
        int y;
        list_insert_element_lazy_sort(cells,&pnewcell,&pcellcmp);
        y = parse_body(lex,pnewcell,cells,globals,criticalnets,modifiers,0,parsedLines);
        printf("define %s with %d subcells\n",pnewcell->name,y);
        }
      lex_eat_sym(lex,"}");
      }
    else if (lex_eatif_sym(lex,"."))
      {
      if (lex_eatif_keyword(lex,"include"))
        {
        char *str;
        lex_push_position(lex);
        str=lex_eat_quote(lex);
	pc=find_filename(str,"",".:$ASPICE_PATH");
	if (pc==NULL)
          {
          lex_restore_position(lex);
          lex_file_error(lex,str);
          }
        else lex_pop_position(lex);
        if (!unique_include ||
            (find_element_lazy_sort(included_files,&pc,&pstrcmp)<0))
          {
          int parsedLines=0;
          file=fopen(pc,"r");
          if (file==NULL) lex_file_error(lex,pc);
          bigbuffer(file);
          newlex=lex_file_with_name(file,pc);
          list_insert_element_lazy_sort(included_files,&pc,&pstrcmp);
          parse_body(newlex,pcell,cells,globals,criticalnets,modifiers,top,&parsedLines);
          lex_free(newlex);
          fclose(file);
          }
        }
      else if (lex_eatif_keyword(lex,TYPE_CORNER))
	{
        act.type=TYPE_CORNER;
        act.line=lex_line(lex);
        act.action=ACTION_MODEL;
        act.name=super_strdup(lex_eat_quote(lex));
        act.nodes=NULL;
        act.parms=NULL;
        list_append_element(pcell->actions,&act);
	}
      else if (lex_is_keyword(lex,TYPE_BSIM4))
        {
        if (lex_eatif_keyword(lex,TYPE_BSIM4)) act.type=TYPE_BSIM4;
        else act.type=NULL;
	act.line=lex_line(lex);
	act.action=ACTION_MODEL;
	act.name=super_strdup(lex_eat_quote(lex));
        act.nodes=list_create(sizeof(char *));
        act.parms=list_create(sizeof(double));
        while (lex_is_quote(lex)) /* can specify lib, devicename here */
          {
          char *lib;
          lib=lex_eat_quote(lex);
          lib=super_strdup(lib);
          list_append_element(act.nodes,&lib);
          }
	if (lex_is_real(lex)) /* can specify temperature here */
	  {
	  double x;
	  x=lex_eat_real(lex);
	  list_append_element(act.parms,&x);
	  }
	list_append_element(pcell->actions,&act);
        }
      else if (lex_eatif_keyword(lex,"global"))
        {
        if (!top) lex_error(lex,"global at top level only");
        do
          {
          pc=super_parse_nodename(lex);
          if (find_element_lazy_sort(globals,&pc,&pstrcmp)<0)
            list_insert_element_lazy_sort(globals,&pc,&pstrcmp);
          } while (lex_eatif_sym(lex,","));
        }
      else if (lex_eatif_keyword(lex,"critical"))
        {
        if (!top) lex_error(lex,"critical at top level only");
        do
          {
          pc=super_parse_nodename(lex);
          if (find_element_lazy_sort(criticalnets,&pc,&pstrcmp)<0)
            list_insert_element_lazy_sort(criticalnets,&pc,&pstrcmp);
          } while (lex_eatif_sym(lex,","));
        }
      else if (lex_eatif_keyword(lex,"unique_include"))
        {
        if (!top) lex_error(lex,"unique_include at top level only");
        lex_eat_sym(lex,"=");
        unique_include=lex_eat_integer(lex);
        }
      else if (lex_eatif_keyword(lex,"modify"))
        {
        int x;
        MODIFIER *pm;
        if (!top) lex_error(lex,"modify at top level only");
        pm = create_modifier();
        if      (lex_eatif_keyword(lex,"env"))  pm->env  = 1;
        else if (lex_eatif_keyword(lex,"skip")) pm->skip = 1;
        else lex_error(lex,"env|skip");
        pm->type = parse_typename(lex);
        if (lex_is_id(lex)||lex_is_quote(lex)) pm->name = parse_cellname(lex);
        x = find_element_lazy_sort(modifiers,&pm,&pmodifiercmp);
        if (x<0) list_insert_element_lazy_sort(modifiers,&pm,&pmodifiercmp);
        else
          {
          modifiers->p.modifier[x]->env  |= pm->env;
          modifiers->p.modifier[x]->skip |= pm->skip;
          leak_free(pm);
          }
        }
      else if (lex_is_id(lex))
        parse_assignment(lex,pcell->actions);
      else lex_error(lex,".option [setting];");
      lex_eatif_sym(lex,";");
      }
    else if (lex_is_sym(lex,"{"))
      {
      lex_eat_sym(lex,"{");
      act.line=lex_line(lex);
      act.action=ACTION_ADDLEVEL;
      list_append_element(pcell->actions,&act);
      parse_body(lex,pcell,cells,globals,criticalnets,modifiers,0,parsedLines);
      act.line=lex_line(lex);
      act.action=ACTION_REMOVELEVEL;
      list_append_element(pcell->actions,&act);
      lex_eat_sym(lex,"}");
      }
    else
      {
      parse_action(lex,pcell->actions,ACTION_INSTANCE);
      instances++;
      }
    }
  return instances;
  }

void parse_circuit(char *basename, LIST *cells,
                   LIST *globals, LIST *criticalnets, LIST *modifiers)
  {
  FILE *fasp;
  char filename[STRMAX];
  LEX *lex;
  PARSECELL *pcell;
  int parsedLines=0;
  safe_sprintf(filename,"%s.asp",basename);
  fasp=fopen(filename,"r");
  if (fasp==NULL) {fprintf(stderr,"ERROR: Can't open %s.asp.\n",basename); exit(1);}
  bigbuffer(fasp);
  alloc_super_strings();
  included_files=list_create(sizeof (char *));
  lex=lex_file_with_name(fasp,filename);
  pcell=leak_malloc(sizeof (PARSECELL));
  pcell->name=super_strdup("");
  pcell->actions=list_create(sizeof(PARSEACTION));
  pcell->nodes=list_create(sizeof(char *));
  pcell->parms=list_create(sizeof(char *));
  list_insert_element_lazy_sort(cells,&pcell,&pcellcmp);
  parse_body(lex,pcell,cells,globals,criticalnets,modifiers,1,&parsedLines);
  free_string_list(included_files);
  lex_free(lex);
  fclose(fasp);

  // sort for quicker access
  list_finish_lazy_sort(cells,&pcellcmp);
  list_finish_lazy_sort(globals,&pstrcmp);
  list_finish_lazy_sort(criticalnets,&pstrcmp);
  list_finish_lazy_sort(modifiers,&pmodifiercmp);
  }

void free_string_list(LIST *l)
  {
  int j;
  for (j=0; j<l->max; j++) leak_free(l->p.pc[j]);
  list_free(l);
  }

void free_fmla_list(LIST *l)
  {
  int j;
  for (j=0; j<l->max; j++) free_fmla(l->p.fmla[j]);
  list_free(l);
  }

void free_parse_memory(LIST *cells, LIST *globals, LIST *criticalnets, LIST *modifiers)
  {
  int j;
  for (j=0; j<cells->max; j++)
    free_parse_cell(cells->p.parsecell[j]);
  for (j=0; j<modifiers->max; j++)
    leak_free(modifiers->p.modifier[j]);
  list_free(modifiers);
  list_free(globals);
  list_free(criticalnets);
  list_free(cells);
  free_super_strings();
  }
