/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** global MODEL variables ***/
LIST *MODEL_Decks=NULL;  /* all unique filename/lib/temperature combinations */
LIST *MODEL_Models=NULL; /* current binned model set used by MODEL_FindModel */
char *MODEL_Corner=NULL; /* current process corner used by MODEL_SetModel */
int MODEL_RandomCorner=0; /* choose randomly between FF and SS */

/*** typical whitespace ***/
char whitespace[5];

/*** create a MODEL, set all parameters not given ***/
MODEL *MODEL_CreateModel(int bsim, int type, double temp)
  {
  int i;
  MODEL *m;

  /*** set BSIM4 standard fields ***/
  if (bsim==4)
    {
    m=(MODEL *) leak_malloc(sizeof(BSIM4model));
    for (i=0; i<sizeof(BSIM4model); i++) ((char *)m)[i]=0;
    }
  else return NULL;
  m->bsim=bsim;
  m->type=type;
  m->Wmin=0;
  m->Wmax=1;
  m->Lmin=0;
  m->Lmax=1;
  m->temp=temp + CONSTCtoK;
  m->deck=NULL; // fill in later
  return m;
  }

/*** parse and evaluate an expression for MODEL parameter ***/
double MODEL_ParseExpression(LEX *lex, LIST *assignments)
  {
  char s[1];
  double value=0;
  FMLA fmla;
  s[0]=0;
  if (lex_is_real(lex)) // a real number
    {
    value = lex_eat_real(lex);
    if (lex_is_whitespace(lex)); // avoids nasty bug
    else if (lex_eatif_sym(lex,"m")) value*=1e-3;
    else if (lex_eatif_sym(lex,"u")) value*=1e-6;
    else if (lex_eatif_sym(lex,"n")) value*=1e-9;
    else if (lex_eatif_sym(lex,"p")) value*=1e-12;
    }
  else if (lex_is_sym(lex,"'")) { // a quoted expression
    lex_eat_sym(lex,"'");
    lex_define_comments(lex,"/*","*/",0);
    fmla=create_fmla();
    parse_expression(lex,fmla.ops,fmla.nodes,0);
    value=evaluate_by_name(assignments,fmla.ops,fmla.nodes);
    lex_eat_sym(lex,"'");
    free_fmla(fmla);
    lex_define_comments(lex,"*","\n",0);
  }
  else if (lex_is_id(lex)) { // an unquoted expression
    lex_define_whitespace(lex,s); // no whitespace allowed
    lex_define_comments(lex,"/*","*/",0);
    fmla=create_fmla();
    parse_expression(lex,fmla.ops,fmla.nodes,0);
    value=evaluate_by_name(assignments,fmla.ops,fmla.nodes);
    free_fmla(fmla);
    lex_define_whitespace(lex,whitespace);
    lex_define_comments(lex,"*","\n",0);
  }
  else {
    lex_error(lex,"expression");
  }
  return value;
}

/*** parse parameter assignments ***/
void MODEL_ParseParams(LEX *lex, LIST *assignments)
  {
  char *name;
  double value;
  while (!lex_is_sym(lex,"."))
    {
    lex_eatif_sym(lex,"+");
    name=leak_strdup(lex_eat_id(lex));
    lex_eat_sym(lex,"=");
    value=MODEL_ParseExpression(lex,assignments);
    assign(assignments,name,value);
    leak_free(name);
    }
  }

/*** recursive part of MODEL_ParseModels (NOTE: uses external functions!) ***/
void MODEL_ParseModelsRecursive(int bsim, LIST *assignments, LIST *unknown,
                                char *filename, char *lib, char *dev,
                                double Temp)
  {
  LEX *lex;
  MODEL *m;
  char s[STRMAX],dn[STRMAX],*pc;
  int inside_lib=0,found_lib=0;
  char *newfilename,*newlib;
  FILE *file=NULL;

  /*** open file, start parser ***/
  pc=find_filename(filename,"",".:$ASPICE_PATH");
  if (pc!=NULL) file=fopen(pc,"r");
  if (file==NULL) {fprintf(stderr,"ERROR: can't load model %s\n",filename); exit(1);}
  lex=lex_file_with_name(file,filename);
  leak_free(pc);
  if (lex==NULL) error("Couldnt read model file.\n");
  lex_define_comments(lex,"*","\n",0);
  whitespace[0]=' ';
  whitespace[1]='\n';
  whitespace[2]='\t';
  whitespace[3]=0xD; // wtf is this character, and why is it in my models?
  whitespace[4]=0;
  lex_define_whitespace(lex,whitespace);

  /*** main parsing loop ***/
  while (!lex_is_eof(lex))
    {
    if (lex_eatif_sym(lex,".PARAM")||lex_eatif_sym(lex,".param"))
      MODEL_ParseParams(lex,assignments);
    else if (lex_eatif_sym(lex,".SUBCKT")||lex_eatif_sym(lex,".subckt")) // ignore SUBCKT's
      {
      while (!lex_is_eof(lex))
        {
        lex_do_whitespace(lex);
        if (lex_eatif_sym(lex,".ENDS")||lex_eatif_sym(lex,".ends")) break;
        lex_eat_until(lex,"\n");
        }
      lex_eat_until(lex,"\n");
      }
    else if (lex_eatif_sym(lex,".LIB")||lex_eatif_sym(lex,".lib"))
      {
      if (inside_lib)
        {
        /*** inside a lib block, .lib includes another file ***/
        lex_eat_sym(lex,"'");
        newfilename=lex_eat_until(lex,"'");
        newfilename=leak_strdup(newfilename);
        lex_eat_sym(lex,"'");
        newlib=lex_eat_until_whitespace(lex);
        newlib=leak_strdup(newlib);
        MODEL_ParseModelsRecursive(bsim,assignments,unknown,newfilename,newlib,
                                   dev,Temp);
        leak_free(newfilename);
        leak_free(newlib);
        }
      else
        {
        newlib=lex_eat_until_whitespace(lex);
        if (strcmp(newlib,lib)==0)
          {
          /*** matches desired lib, continue processing ***/
          inside_lib=1;
          found_lib=1;
          }
        else
          {
          /*** doesn't match desired lib, so skip to .endl statement ***/
          while (!lex_is_eof(lex))
            {
            lex_do_whitespace(lex);
            if (lex_is_sym(lex,".ENDL")||lex_is_sym(lex,".endl")) break;
            lex_eat_until(lex,"\n");
            }
          }
        }
      }
    else if (lex_eatif_sym(lex,".ENDL")||lex_eatif_sym(lex,".endl"))
      {
      newlib=lex_eat_until_whitespace(lex);
      inside_lib=0;
      }
    else if (lex_eatif_sym(lex,".MODEL")||lex_eatif_sym(lex,".model"))
      {
      pc=lex_eat_until_whitespace(lex); /* model device name */
      strcpy(dn,pc);
      if      (lex_eatif_keyword(lex,"NMOS")||lex_eatif_keyword(lex,"nmos"))
        m=MODEL_CreateModel(bsim,NMOS,Temp);
      else if (lex_eatif_keyword(lex,"PMOS")||lex_eatif_keyword(lex,"pmos"))
        m=MODEL_CreateModel(bsim,PMOS,Temp);
      else lex_error(lex,"NMOS OR PMOS");
      lex_eatif_sym(lex,"(");

      /*** parse parameters of model ***/
      while (!lex_eatif_sym(lex,")") && !lex_is_sym(lex,".") && !lex_is_eof(lex))
        {
        if (lex_eatif_sym(lex,"+")) continue; /* line continuation */
        pc = lex_eat_id(lex);
        strcpy(s,pc);
        lex_eat_sym(lex,"=");
        if (bsim==4)
          {
          if (BSIM4_ParseParameter((BSIM4model *) m,s,lex,assignments)) continue;
          }

        /*** add unknown parameters to warning list ***/
        pc=leak_strdup(s);
        lex_eat_until_whitespace(lex);
        if (find_element_lazy_sort(unknown,&pc,&pstrcmp)<0)
          list_insert_element_lazy_sort(unknown,&pc,&pstrcmp);
        else leak_free(pc);
        }

      /*** discard model if device names doesn't start with proper prefix ***/
      if ((dev[0]!=0) && (strncasecmp(dn,dev,strlen(dev))!=0)) {
        leak_free(m);
        continue;
      }

      /*** prepare model ***/
      if (bsim==4)
        {
        BSIM4model *m4 = (BSIM4model *) m;
        // TODO: just get rid of the BSIM4 prefix, and it will just work
        if (m4->BSIM4WminGiven) m4->Wmin=m4->BSIM4Wmin;
        if (m4->BSIM4WmaxGiven) m4->Wmax=m4->BSIM4Wmax;
        if (m4->BSIM4LminGiven) m4->Lmin=m4->BSIM4Lmin;
        if (m4->BSIM4LmaxGiven) m4->Lmax=m4->BSIM4Lmax;
        BSIM4_SetupModel(m4);
        BSIM4_TempModel(m4);
        BSIM4_CheckModel(m4,stderr);
        // BSIM4_PrintModel(m4,stdout);
        }

      /*** add to current list of models ***/
      list_append_element(MODEL_Models,&m);
      }
    else lex_error(lex,"expecting .lib|.endl|.model|.param|.subckt");
    }
  lex_free(lex);
  fclose(file);

  /*** report missing lib ***/
  if ((found_lib==0)&&(lib[0]!=0))
    {
    fprintf(stderr,"ERROR: can't find lib %s in model %s\n",lib,filename);
    exit(1);
    }
  }

/*** parse a set of MODEL models ***/
void MODEL_ParseModels(int bsim, LIST *assignments, char *filename, char *lib, char *dev,
                       double Temp)
  {
  LIST *unknown,*l;
  int i;

  /*** debugging ***/
  printf("Loading BSIM%d file=\"%s\" lib=\"%s\" dev=\"%s\" temp=%gC\n",
         bsim,filename,lib,dev,Temp);
  l = assignments->p.list[assignments->max-1];
#ifdef VERBOSE
  for (i=0; i<l->max; i++)
    printf("  %s=%g\n", l->p.assignment[i].name,l->p.assignment[i].value);
#endif

  /*** create lists and assignment scope ***/
  MODEL_Models=list_create(sizeof(MODEL *));
  unknown=list_create(sizeof(char *));
  add_assignment_level(assignments); // preserve assignment scope from aspice

  /*** start parsing model files ***/
  MODEL_ParseModelsRecursive(bsim,assignments,unknown,filename,lib,dev,Temp);

  /*** print warnings about unknown parameters ***/
  if (unknown->max>0)
    {
    list_finish_lazy_sort(unknown,&pstrcmp);
    fprintf(stderr,"WARNING: unsupported model parameters:\n");
    for (i=0; i<unknown->max; i++)
      {
      fprintf(stderr," %s",unknown->p.pc[i]);
      leak_free(unknown->p.pc[i]);
      }
    fprintf(stderr,"\n");
    }  
  list_free(unknown);
  remove_assignment_level(assignments);
  }

/*** compare decks by filename, lib, dev, and temperature ***/
int deck_cmp(void *p1, void *p2)
  {
  int c;
  DECK *pd1=((DECK *)p1),*pd2=((DECK *)p2);
  c=strcmp(pd1->filename,pd2->filename);
  if (c!=0) return c;
  c=strcmp(pd1->lib,pd2->lib);
  if (c!=0) return c;
  c=strcmp(pd1->dev,pd2->dev);
  if (c!=0) return c;
  if (pd1->temperature<pd2->temperature) return -1;
  if (pd1->temperature>pd2->temperature) return +1;
  // compare parameters of outermost assignment level only
  return assignment_list_cmp(pd1->parameters,pd2->parameters);
  }

/*** set current corner model, "" for normal ***/
void MODEL_SetCorner(char *corner)
  {
  int x;
  if (MODEL_Corner!=NULL) leak_free(MODEL_Corner);
  MODEL_Corner=leak_strdup(corner);
  // handle random corner
  if (strncasecmp(MODEL_Corner,"RANDOM",6)==0 && MODEL_RandomCorner==0)
    {
    MODEL_RandomCorner=1;
    if (sscanf(MODEL_Corner+6,"%u",&x)==1) srand(x);
    }
  }
 
/*** set current MODEL_Models, parse if new filename or temperature ***/
void MODEL_SetModel(int bsim, LIST *assignments, char *filename, char *lib, char *dev,
                    double Temp)
  {
  DECK deck;
  int i;
  char temp[STRMAX];
  LIST *parameters;

  /*** if lib starts with CORNER, substitute all caps MODEL_Corner ***/
  if ((MODEL_Corner!=NULL) && (strncmp(lib,"CORNER",6)==0))
    {
    if (MODEL_RandomCorner)
      {
      i=2;
      if (rand()%2==1) sprintf(temp,"FF");
      else             sprintf(temp,"SS");
      }
    else for (i=0; i<strlen(MODEL_Corner); i++) temp[i]=toupper(MODEL_Corner[i]);
    snprintf(temp+i,STRMAX-i,"%s",lib+6);
    deck.lib=temp;
    }
  else deck.lib=lib;
  deck.dev=dev;
  
  /*** sort outermost assignment scope ***/
  assert(assignments->max>0);
  parameters = assignments->p.list[assignments->max-1];
  list_finish_lazy_sort(parameters,&passignstrcmp);

  /*** find or add the new model deck by filename, lib, temperature ***/
  deck.temperature=Temp;
  deck.filename=filename;
  deck.parameters=parameters;
  if (MODEL_Decks==NULL) MODEL_Decks=list_create(sizeof(DECK));
  i=find_element_lazy_sort(MODEL_Decks,&deck,&deck_cmp);
  if (monte_carlo_seed<=0 && i>=0) MODEL_Models=MODEL_Decks->p.deck[i].models; /* set current model */
  else
    {
    /* parse a new model, set MODEL_Models */
    MODEL_ParseModels(bsim,assignments,deck.filename,deck.lib,deck.dev,Temp);

    /* add a new deck */
    deck.filename=leak_strdup(deck.filename);
    deck.lib=leak_strdup(deck.lib);
    deck.dev=leak_strdup(deck.dev);
    deck.parameters=duplicate_assignment_list(parameters);
    deck.models=MODEL_Models;
    list_insert_element_lazy_sort(MODEL_Decks,&deck,&deck_cmp);
    }
  }

/*** find a matching MODEL model (round to nearest pm first) ***/
MODEL *MODEL_FindModel(double Type, double W, double L)
  {
  int i,pmW,pmL,pmWmin,pmWmax,pmLmin,pmLmax;
  MODEL *m,*m_Wmax=NULL;
  pmW = (int) (W * 1e12 + 0.5);
  pmL = (int) (L * 1e12 + 0.5);
  if (MODEL_Models!=NULL) for (i=0; i<MODEL_Models->max; i++)
    {
    m=MODEL_Models->p.model[i];
    pmLmin = (int) (m->Lmin * 1e12 + 0.5);
    pmLmax = (int) (m->Lmax * 1e12 + 0.5);
    pmWmin = (int) (m->Wmin * 1e12 + 0.5);
    pmWmax = (int) (m->Wmax * 1e12 + 0.5);
    if ((Type == m->type) && (pmL >= pmLmin) && (pmL < pmLmax) && (pmW >= pmWmin))
      {
      if (pmW < pmWmax) return m;
      else if ((m_Wmax == NULL) || (m->Wmax > m_Wmax->Wmax)) m_Wmax = m;
      }
    }
  if (m_Wmax!=NULL)
    {
    /** use largest Wmax model which otherwise fits **/
    fprintf(stderr,"WARNING: no matching model found for T=%g, W=%g, L=%g."
            "  Using Wmax=%g instead\n",Type,W,L,m_Wmax->Wmax);
    return m_Wmax;
    }

  /** report errors **/
  fprintf(stderr,"ERROR: no matching model found for T=%g, W=%g, L=%g.\n",
          Type,W,L);
  if (MODEL_Models!=NULL) for (i=0; i<MODEL_Models->max; i++)
    {
    m=MODEL_Models->p.model[i];
    if (m->type!=Type) continue;
    fprintf(stderr,"  %s LMIN=%g LMAX=%g WMIN=%g WMAX=%g\n",
            m->type==NMOS ? "NMOS" : "PMOS",
            m->Lmin,m->Lmax,m->Wmin,m->Wmax);
    }
  exit(1);
  return NULL;
  }

/*** Final steps.  Link models to their decks. ***/
void MODEL_Finish()
  {
  DECK *deck;
  MODEL *model;
  int i,j;
  if (MODEL_Decks==NULL) return;
  for (i=0; i<MODEL_Decks->max; i++)
    {
    deck=&MODEL_Decks->p.deck[i];
    for (j=0; j<deck->models->max; j++)
      {
      model=deck->models->p.model[j];
      model->deck=deck;
      }
    }
  }

/*** free up the memory ***/
void MODEL_FreeMemory() {
  int i,j;
  DECK *deck;
  MODEL *model;
  if (MODEL_Corner!=NULL) leak_free(MODEL_Corner);
  if (MODEL_Decks==NULL) return;
  for (i=0; i<MODEL_Decks->max; i++) {
    deck = &MODEL_Decks->p.deck[i];
    for (j=0; j<deck->models->max; j++) {
      model = deck->models->p.model[j];
      leak_free(model);
    }
    leak_free(deck->filename);
    leak_free(deck->lib);
    leak_free(deck->dev);
    list_free(deck->models);
  }
  list_free(MODEL_Decks);
}
