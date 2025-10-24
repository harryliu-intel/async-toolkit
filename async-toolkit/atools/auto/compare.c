#include "auto.h"

int nodecmp(void *p1, void *p2)
  {
  return strcmp(((NODE *)p1)->name,((NODE *)p2)->name);
  }

int rect_x0_cmp(void *p1, void *p2)
  {
  RECT *pr1,*pr2;

  pr1=((RECT *)p1);
  pr2=((RECT *)p2);
  if      (pr1->x0<pr2->x0) return -1;
  else if (pr1->x0>pr2->x0) return 1;
  else                      return 0;
  }

int rect_y0_cmp(void *p1, void *p2)
  {
  RECT *pr1,*pr2;

  pr1=((RECT *)p1);
  pr2=((RECT *)p2);
  if      (pr1->y0<pr2->y0) return -1;
  else if (pr1->y0>pr2->y0) return 1;
  else                      return 0;
  }

int wire_cmp(void *p1, void *p2)
  {
  WIRE *pw1, *pw2;

  pw1=((WIRE *)p1);
  pw2=((WIRE *)p2);
  if      (pw1->distance<pw2->distance) return -1;
  else if (pw1->distance>pw2->distance) return 1;
  else                                  return 0;
  }

int paint_str_cmp(void *p1, void *p2)
  {
  return strcmp(((PAINT *)p1)->name,(char *)p2);
  }

int rename_cmp (void *p1, void *p2)
  {
  RENAME *pp1, *pp2;

  pp1=((RENAME *)p1);
  pp2=((RENAME *)p2);
  return strcmp(pp1->old,pp2->old);
  }

int rename_str_cmp(void *p1, void *p2)
  {
  return strcmp(((RENAME *)p1)->old,(char *)p2);
  }

int stretch_cmp (void *p1, void *p2)
  {
  STRETCH *pp1, *pp2;

  pp1=((STRETCH *)p1);
  pp2=((STRETCH *)p2);
  return strcmp(pp1->label,pp2->label);
  }

int stretch_str_cmp(void *p1, void *p2)
  {
  return strcmp(((STRETCH *)p1)->label,(char *)p2);
  }

int ruletarget_cmp(void *p1, void *p2)
  {
  RULE *pr1=(RULE *)p1,*pr2=(RULE *)p2;
  if (pr1->dir<pr2->dir) return -1;
  if (pr1->dir>pr2->dir) return 1;
  return strcmp(pr1->target,pr2->target);
  }

int str_cmp(void *p1, void *p2)
  {
  return strcmp(*((char **)p1),*((char **)p2));
  }

int parm_cmp(void *p1, void *p2)
  {
  PARM *parm1=*((PARM **)p1),*parm2=*((PARM **)p2);
  return strcmp(parm1->name,parm2->name);
  }

int parm_str_cmp(void *p1, void *p2)
  {
  PARM *parm1=*((PARM **)p1);
  char *name=(char *)p2;
  return strcmp(parm1->name,name);
  }

int rule_delay_cmp(void *p1, void *p2)
  {
  RULE *pr1=*((RULE **)p1),*pr2=*((RULE **)p2);
  return pr1->delay - pr2->delay; /* compare VAR *'s directly */
  }

int path_cmp(void *p1, void *p2) /* check if paths have same sequence of rule delays */
  {
  PATH *path1=(PATH *)p1,*path2=(PATH *)p2;
  return list_compare(path1->rules,path2->rules,&rule_delay_cmp);
  }

int ppath_cmp(void *p1, void *p2) /* check if paths have same sequence of rule delays */
  {
  PATH *path1=*((PATH **)p1),*path2=*((PATH **)p2);
  return list_compare(path1->rules,path2->rules,&rule_delay_cmp);
  }

int catpath_cmp(void *p1, void *p2) /* check if catpaths have same sequence of rule delays */
  {
  CATPATH *pcp1=(CATPATH *)p1,*pcp2=(CATPATH *)p2;
  return list_compare(pcp1->paths,pcp2->paths,&ppath_cmp);
  }

int staticizer_cmp(void *p1, void *p2)
  {
  STATICIZER *ps1=(STATICIZER *)p1, *ps2=(STATICIZER *)p2;
  return strcmp(ps1->name,ps2->name);
  }

int pblock_rulenum_cmp(void *p1, void *p2)
  {
  BLOCK *pb1,*pb2;
  pb1=*((BLOCK **)p1);
  pb2=*((BLOCK **)p2);
  return pb1->rulenum - pb2->rulenum;
  }

/*** compare two circuits by name ***/
int pcircuit_cmp(void *p1, void *p2)
  {
  return strcmp((*((CIRCUIT **)p1))->name,(*((CIRCUIT **)p2))->name);
  }

/*** compare a circuit to a name ***/
int pcircuit_str_cmp(void *p1, void *p2)
  {
  return strcmp((*((CIRCUIT **)p1))->name,(char *)p2);
  }

/*** compare instances by name length ***/
int instance_cmp(void *p1, void *p2)
  {
  return strcmp((((INSTANCE *)p1)->name),(((INSTANCE *)p2)->name));
  }

/*** compare pin directives by name ***/
int directive_cmp(void *p1, void *p2)
  {
  return strcmp( ((DIRECTIVE *)p1)->name, ((DIRECTIVE *)p2)->name );
  }

/*** compare block pointers by width ***/
int pblock_width_cmp(void *p1, void *p2)
  {
  BLOCK *pb1=*((BLOCK **)p1),*pb2=*((BLOCK **)p2);
  double w1,w2;
  w1=pb1->bbox.x1-pb1->bbox.x0;
  w2=pb2->bbox.x1-pb2->bbox.x0;
  if (w1<w2) return -1;
  if (w2>w1) return  1;
  return 0;
  }

/*** compare length of rules ***/
int rule_length_cmp(void *p1, void *p2)
  {
  RULE *pr1=(RULE *)p1,*pr2=(RULE *)p2;
  return pr1->guard->max - pr2->guard->max;
  }

/*** sort in decreasing number of gates in series ***/
int rule_decreasing_length_cmp(void *p1, void *p2)
  {
  RULE *pr1=(RULE *)p1,*pr2=(RULE *)p2;
  return pr2->guard->max - pr1->guard->max;
  }

