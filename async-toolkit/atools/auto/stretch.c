#include "auto.h"

#define DIR_RIGHT 3
#define DIR_LEFT 7
#define DIR_UP 1
#define DIR_DOWN 5

/*** parse a user defined stretch ***/
STRETCH parse_stretch(LEX *lex)
  {
  STRETCH stretch;
  stretch.label=get_name(parse_nodename(lex));
  stretch.amount=lex_eat_integer(lex);
  return stretch;
  }

/*** debugging info ***/
void print_stretch(STRETCH *pstretch)
  {
  printf("  %s %d ",pstretch->label,pstretch->amount);
  print_expression(stdout,pstretch->expression);
  }

/*** stretch a point for each stretchlabel it lies in ***/
void stretch_point(double *x, double *y, LIST *stretches, LIST *stretchlabels)
  {
  int i,amount;
  double dx=0,dy=0;
  SLAB slab;

  for (i=0; i<stretchlabels->max; i++)
    {
    slab=stretchlabels->p.slab[i];
    amount=stretches->p.stretch[slab.n].amount;
    if (amount<0) continue;
    if ((*x>=slab.x0)&&(*x<=slab.x1)&&(*y>=slab.y0)&&(*y<=slab.y1))
      {
      if      (slab.dir==DIR_UP)    dy+=amount;
      else if (slab.dir==DIR_DOWN)  dy-=amount;
      else if (slab.dir==DIR_LEFT)  dx-=amount;
      else if (slab.dir==DIR_RIGHT) dx+=amount;
      }
    }
  *x=*x+dx;
  *y=*y+dy;
  }

/*** stretch rectangle (stretch corners independently, then rectangularize again) ***/
void stretch_rect(double *x0, double *y0, double *x1, double *y1, LIST *stretches, LIST *stretchlabels)
  {
  double x[4],y[4];
  int i;

  x[0]=*x0; y[0]=*y0;
  x[1]=*x1; y[1]=*y0;
  x[2]=*x0; y[2]=*y1;
  x[3]=*x1; y[3]=*y1;
  for (i=0; i<4; i++) stretch_point(&x[i],&y[i],stretches,stretchlabels);
  *x0=min(x[0],x[2]);
  *x1=max(x[1],x[3]);
  *y0=min(y[0],y[1]);
  *y1=max(y[2],y[3]);
  }
