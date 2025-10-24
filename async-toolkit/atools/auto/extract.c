#include "auto.h"

/*** draw rectangle nets as labels ***/
void debug_rectangle_nets(LIST *blocks, LIST *nodes)
  {
  int i,j;
  BLOCK *pb;
  RECT *pr;
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    for (j=0; j<pb->rects->max; j++)
      {
      pr=&pb->rects->p.rect[j];
      if (pr->name!=NULL)
	add_port(pr->name,pr->paint,pr->x0,pr->y0,pr->x1,pr->y1,pr->block,nodes);
      }
    }
  }

/*** draw abutments as labels somehow ***/
void debug_abut(LIST *blocks, LIST *nodes)
  {
  int i,j;
  BLOCK *pb;
  RECT *pr;
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    for (j=0; j<pb->rects->max; j++)
      {
      pr=&pb->rects->p.rect[j];
      if (pr->abutW!=NULL) add_port(pr->abutW,pr->paintW,pr->x0,pr->y0,pr->x0,pr->y1,pr->block,nodes);
      if (pr->abutE!=NULL) add_port(pr->abutE,pr->paintE,pr->x1,pr->y0,pr->x1,pr->y1,pr->block,nodes);
      if (pr->abutN!=NULL) add_port(pr->abutN,pr->paintN,pr->x0,pr->y1,pr->x1,pr->y1,pr->block,nodes);
      if (pr->abutS!=NULL) add_port(pr->abutS,pr->paintS,pr->x0,pr->y0,pr->x1,pr->y0,pr->block,nodes);
      }
    }
  }

/*** propagate node fields of rectangles by electrical connectivity ***/
void propagate_nodes(LIST *blocks, LIST *nodes)
  {
  int i,j,k,progress;
  BLOCK *pb;
  RECT *pr1,*pr2;

  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];

    /*** clear abut fields ***/
    for (j=0; j<pb->rects->max; j++)
      {
      pr1=&pb->rects->p.rect[j];
      pr1->abutN=pr1->abutS=pr1->abutE=pr1->abutW=NULL;
      pr1->paintN=pr1->paintS=pr1->paintE=pr1->paintW=-1;
      }

    /*** propagate node information, fill in abut ***/
    do
      {
      progress=0;
      for (j=0; j<pb->rects->max; j++)
	{
	pr1=&pb->rects->p.rect[j];
	for (k=j+1; k<pb->rects->max; k++)
	  {
	  pr2=&pb->rects->p.rect[k];
	  if (Connect[pr1->paint][pr2->paint]&&
	      overlap(pr1->x0,pr1->x1,pr2->x0,pr2->x1)&&
	      overlap(pr1->y0,pr1->y1,pr2->y0,pr2->y1))
	    {
	    if      ((pr1->name!=NULL)&&(pr2->name==NULL)) {pr2->name=pr1->name; progress++;}
	    else if ((pr2->name!=NULL)&&(pr1->name==NULL)) {pr1->name=pr2->name; progress++;}
	    }
	  }
        }
      } while (progress);

    /*** fill in abut information ***/
    for (j=0; j<pb->rects->max; j++)
      {
      pr1=&pb->rects->p.rect[j];
      for (k=j+1; k<pb->rects->max; k++)
        {
        pr2=&pb->rects->p.rect[k];
        if (Abut[pr1->paint][pr2->paint]&&
	    overlap(pr1->x0,pr1->x1,pr2->x0,pr2->x1)&&
	    overlap(pr1->y0,pr1->y1,pr2->y0,pr2->y1))
	  {
	  if ((pr1->x0==pr2->x0)&&(pr1->x1==pr2->x1)&&(pr1->y0==pr2->y1))
	    {
	    pr1->abutS=pr2->name; pr1->paintS=pr2->paint;
	    pr2->abutN=pr1->name; pr2->paintN=pr1->paint;
	    }
	  else if ((pr1->x0==pr2->x0)&&(pr1->x1==pr2->x1)&&(pr1->y1==pr2->y0))
	    {
	    pr1->abutN=pr2->name; pr1->paintN=pr2->paint;
	    pr2->abutS=pr1->name; pr2->paintS=pr1->paint;
	    }
	  else if ((pr1->y0==pr2->y0)&&(pr1->y1==pr2->y1)&&(pr1->x0==pr2->x1))
	    {
	    pr1->abutW=pr2->name; pr1->paintW=pr2->paint;
	    pr2->abutE=pr1->name; pr2->paintE=pr1->paint;
	    }
	  else if ((pr1->y0==pr2->y0)&&(pr1->y1==pr2->y1)&&(pr1->x1==pr2->x0))
	    {
	    pr1->abutE=pr2->name; pr1->paintE=pr2->paint;
	    pr2->abutW=pr1->name; pr2->paintW=pr1->paint;
	    }
	  }
        }
      }
    }

  /*** debugging ***/
  /* debug_rectangle_nets(blocks,nodes); */
  /* debug_abut(blocks,nodes); */
  }
