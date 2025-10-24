#include "auto.h"

/*** distance energy between ports and -gradient ***/
double port_distance(PORT *pp0, PORT *pp1, LIST *blocks, double cost)
  {
  BLOCK *pb0=NULL,*pb1=NULL;
  double dx,dy,t,invt;
  if (blocks!=NULL)
    {
    pb0=&blocks->p.block[pp0->block];
    pb1=&blocks->p.block[pp1->block];
    dx=(pb1->x+0.5*(pp1->x0+pp1->x1)) - (pb0->x+0.5*(pp0->x0+pp0->x1));
    dy=(pb1->y+0.5*(pp1->y0+pp1->y1)) - (pb0->y+0.5*(pp0->y0+pp0->y1));
    }
  else /* no blocks for estimate_wirecap */
    {
    dx=0.5*(pp1->x0+pp1->x1) - 0.5*(pp0->x0+pp0->x1);
    dy=0.5*(pp1->y0+pp1->y1) - 0.5*(pp0->y0+pp0->y1);
    }

  /* scale x and y coordinates */
  dx*=Size.xscale; dy*=Size.yscale;
  
  /* compute distance */
  if (distance_measure==0)
    {
    if (GRADIENT)
      {
      pb0->dx+= cost*((dx>=0) ? 1 : -1);
      pb0->dy+= cost*((dy>=0) ? 1 : -1);
      pb1->dx-= cost*((dx>=0) ? 1 : -1);
      pb1->dy-= cost*((dy>=0) ? 1 : -1);
      }
    return cost*(fabs(dx)+fabs(dy));
    }
  else if (distance_measure==1)
    {
    if (GRADIENT)
      {
      pb0->dx+= cost*2*dx;
      pb0->dy+= cost*2*dy;
      pb1->dx-= cost*2*dx;
      pb1->dy-= cost*2*dy;
      }
    return cost*(dx*dx + dy*dy);
    }
  else
    {
    t=sqrt(dx*dx + dy*dy);
    if (GRADIENT)
      {
      invt=1/t;
      pb0->dx+= cost*dx*invt;
      pb0->dy+= cost*dy*invt;
      pb1->dx-= cost*dx*invt;
      pb1->dy-= cost*dy*invt;
      }
    return cost*t;
    }
  }

/*** paint block rectangles ***/
LIST *generate_paint(LIST *blocks)
  {
  static LIST *rects=NULL,*rlist;
  int i,j;
  BLOCK *pblock;
  RECT *pr;

  /*** initialize are reset rectangle lists ***/
  if (rects==NULL)
    {
    rects=list_create(sizeof(RECT));
    rects->shrink=0;
    }
  list_realloc(rects,0);

  /*** identify block rectangles ***/
  for (i=0; i<blocks->max; i++)
    {
    pblock=&blocks->p.block[i];
    if      (detail_level==0) rlist=pblock->sboxs;
    else if (detail_level==1) rlist=pblock->bboxs;
    else                      rlist=pblock->rects;
    for (j=0; j<rlist->max; j++)
      {
      pr=&rlist->p.rect[j];
      add_rect(rects,*pr,Paints->p.paint[pr->paint].maxspacing,
               pblock->x,pblock->y);
      }
    }

  return rects;
  }

/*** area energy and -gradient ***/
double compute_area(LIST *blocks, double cost, 
                    double *x0, double *y0, double *x1, double *y1)
  {
  int i,left,right,top,bottom;
  double width,height,area;
  RECT rect;
  BLOCK *pb;

  left=right=top=bottom=-1;
  *x0=*y0= 1e10;
  *x1=*y1=-1e10;
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    if (pb->fixed) continue; /* don't count fixed blocks in area */
    rect=pb->bbox;
    rectangle_move(&rect,pb->x,pb->y);
    if (rect.x0<*x0) {*x0=rect.x0; left=rect.block;}
    if (rect.x1>*x1) {*x1=rect.x1; right=rect.block;}
    if (rect.y0<*y0) {*y0=rect.y0; bottom=rect.block;}
    if (rect.y1>*y1) {*y1=rect.y1; top=rect.block;}
    }
  width= (*x1-*x0);
  height=(*y1-*y0);
  area=cost*max(width,xpitch)*max(height,ypitch);

  /*** gradient ***/
  if (GRADIENT)
    {
    if (width>xpitch)
      {
      pb=&blocks->p.block[left];
      pb->dx+=cost*max(height,ypitch);
      pb=&blocks->p.block[right];
      pb->dx-=cost*max(height,ypitch);
      }
    if (height>ypitch)
      {
      pb=&blocks->p.block[bottom];
      pb->dy+=cost*max(width,xpitch);
      pb=&blocks->p.block[top];
      pb->dy-=cost*max(width,xpitch);
      }
    }

  /*** return ***/
  return area;
  }

/*** compute energy ***/
double compute_energy(LIST *blocks, LIST *nodes,
                      double *poverlap, double *parea, double *pcap,
                      double *pxmin, double *pymin,
                      double *pxmax, double *pymax)
  {
  LIST *rects;
  NODE *pn;
  WIRE *pw;
  int i,j;
  double x0,y0,x1,y1;
  double overlap=0,cap=0,area=0,cost;

  /*** identify wire segments, compute cap energy ***/
  if (cap_cost>0) for (i=0; i<nodes->max; i++)
    {
    pn=&nodes->p.node[i];
    if      (pn->global)   cost=cap_cost*global_cap_cost_ratio;
    else if (pn->internal) cost=cap_cost*internal_cap_cost_ratio;
    else                   cost=cap_cost;
    for (j=0; j<pn->wires->max; j++)
      {
      pw=&pn->wires->p.wire[j];
      cap+=port_distance(&pn->ports->p.port[pw->port[0]],
                         &pn->ports->p.port[pw->port[1]],blocks,cost);
      }
    }
  
  /*** generate layer paint ***/
  rects=generate_paint(blocks);

  /*** compute overlap energy ***/
  if (overlap_cost>0) overlap=layer_overlap(blocks,rects,overlap_cost);

  /*** compute area energy ***/
  if (area_cost>0) area=compute_area(blocks,area_cost,&x0,&y0,&x1,&y1);

  /*** return values ***/
  if (poverlap!=NULL) *poverlap=overlap; 
  if (parea!=NULL)    *parea=area;
  if (pcap!=NULL)     *pcap=cap;
  if (pxmin!=NULL)    *pxmin=x0;
  if (pxmax!=NULL)    *pxmax=x1;
  if (pymin!=NULL)    *pymin=y0;
  if (pymax!=NULL)    *pymax=y1;
  return cap+area+overlap;
  }

/********************** Interface to minimize.c ********************/

int sizeof_state(LIST *blocks)
  {
  return 2*blocks->max;
  }

void unpack_state(LIST *blocks, double *p)
  {
  int i,n=0;
  BLOCK *pb;

  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    pb->x=p[n++];
    pb->y=p[n++];
    }
  }

void pack_state(LIST *blocks, double *p)
  {
  int i,n=0;
  BLOCK *pb;

  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    p[n++]=pb->x;
    p[n++]=pb->y;
    }
  }

/*** compute energy (and -gradient) ***/
double energy(PARAMETERS *parms, double *p)
  {
  unpack_state(parms->blocks,p);
  return compute_energy(parms->blocks,parms->nodes,
           NULL,NULL,NULL,NULL,NULL,NULL,NULL);
  }

/*** compute -gradient ***/
void down(PARAMETERS *parms, double *p, double *d)
  {
  int i,n=0;
  BLOCK *pb;

  for (i=0; i<parms->blocks->max; i++)
    {
    pb=&parms->blocks->p.block[i];
    pb->dx=0;
    pb->dy=0;
    }
  GRADIENT=1; energy(parms,p); GRADIENT=0;
  for (i=0; i<parms->blocks->max; i++)
    {
    pb=&parms->blocks->p.block[i];
    if (pb->fixed) continue;
    d[n++]=pb->dx;
    d[n++]=pb->dy;
    }
  }
