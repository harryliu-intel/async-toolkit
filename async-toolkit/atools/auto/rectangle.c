#include "auto.h"

void rectangle_move(RECT *pr, double dx, double dy)
  {
  pr->x0+=dx;
  pr->x1+=dx;
  pr->y0+=dy;
  pr->y1+=dy;
  }

void rectangle_bloat(RECT *pr, double bloat)
  {
  bloat*=0.5;
  pr->x0-=bloat;
  pr->x1+=bloat;
  pr->y0-=bloat;
  pr->y1+=bloat;
  }

void rectangle_union(RECT *pr1, RECT *pr2)
  {
  pr1->x0=min(pr1->x0,pr2->x0);
  pr1->y0=min(pr1->y0,pr2->y0);
  pr1->x1=max(pr1->x1,pr2->x1);
  pr1->y1=max(pr1->y1,pr2->y1);
  }

void add_rect(LIST *rects, RECT rect, double bloat, double dx, double dy)
  {
  rectangle_move(&rect,dx,dy);
  rectangle_bloat(&rect,bloat);
  list_append_element(rects,&rect);
  }
