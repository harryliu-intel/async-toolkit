#include "auto.h"

/*** evaluate the separable part of the overlap convolution ***/
/*** also return partial derivative for first and last parameters ***/
double overlap_integral(double u, double a0, double a1, double x0, double x1,
                        double *du, double *dx1)
  {
  double energy,third=1.0/3.0;
  double ux0,ux1,dx;
  double ux0_2,ux0_3,ux0_4,ux0_5,ux1_2,ux1_3,dx_2,a01;

/* mathematical expression for energy
  energy= (u-x0)
         -third*a0*(u-x0)^3
         -third*a1*(u-x1)^3
         +a0*a1*(0.2*(u-x0)^5
                -0.5*(u-x0)^4*(x1-x0)
                +third*(u-x0)^3*(x1-x0)^2);
*/
  ux0=u-x0;
  ux1=u-x1;
  dx=x1-x0;
  ux0_2=ux0*ux0;
  ux0_3=ux0_2*ux0;
  ux0_4=ux0_3*ux0;
  ux0_5=ux0_4*ux0;
  ux1_2=ux1*ux1;
  ux1_3=ux1_2*ux1;
  dx_2=dx*dx;
  a01=a0*a1;

  energy= ux0 - third*(a0*ux0_3 + a1*ux1_3)
        + a01*(0.2*ux0_5 - 0.5*ux0_4*dx + third*ux0_3*dx_2);
  if (GRADIENT)
    {
    *du= 1 - a0*ux0_2 - a1*ux1_2 + a01*(ux0_4 - 2*ux0_3*dx + ux0_2*dx_2);
    *dx1= a1*ux1_2 + a01*(-0.5*ux0_4 + 2*third*ux0_3*dx);
    }
  return energy;
  }

/*** convolve two upsidedown parabolic separable kernals ***/
/*** also compute analytic gradient with respect to pb1 displacement ***/
double overlap_energy(LIST *blocks, RECT *pr0, RECT *pr1, double cost)
  {
  double xmin,ymin,xmax,ymax,x0,x1,y0,y1,w0,w1,h0,h1,overlap;
  double xmindx,ymindy,xmaxdx,ymaxdy;
  double o1,o2,o3,o4,do1,do2,do3,do4;
  double du1,du2,du3,du4,dv1,dv2,dv3,dv4;
  double gx,gy;
  BLOCK *pb0=NULL,*pb1=NULL;

  /*** find overlap region (and derivatives) ***/
  if (pr1->x0>pr0->x0) {xmin=pr1->x0; xmindx=1;}
  else                 {xmin=pr0->x0; xmindx=0;}
  if (pr1->x1<pr0->x1) {xmax=pr1->x1; xmaxdx=1;}
  else                 {xmax=pr0->x1; xmaxdx=0;}
  if (pr1->y0>pr0->y0) {ymin=pr1->y0; ymindy=1;}
  else                 {ymin=pr0->y0; ymindy=0;}
  if (pr1->y1<pr0->y1) {ymax=pr1->y1; ymaxdy=1;}
  else                 {ymax=pr0->y1; ymaxdy=0;}
  if ((xmax<=xmin)||(ymax<=ymin)) return 0;

  /*** find blocks ***/
  if (pr0->block>=0) pb0=&blocks->p.block[pr0->block];
  if (pr1->block>=0) pb1=&blocks->p.block[pr1->block];

  /*** compute parameters describing convolution kernals ***/
  w0=pr0->x1-pr0->x0;
  h0=pr0->y1-pr0->y0;
  x0=0.5*(pr0->x0+pr0->x1);
  y0=0.5*(pr0->y0+pr0->y1);
  w1=pr1->x1-pr1->x0;
  h1=pr1->y1-pr1->y0;
  x1=0.5*(pr1->x0+pr1->x1); /* dx1/ddx = 1 */
  y1=0.5*(pr1->y0+pr1->y1); /* dy1/ddy = 1 */
  w0=4.0/(w0*w0);
  h0=4.0/(h0*h0);
  w1=4.0/(w1*w1);
  h1=4.0/(h1*h1);

  /*** convolution energy of r0 and r1 ***/
  o1=overlap_integral(xmax,w0,w1,x0,x1,&du1,&dv1);
  o2=overlap_integral(xmin,w0,w1,x0,x1,&du2,&dv2);
  o3=overlap_integral(ymax,h0,h1,y0,y1,&du3,&dv3);
  o4=overlap_integral(ymin,h0,h1,y0,y1,&du4,&dv4);
  overlap=cost*(o1-o2)*(o3-o4);

  /*** gradient with respect to change in pb1->x, pb1->y ***/
  if (GRADIENT)
    {
    do1=du1*xmaxdx + dv1;
    do2=du2*xmindx + dv2;
    gx=cost*(do1-do2)*(o3-o4);

    do3=du3*ymaxdy + dv3;
    do4=du4*ymindy + dv4;
    gy=cost*(o1-o2)*(do3-do4);

    if (pb1!=NULL) {pb1->dx-=gx; pb1->dy-=gy;}
    if (pb0!=NULL) {pb0->dx+=gx; pb0->dy+=gy;}
    }

  /*** return energy ***/
  return overlap;
  }

/*** check the overlap of a pair of rectangles ***/
double rectangle_overlap(LIST *blocks, RECT r0, RECT r1, double cost, int modulo)
  {
  int same_node;
  double spacing;

  /*** compute spacing ***/
  same_node=(!modulo)&&(r0.name==r1.name)&&(r0.name!=NULL);
  if ((r0.paint<0)&&(r1.paint<0)) return 0; /* ignore subcell to subcell overlap */
  else if ((r0.paint<0)||(r1.paint<0)) spacing=subcell_spacing;
  else spacing=Spacing[r0.paint][r1.paint][same_node];
  if (spacing<0) return 0;
  spacing+=epsilon;

  /*** check for matching block_indices ***/
  if ((!modulo)&&(r0.block==r1.block)) return 0;

  /*** unbloat rectangles ***/
  rectangle_bloat(&r0,spacing - Paints->p.paint[r0.paint].maxspacing);
  rectangle_bloat(&r1,spacing - Paints->p.paint[r1.paint].maxspacing);

  /*** get rectangle overlap energy ***/
  return overlap_energy(blocks,&r0,&r1,cost);
  }

/*** try worst case overlap modulo xpitch, ypitch ***/
double modulo_overlap(LIST *blocks, RECT *pr0, RECT *pr1, double cost)
  {
  double overlap=0,x0,x1,y0,y1;
  int i,j,ni,nj;

  ni=(xpitch!=0);
  nj=(ypitch!=0);
  x0=pr1->x0;
  x1=pr1->x1;
  y0=pr1->y0;
  y1=pr1->y1;

  for (i=-ni; i<=ni; i++)
    {
    pr1->x0=x0+i*xpitch;
    pr1->x1=x1+i*xpitch;
    if (overlap(pr0->x0,pr0->x1,pr1->x0,pr1->x1))
      for (j=-nj; j<=nj; j++)
	{
        pr1->y0=y0+j*ypitch;
        pr1->y1=y1+j*ypitch;
        if (overlap(pr0->y0,pr0->y1,pr1->y0,pr1->y1))
          overlap+=rectangle_overlap(blocks,*pr0,*pr1,cost,(i!=0)||(j!=0));
	}
    }

  pr1->x0=x0;
  pr1->x1=x1;
  pr1->y0=y0;
  pr1->y1=y1;
  return overlap;
  }

/*** N*sqrt(N) tp N^2 algorithm for overlap ***/
double layer_overlap(LIST *blocks, LIST *rects, double cost)
  {
  int i,j;
  double overlap=0;
  RECT *pr0,*pr1;

  if (ypitch>0) list_sort(rects,&rect_x0_cmp);
  else          list_sort(rects,&rect_y0_cmp);
  for (i=0; i<rects->max; i++)
    {
    pr0=&rects->p.rect[i];
    for (j=i+1; j<rects->max; j++)
      {
      pr1=&rects->p.rect[j];
      if (((ypitch> 0)&&(pr1->x0>pr0->x1)) ||
          ((ypitch<=0)&&(pr1->y0>pr0->y1))) break;
      overlap+=modulo_overlap(blocks,pr0,pr1,cost);
      }
    }
  return overlap;
  }
