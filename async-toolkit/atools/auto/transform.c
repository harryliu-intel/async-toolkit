#include "auto.h"

/*** the identity transform ***/
TRANSFORM identity_transform()
  {
  TRANSFORM T;
  T.M[0][0]=1; T.M[0][1]=0;
  T.M[1][0]=0; T.M[1][1]=1;
  T.O[0]=0;    T.O[1]=0;
  return T;
  }

/*** apply the transform to the labels rectangle ***/
void apply_transform(LABEL *pl, TRANSFORM T)
  {
  int x0,y0,x1,y1;
  x0=T.M[0][0]*pl->x0 + T.M[0][1]*pl->y0 + T.O[0];
  y0=T.M[1][0]*pl->x0 + T.M[1][1]*pl->y0 + T.O[1];
  x1=T.M[0][0]*pl->x1 + T.M[0][1]*pl->y1 + T.O[0];
  y1=T.M[1][0]*pl->x1 + T.M[1][1]*pl->y1 + T.O[1];
  pl->x0=min(x0,x1);
  pl->x1=max(x0,x1);
  pl->y0=min(y0,y1);
  pl->y1=max(y0,y1);
  }

/*** compose two transforms (AB) ***/
TRANSFORM compose_transforms(TRANSFORM A, TRANSFORM B)
  {
  TRANSFORM T;
  T.M[0][0]=B.M[0][0]*A.M[0][0] + B.M[0][1]*A.M[1][0];
  T.M[0][1]=B.M[0][0]*A.M[0][1] + B.M[0][1]*A.M[1][1];
  T.M[1][0]=B.M[1][0]*A.M[0][0] + B.M[1][1]*A.M[1][0];
  T.M[1][1]=B.M[1][0]*A.M[0][1] + B.M[1][1]*A.M[1][1];
  T.O[0]=B.M[0][0]*A.O[0] + B.M[0][1]*A.O[1] + B.O[0];
  T.O[1]=B.M[1][0]*A.O[0] + B.M[1][1]*A.O[1] + B.O[1];
  return T;
  }

/*** compose and offset ***/
TRANSFORM offset_transform(int dx, int dy)
  {
  TRANSFORM T;
  T=identity_transform();
  T.O[0]=dx;
  T.O[1]=dy;
  return T;
  }
