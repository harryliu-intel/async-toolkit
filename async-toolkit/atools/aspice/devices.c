/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*********************** CREATE DEVICES ***********************/

/*** current/voltage source ***/
void create_source(DEVICE *pdev, int Nnodes, NODE **nodes, int Nops, OP *ops, int vsource)
  {
  int j;
  pdev->name=NULL;
  pdev->device_func=vsource ? &voltage_source : &current_source;
  pdev->sub.source=(SOURCE *) leak_malloc(sizeof(SOURCE));
  pdev->sub.source->Nnodes=Nnodes;
  pdev->sub.source->nodes=(NODE **) leak_malloc(Nnodes*sizeof(NODE *));
  memcpy(pdev->sub.source->nodes,nodes,Nnodes*sizeof(NODE *));
  pdev->sub.source->Nops=Nops;
  pdev->sub.source->ops=(OP *) leak_malloc(Nops*sizeof(OP));
  memcpy(pdev->sub.source->ops,ops,Nops*sizeof(OP));
  pdev->sub.source->Aentry=(REAL **) leak_malloc(Nnodes*Nnodes*sizeof(REAL *));
  for (j=0; j<Nnodes*Nnodes; j++) pdev->sub.source->Aentry[j]=NULL;
  pdev->sub.source->I=0;
  }

/*** nodes=a,b parms=C ***/
void create_capacitor(DEVICE *pdev, NODE *A, NODE *B, REAL C)
  {
  int j;
  pdev->name=NULL;
  pdev->device_func=&capacitor;
  pdev->sub.cap=(CAPACITOR *) leak_malloc(sizeof(CAPACITOR));
  pdev->sub.cap->nodes[0]=A;
  pdev->sub.cap->nodes[1]=B;
  pdev->sub.cap->C=C;
  for (j=0; j<4; j++) pdev->sub.cap->Aentry[j]=NULL;
  }

/*** nodes=a,b parms=R ***/
void create_resistor(DEVICE *pdev, NODE *A, NODE *B, REAL R)
  {
  int j;
  assert(R>0);
  pdev->name=NULL;
  pdev->device_func=&resistor;
  pdev->sub.res=(RESISTOR *) leak_malloc(sizeof(RESISTOR));
  pdev->sub.res->nodes[0]=A;
  pdev->sub.res->nodes[1]=B;
  pdev->sub.res->G=1/R;
  pdev->sub.res->I=0;
  for (j=0; j<4; j++) pdev->sub.res->Aentry[j]=NULL;
  }

/*** nodes=s,b parms=W,L,AS,PS ***/
void create_diode(DEVICE *pdev, int type,
		  NODE *s, NODE *b, REAL W, REAL L, REAL AS, REAL PS)
  {
  int j;
  pdev->name=NULL;
  pdev->sub.diode=(DIODE *) leak_malloc(sizeof(DIODE));
  pdev->sub.diode->model=MODEL_FindModel(type,W,L);
  if (pdev->sub.diode->model->bsim==4)
    error("BSIM4 source/drain diode is integrated");
  else error("only BSIM4 supported");
  pdev->sub.diode->nodes[0]=s;
  pdev->sub.diode->nodes[1]=b;
  pdev->sub.diode->W=W;
  pdev->sub.diode->L=L;
  pdev->sub.diode->AS=AS;
  pdev->sub.diode->PS=PS;
  for (j=0; j<4; j++) pdev->sub.diode->Aentry[j]=NULL;
  }

/*** nodes=s,d,g,b,sp,dp parms=W,L,AS,PS,NRS,AD,PD,NRD,SA,SB,SC,SD,NF,SCA,SCB,SCC ***/
void create_transistor(DEVICE *pdev, int type,
                       int Nnodes, NODE **nodes,
                       int Nparms, double *parms)
  {
  int j;
  double NF;
  pdev->name=NULL;
  pdev->sub.trans=(TRANSISTOR *) leak_malloc(sizeof(TRANSISTOR));
  assert(Nnodes==6);
  for (j=0; j<Nnodes; j++) pdev->sub.trans->nodes[j]=nodes[j];
  pdev->sub.trans->W   = Nparms>0  ? parms[0]  : 0;
  pdev->sub.trans->L   = Nparms>1  ? parms[1]  : 0;
  pdev->sub.trans->AS  = Nparms>2  ? parms[2]  : 0;
  pdev->sub.trans->PS  = Nparms>3  ? parms[3]  : 0;
  pdev->sub.trans->NRS = Nparms>4  ? parms[4]  : 0;
  pdev->sub.trans->AD  = Nparms>5  ? parms[5]  : 0;
  pdev->sub.trans->PD  = Nparms>6  ? parms[6]  : 0;
  pdev->sub.trans->NRD = Nparms>7  ? parms[7]  : 0;
  pdev->sub.trans->SA  = Nparms>8  ? parms[8]  : -1;
  pdev->sub.trans->SB  = Nparms>9  ? parms[9]  : -1;
  pdev->sub.trans->SC  = Nparms>10 ? parms[10] : -1;
  pdev->sub.trans->SD  = Nparms>11 ? parms[11] : -1;
  pdev->sub.trans->NF  = Nparms>12 ? parms[12] : -1;
  pdev->sub.trans->SCA = Nparms>13 ? parms[13] : -1;
  pdev->sub.trans->SCB = Nparms>14 ? parms[14] : -1;
  pdev->sub.trans->SCC = Nparms>15 ? parms[15] : -1;
  NF = pdev->sub.trans->NF;
  if (NF<0) NF=1;
  pdev->sub.trans->model=MODEL_FindModel(type,pdev->sub.trans->W/NF,pdev->sub.trans->L);
  if (pdev->sub.trans->model->bsim==4) pdev->device_func=&bsim4_transistor;
  else error("only BSIM4 supported");
  for (j=0; j<36; j++) pdev->sub.trans->Aentry[j]=NULL;
  }

/***************************** TEST DEVICE IDENTITY ************************************/

int isTransistor(DEVICE *pdev)
  {
  return (pdev->device_func==&bsim4_transistor);
  }

int isDiode(DEVICE *pdev)
  {
  return (pdev->device_func==&bsim4_diode);
  }

int isCapacitor(DEVICE *pdev)
  {
  return (pdev->device_func==&capacitor);
  }

int isResistor(DEVICE *pdev)
  {
  return (pdev->device_func==&resistor);
  }

int isCurrentSource(DEVICE *pdev)
  {
  return (pdev->device_func==&current_source);
  }

int isVoltageSource(DEVICE *pdev)
  {
  return (pdev->device_func==&voltage_source);
  }

int isSource(DEVICE *pdev)
  {
  return isCurrentSource(pdev) || isVoltageSource(pdev);
  }

/***************************** HANDLE DEVICE STRUCTURE *********************************/

int device_Nnodes(DEVICE *pdev)
  {
  if (isSource(pdev)) return pdev->sub.source->Nnodes;
  else if (isTransistor(pdev)) return 6;
  else return 2;
  }

NODE **device_nodes(DEVICE *pdev)
  {
  if      (isSource(pdev))        return pdev->sub.source->nodes;
  else if (isTransistor(pdev))    return pdev->sub.trans->nodes;
  else if (isDiode(pdev))         return pdev->sub.diode->nodes;
  else if (isCapacitor(pdev))     return pdev->sub.cap->nodes;
  else if (isResistor(pdev))      return pdev->sub.res->nodes;
  else return NULL;
  }

REAL **device_Aentry(DEVICE *pdev)
  {
  if      (isSource(pdev))        return pdev->sub.source->Aentry;
  else if (isTransistor(pdev))    return pdev->sub.trans->Aentry;
  else if (isDiode(pdev))         return pdev->sub.diode->Aentry;
  else if (isCapacitor(pdev))     return pdev->sub.cap->Aentry;
  else if (isResistor(pdev))      return pdev->sub.res->Aentry;
  else return NULL;
  }  

int device_mem(DEVICE *pdev)
  {
  int mem,N;
  N=device_Nnodes(pdev);
  mem=sizeof(DEVICE);
  if (isSource(pdev))
    mem+=sizeof(SOURCE) + N*sizeof(NODE *) + 
      pdev->sub.source->Nops*sizeof(OP) +
      pdev->sub.source->Nnodes*pdev->sub.source->Nnodes*sizeof(REAL *);
  else if (isTransistor(pdev)) mem+=sizeof(TRANSISTOR);
  else if (isDiode(pdev))      mem+=sizeof(DIODE);
  else if (isCapacitor(pdev))  mem+=sizeof(CAPACITOR);
  else if (isResistor(pdev))   mem+=sizeof(RESISTOR);
  else assert(0);
  if (pdev->name!=NULL) mem+=strlen(pdev->name)+1;
  return mem;
  }

void free_device(DEVICE *pdev)
  {
  if (isSource(pdev))
    {
    leak_free(pdev->sub.source->Aentry);
    leak_free(pdev->sub.source->nodes);
    leak_free(pdev->sub.source->ops);
    }
  if (pdev->name!=NULL) leak_free(pdev->name);
  leak_free(pdev->sub.any);
  }

/************************* compare devices *******************************/

/*** compare transistor model and parameters ***/
int ptranscmp(void *p1, void *p2)
  {
  TRANSISTOR *pt1=((TRANSISTOR *)p1), *pt2=((TRANSISTOR *)p2);
  int c;
  c=pt1->model->type - pt2->model->type;
  if (c!=0) return c;
  c=pt1->model - pt2->model;
  if (c!=0) return c;
  if (pt1->L>pt2->L)     return 1;
  if (pt1->L<pt2->L)     return -1;
  if (pt1->W>pt2->W)     return 1;
  if (pt1->W<pt2->W)     return -1;
  if (pt1->AS>pt2->AS)   return 1;
  if (pt1->AS<pt2->AS)   return -1;
  if (pt1->PS>pt2->PS)   return 1;
  if (pt1->PS<pt2->PS)   return -1;
  if (pt1->NRS>pt2->NRS) return 1;
  if (pt1->NRS<pt2->NRS) return -1;
  if (pt1->AD>pt2->AD)   return 1;
  if (pt1->AD<pt2->AD)   return -1;
  if (pt1->PD>pt2->PD)   return 1;
  if (pt1->PD<pt2->PD)   return -1;
  if (pt1->NRD>pt2->NRD) return 1;
  if (pt1->NRD<pt2->NRD) return -1;
  if (pt1->SA>pt2->SA)   return 1;
  if (pt1->SA<pt2->SA)   return -1;
  if (pt1->SB>pt2->SB)   return 1;
  if (pt1->SB<pt2->SB)   return -1;
  if (pt1->SC>pt2->SC)   return 1;
  if (pt1->SC<pt2->SC)   return -1;
  if (pt1->SD>pt2->SD)   return 1;
  if (pt1->SD<pt2->SD)   return -1;
  if (pt1->NF>pt2->NF)   return 1;
  if (pt1->NF<pt2->NF)   return -1;
  if (pt1->SCA>pt2->SCA) return 1;
  if (pt1->SCA<pt2->SCA) return -1;
  if (pt1->SCB>pt2->SCB) return 1;
  if (pt1->SCB<pt2->SCB) return -1;
  if (pt1->SCC>pt2->SCC) return 1;
  if (pt1->SCC<pt2->SCC) return -1;
  return 0;
  }

/*** compare diode model and parameters ***/
int pdiodecmp(void *p1, void *p2)
  {
  DIODE *pd1=((DIODE *)p1), *pd2=((DIODE *)p2);
  int c;
  c=pd1->model->type - pd2->model->type;
  if (c!=0) return c;
  c=pd1->model - pd2->model;
  if (c!=0) return c;
  if (pd1->L>pd2->L)     return 1;
  if (pd1->L<pd2->L)     return -1;
  if (pd1->W>pd2->W)     return 1;
  if (pd1->W<pd2->W)     return -1;
  if (pd1->AS>pd2->AS)   return 1;
  if (pd1->AS<pd2->AS)   return -1;
  if (pd1->PS>pd2->PS)   return 1;
  if (pd1->PS<pd2->PS)   return -1;
  return 0;
  }

/*** compare capacitor parameters ***/
int pcapcmp(void *p1, void *p2)
  {
  CAPACITOR *pc1=((CAPACITOR *)p1), *pc2=((CAPACITOR *)p2);
  if (pc1->C<pc2->C) return -1;
  if (pc1->C>pc2->C) return 1;
  return 0;
  }

/*** compare resistor parameters ***/
int prescmp(void *p1, void *p2)
  {
  RESISTOR *pr1=((RESISTOR *)p1), *pr2=((RESISTOR *)p2);
  if (pr1->G<pr2->G) return -1;
  if (pr1->G>pr2->G) return 1;
  return 0;
  }

/*** compare devices ***/
int pdevcmp(void *p1, void *p2)
  {
  DEVICE *pdev1=((DEVICE *)p1),*pdev2=((DEVICE *)p2);
  int c;
  c=pdev1->device_func - pdev2->device_func;
  if (c!=0) return c;
  if (isTransistor(pdev1)) return ptranscmp(pdev1->sub.trans,pdev2->sub.trans);
  if (isDiode(pdev1))      return pdiodecmp(pdev1->sub.diode,pdev2->sub.diode);
  if (isCapacitor(pdev1))  return pcapcmp(pdev1->sub.cap,pdev2->sub.cap);
  if (isResistor(pdev1))   return prescmp(pdev1->sub.res,pdev2->sub.res);
  return 0;
  }

/*************************** debugging output **********************************/

void print_nodes(FILE *fout, int Nnodes, NODE **nodes)
  {
  int i;
  for (i=0; i<Nnodes; i++)
    {
    fprintf(fout,"\"%s\"",get_node_name(nodes[i]));
    if (i+1<Nnodes) fprintf(fout,", ");
    }
  }

void print_device(FILE *fout, DEVICE *pdev)
  {
  int Nnodes;
  NODE **nodes;
  MODEL *model;
  DECK *deck;
  char *name;
  Nnodes=device_Nnodes(pdev);
  nodes=device_nodes(pdev);
  name=pdev->name!=NULL ? pdev->name : "";
  if      (isVoltageSource(pdev))
    {
    fprintf(fout, "/* voltage source %s(",name);
    print_nodes(fout,Nnodes,nodes);
    fprintf(fout,") */\n");
    }
  else if (isCurrentSource(pdev))
    {
    fprintf(fout, "/* current source %s(",name);
    print_nodes(fout,Nnodes,nodes);
    fprintf(fout,") */\n");
    }
  else if (isTransistor(pdev))
    {
    TRANSISTOR *t = pdev->sub.trans;
    model = t->model;
    deck = model->deck;
    fprintf(fout,".bsim4 \"%s\" \"%s\" %g; ",deck->filename,deck->lib,deck->temperature);
    fprintf(fout,"%s %s(",model->type==NTYPE ? TYPE_N_TRANSISTOR : TYPE_P_TRANSISTOR,name);
    print_nodes(fout,Nnodes,nodes);
    fprintf(fout,") (%g, %g, %g, %g, %g, %g, %g, %g)\n",
            t->W,t->L,t->AS,t->PS,t->NRS,t->AD,t->PD,t->NRD);
    }
  else if (isDiode(pdev))
    {
    DIODE *d = pdev->sub.diode;
    model = d->model;
    deck = model->deck;
    fprintf(fout,".bsim4 \"%s\" \"%s\" %g; ",deck->filename,deck->lib,deck->temperature);
    fprintf(fout,"%s %s(",model->type==NTYPE ? TYPE_N_DIODE : TYPE_P_DIODE,name);
    print_nodes(fout,Nnodes,nodes);
    fprintf(fout,") (%g, %g, %g, %g)\n",
            d->W,d->L,d->AS,d->PS);
    }
  else if (isCapacitor(pdev))
    {
    fprintf(fout,"%s %s(",TYPE_CAP,name);
    print_nodes(fout,Nnodes,nodes);
    fprintf(fout,") (%g)\n",pdev->sub.cap->C);
    }
  else if (isResistor(pdev))
    {
    fprintf(fout,"%s %s(",TYPE_RES,name);
    print_nodes(fout,Nnodes,nodes);
    fprintf(fout,") (%g)\n",1/pdev->sub.res->G);
    }
  }
