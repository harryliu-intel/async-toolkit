/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"
#define gmin 0

void test_transistor(double Type, double W, double L,
		     double AS, double PS, double AD, double PD,
                     double SA, double SB, double SC, double SD, double NF,
                     double SCA, double SCB, double SCC,
                     double minVg, double maxVg, double stepVg,
                     double minVd, double maxVd, double stepVd)
  {
  MODEL *m;
  FILE *IS,*ID,*IG,*IB,*QS,*QD,*QG,*QB;
  double Vg,Vs,Vd,Vb,Vds;
  double TQ[6],TI[6],TdQ[36],TdI[36];
  IS=fopen("IS.out","wt");
  ID=fopen("ID.out","wt");
  IG=fopen("IG.out","wt");
  IB=fopen("IB.out","wt");
  QS=fopen("QS.out","wt");
  QD=fopen("QD.out","wt");
  QG=fopen("QG.out","wt");
  QB=fopen("QB.out","wt");

  m=MODEL_FindModel(Type,W,L);
  Vb = 0;
  Vs = 0;

  for (Vg=minVg; Vg<=maxVg; Vg+=stepVg)
    {
    for (Vd=minVd; Vd<=maxVd; Vd+=stepVd)
      {
      if (m->bsim==4)
        {
        BSIM4model *m4;
        m4 = (BSIM4model *) m;
        /* need to update
        BSIM4_Transistor(m4,1,Type*Vs,Type*Vd,Type*Vg,Type*Vb,Type*Vs,Type*Vd,
                         W,L,AS,PS,0,AD,PD,0,
                         SA,SB,SC,SD,NF,SCA,SCB,SCC,
                         TQ,TI,TdQ,TdI);
        */
        }
      Vds=(Vd-Vs)*Type;
      fprintf(IS,"%g %g\n",Vds,TI[0]);
      fprintf(ID,"%g %g\n",Vds,TI[1]);
      fprintf(IG,"%g %g\n",Vds,TI[2]);
      fprintf(IB,"%g %g\n",Vds,TI[3]);
      fprintf(QS,"%g %g\n",Vds,TQ[0]);
      fprintf(QD,"%g %g\n",Vds,TQ[1]);
      fprintf(QG,"%g %g\n",Vds,TQ[2]);
      fprintf(QB,"%g %g\n",Vds,TQ[3]);
      }
    fprintf(IS,"\n");
    fprintf(ID,"\n");
    fprintf(IG,"\n");
    fprintf(IB,"\n");
    fprintf(QS,"\n");
    fprintf(QD,"\n");
    fprintf(QG,"\n");
    fprintf(QB,"\n");
    }
  }

void banner()
  {
  fprintf(stderr,"USAGE: test_model\n"
          "  -bsim 4\n"
          "  -deck   deck\n"
          "  -lib    lib\n"
          "  -device device\n"
          "  -n | -p\n"
          "  -w W\n"
          "  -l L\n"
          "  [-temp temp]\n"
          "  [-minVg minVg]\n"
          "  [-maxVg maxVg]\n"
          "  [-stepVg stepVg]\n"
          "  [-minVd minVd]\n"
          "  [-maxVd maxVd]\n"
          "  [-stepVd stepVd]\n"
          "  [-as AS]\n"
          "  [-ps PS]\n"
          "  [-ad AD]\n"
          "  [-pd PD]\n");          
  exit(1);
  }

int main(int argc, char *argv[])
  {
  int i,bsim=-1;
  double Type=0,Temp=125,W=-1,L=-1,AS=0,PS=0,AD=0,PD=0;
  double SA=-1,SB=-1,SC=-1,SD=-1,NF=-1,SCA=-1,SCB=-1,SCC=-1;
  double minVg=0,maxVg=1,stepVg=0.1;
  double minVd=0,maxVd=1,stepVd=0.01;
  char *deck=NULL,*lib=NULL,*device=NULL;
  LIST *assignments;

  /*** get command line arguments ***/
  if (argc==1) banner();
  for (i=1; i<argc; i++)
    {
    if      (strcmp(argv[i],"-deck")==0)    deck=argv[++i];
    else if (strcmp(argv[i],"-lib")==0)     lib=argv[++i];
    else if (strcmp(argv[i],"-device")==0)  device=argv[++i];
    else if (strcmp(argv[i],"-bsim")==0)    sscanf(argv[++i],"%d",&bsim);
    else if (strcmp(argv[i],"-n")==0)       Type=NMOS;
    else if (strcmp(argv[i],"-p")==0)       Type=PMOS;
    else if (strcmp(argv[i],"-w")==0)       sscanf(argv[++i],"%lf",&W);
    else if (strcmp(argv[i],"-l")==0)       sscanf(argv[++i],"%lf",&L);
    else if (strcmp(argv[i],"-temp")==0)    sscanf(argv[++i],"%lf",&Temp);
    else if (strcmp(argv[i],"-minVg")==0)   
      {sscanf(argv[++i],"%lf",&minVg); maxVg=minVg;}
    else if (strcmp(argv[i],"-maxVg")==0)   sscanf(argv[++i],"%lf",&maxVg);
    else if (strcmp(argv[i],"-stepVg")==0)  sscanf(argv[++i],"%lf",&stepVg);
    else if (strcmp(argv[i],"-minVd")==0)
      {sscanf(argv[++i],"%lf",&minVd); maxVd=minVd;}
    else if (strcmp(argv[i],"-maxVd")==0)   sscanf(argv[++i],"%lf",&maxVd);
    else if (strcmp(argv[i],"-stepVd")==0)  sscanf(argv[++i],"%lf",&stepVd);
    else if (strcmp(argv[i],"-as")==0)      sscanf(argv[++i],"%lf",&AS);
    else if (strcmp(argv[i],"-ps")==0)      sscanf(argv[++i],"%lf",&PS);
    else if (strcmp(argv[i],"-ad")==0)      sscanf(argv[++i],"%lf",&AD);
    else if (strcmp(argv[i],"-pd")==0)      sscanf(argv[++i],"%lf",&PD);
    else {fprintf(stderr,"ERROR: unrecognized option %s\n",argv[i]); return 1;}
    }
  if ((deck==NULL)||(lib==NULL)||(device==NULL)||
      (W<0)||(L<0)||(bsim!=4)||(Type==0))
    banner();

  /*** read in model parameters ***/
  alloc_super_strings();
  assignments = list_create(sizeof(LIST *));
  add_assignment_level(assignments);
  MODEL_SetModel(bsim,assignments,deck,lib,device,Temp);
  
  /*** test some circuits ***/
  test_transistor(Type,W,L,AS,PS,AD,PD,SA,SB,SC,SD,NF,SCA,SCB,SCC,
                  minVg,maxVg,stepVg,minVd,maxVd,stepVd);
  free_super_strings();

  /** free and exit **/
  remove_assignment_level(assignments);
  list_free(assignments);
  return 0;
  }
