#include "../aspice.h"

int bsim4_warn=0;

/****************************** SPICE functions ********************************/

int nextNodeNum=0;

int CKTmkVolt(CKTcircuit *ckt, int *num, char *BSIM4name, char *portname)
  {
  *num = nextNodeNum;
  nextNodeNum++;
  return 0;
  }

int NIintegrate(CKTcircuit *ckt, double *geq, double *ceq, double zero, double q)
  {
  return 0;
  }

/******************** ASPICE WRAPPER OF TRANSISTOR/DIODE **************************/

/*** BSIM4 source/drain resistance (SIMPLE!) ***/
double BSIM4_Resistance(BSIM4model *m, double Squares, double Rcontact)
  {
  double R=0;
  if (Squares>0) R += m->BSIM4sheetResistance * Squares;
  if (Rcontact>0) R += Rcontact;
  return R;
  }

void BSIM4_Transistor(DEVICE *pdev, int check,
		      double Q[6], double I[6], double dQ[36], double dI[36]) {
  static BSIM4instance instance, *here = &instance;
  static struct bsim4SizeDependParam sized, *pParam = &sized;
  static CKTcircuit circuit, *ckt = &circuit;
  static BSIM4model *lastModel = NULL;
  int i;

  /*** get transistor voltages and parameters ***/
  BSIM4model *m = (BSIM4model *) pdev->sub.trans->model;
  NODE **pn=pdev->sub.trans->nodes;
  double Vs=pn[0]->V, Vd=pn[1]->V, Vg=pn[2]->V, Vb=pn[3]->V, Vsp=pn[4]->V, Vdp=pn[5]->V;
  double W=pdev->sub.trans->W, L=pdev->sub.trans->L;
  double AS=pdev->sub.trans->AS, PS=pdev->sub.trans->PS, NRS=pdev->sub.trans->NRS;
  double AD=pdev->sub.trans->AD, PD=pdev->sub.trans->PD, NRD=pdev->sub.trans->NRD;
  double SA=pdev->sub.trans->SA, SB=pdev->sub.trans->SB, SC=pdev->sub.trans->SC;
  double SD=pdev->sub.trans->SD, NF=pdev->sub.trans->NF;
  double SCA=pdev->sub.trans->SCA, SCB=pdev->sub.trans->SCB, SCC=pdev->sub.trans->SCC;

  /*** initialize static data structures ***/
  if (lastModel==NULL)
    {
    /*** fill in default ckt fields ***/
    for (i=0; i<BSIM4numNodes; i++)  ckt->CKTrhsOld[i]=0;

    /*** clear last/here/pParam structures ***/
    for (i=0; i<sizeof(BSIM4instance); i++) ((char *) here)[i]=0;
    here->pParam = pParam;
    for (i=0; i<sizeof(struct bsim4SizeDependParam); i++) ((char *) pParam)[i]=0;

    /*** the following parameters must always be given ***/
    here->BSIM4wGiven=1;
    here->BSIM4lGiven=1;
    }

  /*** set up here/pParam again only if model/parameters changed ***/
  if ((m!=lastModel) || 
      (W!=here->BSIM4w) || 
      (L!=here->BSIM4l) ||
      (AS!=here->BSIM4sourceArea) || 
      (PS!=here->BSIM4sourcePerimeter) ||
      (NRS!=here->BSIM4sourceSquares) ||
      (AD!=here->BSIM4drainArea) || 
      (PD!=here->BSIM4drainPerimeter) ||
      (NRD!=here->BSIM4drainSquares) ||
      (SA!=here->BSIM4sa) ||
      (SB!=here->BSIM4sb) ||
      (SC!=here->BSIM4sc) ||
      (SD!=here->BSIM4sd) ||
      (NF!=here->BSIM4nf) ||
      (SCA!=here->BSIM4sca) ||
      (SCB!=here->BSIM4scb) ||
      (SCC!=here->BSIM4scc))
    {
    /*** required parameters ***/
    lastModel=m;
    here->BSIM4w=W;
    here->BSIM4l=L;

    /*** standard optional parameters ***/
    here->BSIM4sourceArea=AS;      here->BSIM4sourceAreaGiven=AS>=0;
    here->BSIM4sourcePerimeter=PS; here->BSIM4sourcePerimeterGiven=PS>=0;
    here->BSIM4sourceSquares=NRS;  here->BSIM4sourceSquaresGiven=NRS>=0;
    here->BSIM4drainArea=AD;       here->BSIM4drainAreaGiven=AD>=0;
    here->BSIM4drainPerimeter=PD;  here->BSIM4drainPerimeterGiven=PD>=0;
    here->BSIM4drainSquares=NRD;   here->BSIM4drainSquaresGiven=NRD>=0;

    /*** more optional parameters ***/
    here->BSIM4sa=SA;   here->BSIM4saGiven=SA>=0;
    here->BSIM4sb=SB;   here->BSIM4sbGiven=SB>=0;
    here->BSIM4sc=SC;   here->BSIM4scGiven=SC>=0;
    here->BSIM4sd=SD;   here->BSIM4sdGiven=SD>=0;
    here->BSIM4nf=NF;   here->BSIM4nfGiven=NF>=0;
    here->BSIM4sca=SCA; here->BSIM4scaGiven=SCA>=0;
    here->BSIM4scb=SCB; here->BSIM4scbGiven=SCB>=0;
    here->BSIM4scc=SCC; here->BSIM4sccGiven=SCC>=0;

    /*** fill in dummy node numbers for ports ***/
    here->BSIM4sNode      = 1;
    here->BSIM4sNodePrime = 0; // set by BSIM4
    here->BSIM4dNode      = 2;
    here->BSIM4dNodePrime = 0; // set by BSIM4
    here->BSIM4gNodeExt   = 3;
    here->BSIM4gNodeMid   = 0; // set by BSIM4
    here->BSIM4gNodePrime = 0; // set by BSIM4
    here->BSIM4bNode      = 4;
    here->BSIM4bNodePrime = 0; // set by BSIM4
    here->BSIM4dbNode     = 0; // set by BSIM4
    here->BSIM4sbNode     = 0; // set by BSIM4
    here->BSIM4qNode      = 0; // set by BSIM4
    nextNodeNum = 5;

    /*** prepare this instance ***/
    BSIM4_SetupInstance(m,here,ckt,check);

    /*** renumber sNodePrime/dNodePrime if they exist ***/
    if (here->BSIM4sNodePrime != here->BSIM4sNode) here->BSIM4sNodePrime = 5;
    if (here->BSIM4dNodePrime != here->BSIM4dNode) here->BSIM4dNodePrime = 6;

    /*** check that transistor has supported number of terminals ***/
    if (here->BSIM4gNodeMid != here->BSIM4gNodeExt)
      fprintf(stderr,"WARNING: aspice doesn't support gNodeMid\n");
    if (here->BSIM4gNodePrime != here->BSIM4gNodeExt)
      fprintf(stderr,"WARNING: aspice doesn't support gNodePrime\n");
    if (here->BSIM4bNodePrime != here->BSIM4bNode)
      fprintf(stderr,"WARNING: aspice doesn't support bNodePrime\n");
    if (here->BSIM4dbNode != here->BSIM4bNode)
      fprintf(stderr,"WARNING: aspice doesn't support dbNode\n");
    if (here->BSIM4sbNode != here->BSIM4bNode)
      fprintf(stderr,"WARNING: aspice doesn't support sbNode\n");
    if (here->BSIM4qNode != 0)
      fprintf(stderr,"WARNING: aspice doesn't support qNode\n");

    /*** finish ***/
    BSIM4_TempInstance(m,here,check);
    if (check) BSIM4_CheckInstance(m,here,stderr);
    }

  /*** disable sNodePrime/dNodePrime if unused (prevents warning/drift) ***/
  if (here->BSIM4sNodePrime == here->BSIM4sNode) { Vsp=Vs; pn[4]->fixed=1; }
  if (here->BSIM4dNodePrime == here->BSIM4dNode) { Vdp=Vd; pn[5]->fixed=1; }

  /*** pass input voltages to BSIM4instance ***/
  ckt->CKTrhsOld[here->BSIM4dNodePrime] = Vdp;
  ckt->CKTrhsOld[here->BSIM4dNode]      = Vd;

  ckt->CKTrhsOld[here->BSIM4sNodePrime] = Vsp;
  ckt->CKTrhsOld[here->BSIM4sNode]      = Vs;

  ckt->CKTrhsOld[here->BSIM4gNodeExt]   = Vg;
  ckt->CKTrhsOld[here->BSIM4gNodeMid]   = Vg;
  ckt->CKTrhsOld[here->BSIM4gNodePrime] = Vg;

  ckt->CKTrhsOld[here->BSIM4bNode]      = Vb;
  ckt->CKTrhsOld[here->BSIM4bNodePrime] = Vb;
  ckt->CKTrhsOld[here->BSIM4sbNode]     = Vb;
  ckt->CKTrhsOld[here->BSIM4dbNode]     = Vb;

  /*** evaluate transistor ***/
  BSIM4_Evaluate(m,here,ckt,Q,I,dQ,dI);
}
