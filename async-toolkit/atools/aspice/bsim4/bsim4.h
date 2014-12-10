/*** physical constants ***/
#define Charge_q   1.60219e-19
#define CONSTboltz 1.3806226e-23
#define CONSTroot2 1.41421356237309504880
#define CONSTvt0 CONSTboltz * (27 + CONSTCtoK ) / Charge_q

/*** BSIM4 constants ***/
#define BSIM4numStates 29
#define BSIM4numNodes 6

/*** SPICE typedefs ***/
typedef struct CKTcircuit
  {
  double CKTrhsOld[BSIM4numNodes];
  } CKTcircuit;

typedef struct IFvalue
  {
  int iValue;
  double rValue;
  char *sValue;
  } IFvalue;

typedef struct IFparm
  {
  char *name;
  int num;
  int type;
  char *comment;
  } IFparm;

typedef char *IFuid;

/*** boolean and integer defines ***/
#define TRUE 1
#define OK 0
#define E_BADPARM 1
#define IF_REAL 0
#define IF_FLAG 1
#define IF_INTEGER 2

/*** constant definitions ***/
#define CHARGE Charge_q

/*** compilation options ***/
#undef PREDICTOR
#undef NEWCONV

/*** arithmetic macros ***/
#define MAX(a,b) ((a)>(b)?(a):(b))
#define FABS(a) fabs(a)

/*** other macros ***/
#define IOP(name,num,type,comment) {name,num,type,comment}
#define OP(name,num,type,comment)  {name,num,type,comment}

/*** main bsim4 header definitions ***/
#include "bsim4def.h"

/************************ INTERNAL FUNCTION PROTOTYPES ************************/

int BSIM4mParam(int param, IFvalue *value, BSIM4model *inMod);
int BSIM4mAsk(BSIM4model *inst, int which, IFvalue *value);
int BSIM4RdseffGeo(double nf, int geo, int rgeo, int minSD,
                   double Weffcj, double Rsh, double DMCG,
                   double DMCI, double DMDG, int Type,
                   double *Rtot);
int BSIM4PAeffGeo(double nf, int geo, int minSD, double Weffcj,
                  double DMCG, double DMCI, double DMDG,
                  double *Ps, double *Pd, double *As, double *Ad);
int BSIM4RdsEndIso(double Weffcj, double Rsh, double DMCG,
                   double DMCI, double DMDG, double nuEnd,
                   int rgeo, int Type,
                   double *Rend);
int BSIM4RdsEndSha(double Weffcj, double Rsh, double DMCG, double DMCI, double DMDG,
                   double nuEnd, int rgeo, int Type, double *Rend);

int BSIM4_SetupModel(BSIM4model *model);
int BSIM4_SetupInstance(BSIM4model *model, BSIM4instance *here, CKTcircuit *ckt, int warn);
int BSIM4_TempModel(BSIM4model *model);
int BSIM4_TempInstance(BSIM4model *model, BSIM4instance *here, int warn);
int BSIM4_CheckModel(BSIM4model *model, FILE *fplog);
int BSIM4_CheckInstance(BSIM4model *model, BSIM4instance *here, FILE *fplog);
int BSIM4_Evaluate(BSIM4model *model, BSIM4instance *here, CKTcircuit *ckt,
                   double Q[6], double I[6], double dQ[36], double dI[36]);

/************************ SPICE FUNCTION PROTOTYPES ***************************/

#define DEVfetlim(v,vlim,von) (v)
#define DEVlimvds(v,vlim) (v)
#define DEVpnjlim(v,vlim,vt0,vcrit,Check) (v)

int CKTmkVolt(CKTcircuit *ckt, int *num, char *BSIM4name, char *portname);
int NIintegrate(CKTcircuit *ckt, double *geq, double *ceq, double zero, double q);

/*********************** EXTERNAL FUNCTION DECLARATIONS ***********************/

double BSIM4_Resistance(BSIM4model *m, double Squares, double Rcontact);
void BSIM4_Transistor(DEVICE *pdev, int check,
                      double Q[6], double I[6], double dQ[36], double dI[36]);
void BSIM4_PrintModel(BSIM4model *m, FILE *fout);
int BSIM4_ParseParameter(BSIM4model *m, char *s, LEX *lex, LIST *assignments);
