/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#define FORCE_NONE 0
#define FORCE_DN   1
#define FORCE_UP   2
#define FORCE_MID  3
#define FORCE_V    4

#define EXCLLO 0
#define EXCLHI 1
#define EXCLCC 2
#define NOCC 3

#define MARK_UNKNOWN 0
#define MARK_VICTIM 1
#define MARK_GLOBAL 2
#define MARK_GATE 3
#define MARK_SHARED 4
#define MARK_NINTERNAL 5
#define MARK_PINTERNAL 6
#define MARK_SHARED_NINTERNAL 7
#define MARK_SHARED_PINTERNAL 8
#define MARK_INVERSE 9
#define MARK_SHARED_INVERSE 10
#define MARK_LOAD 11
#define MARK_COUPLING 12
#define MARK_BARRIER 13
#define MARK_SHARED_GATE 14
#define MARK_PRIME 15
#define MARK_MAX 16

/*** partition and simulate data structures ***/

typedef void DEVICE_FUNC(struct _device *, double, double, double, double, double);

typedef struct _block
  {
  struct _node **nodes; /* the local and neighbor nodes */
  int active, /* has any node in this block moved by maxerr */
    Nlocal, /* number of local nodes */
    Nnodes; /* the total number of local and neighbor nodes */
  } BLOCK;

typedef struct _node
  {
  REAL *A; /* sparse matrix entries */
  int diagonal_index; /* index of diagonal element of A */
  DIGITAL_NODE *digital_node;
  PARSENODE *name; /* name and aliases for interactive debug */
  BLOCK *pblock; /* block pointer */
  struct _node *seed; /* for alint */
  struct _node *map; /* for alint */
  double V; /* current voltage */
  double X; /* last dV/dt step */
  double smoothV; /* low pass filtered V */
  double smoothX; /* delta of smoothV from last timestep */
  double B; /* vector of sparse matrix */
  REAL CG; /* lumped capacitance */
  REAL C; /* capacitance accumulator for alint purposes */
  int jblock; /* block number, used only for partitioning */
  REAL forceV; /* target voltage if force==FORCE_V */
  unsigned watch:1,fixed:1,floatingC:1,vsource:1,
           /* for A/D cosim */
           pending:2,unstab:2,
           /* for alint */
           top_port:1, /* is node port of top cell */
           portG:1,portN:1,portP:1, /* does this node connect to a gate, nmos, or pmos */
           mark:4,force:3,old_force:3,aggressor:1,
           custom_force:3,custom_aggressor:1,
           exposed:1,initial_exposed:1,
           relevant:1,canonical:1,
           good:1,bad:1,oldgood:1,oldbad:1,
           leaky:1,nocc:1;
  } NODE;

typedef struct _capacitor
  {
  NODE *nodes[2];
  REAL *Aentry[4];
  REAL C;
  } CAPACITOR;

typedef struct _resistor
  {
  NODE *nodes[2];
  REAL *Aentry[4];
  REAL G;
  REAL I; // cache current here
  } RESISTOR;

typedef struct _transistor
  {
  NODE *nodes[6]; /*** s,d,g,b,sp,dp ***/
  REAL *Aentry[36];
  REAL W,L,AS,PS,NRS,AD,PD,NRD;
  REAL SA,SB,SC,SD,NF,SCA,SCB,SCC;
  struct MODEL *model;
  } TRANSISTOR;

typedef struct _diode
  {
  NODE *nodes[2]; /*** s,b ***/
  REAL *Aentry[4];
  REAL W,L,AS,PS;
  struct MODEL *model;
  } DIODE;

typedef struct _source
  { 
  NODE **nodes; /*** node[0] is output ***/
  struct _op *ops;
  REAL **Aentry;
  int Nnodes,Nops;
  REAL I; // cache current here
  } SOURCE;

typedef struct _device
  {
  DEVICE_FUNC *device_func;
  char *name; // optional device name for measuring currents
  union
    {
    void *any;
    CAPACITOR *cap;
    RESISTOR *res;
    TRANSISTOR *trans;
    DIODE *diode;
    SOURCE *source;
    } sub;
  } DEVICE;

typedef struct _exclusive
  {
  NODE **nodes;
  int type;
  int Nnodes;
  } EXCLUSIVE;

typedef struct _circuit
  {
  /*** description of circuit ***/
  int Nnodes,Ndevices,Nblocks,Ndnodes,Ndrules,Ngmas,Ngroups,Nexclusives;
  NODE *nodes;
  DEVICE *devices;
  BLOCK *blocks;
  DIGITAL_NODE *dnodes;
  DIGITAL_RULE *drules;
  DIGITAL_GMA *gmas;
  DIGITAL_GROUP *groups;
  EXCLUSIVE *exclusives;

  /*** more circuit state ***/
  LIST *names;     /* full name space */
  unsigned trace_started:1, /* have names/tracefile been started yet? */ 
           do_restore:1, /* should we restore from checkpoint? */
           do_reorder:1; /* should we reorder when we finish? */
  double time;     /* analog time of simulation */
  FILE *ftrace;    /* trace file */
  char *run;       /* base name of run */
  } CIRCUIT;

/*** function prototypes ***/

DEVICE_FUNC bsim4_transistor;
DEVICE_FUNC bsim4_diode;
DEVICE_FUNC resistor;
DEVICE_FUNC capacitor;
DEVICE_FUNC voltage_source;
DEVICE_FUNC current_source;

int isTransistor(DEVICE *pdev);
int isDiode(DEVICE *pdev);
int isCapacitor(DEVICE *pdev);
int isResistor(DEVICE *pdev);
int isSource(DEVICE *pdev);
int isCurrentSource(DEVICE *pdev);
int isVoltageSource(DEVICE *pdev);

int ptranscmp(void *p1, void *p2);
int pdiodecmp(void *p1, void *p2);
int pcapcmp(void *p1, void *p2);
int prescmp(void *p1, void *p2);
int pdevcmp(void *p1, void *p2);

void create_source(DEVICE *pdev, int Nnodes, NODE **nodes,
                   int Nops, struct _op *ops, int vsource);
void create_capacitor(DEVICE *pdev, NODE *A, NODE *B, REAL C);
void create_resistor(DEVICE *pdev, NODE *A, NODE *B, REAL R);
void create_source_resistor(DEVICE *pdev, int type, int source,
                            NODE *A, NODE *B,
                            REAL W, REAL L, REAL NRS, REAL RSC);
void create_diode(DEVICE *pdev, int type,
                  NODE *s, NODE *b,
                  REAL W, REAL L, REAL AS, REAL PS);
void create_transistor(DEVICE *pdev, int type,
                       int Nnodes, NODE **nodes,
                       int Nparms, double *parms);

int device_Nnodes(DEVICE *pdev);
NODE **device_nodes(DEVICE *pdev);
REAL **device_Aentry(DEVICE *pdev);
void free_device(DEVICE *pdev);
int device_mem(DEVICE *pdev);

void create_node(NODE *pn, DIGITAL_NODE *dnode, int watch, int fixed,
                 int top_port, PARSENODE *name);
void free_node(NODE *pn);
void free_block(BLOCK *pblock);
void free_exclusive(EXCLUSIVE *pex);
CIRCUIT create_circuit();
void free_circuit(CIRCUIT *circuit);
void summarize_circuit(CIRCUIT *circuit, FILE *out);

char *get_node_name(NODE *pn);
void print_nodes(FILE *fout, int Nnodes, NODE **nodes);
void print_device(FILE *fout, DEVICE *pdev);
