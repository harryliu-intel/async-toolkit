/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** compile options ***/

#undef DEBUG
#define NDEBUG
#undef TEMPERATURE_EFFECTS
#undef ADAPTIVE_TIMESTEP

/**
 * Select the precision of REAL numbers.  Used only for sparse matrix
 * and device parameters to save memory.  Must be float for SIMD code
 * to work.  Use doubles for everything else to avoid nasty precision
 * problems.
 **/
typedef float REAL;

/*** LIST data structures ***/

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    char **pc;
    int *i;
    double *f;
    struct _list **list;

    struct _parsecell **parsecell;
    struct _parseaction *parseaction;
    struct _parsenode **parsenode;
    struct _modifier **modifier;
    struct _device *device;
    struct _exclusive **pex;
    struct _fanin *fanin;
    struct _scenario *scenario;

    struct _node *node,**pn;
    struct _device *dev,**pdev;
    struct _block *block,**pblock;
    struct _matrix_entry *entry;

    struct _op *op;
    struct BIOP *biop;
    struct _assignment *assignment;
    struct _fmla *fmla;

    struct DIGITAL_NODE **dnode;
    struct DIGITAL_RULE **drule;
    struct DIGITAL_GROUP **dgrp;

    struct MODEL **model;
    struct DECK *deck;
    } p;
  } LIST;

/*** include file ***/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <malloc.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>
#include <readline/readline.h>
#include <readline/history.h>
#ifdef SUN
#include <alloca.h>
#endif
#include "prefetch.h"
#include "misc.h"
#include "leak.h"
#include "list.h"
#include "newlex.h"
#include "path.h"
#include "tracelib.h"
#include "options.h"
#include "bigint.h"
#include "glob.h"

/*** macros ***/

#define max(a,b) (a>b?a:b)
#define min(a,b) (a<b?a:b)
#define step(x) (x>=0?1:0)
#define seq(name1,name2) (strcmp(name1,name2)==0)

/*** definitions ***/

#define REORDERMEM 256*1024*1024 /*** 256MB for reorder_trace ***/
#define STACKMAX 1024
#define TINY 1e-8

#define OP_CONST -1
#define OP_TIME -2
#define OP_E -3
#define OP_NE -4
#define OP_GT -5
#define OP_GTE -6
#define OP_LT -7
#define OP_LTE -8
#define OP_AND -9
#define OP_OR -10
#define OP_NOT -11
#define OP_TRUE -12
#define OP_FALSE -13
#define OP_ADD -14
#define OP_SUB -15
#define OP_MUL -16
#define OP_DIV -17
#define OP_ABS -18
#define OP_IF -19
#define OP_BOOL -20
#define OP_MINUS -21
#define OP_PLUS -22
#define OP_SIN -23
#define OP_COS -24
#define OP_EXP -25
#define OP_LOG -26
#define OP_SGN -27
#define OP_MAX -28
#define OP_MIN -29
#define OP_TAN -30
#define OP_TANH -31
#define OP_COSH -32
#define OP_SINH -33
#define OP_PWR -34
#define OP_SQRT -35
#define OP_INT -36
#define OP_FLOOR -37
#define OP_CEIL -38
#define OP_SELMIN -39
#define OP_AGAUSS -40

#define ACTION_DEVICE 0
#define ACTION_INSTANCE 1
#define ACTION_ASSIGNMENT 2
#define ACTION_ADDLEVEL 3
#define ACTION_REMOVELEVEL 4
#define ACTION_FMLA 5
#define ACTION_WIRE 6
#define ACTION_PRS 7
#define ACTION_EXCLUSIVE 8
#define ACTION_MODEL 9
#define ACTION_GMA 10
#define ACTION_GROUP 11

#define DoInit 0
#define DoStart 1
#define DoRelax 2
#define DoFinish 3

#define BIOP_TIME (BIOP_USR+0)
#define BIOP_GRP  (BIOP_USR+1)

#define NUM_NOTIFY 1000000

#define SIM_ALL   0
#define SIM_BUMP  1
#define SIM_DELAY 2
#define SIM_LEAK  3

/*** SIMD data types ***/
#ifdef SIMD
typedef float v4sf __attribute__ ((mode(V4SF)));
// typedef float v4sf __attribute__ ((vector_size(4*sizeof(float))));
typedef union {
  v4sf v;
  float f[4];
} V4;
#define scalar_to_v4sf(y,x) {v4sf t = {x,x,x,x}; y=t;}
#define v4sf_to_scalar(y,x,i) {V4 t; t.v=x; y=t.f[i];}
#endif

/*** expression data structures ***/

typedef struct _op
  {
  int type;
  double value;
  } OP;

typedef struct _fmla
  {
  LIST *nodes;
  LIST *ops;
  } FMLA;

typedef struct _assignment
  {
  char *name;
  double value;
  } ASSIGNMENT;

/*** parse data structures ***/

typedef struct _parsecell
  {
  char *name;
  LIST *nodes,*parms,*actions;
  } PARSECELL;

typedef struct _parseaction
  {
  unsigned line,action:4,flags:8,unnamed:1;
  int delay;
  char *name,*type;
  LIST *nodes,*parms;
  } PARSEACTION;

typedef struct _parsenode
  {
  int num,dnum,pnum;
  unsigned global:2,critical:1,unnamed:3,analog_used:1,active_used:1,fixed:1,
    digital_used:1,group_used:1,created:2,subnet:2,internal:1,top_port:1;
  char *name;
  struct _parsenode *palias,*proot;
  } PARSENODE;

typedef struct _modifier // modifies behavior of a subcell
  {
  char *type, *name;      // cell type and instance names
  unsigned env:1, skip:1, // modifications to make when you construct it
    applied:1;            // mark when used
  } MODIFIER;

typedef struct _fanin // for alint custom scenarios
  {
  struct _node *pn;
  unsigned aggressor:1,initial:1;
  } FANIN;

typedef struct _scenario // for alint custom scenarios
  {
  int dir; // use dn==0, up==1, either==2
  int sim; // use SIM_DELAY/SIM_BUMP/SIM_LEAK/SIM_ALL
  LIST *fanin; // list of FANIN's
  } SCENARIO;

/*** more include files after data structures defined ***/

#include "digital_sim.h"
#include "circuit.h"
#include "model.h"

/*** function prototypes ***/

void circuit_warning();
int paddrcmp (void *p1, void *p2);
double evaluate_by_nodes(int Nops, OP *ops, int Nnodes, NODE **nodes, double t);
void prepare_circuit(CIRCUIT *circuit);
int partition_circuit(CIRCUIT *circuit);
void evaluate_devices(CIRCUIT *circuit, double mAq, double mAi, double mBq, double mBi, double time, double h);
void simulate_circuit(CIRCUIT *circuit, int interactive, int report);
DIGITAL_NODE *simulate_circuit_inner(CIRCUIT *circuit);
int alint(CIRCUIT *circuit, char *AlintVictim, LIST *CustomScenarios, int CustomForce);
void set_subnets(CIRCUIT *circuit);
void propagate_voltages(CIRCUIT *circuit, int dir, int d);
void set_true_false_mid();
DIGITAL_NODE *analog2digital(CIRCUIT *circuit);
void digital2analog(CIRCUIT *circuit, double mAq, double mAi, double mBq, double mBi, double time, double h);
LIST list_wrapper(int max, int size, int mem, void *p);

void mark_transistor_networks(CIRCUIT *circuit);
void mark_gates(CIRCUIT *circuit);
void mark_capacitors(CIRCUIT *circuit);
CIRCUIT create_subcircuit(CIRCUIT *circuit);
void free_subcircuit(CIRCUIT *circuit);
void mark_clear(CIRCUIT *circuit);
void mark_fanin(CIRCUIT *circuit, int max_levels);
int marked_subcircuit(CIRCUIT *circuit, char *filename);

int pstrcmp (void *p1, void *p2);
int passigncmp(void *p1, void *p2);
int passignstrcmp(void *p1, void *p2);
int pmodifiercmp(void *p1, void *p2);
int pmodifierprefixcmp(void *p1, void *p2);
int pcellcmp (void *p1, void *p2);
int pcellactioncmp (void *p1, void *p2);
int pcellnamecmp (void *p1, void *p2);

void alloc_super_strings();
void free_super_strings();
char *parse_nodename(LEX *lex);
char *super_parse_nodename(LEX *lex);
double evaluate(LIST *ops, double t, double *v);
double evaluate_by_nodes(int Nops, OP *ops, int Nnodes, NODE **nodes, double t);
void parse_expression(LEX *lex, LIST *ops, LIST *nodes, int precedence);
void print_expression(FILE *fout, LIST *ops, LIST *nodes);
int get(LIST *assignments, char *name, double *pv);
void assign(LIST *assignments, char *name, double value);
void add_assignment_level(LIST *assignments);
void remove_assignment_level(LIST *assignments);
LIST *duplicate_assignment_list(LIST *l);
int assignment_list_cmp(void *p1, void *p2);
double evaluate_by_name(LIST *assignments, LIST *ops, LIST *nodes);
LIST *evaluate_parms(LIST *assignments, LIST *fmlas);
FMLA create_fmla();
void free_fmla(FMLA fmla);
void free_string_list(LIST *l);
void free_fmla_list(LIST *l);

MODIFIER *get_modifier(LIST *modifiers, char *type, char *name);
void parse_circuit(char *basename, LIST *cells,
                   LIST *globals, LIST *criticalnets, LIST *modifiers);
void free_parse_memory(LIST *cells, LIST *globals, LIST *criticalnets, LIST *modifiers);
CIRCUIT construct_circuit(char *basename, LIST *cells,
                          LIST *globals, LIST *criticalnets, LIST *modifiers);
void write_names(CIRCUIT *circuit);

int pparsenodecmp (void *p1, void *p2);
int pnodenamecmp (void *p1, void *p2);
PARSENODE *root_alias(PARSENODE *pn);
void alias_parsenodes(PARSENODE *pnode0, PARSENODE *pnode1);
PARSENODE *add_parsenode(LIST *names, LIST *globals, LIST *criticalnets,
                         char *prefix, char *name,
                         int fixed, int unnamed,
			 int analog_used, int active_used,
                         int digital_used, int group_used);
int better_name_cmp(PARSENODE *pn1, PARSENODE *pn2);
int node_cmp(void *p1, void *p2);
int dnode_cmp(void *p1, void *p2);
PARSENODE *find_parsenode(CIRCUIT *circuit, char *prefix, char *name);
NODE *find_node(CIRCUIT *circuit, char *prefix, char *name);
DIGITAL_NODE *find_digital_node(CIRCUIT *circuit, char *prefix, char *name);
DIGITAL_GROUP *find_digital_group(CIRCUIT *circuit, char *prefix, char *name);

void save_state(CIRCUIT *circuit);
void restore_state(CIRCUIT *circuit);
void append_tracefile(CIRCUIT *circuit);
void start_tracefile(CIRCUIT *circuit);
void prepare_tracefile(CIRCUIT *circuit);
void reorder_tracefile(char *run);
void finish_tracefile(CIRCUIT *circuit);

void parse_user(char *line, CIRCUIT *circuit);
void dsim_init();
void dsim_free(CIRCUIT *circuit);

/*** global variables ***/

extern char *topCell;
extern LIST *bumpCC,*bumpTau;
extern LIST *delayCC,*delayTau,*delayFast,*delayCap;
extern LIST *threshCC,*threshTau,*threshPercent;
extern int doLeakage; /*** enables alint leakage simulations ***/
extern int doInverterLeakage; /*** enables alint leakage simulations for inverters ***/
extern int noResistors; /*** if true, turn resistors into wires ***/
extern int trace_smoothV; /*** save smoothV instead of V to trace file ***/
extern double minCouplingCapacitance; /*** lump capacitors smaller than this ***/
extern int traceResistiveSubnets,traceInternalNodes,traceAllNodes;
extern int CountScenarios; /*** just count alint scenarios ***/
extern int doCouplingCap; /*** do coupling cap aggressors for alint ***/
extern int verbose; /*** more output for alint ***/
extern int debug_alint; /*** enable debugging for alint ***/
extern char *WorkDir; /*** directory for trace/names/check files ***/
extern char *TYPE_EXCLLO,*TYPE_EXCLHI,*TYPE_EXCLCC,*TYPE_NOCC,*TYPE_RES,*TYPE_CAP,
  *TYPE_N_TRANSISTOR,*TYPE_P_TRANSISTOR,*TYPE_N_DIODE,*TYPE_P_DIODE,
  *TYPE_BSIM4,*TYPE_CORNER,*TYPE_CURRENT_SOURCE,*TYPE_VOLTAGE_SOURCE;
extern char *mark_name[MARK_MAX];
extern int bsim4_warn;
extern int monte_carlo_seed;
extern int circuit_warnings, max_circuit_warnings;
