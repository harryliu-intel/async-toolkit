/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** constant definitions ***/

/* constants for prs node values */
#define Value0 0
#define Value1 1
#define ValueU 2

/* constants for rule state */
#define RuleOFF 0
#define RuleON  1
#define RuleU   2

/* constants for turning analog simulation on/off */
#define ANALOG_DISABLED 0
#define ANALOG_ENABLED  1
#define ANALOG_SETTLING 2

/* constants used for event times */
#define INSTANT_TIME 0x00000000 /* time of isochronic events */
#define RANDOM_TIME  0x7FFFFFFF /* time of random events */
#define LAZY_TIME    0x80000000 /* offset for lazy events */

/* constants for cosimulation */
#define LEVEL_ENV      0
#define LEVEL_LOWER    1
#define LEVEL_UPPER    2
#define LEVEL_BOUNDARY 3

/* constants for flags bitvectors */
#define FLAG_ENV        1
#define FLAG_TIMED      2
#define FLAG_INSTANT    4
#define FLAG_ISOCHRONIC 8
#define FLAG_UNSTAB     16
#define FLAG_METASTAB   32
#define FLAG_REALTIME   64

/* pick LOGFILE type */
#ifdef SST2
typedef void LOGFILE;
#else
typedef FILE LOGFILE;
#endif

/*** data structures ***/
typedef struct DIGITAL_NODE
  {
  PARSENODE *name;
  BIGINT *value, *pending;
  unsigned watch:1,log:1, // logging output
    has_digital_fanin:1,has_env_fanin:1,has_analog_fanin:1, // node usage
    unstab:2, // normal/unstab/metastab
    string:1,  // should be printed as a string
    group:1, // avoids unstabs in group assignment with redundant nodes
    effective:1,level:2,event_level:2; // cosim support
  unsigned breakpt; // count down to breakpt, or 0 for none
  unsigned tcount; // transition count
  unsigned event_time; // when will next event fire?
  int      event_index; // where is this in the event queue, or -1
  unsigned enabler_tcount; // for critical path
  int Count[2],UCount[2],Nrules[2],Ngmas;
  struct DIGITAL_NODE *enabler_node; // for critical path
  struct DIGITAL_RULE **rules[2];
  struct DIGITAL_GMA **gmas;
#ifdef SST2
  void *fiber;
  void *transaction;
#endif
  } DIGITAL_NODE;

typedef struct DIGITAL_RULE
  {
  struct DIGITAL_NODE *target;
  unsigned FalseConjuncts:8, /* sum of false inputs */
           UnknownConjuncts:8, /* sum of unknown inputs */
           dir:1, /* down=0, up=1 */
           old:2,new:2, /* state of rule */
           instant:1,realtime:1,timed:1,isochronic:1,unstab:2,level:2; /* rule flags */
  int delay;
  } DIGITAL_RULE;

/*** a guarded multiple assignment ***/
typedef struct DIGITAL_GMA
  {
  unsigned enabled:1,level:2;
  LIST *guard;       // the BIOP expression for the guard
  LIST *nodes;       // the DIGITAL_NODE's used as lvalues
  LIST *groups;      // or DIGITAL_GROUP's used as lvalues
  LIST *indices;     // the group index for each node, or NULL for non-group
  LIST *delays;      // the BIOP expressions for the delays
  LIST *flags;       // int flags for each assignment
  LIST *expressions; // the BIOP expressions for rvalues
  } DIGITAL_GMA;

/*** a group of nodes ***/
typedef struct DIGITAL_GROUP
  {
  PARSENODE *name; // name of the group
  LIST *nodes; // DIGITAL_NODE's in the group
  } DIGITAL_GROUP;

/*** track event history ***/
typedef struct HISTORY
  {
  struct DIGITAL_NODE *node,*enabler_node;
  unsigned time,tcount,enabler_tcount;
  BIGINT *value;
  } HISTORY;

/*** function prototypes ***/
void digital_initialize(int Ndnodes, DIGITAL_NODE *dnodes, DIGITAL_GROUP *groups);
void reheap(int event_index);
unsigned trivalue(BIGINT *value);
void create_digital_node(DIGITAL_NODE *dnode, PARSENODE *name);
void create_digital_rule(DIGITAL_RULE *drule,
                         int Nguards, DIGITAL_NODE **guards, int *sense,
                         DIGITAL_NODE *target, int dir,
                         int delay, int flags, int level);
void create_digital_gma(DIGITAL_GROUP *groups,
                        DIGITAL_GMA *gma,
                        LIST *guard, LIST *nodes, LIST *lgrps,
                        LIST *delays, LIST *flags, 
                        LIST *indices, LIST *expressions,
                        LIST *fanin_nodes,
                        int level);
void create_digital_group(DIGITAL_GROUP *group, PARSENODE *name);
void set_digital_node(DIGITAL_GROUP *groups, DIGITAL_NODE *dnode, BIGINT *value);
DIGITAL_NODE *cycle(DIGITAL_GROUP *groups, int timestep);
void free_digital_node(DIGITAL_NODE *dnode);
void free_digital_rule(DIGITAL_RULE *drule);
void free_digital_gma(DIGITAL_GMA *gma);
void free_digital_group(DIGITAL_GROUP *group);
void free_event_queue();
void allocate_event_queue(int size);
void allocate_history(int Max);
void assert_node(DIGITAL_NODE *dnode);
void assert_rule(DIGITAL_RULE *drule);
char *get_digital_node_name(DIGITAL_NODE *dnode);
void schedule_event(DIGITAL_NODE *dnode, BIGINT *pending, unsigned time,
                    int unstab, int level, int normal, DIGITAL_NODE *lastevent);

/*** logging ***/
void print_value(FILE *fout, BIGINT *value, int string);
void print_dnode(FILE *fout, DIGITAL_NODE *dnode);
void print_pending_dnode(FILE *fout, DIGITAL_NODE *dnode);
void print_digital_group(FILE *fout, DIGITAL_GROUP *dgrp);
void event_warning(DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message);
void event_notice(DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message);
void log_dnode(LOGFILE *log, DIGITAL_NODE *dnode);
void log_dnode_on(LOGFILE *log, DIGITAL_NODE *dnode);
void log_dnode_off(LOGFILE *log, DIGITAL_NODE *dnode);
LOGFILE *start_log(char *filename, int Ndnodes, DIGITAL_NODE *dnodes);
void stop_log(LOGFILE *log, int Ndnodes, DIGITAL_NODE *dnodes);
void flush_log(LOGFILE *log);

/*** external global variables ***/
extern int ERROR;
extern int analog;
extern int Nevents,HistoryN,HistoryMax;
extern unsigned digital_time;
extern int warnall,random_order,ignoreerror,interrupt_sim;
extern DIGITAL_NODE **events;
extern HISTORY *History;
extern double slow_delay,fast_delay;
extern const char *ValueName[], *LevelName[];
extern BIGINT *Big0,*Big1,*BigU;
extern FILE *dsim_in;  /*** input  file for dsim stdin ***/
extern FILE *dsim_out; /*** output file for dsim stdout ***/
extern FILE *dsim_err; /*** output file for dsim stderr ***/
extern LOGFILE *dsim_log; /*** output file for dsim logging ***/
extern int floating_unstab_is_U;
