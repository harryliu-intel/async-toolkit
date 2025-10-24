#ifndef AUTO_LAYOUT_MAIN_HEADER_H
#define AUTO_LAYOUT_MAIN_HEADER_H
/*** list type ***/

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    struct _block *block;
    struct _block **pblock;
    struct _rect  *rect;
    struct _node  *node;
    struct _wire  *wire;
    struct _port  *port;
    struct _port  **pp;
    struct _paint *paint;
    struct _rename *rename;
    struct _stretch *stretch;
    struct _slab *slab;
    struct _rule *rule;
    struct _rule **prule;
    struct _staticizer *staticizer;
    struct _property *prop;
    struct _gate *gate;
    struct _stack *stack;
    struct _sub *sub;
    struct _list **plist;
    struct _branch *branch;
    struct _dag **pdag;
    struct _dnf *dnf;
    struct _alias **palias;
    struct _circuit **pcircuit;
    struct _instance *instance;
    struct _nodetype *nodetype;
    struct _localnode *localnode;
    struct _load *load;
    struct _label *label;
    struct _use *use;
    struct _path *path;
    struct _path **ppath;
    struct _catpath *catpath;
    struct _fmla *fmla; /* recalc */
    struct _var  **pvar; /* recalc */
    struct _func **pfunc; /* recalc */
    struct _parm *parm;
    struct _parm **pparm;
    struct _fnode **pfn; /* fullauto */
    struct _column *column; /* fullauto */
    struct _directive *directive; /* fullauto */
    int *i;
    double *d;
    char **pc;
    } p;
  } LIST;

/*** include files ***/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include "leak.h"
#include "newlex.h"
#include "path.h"
#include "misc.h"
#include "list.h"
#include "minimize.h"
#include "expression.h"
#include "recalc.h"


/*** macros ***/

#define max(a,b) (a>b?a:b)
#define min(a,b) (a<b?a:b)
#define round(x) ((int) rint(x))
#define overlap(a0,a1,b0,b1) (max(a0,b0)<=min(a1,b1))
#define index(j,k,NX,NY) (((j+NX)%NX)*NY + k%NY)

/*** definitions ***/

#define FILEMAX   256 /* maximum number of open files */
#define MAXPAINTS 100 /* maximum number of magic layers */
#define LEVELSMAX  10 /* maximum number of gates in series */

/*** data structures ***/

/*
  chrisb 07 Nov 2000
  Adding typedef to refer to a type of paint.
  In the magic documentation a type of paint is called a layer.
  The author this code used introduced the term paint.
  All the PAINT structs appear to be stored in a global
  array.  A PaintType is therefor an index into that array.
 */
typedef unsigned int PaintType;

typedef struct _rect
  {
  int paint,block;
  char *name; /* node name */
  double x0,y0,x1,y1; /* offset position from block */
  struct _rect *pr; /* linked list for constructing blocks */
  char *abutN,*abutS,*abutE,*abutW; /* partial abuttment pointers for fet extraction */
  int paintN,paintS,paintE,paintW; /* abutting paints, -1 for none */
  } RECT;

typedef struct _block
  {
  int fixed;    /* can't move */
  int column;   /* which column it belongs to (for fullauto) */
  int rulenum;  /* first rule it came from (for fullauto) */
  int type;     /* library(0), Nstack(-1), or Pstack(1) (for fullauto) */
  double x,y;   /* reference position of block */
  double dx,dy; /* components of -energy gradient */
  RECT bbox;    /* whole cell bounding box */
  LIST *rects,  /* the rectangles of paint */
       *bboxs,  /* bounding box of each paint */
       *sboxs;  /* symmetrized bounding box of each paint */
  char *filename; /* filename used by read_layout */
  } BLOCK;

typedef struct _port
{
  int n,block;
  char *extra;        /* extra string of attributes for cadence */
  PaintType paint;    /* paint layer of label */
  double x0,y0,x1,y1; /* actual label rectangle */
  } PORT;

typedef struct _wire
  {
  int port[2];
  double distance;
  } WIRE;

typedef struct _node
{
  char *name;
  unsigned global:1,internal:1;
  LIST *wires,*ports;
} NODE;

typedef struct _parameters
  {
  int N;
  LIST *blocks,*nodes;
  } PARAMETERS; 

typedef struct _paint
  {
  char *name;
  double maxspacing;
  } PAINT;

typedef struct _rename
  {
  char *old,*new;
  } RENAME;

typedef struct _stretch
  {
  char *label; /* stretch label */
  int amount;  /* lambda to stretch */
  LIST *expression; /* an optional expression for amount */
  } STRETCH;

typedef struct _slab /* stretch label in layout */
  {
  int n;              /* index in stretches */
  int dir;            /* direction of label */
  double x0,y0,x1,y1; /* geometry info */
  } SLAB; 

typedef struct _rule
  {
  int independent; /* does this rule involve an independent lnw variable? */
  VAR *lnw;   /* independent variable ln(width) */
  VAR *width; /* formula for width=exp(lnw) */
  VAR *delay; /* formula for rule delay */
  double min_width,max_width;
  double delaybias; /* delaybias inherited from circuit */
  double extrabias; /* read from auto file */
  double setdelay; /* used to force better charge-sharing properties */
  char *width_var;
  LIST *guard;
  char *target;
  struct _nodetype *min_pnt,*max_pnt; /* used to debug sizing runs */
  unsigned dir:1,unshared:1,strict:1,reverse:1,endpath:2,beginpath:1,continuepath:1;
  int num; /* position it appeared originally in circuit */
  } RULE;

typedef struct _staticizer
  {
  char *name;
  int rulenum; /* position it appeared originally in circuit */
  } STATICIZER;

typedef struct _dnf
  {
  LIST *conjuncts;
  } DNF;

typedef struct _property
  {
  LIST *items;
  int sense;
  } PROPERTY;

typedef struct _directive
  {
  char *name; /* node name this applies to */
  int type; /* type of layout directive */
  int pos; /* position in wire pitches from bottom or left side */
  } DIRECTIVE;

typedef struct _circuit
  {
  char *name,*filename;
  int instances,fixed,lessfolding;
  double xpitch,ypitch,delay,delaybias;
  LIST *rules;
  LIST *never; /* rules with NULL target, should never fire! */
  LIST *explicit_staticizers;
  LIST *staticizers;
  LIST *properties;
  LIST *loads; /* nodes in this circuit, with capacitance/formulas */
  LIST *requirements; /* list of expressions for subcircuit requirements */
  LIST *stretches; /* list of stretchs with expressions */
  LIST *subcells; /* list of magic subcells */
  LIST *nostaticizers; /* don't staticize these nodes */
  LIST *paths; /* latency paths through circuit */
  VAR  *metric; /* formula for path delay metric */
  LIST *directives; /* fullauto port directives */
  LIST *catpaths; /* list of concatenated paths */
  LIST *feet; /* single gate rules */
  LIST *parms; /* name/value pairs */
  } CIRCUIT;

typedef struct _gate
  {
  int dir;
  char *s,*d,*g;
  VAR *width; /* formula for the width */
  } GATE;

typedef struct _stack
  {
  LIST *gates;
  char *s,*d;
  int dir;
  double minwidth,maxwidth;
  } STACK;

/*** branch to a dag node via a gate ***/
typedef struct _branch
  {
  char *gate; /* name of gate */
  struct _dag *dag; /* dag node it leads too */
  unsigned reverse:1,backward:1; /* created from reverse rule or backward edge? */
  VAR *width; /* formula for the width */
  } BRANCH;

/*** node in netgraph, includes branches to all neighbors ***/
typedef struct _dag
  {
  struct _dag *alias;
  LIST *branches;
  char *name;
  unsigned mark:1, terminal:2;
  } DAG;

typedef struct _load
  {
  char *name;
  VAR *internal; /* formula for internal gate load */
  VAR *maximum;  /* formula for worst case load */
  } LOAD;

typedef struct _path
  {
  LIST *rules;      /* rules on this path */
  VAR *delay;       /* formula for delay of this path */
  double reference; /* constant reference delay of this path */
  } PATH;

typedef struct _catpath
  {
  LIST *paths;
  VAR *metric; /* formula for constrant energy of all paths */
  } CATPATH;

/*** used to provide parameters for expression evaluation ***/
typedef struct _parm
  {
  char *name;
  double value;
  } PARM;

/*** function prototypes ***/

/*** compare.c ***/
int nodecmp(void *, void *);
int rect_x0_cmp(void *, void *);
int rect_y0_cmp(void *, void *);
int wire_cmp(void *, void *);
int paint_str_cmp(void *, void *);
int rename_cmp(void *, void *);
int rename_str_cmp(void *, void *);
int stretch_cmp (void *, void *);
int stretch_str_cmp(void *, void *);
int ruletarget_cmp(void *, void *);
int ruletarget_name_cmp(void *, void *);
int str_cmp(void *, void *);
int parm_cmp(void *, void *);
int parm_str_cmp(void *, void *);
int path_cmp(void *, void *);
int catpath_cmp(void *, void *);
int staticizer_cmp(void *, void *);
int pblock_rulenum_cmp(void *, void *);
int pcircuit_cmp(void *, void *);
int pcircuit_str_cmp(void *, void *);
int instance_cmp(void *, void *);
int directive_cmp(void *, void *);
int pblock_width_cmp(void *, void *);
int rule_length_cmp(void *, void *);
int rule_decreasing_length_cmp(void *, void *);

/*** auto.c ***/
char *lex_eat_filename(LEX *lex, int argc, char *argv[]);

/*** overlap.c ***/
double overlap_energy(LIST*blocks, RECT *pr0, RECT *pr1, double cost);
double layer_overlap(LIST *blocks, LIST *rects, double cost);

/*** energy.c ***/
double port_distance(PORT *pp0, PORT *pp1, LIST *blocks, double cost);
double compute_energy(LIST *blocks, LIST *nodes,
                      double *poverlap, double *parea, double *pcap,
                      double *pxmin, double *pymin,
                      double *pxmax, double *pymax);
double energy(PARAMETERS *parms, double *p);
void down(PARAMETERS *parms, double *p, double *d);
int sizeof_state(LIST *blocks);
void pack_state(LIST *blocks, double *p);
void unpack_state(LIST *blocks, double *p);

/*** rectangle.c ***/
void rectangle_move(RECT *pr, double dx, double dy);
void rectangle_bloat(RECT *pr, double bloat);
void rectangle_union(RECT *pr1, RECT *pr2);
void add_rect(LIST *rects, RECT rect, double bloat, double dx, double dy);

/*** route.c ***/
LIST *wire_ports(LIST *ports, LIST *blocks);

/*** block.c ***/
int find_paint(char *str);
int lex_eat_paint(LEX *lex);
char *parse_nodename(LEX *lex);
char *translate_name(char *old, LIST *renames);
void clear_layout(LIST *blocks, LIST *nodes, LIST *subcells);
BLOCK create_block();
void finish_block(BLOCK *pb, int num);
NODE *add_node(char *name, LIST *nodes);
void add_port_extra(char *name, char *extra, int paint, int x0, int y0, int x1, int y1, int block, LIST *nodes);
void add_port(char *name, int paint, int x0, int y0, int x1, int y1, int block, LIST *nodes);
int read_layout(char *filename, LIST *blocks, LIST *nodes,
                LIST *renames, LIST *stretches, LIST *subcells, int rulenum);

/*** stretch.c ***/
STRETCH parse_stretch(LEX *lex);
void print_stretch(STRETCH *pstretch);
void stretch_point(double *x, double *y, LIST *stretchs, LIST *stretchlabels);
void stretch_rect(double *x0, double *y0, double *x1, double *y1, LIST *stretches, LIST *stretchlabels);

/*** draw.c ***/
void write_layout(char *filename, LIST *blocks, LIST *nodes, LIST *subcells, int blocknum);

/*** circuit.c ***/
void free_rule(RULE rule);
CIRCUIT create_circuit();
CIRCUIT read_circuit(char *name, char *filename, int library);
void free_circuit(CIRCUIT *circuit);
void print_circuit(FILE *fout, CIRCUIT *circuit, int do_rules, int do_staticizers,
		   int do_properties, int do_attributes, int do_magic, int do_path);
void print_rule(FILE *fout, RULE rule);
void print_property(FILE *fout, PROPERTY *pprop);
int load_cmp(void *p1, void *p2);
int load_str_cmp(void *p1, void *p2);
void print_paths(FILE *fout, LIST *paths);
void print_catpaths(FILE *fout, LIST *catpaths);
void print_path(FILE *fout, PATH *pp);
void print_catpath(FILE *fout, CATPATH *pcp);

/*** match.c ***/
int match_circuit(CIRCUIT *circuit, CIRCUIT *sub, LIST *renames, LIST *stretches,
                  int category, int n, int *rulenum);

/*** name.c ***/
void create_names();
void free_names();
char *get_name(char *name);

/*** stack.c ***/
char *unique_name();
void stack_gen(CIRCUIT *circuit, LIST *blocks, LIST *nodes, int fullauto);
LIST *shared_rules(LIST *rules, LIST *properties, LIST *feet, int lessfolding);

/*** network.c ***/
LIST *minimal_gate_network(LIST *rules, LIST *properties);

/*** arg.c ***/
char *lex_eat_quote_arg(LEX *lex, int argc, char *argv[]);
int lex_eat_integer_arg(LEX *lex, int argc, char *argv[]);
double lex_eat_real_arg(LEX *lex, int argc, char *argv[]);

/*** espresso.c ***/
void espresso(CIRCUIT *circuit);

/*** extract.c ***/
void propagate_nodes(LIST *blocks, LIST *nodes);

/*** global variables ***/
extern double area_cost,cap_cost,overlap_cost,
              global_cap_cost_ratio,internal_cap_cost_ratio,
              xpitch,ypitch,Spacing[MAXPAINTS][MAXPAINTS][2],
              width_tolerance,epsilon,subcell_spacing;
extern int transistor_length,transistor_spacing,transistor_contact_spacing,
           contact_width,diffusion_overhang,poly_overhang,
           Connect[MAXPAINTS][MAXPAINTS],Abut[MAXPAINTS][MAXPAINTS];
extern int ERROR;

extern int detail_level,GRADIENT,distance_measure;
extern LIST *Paints,*GlobalNames;
extern char *GND,*VDD;
char *SEARCH_PATH;
extern int unique_num;

/*** estimate_wirecap.c and transform.c ***/
typedef struct _transform
  {
  int M[2][2],O[2];
  } TRANSFORM;

/*
 * chrisb 07 Nov 2000
 * As per the magic file format( man 5 magic ),
 * the position field of a label is an enumeration.
 */
typedef enum _LabelPositionEnum {
	LABEL_POSITION_CENTER		=	0,
	LABEL_POSITION_NORTH		=	1,
	LABEL_POSITION_NORTHEAST	=	2,
	LABEL_POSITION_EAST		=	3,
	LABEL_POSITION_SOUTHEAST	=	4,
	LABEL_POSITION_SOUTH		=	5,
	LABEL_POSITION_SOUTHWEST	=	6,
	LABEL_POSITION_WEST		=	7,
	LABEL_POSITION_NORTHWEST	=	8	
} LabelPosition;

typedef struct _label
  {
  struct _alias *alias;
  /*
    chrisb 07 Nov 2000
    Changed member paint to be a PaintType rather than an int.
  */
  PaintType paint;
  int x0,y0,x1,y1;

  /*
   * chrisb 07 Nov 2000
   * Renamed the dir field to be position
   * because that is the term the file format specification( man 5 spice ) uses. 
   * Also changed the type of the field to be an enumerated type.
   */ 
  LabelPosition position;
  } LABEL;

typedef struct _use
  {
  char *type,*name;
  int x0,x1,dx,y0,y1,dy;
  int timestamp;
  struct _transform transform;
  RECT box;
  } USE;

TRANSFORM identity_transform();
void apply_transform(LABEL *pl, TRANSFORM T);
TRANSFORM compose_transforms(TRANSFORM A, TRANSFORM B);
TRANSFORM offset_transform(int dx, int dy);

void parse_mag(char *filename, char *prefix, TRANSFORM transform,
	       LIST *labels, LIST *aliases, LIST *instances, int recurse);
void parse_cadence(char *filename, LIST *aliases, LIST *instances);
void spanning_trees(LIST *labels, double cap, double res);
void print_caps(int format);
void debug_labels(LIST *labels);
USE parse_use(LEX *lex, int for_auto);
void print_use(FILE *fout, USE use, int for_auto);
void magic_depend(char *filename, LIST *cells);

/*** size.c ***/
typedef struct _instance
  {
  char *type;
  char *name;
  double x0,y0,x1,y1; /* bbox in world coordinates */
  CIRCUIT *pcircuit; /* if type has an .auto file */
  } INSTANCE;

typedef struct _alias
  {
  struct _alias *root,*next;
  char *name;
  LIST *localnodes;
  double cap; /* lumped wire capacitance */
  double res; /* lumped wire resistance */
  unsigned global:1;
  } ALIAS;

typedef struct _localnode
  {
  INSTANCE *pinstance;
  CIRCUIT *pcircuit;
  char *name;
  struct _load *pload;
  } LOCALNODE;

typedef struct _nodetype
  {
  VAR *gateload; /* formula for gateload of this nodetype */
  VAR *maximum; /* formula for gateload*Size.gatecap + maxcap */
  double mincap,maxcap,totcap; /* constants for min/max/tot wirecap on this nodetype */
  double maxres; /* worst lumped wire resistance on this nodetype */
  LIST *localnodes; /* list of all instance/name's that are connected */
  int instances; /* number of times this nodetype appears */
  char *worst_alias; /* alias name with highest wire cap */
  double metric; /* used for error sorting */
  } NODETYPE;

typedef struct _size
  {
  int Nmax,Pmax,BiasType;
  double mincap,gatecap,extcap,wirecap,wireres,
         max_width,min_width,max_lnw,min_lnw,constraint_weight,
         initial_weight,final_weight,step_weight,shrink_factor,
         Nresistance[LEVELSMAX],Presistance[LEVELSMAX],
         Ndelaybias[LEVELSMAX], Pdelaybias[LEVELSMAX],
         xscale,yscale,iteration_ratio,cleanup_ratio;
  LIST *circuits,*instances,*aliases,*nodetypes,*labels;
  VAR *delay;
  } SIZE;

void default_sizing_options();
extern SIZE Size;
extern FUNC *func_add, *func_sub, *func_mul, *func_div, *func_max, *func_min,
            *func_sqr, *func_exp;
int parse_sizing_options(LEX *lex, int argc, char *argv[]);
ALIAS *create_alias(LIST *aliases, char *name);
ALIAS *find_alias(LIST *aliases, char *name);
int alias_cmp(void *p1, void *p2);
ALIAS *root_al(ALIAS *pn);
void free_paths(CIRCUIT *circuit);
void find_paths(CIRCUIT *circuit);

/*** auto.c ***/
void set_parm(char *name, double value);
double *get_parm(char *name);

/*** fullauto.c ***/
void fullauto(LIST *blocks, LIST *nodes, CIRCUIT *circuit);
void dump_blocks(LIST *blocks, LIST *nodes, CIRCUIT *circuit, int language);
#endif
