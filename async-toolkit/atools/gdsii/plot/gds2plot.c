/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "../aag/gdsutil.h"
#include "leak.h"
#include "misc.h"
#include "newlex.h"
#undef DEBUG

#ifdef DEBUG
static long long zero_realloc=0;
extern long long boundary_count;
#endif

static double in_min = 0;
static double levels_gamma = 1;
static double in_max = 1;
static double out_min = 0;
static double out_max = 1;

/***************************** data structures *******************************/

typedef struct {
  float R,G,B;
} RGB;

typedef struct {
  float R,G,B,A;
} RGBA;

typedef struct {
  int x;
  unsigned color:31,on:1;
} KNOT;

typedef struct {
  int layer,purpose,color;
} LAYERMAP;

typedef unsigned char byte;
typedef struct _list {
  int max,size,mem,shrink;
  union {
    byte *b;
    int *i;
    KNOT *knot;
    LAYERMAP *layermap;
  } p;
} LIST;
#include "list.h"

typedef struct {
  RGB *pixels;
  int x0,y0,x1,y1;
  int xsize,ysize;
} SCREEN;

typedef struct {
  LIST **row;
  int x0,y0,x1,y1;
  int xsize,ysize;
} SCANSCREEN;

/********************************* globals ***********************************/

/***
 * Maximum number of paint layers.  Must fit in a bit-vector, plus a
 * 2^Nlayers palette is pre-allocated
 ***/
#define MAXLAYERS  24

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

/*** map gds2 to pixel coordinates, rounding toward negative infinity ***/
#define mapdn(x,Nsubpixels) (((x)<0 ? ((x)-(Nsubpixels)+1) : (x))/(Nsubpixels))
#define mapup(x,Nsubpixels) mapdn((x)+(Nsubpixels)-1,(Nsubpixels))

/*** boolean expressions ***/
#define OP_OR  -1
#define OP_AND -2
#define OP_NOT -3
#define OP_EQ  -4
#define OP_NE  -5
#define STACKMAX 32

static int Nlayers=0;              // defines number of relevant gds2 layers
static LIST *layerMap;             // map layer/purpose to color
SCANSCREEN *subscreen;             // gds2 resolution screen
static int grid=1;                 // gds2 manufacturing grid in nm
LIST *polygon_knots=NULL;          // temporary list used to render polygon spans

/****************************** function prototypes ***************************/

SCANSCREEN *create_scanscreen(int x0, int y0, int x1, int y1);
SCREEN *create_screen(int x0, int y0, int x1, int y1);
static void free_scanscreen(SCANSCREEN *screen);
static void free_screen(SCREEN *screen);
static void render_subscreen(SCANSCREEN *subscreen, SCREEN *screen, int Nsubpixels,
                      RGB *palette);
static FILE *open_ppm(char *filename, int xsize, int ysize, int job, int jobs);
static void append_ppm(FILE *ppm, SCREEN *screen);
static void write_ppm(FILE *ppm, SCREEN *screen);
static FILE *open_tif(char *filename, int xsize, int ysize, int job, int jobs);
static void append_tif(FILE *tif, SCREEN *screen);
static void write_tif(FILE *tif, SCREEN *screen);
RGB *read_conf(char *filename);
static void draw_polygon(int N, int *x, int *y, int color, SCANSCREEN *screen);
static void set_clipping(SCANSCREEN *screen);
static void draw_bndy(GDS_BNDY *bndy);
static void parse_expression_main(LEX *lex, LIST *expression, int precedence);
LIST *parse_expression(LEX *lex);
static int evaluate_expression(LIST *expression, int colormask);
static void print_expression(LIST *expression);
static float apply_levels(float value);
static int layermapcmp(void *p1, void *p2);

/************************* main command-line interface ************************/

/*** usage banner ***/
static void usage( char *msg) {
  if (msg) fprintf (stderr, "ERROR: %s\n", msg);
  fprintf(stderr,"USAGE: gds2plot\n"
          "  --gds file.gds2         # input gds2 file\n"
          "  [--conf file.conf]      # gds2plot configuration file [gds2plot.conf]\n"
          "  [--ppm file.ppm]        # output ppm image file\n"
          "  [--tif file.tif]        # output tif image file\n"
          "  [--cell fqcn]           # cell name to plot, or omit for top cell\n"
          "  [--x xmax]              # maximum horizontal pixel dimension [1600]\n"
          "  [--y ymax]              # maximum vertical pixel dimension [1200]\n"
          "  [--swapXY [0|1]]        # swap X/Y coordinates\n"
          "  [--grid nm]             # gds2 manufacturing grid in nm [1]\n"
          "  [--tile pixels]         # tile size to render in manufacturing grid [20000]\n"
          "  [--exclude fqcn]        # don't plot a specific cell (multiple allowed)\n"
          "  [--exclude-regex regex] # don't plot cells matching regular expression (multiple allowed)\n"
          "  [--job  jobnum]         # index of current parallel job [0]\n"
          "  [--jobs jobmax]         # total number of parallel jobs [1]\n"
          "  Color adjustments compatible with gimp levels\n"
          "  [--in_min in_min]       # min input pix value (0..1)\n"
          "  [--in_max in_max]       # max input pix value (0..1)\n"
          "  [--out_min out_min]     # min output pix value (0..1)\n"
          "  [--out_max out_max]     # max output pix value (0..1)\n"
          "  [--gamma gamma]         # gamma correction to apply (0..1)\n");
  exit(1);
}

/*** main loop ***/
int main (int argc, char *argv[]) {
   GDS_TRANS trans;
   char *gds_filename=NULL,*ppm_filename=NULL,*topcell=NULL,
     *conf_filename=NULL,str[STRMAX], *tif_filename=NULL;
   RGB *palette;
   SCREEN *screen;
   FILE *ppm = NULL;
   FILE *tif = NULL;
   int Nsubpixels=1,xmax=1600,ymax=1200,swapXY=0;
   int i,x0,y0,x1,y1,xsize,ysize,tile=20000;
   int job=0,jobs=1,tiles,current_job,job_tiles,tilesperjob,my_rows;
   int bbox_x0=0,bbox_x1=0,bbox_y0=0,bbox_y1=0; // bounding box of topcell
   int valid_x0=0,valid_x1=0,valid_y0=0,valid_y1=0; // was bounding box specified
   double start_time;
   
   /*** initial output ***/
   start_time=user_time();
   gds_init();
   setlinebuf(stdout);
   if (argc>0) {safe_sprintf(str,"%s.conf",argv[0]); conf_filename=str;}
   printf("%s\n",argv[0]);
   printf("Copyright 2005-2011 Fulcrum Microsystems, 2011-2014 Intel\n");

   /*** check usage, get arguments ***/
   for (i=1; i<argc; i++) {
     if (i+1>=argc) usage(NULL);
     else if (strcmp(argv[i],"--gds")==0)  gds_filename=argv[++i];
     else if (strcmp(argv[i],"--ppm")==0)  ppm_filename=argv[++i];
     else if (strcmp(argv[i],"--tif")==0)  tif_filename=argv[++i];
     else if (strcmp(argv[i],"--cell")==0) topcell=argv[++i];
     else if (strcmp(argv[i],"--conf")==0) conf_filename=argv[++i];
     else if (strcmp(argv[i],"--x")==0)    sscanf(argv[++i],"%d",&xmax);
     else if (strcmp(argv[i],"--y")==0)    sscanf(argv[++i],"%d",&ymax);
     else if (strcmp(argv[i],"--swapXY")==0) sscanf(argv[++i],"%d",&swapXY);
     else if (strcmp(argv[i],"--tile")==0) sscanf(argv[++i],"%d",&tile);
     else if (strcmp(argv[i],"--grid")==0) sscanf(argv[++i],"%d",&grid);
     else if (strcmp(argv[i],"--job")==0)  sscanf(argv[++i],"%d",&job);
     else if (strcmp(argv[i],"--jobs")==0) sscanf(argv[++i],"%d",&jobs);
     else if (strcmp(argv[i],"--exclude")==0)       gds_exclude(argv[++i]);
     else if (strcmp(argv[i],"--exclude-regex")==0) gds_exclude_regex(argv[++i]);
     else if (strcmp(argv[i],"--bbox_x0")==0) valid_x0=sscanf(argv[++i],"%d",&bbox_x0);
     else if (strcmp(argv[i],"--bbox_x1")==0) valid_x1=sscanf(argv[++i],"%d",&bbox_x1);
     else if (strcmp(argv[i],"--bbox_y0")==0) valid_y0=sscanf(argv[++i],"%d",&bbox_y0);
     else if (strcmp(argv[i],"--bbox_y1")==0) valid_y1=sscanf(argv[++i],"%d",&bbox_y1);
     else if (strcmp(argv[i],"--in_min")==0) sscanf(argv[++i],"%lf",&in_min);
     else if (strcmp(argv[i],"--in_max")==0) sscanf(argv[++i],"%lf",&in_max);
     else if (strcmp(argv[i],"--out_min")==0) sscanf(argv[++i],"%lf",&out_min);
     else if (strcmp(argv[i],"--out_max")==0) sscanf(argv[++i],"%lf",&out_max);
     else if (strcmp(argv[i],"--gamma")==0) sscanf(argv[++i],"%lf",&levels_gamma);
     else usage(argv[i]);
   }
   if (gds_filename==NULL) usage ("gds filename required");
   if (ppm_filename==NULL && tif_filename==NULL) usage ("ppm or tif filename required");
   if (job<0) usage ("job<0");
   if (job>=jobs) usage ("job>=jobs");
   if (jobs<1) usage ("jobs<1");
   if (xmax<1) usage ("xmax<1");
   if (ymax<1) usage ("ymax<1");
   if (grid<1) usage ("grid<1");
   if (tile<1) usage("tile<1");
   if (in_min >= in_max) usage("in_min >= in_max");
   if (out_min >= out_max) usage("out_min >= out_max");
   if (levels_gamma <= 0) usage("gamma <= 0");
   if (in_min < 0) usage("in_min < 0");
   if (out_min < 0) usage("out_min < 0");
   if (in_max > 1) usage("in_max > 1");
   if (out_max > 1) usage("out_max > 1");
   if (jobs>1) printf("Job %d of %d\n",job,jobs);

   /*** read configuration file and define palette ***/
   layerMap = list_create(sizeof(LAYERMAP));
   printf("Reading configuration from %s\n",conf_filename);
   palette = read_conf(conf_filename);

   /*** load gds2 file ***/
   printf("Loading %s\n",gds_filename);
   gds_notext(1);
   if (gds_load(gds_filename)) error("can't read gds2 file");
   if (topcell==NULL) topcell = gds_topcell();
   if (topcell==NULL) error("can't find top cell");
   printf("Top cell is %s\n",topcell);

   /*** filter unused layers before the program sees them ***/
   for (i=0; i<layerMap->max; i++) {
     char num[16];
     sprintf (num, "%d", layerMap->p.layermap[i].layer);
     gds_set_layer(num);
   }

   /*** common settings ***/
   gds_clear_trans(&trans);
   if (swapXY) {
     trans.strans=0xffff;
     trans.angle=90;
     trans.x=0;
     trans.y=0;
   }
   gds_path2boundary(1);
   polygon_knots=list_create(sizeof(KNOT));
   polygon_knots->shrink=0;

   /*** choose Nsubpixels and screen dimensions ***/
   printf("Computing bounding boxes at %ds\n",(int) (user_time()-start_time));
   if (!gds_update_bbox(trans,topcell,&x0,&y0,&x1,&y1))
     error("can't process top cell");

   /*** command line can limit size of plot ***/
#ifdef DEBUG
   printf("%lld boundaries\n", boundary_count);
#endif
   if (valid_x0 && bbox_x0 > x0)
      bbox_x0=mapdn(bbox_x0,grid);
   else
      bbox_x0=mapdn(x0,grid);
   if (valid_y0 && bbox_y0 > y0)
      bbox_y0=mapdn(bbox_y0,grid);
   else
      bbox_y0=mapdn(y0,grid);
   if (valid_x1 && bbox_x1 < x1)
      bbox_x1=mapdn(bbox_x1,grid)+1;
   else
      bbox_x1=mapdn(x1,grid)+1;
   if (valid_y1 && bbox_y1 < y1)
      bbox_y1=mapdn(bbox_y1,grid)+1;
   else
      bbox_y1=mapdn(y1,grid)+1;
   if ((bbox_x1-bbox_x0>bbox_y1-bbox_y0) == (xmax>ymax)) {
     Nsubpixels = max(Nsubpixels,(bbox_x1-bbox_x0+xmax-1)/xmax);
     Nsubpixels = max(Nsubpixels,(bbox_y1-bbox_y0+ymax-1)/ymax);
   }
   else {
     Nsubpixels = max(Nsubpixels,(bbox_x1-bbox_x0+ymax-1)/ymax);
     Nsubpixels = max(Nsubpixels,(bbox_y1-bbox_y0+xmax-1)/xmax);
   }
   bbox_x0 = Nsubpixels * mapdn(bbox_x0,Nsubpixels);
   bbox_x1 = Nsubpixels * mapup(bbox_x1,Nsubpixels);
   bbox_y0 = Nsubpixels * mapdn(bbox_y0,Nsubpixels);
   bbox_y1 = Nsubpixels * mapup(bbox_y1,Nsubpixels);
   xsize = (bbox_x1-bbox_x0)/Nsubpixels;
   ysize = (bbox_y1-bbox_y0)/Nsubpixels;

   /*** choose tile size ***/
   tile = Nsubpixels * mapup(tile,Nsubpixels);
   tiles = (bbox_y1-bbox_y0+tile-1)/tile;

   /*** feedback ***/
   printf("xsize=%d ysize=%d Nsubpixels=%d\n",xsize,ysize,Nsubpixels);
   printf("x0=%d y0=%d x1=%d y1=%d tile=%d\n",
          bbox_x0,bbox_y0,bbox_x1,bbox_y1,tile);

   /*** start ppm file ***/
   if (ppm_filename) ppm = open_ppm(ppm_filename,xsize,ysize,job,jobs);
   if (tif_filename) tif = open_tif(tif_filename,xsize,ysize,job,jobs);

   /*** generate blank file ***/
   current_job=0;
   job_tiles=0;
   my_rows=0;
   tilesperjob=(tiles+jobs-1)/jobs;
   int save_jobs = jobs;
   int save_tiles = tiles;
   for (y1=bbox_y1; y1>bbox_y0; y1-=tile, tiles--) {
     
     /*** check for which stripes belong to which jobs ***/
     if (job_tiles>=tilesperjob) {
       jobs--;        // decrement remaining jobs 
       current_job++; // increment current job
       job_tiles=0;   // reset tile counter for this job
       tilesperjob=(tiles+jobs-1)/jobs; // recompute share of remaining tiles
     }
     job_tiles++; // count another tile for this job
     if (current_job!=job) continue; // not my problem
     my_rows++;
     
     /*** start this stripe ***/
     y0=y1-tile; if (y0<bbox_y0) y0=bbox_y0;

     /*** create destination screen ***/
     screen = create_screen(mapdn(bbox_x0,Nsubpixels),mapdn(y0,Nsubpixels),
                            mapdn(bbox_x1,Nsubpixels),mapdn(y1,Nsubpixels));

     /*** write ppm output ***/
     if (ppm) append_ppm(ppm,screen);
     if (tif) append_tif(tif,screen);
     free_screen(screen);
   }
   if (ppm) fflush(ppm);
   if (tif) fflush(tif);
   printf("Will render %d rows\n",my_rows);

   /*** process stripes ***/
   current_job=0;
   job_tiles=0;
   jobs = save_jobs;
   tiles = save_tiles;
   tilesperjob=(tiles+jobs-1)/jobs;
   for (y1=bbox_y1; y1>bbox_y0; y1-=tile, tiles--) {
     
     /*** check for which stripes belong to which jobs ***/
     if (job_tiles>=tilesperjob) {
       jobs--;        // decrement remaining jobs 
       current_job++; // increment current job
       job_tiles=0;   // reset tile counter for this job
       tilesperjob=(tiles+jobs-1)/jobs; // recompute share of remaining tiles
     }
     job_tiles++; // count another tile for this job
     if (current_job!=job) continue; // not my problem
     
     /*** start this stripe ***/
     y0=y1-tile; if (y0<bbox_y0) y0=bbox_y0;
     printf("Rendering rows %d <= y < %d at %ds\n",y0,y1,(int) (user_time()-start_time));

     /*** create destination screen ***/
     screen = create_screen(mapdn(bbox_x0,Nsubpixels),mapdn(y0,Nsubpixels),
                            mapdn(bbox_x1,Nsubpixels),mapdn(y1,Nsubpixels));

     /*** tile horizontally ***/
     for (x0=bbox_x0; x0<bbox_x1; x0+=tile) {
       x1=x0+tile; if (x1>bbox_x1) x1=bbox_x1;

       /*** create subscreen and set clipping region ***/
       subscreen = create_scanscreen(x0,y0,x1,y1);
       set_clipping(subscreen);

       /*** recursively process gds2 file ***/
       if (!gds_func_structure(trans,topcell,&draw_bndy))
         error("can't process top cell");

       /*** scale subscreen to screen ***/
       render_subscreen(subscreen,screen,Nsubpixels,palette);
       free_scanscreen(subscreen);
     }

     /*** write ppm output ***/
     if (ppm) write_ppm(ppm,screen);
     if (tif) write_tif(tif,screen);
     free_screen(screen);
   }

   /*** free and exit ***/
   list_free(layerMap);
   list_free(polygon_knots);
   free_temp_list();
   leak_free(palette);
   leak_check();
#ifdef DEBUG
   printf("%lld zero reallocs\n", zero_realloc);
   printf("%lld boundaries\n", boundary_count);
#endif
   printf("Finished at %ds\n",(int) (user_time()-start_time));
   return 0;
}

/*********************** interface to gdsii library ***********************/

/*** set GDS clipping given for a screen ***/
static void set_clipping(SCANSCREEN *screen) {
  gds_clear_rect();
  gds_include_rect((screen->x0-1)*grid,(screen->y0-1)*grid,
                   screen->x1*grid,screen->y1*grid);
}

/*** render each shape into the current subscreen ***/
static void draw_bndy(GDS_BNDY *bndy) {
  LAYERMAP map;
  int k;
  map.layer=bndy->layer;
  map.purpose=bndy->datatype;
  map.color=0;
  k = find_element_sorted(layerMap,&map,&layermapcmp);
  if (k<0) { map.purpose=-1; k = find_element_sorted(layerMap,&map,&layermapcmp); }
  if (k<0) return;
  map = layerMap->p.layermap[k];
  assert(bndy->kind!=GDS_PATH);
  draw_polygon(bndy->len,bndy->x,bndy->y,map.color,subscreen);
}

/***************************** screen operations ********************************/

/*** create a blank screen ***/
SCREEN *create_screen(int x0, int y0, int x1, int y1) {
  int x,y;
  SCREEN *screen;
  RGB color;
  color.R=color.G=color.B=0;
  screen = leak_malloc(sizeof(SCREEN));
#ifdef DEBUG
  printf("screen %lld\n", (long long) sizeof(SCREEN));
#endif
  screen->x0=x0;
  screen->y0=y0;
  screen->x1=x1;
  screen->y1=y1;
  screen->xsize=x1-x0;
  screen->ysize=y1-y0;
  screen->pixels = leak_malloc(screen->xsize*screen->ysize*sizeof(RGB));
#ifdef DEBUG
  printf("pixels %lld (%lld*%lld*%lld)\n",
     (long long) (screen->xsize*screen->ysize*sizeof(RGB)),
     (long long) (screen->xsize),
     (long long) (screen->ysize),
     (long long) (sizeof(RGB)));
#endif
  for (x=0; x<screen->xsize; x++)
    for (y=0; y<screen->ysize; y++)
      screen->pixels[y*screen->xsize+x]=color;
  return screen;
}

/*** free screen memory ***/
static void free_screen(SCREEN *screen) {
  leak_free(screen->pixels);
  leak_free(screen);
}

/*** create a blank screen ***/
SCANSCREEN *create_scanscreen(int x0, int y0, int x1, int y1) {
  int y;
  SCANSCREEN *screen;
  screen = leak_malloc(sizeof(SCANSCREEN));
#ifdef DEBUG
  printf ("scan screen %lld\n", (long long) sizeof(SCANSCREEN));
#endif
  screen->x0=x0;
  screen->y0=y0;
  screen->x1=x1;
  screen->y1=y1;
  screen->xsize=x1-x0;
  screen->ysize=y1-y0;
  screen->row = leak_malloc(screen->ysize*sizeof(LIST *));
#ifdef DEBUG
  printf ("scan row %lld\n", (long long) (screen->ysize*sizeof(LIST *)));
#endif
  for (y=0; y<screen->ysize; y++) screen->row[y] = list_create(sizeof(KNOT));
  return screen;
}

/*** free screen memory ***/
static void free_scanscreen(SCANSCREEN *screen) {
  int y;
  for (y=0; y<screen->ysize; y++) list_free(screen->row[y]);
  leak_free(screen->row);
  leak_free(screen);
}

/*** compare two KNOT's by x coordinate ***/
static int knotcmp(void *p1, void *p2) {
  KNOT *pk1=(KNOT *)p1, *pk2=(KNOT *)p2;
  return pk1->x - pk2->x;
}

/*** compare two LAYERMAP's by layer and purpose ***/
static int layermapcmp(void *p1, void *p2) {
  LAYERMAP *pm1=(LAYERMAP *) p1, *pm2 = (LAYERMAP *) p2;
  if (pm1->layer == pm2->layer) return pm1->purpose - pm2->purpose;
  return pm1->layer - pm2->layer;
}

/*** average across a span into screen ***/
static void render_span(int sx0, int sx1, int y, RGB color, SCREEN *screen, int Nsubpixels) {
  int x0,x1,tx0,tx1,x,j;
  x0 = mapdn(sx0,Nsubpixels);
  x1 = mapdn(sx1,Nsubpixels)+1;
  for (x=x0; x<x1; x++) {
    tx0=x*Nsubpixels;
    tx1=(x+1)*Nsubpixels;
    if (tx0<sx0) tx0=sx0;
    if (tx1>sx1) tx1=sx1;
    if (tx1==tx0) continue;
    j = (y-screen->y0)*screen->xsize+(x-screen->x0);
    screen->pixels[j].R += color.R*(tx1-tx0);
    screen->pixels[j].G += color.G*(tx1-tx0);
    screen->pixels[j].B += color.B*(tx1-tx0);
  }
}

/*** convert bitvector pixels into anti-aliased RGB pixels ***/
static void render_subscreen(SCANSCREEN *subscreen, SCREEN *screen, int Nsubpixels,
                      RGB *palette) {
  int y;      // screen coordinates
  int sy,sx0; // subscreen coordinates
  int active[MAXLAYERS];
  int bitmask,i;
  LIST *row;
  RGB color;
  KNOT knot;

  /*** finish sorting subscreen rows ***/
  for (y=0; y<subscreen->ysize; y++)
    list_finish_lazy_sort(subscreen->row[y],&knotcmp);

  /*** project and average subscreen onto screen ***/
  for (sy=subscreen->y0; sy<subscreen->y1; sy++) { // for each subscreen row
    row = subscreen->row[sy-subscreen->y0];
    y = mapdn(sy,Nsubpixels);
    bitmask=0;
    color=palette[0];
    color.R /= Nsubpixels * Nsubpixels;
    color.G /= Nsubpixels * Nsubpixels;
    color.B /= Nsubpixels * Nsubpixels;
    for (i=0; i<Nlayers; i++) active[i]=0;
    sx0 = subscreen->x0;

    /*** visit each knot on row ***/
    for (i=0; i<row->max; i++) {

      /*** render span sx0<=sx<knot.x to screen ***/
      knot = row->p.knot[i];
      render_span(sx0,knot.x,y,color,screen,Nsubpixels);

      /*** find next knot ***/
      if (knot.on) active[knot.color]++;
      else         active[knot.color]--;
      bitmask = bitmask & ~(1<<knot.color);
      if (active[knot.color]>0) bitmask = bitmask | (1<<knot.color);

      /*** get color for next span ***/
      color = palette[bitmask];
      color.R /= Nsubpixels * Nsubpixels;
      color.G /= Nsubpixels * Nsubpixels;
      color.B /= Nsubpixels * Nsubpixels;

      /*** advance sx0 to next knot ***/
      sx0=knot.x;
    }
    render_span(sx0,subscreen->x1,y,color,screen,Nsubpixels);
  }
}

/**************************** ppm file writing *****************************/

off_t ppm_start = 0;

/*** start a ppm file ***/
static FILE *open_ppm(char *filename, int xsize, int ysize, int job, int jobs) {
  char name[STRMAX];
  FILE *out;
  if (jobs>1) safe_sprintf(name,"%s.%d",filename,job);
  else        safe_sprintf(name,"%s",filename);
  out = fopen(name,"wt");
  if (out==NULL) error("can't write ppm output file");
  if (job==0) fprintf(out, "P6 %u %u 255\n",xsize,ysize);
  ppm_start = ftell(out);
  return out;
}

/*** append a screen to a ppm file ***/
static void append_ppm(FILE *out, SCREEN *screen) {
  int x, y, j;
  unsigned char r, g, b;
  RGB color;
  for (y=screen->y1-1; y>=screen->y0; y--) {
    for (x=screen->x0; x<screen->x1; x++) {
      j = (y-screen->y0)*screen->xsize + (x-screen->x0);
      color = screen->pixels[j];
      color.R = apply_levels(color.R);
      color.G = apply_levels(color.G);
      color.B = apply_levels(color.B);
      r = (unsigned char) floor(color.R*255+0.5);
      g = (unsigned char) floor(color.G*255+0.5);
      b = (unsigned char) floor(color.B*255+0.5);
      fwrite(&r,1,1,out);
      fwrite(&g,1,1,out);
      fwrite(&b,1,1,out);
    }
  }
}

static void write_ppm(FILE *out, SCREEN *screen) {
    fseek (out, ppm_start, 0);
    append_ppm(out, screen);
    fflush(out);
    ppm_start = ftell (out);
}

off_t tif_start = 0;

static void tif_write16 (int i, FILE *tif) {
    fputc (i & 0xff, tif);
    i >>= 8;
    fputc (i & 0xff, tif);
}

static void tif_write32 (long i, FILE *tif) {
    fputc (i & 0xff, tif);
    i >>= 8;
    fputc (i & 0xff, tif);
    i >>= 8;
    fputc (i & 0xff, tif);
    i >>= 8;
    fputc (i & 0xff, tif);
}

/*** start a tif file ***/
static FILE *open_tif(char *filename, int xsize, int ysize, int job, int jobs) {
  char name[STRMAX];
  FILE *out;
  long offset;
  if (jobs>1) safe_sprintf(name,"%s.%d",filename,job);
  else        safe_sprintf(name,"%s",filename);
  out = fopen(name,"wt");
  if (out==NULL) error("can't write tif output file");
  if (job == 0) {
      // little endian integers II, big endian would be MM
      fputc('I', out);
      fputc('I', out);
      // TIFF magic number
      tif_write16(42, out);
      // offset to IFD
      tif_write32((long) 8, out);
      // number of directories = 12
      tif_write16(13, out);
      offset = 13*12+4+ftell(out);
      long runoffset=offset;
      // directory #1, 12 bytes, Width
      tif_write16(256, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)xsize, out);
      // directory #2, 12 bytes, Length
      tif_write16(257, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)ysize, out);
      // directory #3, 12 bytes, Bits per Sample
      tif_write16(258, out);
      tif_write16(3, out);
      tif_write32((long)3, out);
      tif_write32(runoffset, out);
      runoffset += 3*2;
      // directory #4, 12 bytes, compression
      tif_write16(259, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)1, out);
      // directory #5, 12 bytes, Photometric Interpretation
      tif_write16(262, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)2, out);
      // directory #6, 12 bytes, Strip Offsets
      tif_write16(273, out);
      tif_write16(4, out);
      tif_write32((long)ysize, out);
      tif_write32(runoffset, out); // offset
      runoffset += ysize*4L;
      // directory #7, 12 bytes, samples per pixel
      tif_write16(277, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)3, out);
      // directory #8, 12 bytes, Rows per Strip
      tif_write16(278, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32(1L, out);
      // directory #9, 12 bytes, StripByteCounts
      tif_write16(279, out);
      tif_write16(4, out);
      tif_write32((long)ysize, out);
      tif_write32(runoffset, out);
      runoffset += ysize*4L;
      // directory #10, 12 bytes, XResolution
      tif_write16(282, out);
      tif_write16(5, out);
      tif_write32((long)1, out);
      tif_write32(runoffset, out);
      runoffset += 2*4L;
      // directory #11, 12 bytes, YResolution
      tif_write16(283, out);
      tif_write16(5, out);
      tif_write32((long)1, out);
      tif_write32(runoffset, out);
      runoffset += 2*4L;
      // directory #11, 12 bytes, YResolution
      // directory #12, 12 bytes, planar config
      tif_write16(284, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)1, out);
      // directory #13, 12 bytes, Res Unit
      tif_write16(296, out);
      tif_write16(3, out);
      tif_write32((long)1, out);
      tif_write32((long)2, out);
      // end directory
      tif_write32(0L, out);
      // offset bits
      tif_write16(8, out);
      tif_write16(8, out);
      tif_write16(8, out);
      // rows
      long i;
      for (i = 0; i < ysize; i++) {
        tif_write32(i*3L*xsize+runoffset, out);
      }
      for (i = 0; i < ysize; i++) {
        tif_write32(3L*xsize, out);
      }
      // resX offset+0
      tif_write32(72L, out); tif_write32(1L, out);
      // resY offset+8
      tif_write32(72L, out); tif_write32(1L, out);
  }
  tif_start = ftell(out);
  return out;
}

/*** append a screen to a tif file ***/
static void append_tif(FILE *out, SCREEN *screen) {
  int x, y, j;
  unsigned char r, g, b;
  RGB color;
  for (y=screen->y1-1; y>=screen->y0; y--) {
    for (x=screen->x0; x<screen->x1; x++) {
      j = (y-screen->y0)*screen->xsize + (x-screen->x0);
      color = screen->pixels[j];
      color.R = apply_levels(color.R);
      color.G = apply_levels(color.G);
      color.B = apply_levels(color.B);
      r = (unsigned char) floor(color.R*255+0.5);
      g = (unsigned char) floor(color.G*255+0.5);
      b = (unsigned char) floor(color.B*255+0.5);
      fwrite(&r,1,1,out);
      fwrite(&g,1,1,out);
      fwrite(&b,1,1,out);
    }
  }
}

static void write_tif(FILE *out, SCREEN *screen) {
    fseek (out, tif_start, 0);
    append_tif(out, screen);
    fflush(out);
    tif_start = ftell (out);
}

/*********************** configuration file processing *************************/

/*** read configuration file and create the palette ***/
RGB *read_conf(char *filename) {
  FILE *fin;
  RGBA paint[MAXLAYERS],color;
  LIST *expression[MAXLAYERS];
  RGB *palette;
  LEX *lex;
  double alpha,beta;
  int i,j,N=0;

  /*** read commands ***/
  fin=fopen(filename,"r");
  if (fin==NULL) error("can't read conf file");
  lex=lex_file_with_name(fin,filename);
  while (!lex_is_eof(lex)) {
    if (lex_eatif_sym(lex,"#")) lex_eat_until(lex,"\n");
    else if (lex_eatif_keyword(lex,"paint")) {
      if (N>=MAXLAYERS) lex_error(lex,"too many layers");
      color.R = lex_eat_real(lex)/255;
      if ((color.R<0) || (color.R>1)) lex_error(lex,"red between 0 and 255");
      color.G = lex_eat_real(lex)/255;
      if ((color.G<0) || (color.G>1)) lex_error(lex,"green between 0 and 255");
      color.B = lex_eat_real(lex)/255;
      if ((color.B<0) || (color.B>1)) lex_error(lex,"blue between 0 and 255");
      color.A = lex_eat_real(lex);
      if ((color.A<0) || (color.A>1)) lex_error(lex,"alpha between 0 and 1");
      paint[N]=color;
      expression[N]=parse_expression(lex);
      N++;
    }
    else lex_error(lex,"paint command or # comment");
  }
  lex_free(lex);
  fclose(fin);
  list_finish_lazy_sort(layerMap,&layermapcmp);

#ifdef DEBUG
  for (j=0; j<N; j++) {
    printf("expression %d is ",j);
    print_expression(expression[j]);
  }
#endif

  /*** now construct bitmask -> RGB mapping palette ***/
  palette = leak_malloc(sizeof(RGB)*(1<<Nlayers));
#ifdef DEBUG
  printf("pallette %lld\n", (long long) (sizeof(RGB)*(1<<Nlayers)));
#endif
  for (i=0; i<(1<<Nlayers); i++) {
    color.G=color.R=color.B=color.A=1;
    for (j=0; j<N; j++) {
      if (evaluate_expression(expression[j],i)) {
        alpha = paint[j].A;
        beta = 1-alpha;
        color.R = alpha * paint[j].R + beta * color.R;
        color.G = alpha * paint[j].G + beta * color.G;
        color.B = alpha * paint[j].B + beta * color.B;
        color.A = 1;
      }
    }
    palette[i].R=color.R;
    palette[i].G=color.G;
    palette[i].B=color.B;
  }

#ifdef DEBUG
  for (i=0; i<N; i++)
    printf("paint[%d]=%g,%g,%g,%g\n",i,
            paint[i].R,paint[i].G,paint[i].B,paint[i].A);
  for (i=0; i<layerMap->max; i++) {
    LAYERMAP map;
    map = layerMap->p.layermap[i];
    printf("layerMap layer=%d purpose=%d color=%d\n",map.layer,map.purpose,map.color);
  }
  for (i=0; i<(1<<Nlayers); i++)
    printf("palette[%d]=%g,%g,%g\n",i,
            palette[i].R,palette[i].G,palette[i].B);
#endif

  /*** free and return ***/
  for (i=0; i<N; i++) list_free(expression[i]);
  return palette;
}

/*********************** polygon rasterization *********************************/

/*** draw a horizonal span into a SCANSCREEN ***/
static void draw_span(int x0, int x1, int y, int color, SCANSCREEN *screen) {
  KNOT knot;

  /*** clip span ***/
  if ((x1<=screen->x0) || (x0>=screen->x1)) return;
  if (x0<screen->x0) x0=screen->x0;
  if (x1>screen->x1) x1=screen->x1;

  /**** add span endpoints to screen's row ***/
  knot.x=x0; knot.color=color; knot.on=1;
  list_insert_element_lazy_sort(screen->row[y-screen->y0],&knot,&knotcmp);
  knot.x=x1; knot.color=color; knot.on=0;
  list_insert_element_lazy_sort(screen->row[y-screen->y0],&knot,&knotcmp);
}

/*** edges have x,y in screen coordinates ***/
static void draw_polygon(int N, int *px, int *py, int color, SCANSCREEN *screen) {
  KNOT knot;
  int e,y,x0,x1,y0,y1,px0,py0,px1,py1;

  /*** avoid degenerate or invisible shapes ***/
  if (N<4) return;

  /*** check bbox ***/
  x0=mapdn(px[0],grid); x1=mapdn(px[0],grid);
  y0=mapdn(py[0],grid); y1=mapdn(py[0],grid);
  for (e=1; e<N; e++) {
    x0 = min(x0,mapdn(px[e],grid));
    x1 = max(x1,mapdn(px[e],grid));
    y0 = min(y0,mapdn(py[e],grid));
    y1 = max(y1,mapdn(py[e],grid));
  }
  if ((x1<screen->x0) || (x0>=screen->x1) ||
      (y1<screen->y0) || (y0>=screen->y1)) return;

  /*** optimized code for rectangles ***/
  if ((N==5) &&
      (((px[0]==px[1]) && (px[2]==px[3]) && (py[1]==py[2]) && (py[3]==py[0])) ||
       ((px[1]==px[2]) && (px[3]==px[0]) && (py[0]==py[1]) && (py[2]==py[3])))) {
    for (y=max(y0,screen->y0); y<min(y1,screen->y1); y++)
      draw_span(x0,x1,y,color,screen);
    return;
  }

  /*** scan rows ***/
  knot.color=0; knot.on=0;
  for (y=max(y0,screen->y0); y<min(y1,screen->y1); y++) {

    /*** find sorted edge intersections ***/
    list_realloc(polygon_knots,0);
#ifdef DEBUG
    zero_realloc++;
#endif
    for (e=0; e+1<N; e++) {
      px0=mapdn(px[e],grid);
      py0=mapdn(py[e],grid);
      px1=mapdn(px[e+1],grid);
      py1=mapdn(py[e+1],grid);
      if ((min(py0,py1)>y) || (max(py0,py1)<=y)) continue; // y out of range
      if (py1==py0) continue; // ignore horizontal lines
      knot.x = ((long) px0) + ((long) (y-py0)) * ((long) (px1-px0)) / ((long) (py1-py0));
      list_insert_element_lazy_sort(polygon_knots,&knot,&knotcmp);
    }

    /*** draw spans from even to odd intersections ***/
    list_finish_lazy_sort(polygon_knots,&knotcmp);
    for (e=0; e+1<polygon_knots->max; e+=2) {
      px0=polygon_knots->p.knot[e].x;
      px1=polygon_knots->p.knot[e+1].x;
      draw_span(px0,px1,y,color,screen);
    }
  }
}

/************************ boolean expressions ******************************/

/*** add variable to expression ***/
static void add_variable(LEX *lex, LIST *expression, int layer, int purpose) {
  LAYERMAP map;
  int k;
  if (layer<0) lex_error(lex,"gds2 layer number out of range");
  map.layer=layer; map.purpose=purpose; map.color=Nlayers;
  k = find_element_lazy_sort(layerMap,&map,&layermapcmp);
  if (k<0) { // new layer to color mapping
    list_insert_element_lazy_sort(layerMap,&map,&layermapcmp);
    Nlayers++;
  } else map = layerMap->p.layermap[k];
  list_append_element(expression,&map.color);
}

/*** add operator to expresison ***/
static void add_operator(LIST *expression, int op) {
  list_append_element(expression,&op);
}

/*** parse a single item of an expression ***/
static void parse_expression_item(LEX *lex, LIST *expression) {
  int layer,purpose;
  if (lex_is_integer(lex)) {
    layer = lex_eat_integer(lex);
    purpose = -1;
    if (lex_eatif_sym(lex,":")) purpose = lex_eat_integer(lex);
    add_variable(lex,expression,layer,purpose);
  }
  else if (lex_eatif_sym(lex,"(")) {
    parse_expression_main(lex,expression,0);
    lex_eat_sym(lex,")");
  }
  else if (lex_eatif_sym(lex,"!")) {
    parse_expression_item(lex,expression);
    add_operator(expression,OP_NOT);
  }
}

/*** core parse infix expressions with precedence ***/
static void parse_expression_main(LEX *lex, LIST *expression, int precedence) {
  parse_expression_item(lex,expression);
  while (!lex_is_eof(lex)) {
    if ((precedence<4)&&lex_eatif_sym(lex,"==")) {
      parse_expression_main(lex,expression,4);
      add_operator(expression,OP_EQ);
    }
    else if ((precedence<4)&&lex_eatif_sym(lex,"!=")) {
      parse_expression_main(lex,expression,4);
      add_operator(expression,OP_NE);
    }
    else if ((precedence<3)&&lex_eatif_sym(lex,"&&")) {
      parse_expression_main(lex,expression,3);
      add_operator(expression,OP_AND);
    }
    else if ((precedence<2)&&lex_eatif_sym(lex,"||")) {
      parse_expression_main(lex,expression,2);
      add_operator(expression,OP_OR);
    }
    else break;
  }
}

/*** user interface for parsing expressions ***/
LIST *parse_expression(LEX *lex) {
  LIST *expression;
  expression=list_create(sizeof(int));
  parse_expression_main(lex,expression,0);
  return expression;
}

/*** evaluate an expression ***/
static int evaluate_expression(LIST *expression, int colormask) {
  int s[STACKMAX],tos=0,j,op;
  for (j=0; j<expression->max; j++) {
    op=expression->p.i[j];
    if ((op>=0)&&(tos+1<STACKMAX))   s[tos++]=(colormask>>op)&1;
    else if ((op==OP_EQ)&&(tos>=2))  {s[tos-2]=(s[tos-2]==s[tos-1]); tos--;}
    else if ((op==OP_NE)&&(tos>=2))  {s[tos-2]=(s[tos-2]!=s[tos-1]); tos--;}
    else if ((op==OP_AND)&&(tos>=2)) {s[tos-2]=((s[tos-2]!=0)&&(s[tos-1]!=0)); tos--;}
    else if ((op==OP_OR)&&(tos>=2))  {s[tos-2]=((s[tos-2]!=0)||(s[tos-1]!=0)); tos--;}
    else if ((op==OP_NOT)&&(tos>=1)) s[tos-1]=(s[tos-1]==0);
    else error("problem with boolean expression stack");
  }
  return s[tos-1];
}

/** print an expression **/
static void print_expression(LIST *expression) {
  int j,op;
  for (j=0; j<expression->max; j++) {
    op=expression->p.i[j];
    if (op>=0)   printf("%d ",op);
    else if (op==OP_EQ)  printf("== ");
    else if (op==OP_NE)  printf("!= ");
    else if (op==OP_AND) printf("&& ");
    else if (op==OP_OR)  printf("|| ");
    else if (op==OP_NOT) printf("! ");
    else error("problem with boolean expression stack");
  }
  printf("\n"); 
}

static float apply_levels(float value) {
    //  normalize
    value = (value-in_min) / (in_max-in_min);
    if (value < 0) value = 0.0;
    if (value > 1) value = 1.0;
    if (levels_gamma <= 0.0) levels_gamma = 1.0;
    // transform levels_gamma
    value = pow(value, 1.0/levels_gamma);
    // rescale range
    value *= (out_max-out_min) + out_min;
    // keep in range
    if (value < 0) value = 0;
    if (value > 1) value = 1;
    return value;
}
