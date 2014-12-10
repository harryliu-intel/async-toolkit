// "$Id: //depot/user/aubrey/src/lib/libaag/gdsutil.h#1 $ AAG";
#ifndef _gdsutil_h
#define _gdsutil_h

#include <limits.h>
#include <stdio.h>
#include "btutil.h"

#define GDS_HEADER          0x0002
#define GDS_BGNLIB          0x0102
#define GDS_LIBNAME         0x0206
#define GDS_UNITS           0x0305
#define GDS_ENDLIB          0x0400
#define GDS_BGNSTR          0x0502
#define GDS_STRNAME         0x0606
#define GDS_ENDSTR          0x0700
#define GDS_BOUNDARY        0x0800
#define GDS_PATH            0x0900
#define GDS_SREF            0x0a00
#define GDS_AREF            0x0b00
#define GDS_TEXT            0x0c00
#define GDS_LAYER           0x0d02
#define GDS_DATATYPE        0x0e02
#define GDS_WIDTH           0x0f03
#define GDS_XY              0x1003
#define GDS_ENDEL           0x1100
#define GDS_SNAME           0x1206
#define GDS_COLROW          0x1302
#define GDS_TEXTNODE        0x1400
#define GDS_NODE            0x1500
#define GDS_TEXTTYPE        0x1602
#define GDS_PRESENTATION    0x1701
#define GDS_STRING          0x1906
#define GDS_STRANS          0x1a01
#define GDS_MAG             0x1b05
#define GDS_ANGLE           0x1c05
#define GDS_REFLIBS         0x1f06
#define GDS_FONTS           0x2006
#define GDS_PATHTYPE        0x2102
#define GDS_GENERATIONS     0x2202
#define GDS_ATTRTABLE       0x2306
#define GDS_STYPTABLE       0x2406
#define GDS_STRTYPE         0x2502
#define GDS_ELFLAGS         0x2601
#define GDS_ELKEY           0x2703
#define GDS_LINKTYPE        0x2800
#define GDS_LINKKEYS        0x2900
#define GDS_NODETYPE        0x2a02
#define GDS_PROPATTR        0x2b02
#define GDS_PROPVALUE       0x2c06
#define GDS_BOX             0x2d00
#define GDS_BOXTYPE         0x2e02
#define GDS_PLEX            0x2f03
#define GDS_BGNEXTN         0x3003
#define GDS_ENDEXTN         0x3103
#define GDS_TAPENUM         0x3202
#define GDS_TAPECODE        0x3302
#define GDS_STRCLASS        0x3401
#define GDS_RESERVED        0x3503
#define GDS_FORMAT          0x3602
#define GDS_MASK            0x3706
#define GDS_ENDMASKS        0x3800

#define GDS_NODATA          0
#define GDS_BITARRAY        1
#define GDS_INT2            2
#define GDS_INT4            3
#define GDS_REAL4           4
#define GDS_REAL8           5
#define GDS_ASCII           6

#define GDS_NAMELEN       312

#define GDS_FLAG_CONV   1  /* data already converted */

#define USE_CELL_CNT 512
#define MAX_LAYER 1024
#define MAX_TYPE  256
#define X 0
#define Y 1
#define GDS_MAX_POINTS 200


typedef struct transform GDS_TRANS;
typedef struct structure GDS_STRUCTURE;
typedef struct gds_item_struct GDS_ITEM;
typedef struct gds_bndy GDS_BNDY;
typedef struct gds_text GDS_TXT;
typedef struct gds_org GDS_ORG;
typedef struct lookup_struct GDS_LOOKUP;
typedef struct gds_sref_struct GDS_REF;
typedef struct gds_point_struct GDS_POINT;
typedef struct gds_line_struct GDS_LINE;
typedef struct gds_triangle_struct GDS_TRIANGLE;
typedef struct gds_rectangle_struct GDS_RECT;
typedef struct gds_bbox GDS_BBOX;


#if INT_MAX >= 0x7FFFFFFFL
typedef int      LONGINT;
typedef unsigned ULONGINT;
#else
typedef long          LONGINT;
typedef unsigned long ULONGINT;
#endif

struct gds_bbox {
   LONGINT minx, miny, maxx, maxy;
};

typedef union {
   double          *dptr;
   char            *cptr;
   unsigned char   *ucptr;
   LONGINT         *lptr;
   unsigned short  *sptr;
   } GDS_DATA;

struct transform {
   double angle;
   double mag;
   LONGINT x, y;
   short  strans;
   };

/* first byte:  record_type = item->code >> 8; 
 * second byte: data_type   = item->code & 0xff;
 */
struct gds_item_struct {
   unsigned short  len;
   unsigned short  code;
   GDS_DATA        data;
   int             flags;
   int             fp;
   unsigned char  *next_ptr;
   unsigned char  *ptr;
   };

struct lookup_struct {
   char *name;
   short len;
   short code;
   void (*func)();
   };

#define EXCLUDE_REGEX 1
#define EXCLUDE_NAME  2

struct structure {
   GDS_ITEM *item;
   GDS_BBOX bbox;
   int bbox_updated;
   int children;
   int parents;
   unsigned char *ptr;
   int fp;
   };

struct gds_bndy {
   short kind;
   short datatype;
   short layer;
   unsigned short len;
   LONGINT  width;
   LONGINT bgnextn, endextn;
   LONGINT *x, *y;
   GDS_BNDY *next;
   GDS_ITEM *properties;
   };

struct gds_text {
   char *string;
   LONGINT x, y;
   short layer;
   short texttype;
   GDS_TXT *next;
   };

struct gds_org {
   char *structure;
   GDS_TRANS trans;
   LONGINT cx, cy;
   LONGINT minx, miny, maxx, maxy;
   GDS_ORG *next;
   };

struct gds_sref_struct {
   char strname[64];
   GDS_TRANS   trans;
   short       row, col;
   LONGINT     dx, dy;
   GDS_REF    *next;
   };

struct gds_point_struct {
   long x;
   long y;
   GDS_POINT *next;
};

struct gds_line_struct {
   GDS_POINT p1;
   GDS_POINT p2;
   GDS_LINE *next;
};

struct gds_triangle_struct {
   GDS_POINT p1;
   GDS_POINT p2;
   GDS_POINT p3;
   GDS_TRIANGLE *next;
};

struct gds_rectangle_struct {
   long minx; 
   long maxx;
   long miny;
   long maxy;
   GDS_RECT *next;
};


#ifdef __cplusplus
extern "C" {
#endif
GDS_BNDY *gds_bndy_head (void);
void gds_clear (void);
void gds_clear_layer_mask (void);
void gds_clear_rect (void);
void gds_clear_trans (GDS_TRANS *trans);
int gds_delete_structure (char *name);
int gds_do_convert (int val);
void gds_eoferr (char *s);
void gds_exclude (char *name);
void gds_exclude_rect (LONGINT minx, LONGINT miny, LONGINT maxx, LONGINT maxy);
void gds_exclude_regex (char *name);
int gds_fgetint2 (FILE *fp);
LONGINT gds_fgetint4 (FILE *fp);
int gds_fgetlen (FILE *f);
double gds_fgetreal8 (FILE *fp);
void gds_fgetstring (char *s, int len, FILE *fp);
GDS_ITEM *gds_find_structure (char *name);
void gds_fputint2 (unsigned num, FILE *fp);
void gds_fputint4 (LONGINT num, FILE *fp);
void gds_free_bndy (void);
void gds_free_org (void);
void gds_free_ref (void);
void gds_free_text (void);
int gds_func_structure (GDS_TRANS trans, char *name, void (*func)(GDS_BNDY *));
GDS_ITEM *gds_get_item (void);
void gds_include_rect (LONGINT minx, LONGINT miny, LONGINT maxx, LONGINT maxy);
void gds_init (void);
GDS_ITEM *gds_item_head (void);
int gds_load (char *fn);
int gds_loaded (char *fn);
void gds_minmax (GDS_BNDY *item, LONGINT *minx, LONGINT *maxx, LONGINT *miny, LONGINT *maxy);
void gds_notext (int v);
GDS_ORG *gds_org_head (void);
void gds_path2boundary (int val);
int gds_read_structure (GDS_TRANS trans, char *name);
void gds_set_cell (char *name);
void gds_set_layer (char *string);
void gds_set_layer_mask (void);
int gds_sgetint2 (char *s);
LONGINT gds_sgetint4 (char *s);
double gds_sgetreal4 (char *s);
double gds_sgetreal8 (char *s);
void gds_sputint4 (char *s, LONGINT num);
void gds_sputreal4 (char *s, double num);
void gds_sputreal8 (char *s, double num);
void gds_sputreal8h (char *s, double num, unsigned lb);
GDS_TXT *gds_text_head (void);
char *gds_topcell (void);
int gds_update_bbox (GDS_TRANS trans, char *name, LONGINT *x0, LONGINT *y0, LONGINT *x1, LONGINT *y1);
void gds_write_cell (GDS_TRANS trans, char *name, FILE *fp);
void gds_write_flat (GDS_TRANS trans, char *name, FILE *fp);
void gds_write_lib (char *name, FILE *fp);
void gds_size_flat (GDS_TRANS trans, char *name, LONGINT* minx, LONGINT* miny, LONGINT* maxx, LONGINT* maxy);
void gds_listlayers (char *name, int with_structures, char *file);
void gds_print_hier (char *name, int max_level);
#ifdef __cplusplus
}
#endif

#endif /* _gdsutil_h */

