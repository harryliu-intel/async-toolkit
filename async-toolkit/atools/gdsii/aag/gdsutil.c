// "$Id$ AAG";

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/types.h>
#include <assert.h>
#ifndef WINNT
#include <sys/timeb.h>
#endif
#include <regex.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include "c_lib.h"
#include "btutil.h"
#include "gdsutil.h"
#define  GDS_FILL_SPACE

double round(double);

long long boundary_count;

//#define GDS_DEBUG
#define LONGINT_MAX 2147483647L
#define LONGINT_MIN -2147483647L

int dowarning = 0;
int dorescale = 1;

static int check_flat_list (void);
static void gds_do_aref (GDS_TRANS trans, GDS_STRUCTURE *srf);
static void gds_do_boundary (GDS_TRANS trans, unsigned code);
static void gds_do_node (GDS_TRANS trans);
static void gds_do_sref (GDS_TRANS trans, GDS_STRUCTURE *srf);
static void gds_do_text (GDS_TRANS trans);
static int gds_excluded (char *name);
static int gds_excluded_regex (char *name);
static void gds_fill_space (FILE *fp);
static void gds_flat_aref (GDS_TRANS trans, FILE *fp);
static void gds_flat_sref (GDS_TRANS trans, FILE *fp);
static void gds_flat_structure (GDS_TRANS trans, char *name, FILE *fp);
static void gds_free_items (void);
static void gds_hier_sref_aref (GDS_TRANS trans, FILE *fp);
static unsigned gds_item_len (int len, unsigned code);
static void gds_loc_structure (char *name);
static int gds_mgetint2 (int fp);
static int gds_mgetlen (int f);
static void gds_new_aref (char *name, GDS_TRANS trans, int col, int row, LONGINT dx, LONGINT dy);
static void gds_new_ref (char *name, GDS_TRANS trans);
static GDS_ITEM *gds_next_item (int fp);
static int gds_pt_in_poly (long px, long py, long poly[][2], int nverts);
static void gds_time (char *data);
static void gds_write_header (char *name, FILE *fp);
static void gds_write_structure (GDS_TRANS trans, char *name, FILE *fp);
static void gds_get_next_item (int *ln, unsigned int *cd, int fp);
static void gds_flat_size (GDS_TRANS trans, char *name, LONGINT* minx, LONGINT* miny, LONGINT* maxx, LONGINT* maxy);
static void gds_hier_sref_aref_size (GDS_TRANS trans, LONGINT *minx, LONGINT* miny, LONGINT* maxx, LONGINT* maxy);
static void gds_flat_sref_size (GDS_TRANS trans, LONGINT *minx, LONGINT* miny, LONGINT* maxx, LONGINT *maxy);
static void gds_flat_aref_size (GDS_TRANS trans, LONGINT *minx, LONGINT* miny, LONGINT* maxx, LONGINT *maxy);

typedef struct gds_file GDS_FILE;
#define MAX_FILES 50000
static int gds_next_file=0;
struct gds_file {
   unsigned char *map;
   unsigned char *ptr;
   float    scale;
   off_t          length;
   char           *name;
};

static GDS_FILE gds_files[MAX_FILES];

static double deg2rad, rad2deg, pi;

typedef void (*FUNCT)(GDS_BNDY *);
static FUNCT func_search;

static GDS_RECT *incl_rect, *excl_rect;
static GDS_ITEM *item_head, *item_tail;

GDS_ITEM  *gds_item_ptr;
static int string_case = 0;
static GDS_BNDY *bndy_head, *bndy_tail;
static GDS_TXT *text_head, *text_tail;
static GDS_ORG *org_head, *org_tail;
static GDS_REF *ref_head, *ref_tail;
static double g_tmp[65536];
static GDS_DATA global_tmp;

BTTREE bt_structure;

// list of srefs not to descend into when writing flat gds
BTTREE bt_flat_structure;
BTTREE bt_layersize;

typedef struct size_struct {
    LONGINT minx, miny, maxx, maxy;
} SIZE_STRUCTURE;

static BTTREE bt_list;

static int           initialized;
static int           skip_text = 0;
static ULONGINT      layer_mask[MAX_LAYER];
static int           mask_active = 1;
static LONGINT       LONGINT_units = 1L;
static char          use_cell[USE_CELL_CNT][MAX_LAYER];
static int           current_cell;
static LONGINT       minx, maxx, miny, maxy;
static int           first;
static int           aref_mode = 1;
static int           path2boundary = 0;
static int           do_bndy   = 1;
static int           do_ref    = 1;
static int           do_text   = 1;
static int           do_org    = 1;
static int           wrerr;

/* lookup structure */
GDS_LOOKUP gds_lookup[0x40] = 
   {
      { "HEADER", 2, GDS_HEADER},  
      { "BGNLIB", 24, GDS_BGNLIB},  
      { "LIBNAME", -1, GDS_LIBNAME},  
      { "UNITS", 16, GDS_UNITS},  
      { "ENDLIB", 0, GDS_ENDLIB},  
      { "BGNSTR", 24, GDS_BGNSTR},  
      { "STRNAME", -1, GDS_STRNAME},  
      { "ENDSTR", 0, GDS_ENDSTR},  
      { "BOUNDARY", 0, GDS_BOUNDARY},  
      { "PATH", 0, GDS_PATH},  
      { "SREF", 0, GDS_SREF},  
      { "AREF", 0, GDS_AREF},  
      { "TEXT", 0, GDS_TEXT},  
      { "LAYER", 2, GDS_LAYER},  
      { "DATATYPE", 2, GDS_DATATYPE},  
      { "WIDTH", 4, GDS_WIDTH},  
      { "XY", -1, GDS_XY},  
      { "ENDEL", 0, GDS_ENDEL},  
      { "SNAME", -1, GDS_SNAME},  
      { "COLROW", 4, GDS_COLROW},  
      { "TEXTNODE", 0, GDS_TEXTNODE},  
      { "NODE", 0, GDS_NODE},  
      { "TEXTTYPE", 2, GDS_TEXTTYPE},  
      { "PRESENTATION", 2, GDS_PRESENTATION},  
      { "NONE", -1, 0},  
      { "STRING", -1, GDS_STRING},
      { "STRANS", 2, GDS_STRANS},
      { "MAG", 8, GDS_MAG},
      { "ANGLE", 8, GDS_ANGLE},
      { "NONE", -1, 0},
      { "NONE", -1, 0},
      { "REFLIBS", -1, GDS_REFLIBS},
      { "FONTS", 176, GDS_FONTS},
      { "PATHTYPE", 2, GDS_PATHTYPE},
      { "GENERATIONS", 2, GDS_GENERATIONS},
      { "ATTRTABLE", -1, GDS_ATTRTABLE},
      { "STYPTABLE", -1, GDS_STYPTABLE},
      { "STRTYPE", 2, GDS_STRTYPE},
      { "ELFLAGS", 2, GDS_ELFLAGS},
      { "ELKEY", 4, GDS_ELKEY},  
      { "LINKTYPE", 2, GDS_LINKTYPE},  
      { "LINKKEYS", 4, GDS_LINKKEYS},  
      { "NODETYPE", 2, GDS_NODETYPE},  
      { "PROPATTR", 2, GDS_PROPATTR},  
      { "PROPVALUE", -1, GDS_PROPVALUE},  
      { "BOX", 0, GDS_BOX},  
      { "BOXTYPE", 2, GDS_BOXTYPE},  
      { "PLEX", 4, GDS_PLEX},  
      { "BGNEXTN", 4, GDS_BGNEXTN},  
      { "ENDEXTN", 4, GDS_ENDEXTN},  
      { "TAPENUM", 2, GDS_TAPENUM},  
      { "TAPECODE", 2, GDS_TAPECODE},  
      { "STRCLASS", 2, GDS_STRCLASS},  
      { "RESERVED", 4, GDS_RESERVED},  
      { "FORMAT", 2, GDS_FORMAT},  
      { "MASK", -1, GDS_MASK},  
      { "ENDMASKS", 0, GDS_ENDMASKS},  
   };

typedef struct gds_stack {
    int fp;
    unsigned char *ptr;
    int line;
} GDS_STACK;

#define STACKDEPTH 128
static GDS_STACK stack[STACKDEPTH];
static GDS_STACK *stackptr = stack;

static void push (int line)
{
#ifdef GDS_DEBUG
   fprintf (stderr, "push %d %lld %s %d\n", stackptr-stack, (long long) (gds_item_ptr->ptr-gds_files[gds_item_ptr->fp].map), gds_lookup[gds_item_ptr->code>>8].name, line);
#endif
   stackptr -> fp = gds_item_ptr->fp;
   stackptr -> ptr = gds_item_ptr->ptr;
   stackptr -> line = line;
   stackptr++;
   if (stackptr > &stack[STACKDEPTH-1]) {
      fprintf (stderr, "Error: Item stack overflow\n");
      exit(1);
   }
}

static GDS_ITEM *pop (int line)
{
   static GDS_ITEM item;
   int    len;
   unsigned code;
   stackptr--;
   if (stackptr < &stack[0]) {
      fprintf (stderr, "Error: Item stack underflow\n");
      exit(1);
   }
   if (stackptr -> line != line)
      fprintf (stderr, "Error: Unbalanced pop %d => %d\n",
         stackptr -> line, line);
   gds_files[stackptr -> fp].ptr = stackptr -> ptr;
   gds_get_next_item (&len, &code, stackptr -> fp);
   item.code = code;
   item.len = len;
   item.flags = 0;
   item.fp = stackptr -> fp;
   item.ptr = stackptr -> ptr;
   item.next_ptr = gds_files[stackptr -> fp].ptr;
   item.data.ucptr = item.ptr + 4;
   gds_item_ptr = &item;
#ifdef GDS_DEBUG
   fprintf (stderr, "pop %d %s %d\n", stackptr-stack, gds_lookup[item.code>>8].name, line);
#endif
   return &item;
}


/*
 * Do NO string translation =  0
 *    UpperCase all strings =  1
 *    LowerCase all strings = -1
 */
int gds_string_case(int choice)
{
    string_case = choice;

    return string_case;
}

static int gds_rect_ok (LONGINT minx, LONGINT miny, LONGINT maxx, LONGINT maxy)
{
   GDS_RECT *rect;
   int       incl_ok, excl_ok;

   /* boundary must intersect ok area */
   if (incl_rect)
      incl_ok = 0;
   else
      incl_ok = 1;
   for (rect = incl_rect; !incl_ok && rect; rect = rect -> next) {
      if (rect -> minx <= maxx && rect -> miny <= maxy &&
          rect -> maxx >= minx && rect -> maxy >= miny)
         incl_ok = 1;
   }
   excl_ok = 1;
   for (rect = excl_rect; excl_ok && rect; rect = rect -> next) {
      if (rect -> minx <= minx && rect -> miny <= miny &&
          rect -> maxx >= maxx && rect -> maxy >= maxy)
         excl_ok = 0;
   }

   return incl_ok && excl_ok;
}

static void gds_fwrite (void *buff, int size, int nitems, FILE *fp)
{
   if ((int) fwrite (buff, size, nitems, fp) != nitems) {
      if (wrerr++ == 0)
         fprintf (stderr, "Error: Cannot write, disk probably full\n");
   }
}

static int  is_use_cell (char *name)
{
   int i;

   if (use_cell[0][0] == 0)
      return (-1);
   for (i = 0; use_cell[i][0] && i < USE_CELL_CNT; i++)
      if (!strcmp (use_cell[i], name))
         return (1);
   return (0);
}

static void out_of_memory (int ln)
{
   fprintf (stderr, "Error: Out of memory at %d %s\n", ln, __FILE__);
   exit (1);
}

int gds_do_convert (int val)
{
   //  we always need to convert
   return 1;
}

/* Note that this is called ONLY during gds_load. It is an
   error to do so at other times because this changes the
   memory map of the gds file */
void gds_convert (GDS_ITEM *item)
{
   register int i;
   unsigned     mlen;
   int          length;

   // need to always convert now
   if (! item->len || (item->flags & GDS_FLAG_CONV))
      return;
   item -> flags |= GDS_FLAG_CONV;
   mlen = gds_item_len (item->len, item->code);
   if (item->len != mlen)
      fprintf (stderr, "Error: Item %s %d->%u\n",
         gds_lookup[item->code>>8].name, item->len, mlen);
   switch (item -> code & 0xff)
   {
   case GDS_NODATA:
      break;
   case GDS_BITARRAY:
   case GDS_INT2:
      for (i = 0; i < item -> len; i += 2)
         global_tmp.sptr[i/2] = gds_sgetint2 (item -> data.cptr + i);
      memcpy (item->data.cptr, global_tmp.cptr, mlen);
      break;
   case GDS_INT4:
      for (i = 0; i < item -> len; i += 4)
         global_tmp.lptr[i/4] = gds_sgetint4 (item -> data.cptr + i);
      memcpy (item->data.cptr, global_tmp.cptr, mlen);
      break;
   case GDS_REAL4:
      for (i = 0; i < item -> len; i += 4)
         global_tmp.dptr[i/4] = gds_sgetreal4 (item -> data.cptr + i);
      memcpy (item->data.cptr, global_tmp.cptr, mlen);
      break;
   case GDS_REAL8:
      for (i = 0; i < item -> len; i += 8)
         global_tmp.dptr[i/8] = gds_sgetreal8 (item -> data.cptr + i);
      memcpy (item->data.cptr, global_tmp.cptr, mlen);
      break;
   case GDS_ASCII:
      break;
   }
   if (gds_files[item->fp].scale != 1.0 && dorescale != 0) {
//       char str[256];
       switch (item->code) {
       case GDS_XY:
       case GDS_WIDTH:
       case GDS_BGNEXTN:
       case GDS_ENDEXTN:
         length = item->len / 4;
         if (dowarning) fprintf(stderr, "L %d %X\n", length, item->code);
         for (i = 0; i < length; i++) {
           item->data.lptr[i]
            =(int)round(item->data.lptr[i]*gds_files[item->fp].scale);
         }
         break;
       }
   }
}

GDS_ITEM *gds_get_item (void)
{
   if (gds_item_ptr) {
      int fp = gds_item_ptr->fp;
      unsigned char *ptr = gds_item_ptr->next_ptr;
      gds_files[fp].ptr = ptr;
      gds_item_ptr = gds_next_item (fp);
      gds_item_ptr -> ptr = ptr;
      gds_item_ptr -> next_ptr = gds_files[fp].ptr;
#ifdef GDS_DEBUG
      fprintf (stderr, "gip %lld %d %s %u",
         (long long) (gds_item_ptr->ptr-gds_files[fp].map), fp,
            gds_lookup[gds_item_ptr -> code >> 8].name, gds_item_ptr->len);
      if (gds_item_ptr->code == GDS_STRANS)
         fprintf (stderr, " %04x", gds_item_ptr->data.sptr[0]);
      fprintf (stderr, "\n");
#endif
   }

   return (gds_item_ptr);
}

void gds_get_next_item (int *ln, unsigned int *cd, int fp)
{
   int len = 0;
   unsigned int code = 0;
#ifdef GDS_DEBUG
   static long long count = 0;

   fprintf (stderr, "%lld count\n", count++);
#endif
   len = gds_mgetlen (fp);
   if (len < 0) {
      *ln = -1;
      *cd = 0;
      return;
   }
   if (len < 4 && len != 0) {
      *ln = -1;
      *cd = 0;
      fprintf (stderr, "Error: Illegal length at %lld\n", (long long) (gds_files[fp].ptr - gds_files[fp].map));
      return;
   }
   if (len == 0) {
      *ln = -1;
      *cd = 0;
      return;
   }
   len -= 4;
   code = gds_mgetint2 (fp);
#ifdef GDS_DEBUG
   fprintf (stderr, "%s code\n", gds_lookup[code>>8].name);
#endif
   if ((code >> 8 ) >= 0x40 || (unsigned) gds_lookup[code >> 8].code != code)
      fprintf (stderr, "Error: Illegal code %04X\n", code);
   gds_files[fp].ptr += len;
   *ln = len;
   *cd = code;
}

static unsigned gds_item_len (int len, unsigned code)
{
   unsigned mlen;
   mlen = len;
   switch (code & 0xff)
   {
   case GDS_NODATA:  /* nothing */
      break;
   case GDS_BITARRAY:
   case GDS_INT2:
      /* shorts are length 2 in gds */
      if (mlen < sizeof (unsigned short) * len / 2)
         mlen = sizeof (unsigned short) * len / 2;
      break;
   case GDS_INT4:
      /* int's are length 4 in gds */
      if (mlen < sizeof (LONGINT) * len / 4)
         mlen = sizeof (LONGINT) * len / 4;
      break;
   case GDS_REAL4:
      /* floats are length 4 in gds */
      if (mlen < sizeof (double) * len / 4)
         mlen = sizeof (double) * len / 4;
      break;
   case GDS_REAL8:
      /* long reals are length 8 in gds */
      if (mlen < sizeof (double) * len / 8)
         mlen = sizeof (double) * len / 8;
      break;
   case GDS_ASCII:
      /* strings are strings but without trailing NULL */
      mlen = len;
      break;
   }
   return mlen;
}

#if 0
GDS_ITEM *gds_read_item (int fp)
{
   int i,len;
   unsigned int code;
   GDS_ITEM *item;
   int      skip_item = 0;

   gds_get_next_item (&len, &code, fp);
   if (len < 0)
      return (NULL);
   if ( code == GDS_TEXT && skip_text) skip_item = 1;
   while (skip_item) {
       gds_get_next_item (&len, &code, fp);
       if (len < 0)
          return (NULL);
       if (code == GDS_ENDEL) {
           skip_item = 0;
           gds_get_next_item (&len, &code, fp);
           if (len < 0)
              return (NULL);
           if ( code == GDS_TEXT && skip_text) skip_item = 1;
       }
   }
   item = gds_add_item (len, code, global_data.cptr);
   if (item -> code == GDS_XY || item -> code == GDS_WIDTH ||
         item -> code == GDS_BGNEXTN || item -> code == GDS_ENDEXTN) {
      gds_convert (item);
      for (i = 0; i < item -> len / 4; i++)
         item -> data.lptr[i] *= LONGINT_units;
   }

   return (item);
}
#endif

static GDS_ITEM *gds_next_item (int fp)
{
   int len;
   unsigned int code;
   unsigned char *ptr;
   static GDS_ITEM item;

   ptr = gds_files[fp].ptr;
   gds_get_next_item (&len, &code, fp);
   if (len < 0)
      return (NULL);
   item.len = len;
   item.code=code;
   item.ptr = ptr;
   item.fp = fp;
   item.flags=0;
   item.data.ucptr = ptr+4;
   return (&item);
}

void gds_transform (GDS_TRANS trans, LONGINT *xp, LONGINT *yp)
{
   int ang;
   LONGINT x, y, tmp;
   LONGINT tx, ty;

   x = *xp;
   y = *yp;
   if (trans.strans & 0x8000)
      y = -y;
   while (trans.angle < 0.0)
      trans.angle += 360.0;
   while (trans.angle >= 360.0)
      trans.angle -= 360.0;
   ang = (int) (trans.angle + 0.5);
   if (x < 0)
      tx = -x;
   else
      tx = x;
   if (y < 0)
      ty = -y;
   else
      ty = y;
   tx = (LONGINT)(tx * trans.mag + 0.5);
   ty = (LONGINT)(ty * trans.mag + 0.5);
   if (x < 0)
      x = -tx;
   else
      x = tx;
   if (y < 0)
      y = -ty;
   else
      y = ty;
   switch (ang % 360)
   {
   case 90:
      tmp = x;
      x = -y;
      y = tmp;
      break;
   case 180:
      x = -x;
      y = -y;
      break;
   case 270:
      tmp = x;
      x = y;
      y = -tmp;
      break;
   case 0:
      break;
   default:
      tmp = x * cos (trans.angle * deg2rad) -
            y * sin (trans.angle * deg2rad) + 0.5;
      y = x * sin (trans.angle * deg2rad) +
            y * cos (trans.angle * deg2rad) + 0.5;
      x = tmp;
      break;
   }
   *xp = x + trans.x;
   *yp = y + trans.y;
}

void gds_add_structure (char *name, int fp, unsigned char *ptr)
{
   GDS_STRUCTURE *srf;
   
   if (string_case == 1)
       strupper(name);
   else if (string_case == -1) 
       strlower(name);

   if (!(srf = (GDS_STRUCTURE *) btins (name, &bt_structure))) {
      if ((srf = (GDS_STRUCTURE *) btloc (name, &bt_structure)) && srf->fp>=0)
         if (dowarning)
             fprintf (stderr, "Warning: Duplicate structure name %s\n", name);
   }
   if (!(srf = (GDS_STRUCTURE *) btloc (name, &bt_structure))) {
      if (dowarning)
          fprintf (stderr, "Error: Cannot insert structure name %s\n", name);
      return;
   }
   printf("Adding structure %s\n",name);
   srf -> bbox.minx = srf -> bbox.miny =  0;
   srf -> bbox.maxx = srf -> bbox.maxy = -1; 
   srf -> bbox_updated = 0;
   srf -> fp = fp;
   srf -> ptr = ptr;
}

int gds_func_structure (GDS_TRANS trans, char *name, FUNCT func)
{
   GDS_ITEM *item;
   GDS_ORG  *org;
   GDS_TRANS orgtrans;
   GDS_STRUCTURE *srf;
   int      do_cell;

   if (gds_excluded (name) || gds_excluded_regex (name))
      return (1);
   if (!(srf = (GDS_STRUCTURE *) btloc (name, &bt_structure)))
      return (0);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
      return (0);
   func_search = func;
   do_bndy   = 1;
   do_ref    = 0;
   do_text   = 0;
   do_org    = 0;
   do_cell = (is_use_cell (name) != 0);
   if (do_cell) {
      first = 1;
      minx = miny = maxx = maxy = 0;
      orgtrans = trans;
   }
   gds_item_ptr = item;
   /* preserve mirror only */
   trans.strans &= 0x8000;
   gds_get_item();
   gds_get_item();
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR) {
      current_cell |=do_cell;

      switch (item -> code)
      {
      case GDS_STRCLASS:
      case GDS_STRTYPE:
         break;
      case GDS_BOUNDARY:
      case GDS_BOX:
      case GDS_PATH:
         gds_do_boundary (trans, item -> code);
         break;
      case GDS_SREF:
         gds_do_sref (trans, srf);
         break;
      case GDS_AREF:
         gds_do_aref (trans, srf);
         break;
      case GDS_TEXT:
         gds_do_text (trans);
         break;
      case GDS_NODE:
         gds_do_node (trans);
         break;
      case GDS_ELKEY:
      case GDS_PROPATTR:
      case GDS_PROPVALUE:
         break;
      default:
         fprintf (stderr,
            "Error: Illegal GDS element code %s in structure %s line %d\n",
            gds_lookup[item -> code >> 8].name, name, __LINE__);
         break;
      }
   }
   if (do_org && do_cell && (maxx > minx || maxy > miny || !mask_active)) {
      if ((org = (GDS_ORG *) calloc ((size_t) 1, (size_t) sizeof (GDS_ORG)))) {
         if (org_tail)
            org_tail -> next = org;
         else
            org_head = org;
         org_tail = org;
         org -> trans = orgtrans;
         if ((org -> structure = malloc ((unsigned) strlen (name) + 1)))
            strcpy (org -> structure, name);
         else
            out_of_memory (__LINE__);
      }
      else
         out_of_memory (__LINE__);
      org -> cx = (minx + maxx) / 2;
      org -> cy = (miny + maxy) / 2;
      org -> minx = minx;
      org -> maxx = maxx;
      org -> miny = miny;
      org -> maxy = maxy;
   }
   if (do_cell)
      current_cell = 0;
   func_search = (FUNCT) 0;
   return (1);
}

/*** expand a GDS_BBOX to include a point ***/
void gds_expand_bbox(GDS_BBOX *bbox, LONGINT x, LONGINT y)
{
  if (bbox->minx>bbox->maxx) {
    bbox->minx=bbox->maxx=x;
    bbox->miny=bbox->maxy=y;
  }
  else {
    if (x<bbox->minx) bbox->minx=x;
    if (x>bbox->maxx) bbox->maxx=x;
    if (y<bbox->miny) bbox->miny=y;
    if (y>bbox->maxy) bbox->maxy=y;
  }
}

/*** global to accumulate bbox used with gds_bndy_bbox function ***/
static GDS_BBOX *top_bbox = NULL;

/*** helper function to expand top_bbox from a GDS_BNDY ***/
void gds_bndy_bbox(GDS_BNDY *bndy)
{
  LONGINT x0=0,x1=0,y0=0,y1=0;
  gds_minmax(bndy,&x0,&x1,&y0,&y1);
  if (top_bbox->minx <= top_bbox->maxx) {
    if (x0<top_bbox->minx) top_bbox->minx = x0;
    if (x1>top_bbox->maxx) top_bbox->maxx = x1;
    if (y0<top_bbox->miny) top_bbox->miny = y0;
    if (y1>top_bbox->maxy) top_bbox->maxy = y1;
  }
  else {
    top_bbox->minx = x0;
    top_bbox->miny = y0;
    top_bbox->maxx = x1;
    top_bbox->maxy = y1;
  }
}

/*** fill in the bbox of all cells underneath top cell, return bbox of top cell ***/
int gds_update_bbox(GDS_TRANS trans, char *name,
                    LONGINT *x0, LONGINT *y0, LONGINT *x1, LONGINT *y1)
{
  GDS_STRUCTURE *srf;
  GDS_BBOX bbox;
  bbox.minx=bbox.miny=0; bbox.maxx=bbox.maxy=-1;
  top_bbox = &bbox;
  func_search = &gds_bndy_bbox;
  do_bndy   = 1;
  do_ref    = 0;
  do_text   = 0;
  do_org    = 0;
  gds_read_structure(trans, name);
  if (!(srf = (GDS_STRUCTURE *) btloc (name, &bt_structure))) return (0);
  *x0 = srf->bbox.minx;
  *y0 = srf->bbox.miny;
  *x1 = srf->bbox.maxx;
  *y1 = srf->bbox.maxy;
  return (1);
}

/** transform this srf bounding box top_bbox ***/
void update_parent_bbox(GDS_TRANS trans, GDS_STRUCTURE *srf, char *name) {
  LONGINT x, y;
  GDS_BBOX bbox;
  bbox.minx=bbox.miny=0; bbox.maxx=bbox.maxy=-1;
  // expand bbox of parent cell
  x = srf->bbox.minx; y = srf->bbox.miny;
  gds_transform(trans,&x,&y); gds_expand_bbox(top_bbox,x,y);
  x = srf->bbox.maxx; y = srf->bbox.maxy;
  gds_transform(trans,&x,&y); gds_expand_bbox(top_bbox,x,y);
#if DEBUG
  fprintf(stderr,"Using cached bbox of %s\n",name);
#endif
}

int gds_read_structure (GDS_TRANS trans, char *name)
{
   GDS_ITEM *item;
   GDS_ORG  *org;
   GDS_TRANS orgtrans;
   GDS_STRUCTURE *srf;
   GDS_BBOX  *old_bbox;
   GDS_TRANS old_trans;
   int       do_cell;

   /*** end recursion for various reasons **/
   if (gds_excluded (name) || gds_excluded_regex (name))
      return (1);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
      return (0);
   if (!(srf = (GDS_STRUCTURE *) btloc (name, &bt_structure)))
      return (0);

   /*** skip cells with empty bbox ***/
   if (srf->bbox_updated && (srf->bbox.minx > srf->bbox.maxx)) {
#ifdef GDS_DEBUG
     fprintf(stderr,"Skipping %s because of empty bbox\n",name);
#endif
     return 1;
   }

   /*** clip to visible screen ***/
   if (srf->bbox_updated && func_search!=&gds_bndy_bbox) {
     LONGINT x, y;
     GDS_BBOX bbox;
     // transform to world coordinates
     bbox.minx=bbox.miny=0; bbox.maxx=bbox.maxy=-1;
     x = srf->bbox.minx; y = srf->bbox.miny;
     gds_transform(trans,&x,&y); gds_expand_bbox(&bbox,x,y);
     x = srf->bbox.maxx; y = srf->bbox.maxy;
     gds_transform(trans,&x,&y); gds_expand_bbox(&bbox,x,y);
     if (!gds_rect_ok(bbox.minx, bbox.miny, bbox.maxx, bbox.maxy)) {
#ifdef GDS_DEBUG
       fprintf(stderr,"Skipping %s because of bbox clipping\n",name);
#endif
       return 1;
     }
   }

   /*** used cached bbox to expand parent bbox ***/
   if (srf->bbox_updated && func_search==&gds_bndy_bbox) {
     update_parent_bbox(trans,srf,name);
     return 1;
   }

   /*** point top_bbox to this srf (parameter passing via a global) ***/
   if (func_search==&gds_bndy_bbox) {
     old_bbox=top_bbox;
     top_bbox=&srf->bbox;
     old_trans=trans;
     gds_clear_trans(&trans);
   }

   /*** process contents ***/
   do_cell = (is_use_cell (name) != 0);
   if (do_cell) {
      first = 1;
      minx = miny = maxx = maxy = 0;
      orgtrans = trans;
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "A %s\n", gds_lookup[item -> code >> 8].name);
#endif
   gds_item_ptr = item;
   /* preserve mirror only */
   trans.strans &= 0x8000;
   while (gds_item_ptr->code != GDS_STRNAME)
       gds_get_item();
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR) {
#ifdef GDS_DEBUG
      fprintf (stderr, "B %s\n", gds_lookup[item -> code >> 8].name);
#endif
      current_cell |= do_cell;

      switch (item -> code)
      {
      case GDS_STRCLASS:
      case GDS_STRTYPE:
         break;
      case GDS_BOUNDARY:
      case GDS_BOX:
      case GDS_PATH:
         gds_do_boundary (trans, item -> code);
         break;
      case GDS_SREF:
         gds_do_sref (trans, srf);
         break;
      case GDS_AREF:
         gds_do_aref (trans, srf);
         break;
      case GDS_TEXT:
         gds_do_text (trans);
         break;
      case GDS_NODE:
         gds_do_node (trans);
         break;
      case GDS_ELKEY:
      case GDS_PROPATTR:
      case GDS_PROPVALUE:
         break;
      default:
         fprintf (stderr,
            "Error: Illegal GDS element code %s in structure %s line %d\n",
            gds_lookup[item -> code >> 8].name, name, __LINE__);
         break;
      }
   }
   if (do_org && do_cell && (maxx > minx || maxy > miny || !mask_active)) {
      if ((org = (GDS_ORG *) calloc ((size_t) 1, (size_t) sizeof (GDS_ORG)))) {
         if (org_tail)
            org_tail -> next = org;
         else
            org_head = org;
         org_tail = org;
         org -> trans = orgtrans;
         if ((org -> structure = malloc ((unsigned) strlen (name) + 1)))
            strcpy (org -> structure, name);
         else
            out_of_memory (__LINE__);
      }
      else
         out_of_memory (__LINE__);
      org -> cx = (minx + maxx) / 2;
      org -> cy = (miny + maxy) / 2;
      org -> minx = minx;
      org -> maxx = maxx;
      org -> miny = miny;
      org -> maxy = maxy;
   }
   if (do_cell) {
      current_cell = 0;
   }

   /*** update bbox if its not defined ***/
   if (func_search==&gds_bndy_bbox) {
     top_bbox=old_bbox;
     trans=old_trans;
     if (!srf->bbox_updated) {
       srf->bbox_updated=1;
#ifdef GDS_DEBUG
       fprintf(stderr,"Updated bbox of %s to x0=%d y0=%d x1=%d y1=%d\n",name,
              srf->bbox.minx,srf->bbox.miny,srf->bbox.maxx,srf->bbox.maxy);
#endif
       update_parent_bbox(trans,srf,name);
     }
   }

   return 1;
}

static void gds_do_text (GDS_TRANS trans)
{
   GDS_ITEM *item;
   GDS_TXT *titem;
   LONGINT x, y;
   int layer = 0;
   int texttype = 0;
   ULONGINT typemask = 1;
   char *string = NULL;

   trans.strans &= 0x8000;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL) {
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_PROPATTR:
      case GDS_PRESENTATION:
         break;
      case GDS_LAYER:
         layer = *item -> data.sptr;
         break;
      case GDS_TEXTTYPE:
      case GDS_DATATYPE:
         typemask = (ULONGINT)1<<(*item -> data.sptr);
         texttype = *item -> data.sptr;
         break;
         break;
      case GDS_XY:
         if ((layer_mask[layer] & typemask) && current_cell ) {
            x = item -> data.lptr[0];
            y = item -> data.lptr[1];
         }
         break;
      case GDS_STRING:
         string = calloc (1, (unsigned) item -> len + 2);
         if (!string)
            out_of_memory (__LINE__);
         memcpy (string, item -> data.cptr, (int) item->len);
         string[item -> len] = 0;
         break;
      case GDS_WIDTH:
         break;
      case GDS_STRANS:
      case GDS_ANGLE:
      case GDS_MAG:
         break;
      case GDS_PATHTYPE:
         break;
      default:
         fprintf (stderr, "Error: Illegal code %s in TEXT\n",
            gds_lookup[item -> code >> 8].name);
         break;
      }
   }
   if (do_text && (layer_mask[layer] & typemask) && current_cell ) {
      titem = (GDS_TXT *) calloc (1, (unsigned) sizeof (GDS_TXT));
      if (!titem)
         out_of_memory (__LINE__);
      if (text_head) {
         text_tail -> next = titem;
         text_tail = titem;
      }
      else
         text_head = text_tail = titem;
      titem -> next = NULL;
      titem -> layer = layer;
      titem -> texttype = texttype;
      titem -> string = string;
      gds_transform (trans , &x, &y);
      titem -> x = x;
      titem -> y = y;
   }
   else if (string)
      free (string);
}

static void gds_do_node (GDS_TRANS trans)
{
   GDS_ITEM *item;

   trans.strans &= 0x8000;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL) {
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_PROPATTR:
         break;
      case GDS_LAYER:
         break;
      case GDS_NODETYPE:
         break;
      case GDS_XY:
         break;
      default:
         fprintf (stderr, "Error: Illegal code %s in NODE\n",
            gds_lookup[item -> code >> 8].name);
         break;
      }
   }
}

static void convert2boundary (int pathtype, int width, unsigned *length,
   LONGINT *x, LONGINT *y, LONGINT bgnextn, LONGINT endextn)
{
   LONGINT newx[8192], newy[8192];
   unsigned i = 0, j = 0;
   double  a1, a2, ad;
   GDS_TRANS trans;

   trans.strans = 0;
   trans.mag = 1.0;
   if (y[1] == y[0] && x[1] == x[0])
      trans.angle = 0;
   else
      trans.angle =
         rad2deg * atan2((double)(y[1] - y[0]), (double)(x[1] - x[0]));

   trans.x = x[i];
   trans.y = y[i];

   switch (pathtype) 
   {
   case 0:
   case 4:
      newx[j] = newx[j+1] = -bgnextn;
      break;
   case 1:
   case 2:
      newx[j] = newx[j+1] = -width / 2 - bgnextn;
      break;
   }
   newy[j] = width / 2;
   gds_transform (trans, &newx[j], &newy[j]);
   j++;
   newy[j] = -width / 2;
   gds_transform (trans, &newx[j], &newy[j]);
   j++;
   for (i = 1; i < *length - 1; i++) {
      a1 = trans.angle;
      if (y[i+1] == y[i] && x[i+1] == x[i])
	 continue; /* skip this because it is the same as prior point */
      else
	 a2 = rad2deg * atan2((double)(y[i+1] - y[i]), (double)(x[i+1] - x[i]));
      ad = a2 - a1;
      while (ad < -180.0)
           ad = ad + 360.0;
      while (ad > 180.0)
        ad = ad - 360.0;
      trans.x = x[i];
      trans.y = y[i];
      newy[j] = -width / 2;
      if (ad < -90.0) {
           newx[j] = width / 2 / tan ((pi - ad * deg2rad) / 2);
           gds_transform (trans, &newx[j], &newy[j]);
           j++;
      }
      else if (ad < 0.0) {
           newx[j] = width / 2 * tan (ad * deg2rad / 2);
           gds_transform (trans, &newx[j], &newy[j]);
           j++;
      }
      else if (ad <= 90.0) {
           newx[j] = width / 2 * tan (ad * deg2rad / 2);
           gds_transform (trans, &newx[j], &newy[j]);
           j++;
      }
      else {
           newx[j] = width / pi * ( pi / 2.0 - (180.0 - ad) * deg2rad ) + 0.5;
           gds_transform (trans, &newx[j], &newy[j]);
           j++;
           newy[j] = -width / 2;
           newx[j] = -width / pi * ( pi / 2.0 - (180.0 - ad) * deg2rad ) + 0.5;
           trans.angle = a2;
           gds_transform (trans, &newx[j], &newy[j]);
           j++;
      }
      trans.angle = a2;
   }

   switch (pathtype) {
   case 0:
   case 4:
      newx[j] = newx[j+1] = endextn;
      break; 
   case 1:
   case 2:
      newx[j] = newx[j+1] = width / 2 + endextn;
      break; 
   }

   trans.x = x[i];
   trans.y = y[i];
   if (y[i] == y[i-1] && x[i] == x[i-1])
      trans.angle = 0;
   else
      trans.angle = rad2deg *
	 atan2 ((double) (y[i] - y[i-1]), (double) (x[i] - x[i-1]));
   newy[j] = -width / 2;
   newy[j+1] = width / 2;
   gds_transform (trans, &newx[j], &newy[j]);
   j++;
   gds_transform (trans, &newx[j], &newy[j]);
   j++;
   trans.angle = 180.0 + trans.angle;
   while (trans.angle > 180.0)
      trans.angle = trans.angle - 360.0;
   while (trans.angle < -180)
      trans.angle = trans.angle + 360.0;
   for (i--; i > 0; i--) {
      a1 = trans.angle;
      if (y[i-1] == y[i] && x[i-1] == x[i])
	 continue; /* skip this because it is the same as prior point */
      else
	 a2 = rad2deg * atan2((double)(y[i-1] - y[i]), (double)(x[i-1] - x[i]));
      ad = a2 - a1;
      while (ad < -180.0)
         ad = ad + 360.0;
      while (ad > 180.0)
         ad = ad - 360.0;
      trans.x = x[i];
      trans.y = y[i];
      newy[j] = -width / 2;
      if (ad < -90.0) {
         newx[j] = width / 2 / tan ((pi - ad * deg2rad) / 2);
         gds_transform (trans, &newx[j], &newy[j]);
         j++;
      }
      else if (ad < 0.0) {
         newx[j] = width / 2 * tan (ad * deg2rad / 2);
         gds_transform (trans, &newx[j], &newy[j]);
         j++;
      }
      else if (ad <= 90.0) {
         newx[j] = width / 2 * tan (ad * deg2rad / 2);
         gds_transform (trans, &newx[j], &newy[j]);
         j++;
      }
      else {
         newx[j] = width / pi * ( pi / 2.0 - (180.0 - ad) * deg2rad ) + 0.5;
         gds_transform (trans, &newx[j], &newy[j]);
         j++;
         newy[j] = -width / 2;
         newx[j] = -width / pi * ( pi / 2.0 - (180.0 - ad) * deg2rad ) + 0.5;
         trans.angle = a2;
         gds_transform (trans, &newx[j], &newy[j]);
         j++;
      }
      trans.angle = a2;
   }
   newx[j] = newx[0];
   newy[j] = newy[0];
   *length = ++j;
   memcpy ((void *) x, (void *) newx, j * sizeof (LONGINT));
   memcpy ((void *) y, (void *) newy, j * sizeof (LONGINT));
}

static void gds_do_boundary (GDS_TRANS trans, unsigned code)
{
   int layer = 0, datatype = 0, pathtype=0;
   ULONGINT typemask = 1;
   unsigned int length = 0;
   LONGINT width = 0;
   LONGINT bgnextn = 0, endextn = 0;
   LONGINT x[8192], y[8192]; /* spec says no more than 200 points (GDS_MAX_POINTS), but
                              allow room to expand paths, these are transformed
                              coordinates */
   LONGINT lminx = 0,
           lmaxx = 0,
           lminy = 0,
           lmaxy = 0;
   GDS_ITEM *item;
   GDS_BNDY *bitem, bndy;
   GDS_ITEM  properties;
   GDS_ITEM *propptr = &properties;
   unsigned  i;

   trans.strans &= 0x8000;
   properties.len = 0;
   boundary_count++;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL) {
      if (!current_cell)
         continue;
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_BOXTYPE:
         break;
      case GDS_PATHTYPE:
         pathtype = *item -> data.sptr;
         break;
      case GDS_DATATYPE:
         datatype = *item -> data.sptr;
         typemask = (ULONGINT)1<<(*item -> data.sptr);
         break;
      case GDS_PROPATTR:
         if (properties.len == 0)
             properties = *item;
         break;
      case GDS_LAYER:
         layer = *item -> data.sptr;
         break;
      case GDS_BGNEXTN:
         bgnextn = *item -> data.lptr;
         break;
      case GDS_ENDEXTN:
         endextn = *item -> data.lptr;
         break;
      case GDS_WIDTH:
         width = *item -> data.lptr;
         break;
      case GDS_XY:
         length = item -> len / 8;
         if (length > GDS_MAX_POINTS && dowarning) {
            fprintf (stderr, "Warning: GDS Boundary exceeds SPEC of %d points (%d points)\n", GDS_MAX_POINTS, length);
            //length = GDS_MAX_POINTS;
         }
         if (length > 8192) {
            fprintf (stderr, "Error: GDS Boundary exceeds compiled limit of %d points (%d points)\n", 8192, length);
            length=8192;
         }
         if ((layer_mask[layer] & typemask) && current_cell ) {
            for (i = 0; i < length; i++) {
               x[i] = item -> data.lptr[i * 2];
               y[i] = item -> data.lptr[i * 2 + 1];
               gds_transform (trans, &x[i], &y[i]);
            }
         }
         break;
      default:
         fprintf (stderr, "Error: Illegal code %s in %s\n",
            gds_lookup[item -> code >> 8].name, gds_lookup[code >> 8].name);
         break;
      }
   }

   if (do_bndy && (layer_mask[layer] & typemask) && current_cell) {
       if (length == 0) {
         fprintf (stderr, "Error: No XY\n");
      }
      if (code == GDS_PATH && path2boundary && length) {
         convert2boundary (pathtype, width, &length, x, y, bgnextn, endextn);
         code = GDS_BOUNDARY;
         bgnextn  = 0;
         endextn  = 0;
         width    = 0;
      }

      /** check bounding box (after converting paths to polygons!) **/
      for (i = 0; i < length; i++) {
        if (i == 0) {
          lminx = lmaxx = x[0];
          lminy = lmaxy = y[0];
        }
        else {
          if (lminx > x[i])
            lminx = x[i];
          if (lminy > y[i])
            lminy = y[i];
          if (lmaxx < x[i])
            lmaxx = x[i];
          if (lmaxy < y[i])
            lmaxy = y[i];
        }
        if (first) {
          minx = maxx = x[0];
          miny = maxy = y[0];
          first = 0;
        }
        else {
          if (minx > x[i])
            minx = x[i];
          if (miny > y[i])
            miny = y[i];
          if (maxx < x[i])
            maxx = x[i];
          if (maxy < y[i])
            maxy = y[i];
        }
      }
      if (!gds_rect_ok(lminx, lminy, lmaxx, lmaxy)) return;

      if (func_search) {
         bitem = &bndy;
         bitem -> x = x;
         bitem -> y = y;
      }
      else {
         bitem = (GDS_BNDY *) calloc (1, (unsigned) sizeof (GDS_BNDY));
         if (!bitem)
            out_of_memory (__LINE__);
         bitem -> x = (LONGINT *) malloc (sizeof (LONGINT) * length);
         bitem -> y = (LONGINT *) malloc (sizeof (LONGINT) * length);
         if (!bitem -> y || !bitem -> x)
            out_of_memory (__LINE__);
         memcpy((void *)bitem->x, (void *)x, sizeof(LONGINT) * (int)length);
         memcpy((void *)bitem->y, (void *)y, sizeof(LONGINT) * (int)length);
      }
      bitem -> kind = code;
      bitem -> next = NULL;
      bitem -> len  = length;
      bitem -> datatype = datatype;
      bitem -> bgnextn    = bgnextn;
      bitem -> endextn    = endextn;
      bitem -> width      = width;
      bitem -> layer      = layer;
      bitem -> properties = propptr;
      if (func_search)
         func_search (bitem);
      else {
         if (bndy_head)
            bndy_tail -> next = bitem;
         else
            bndy_head = bitem;
         bndy_tail = bitem;
      }
   }
}

static void gds_do_sref (GDS_TRANS trans, GDS_STRUCTURE *srf)
{
   GDS_TRANS otrans;
   GDS_TRANS calling_trans;
   short     istrans;
   GDS_ITEM *item;
   char name[GDS_NAMELEN];
   LONGINT x0, y0;
   int  st;
   double iangle;

#ifdef GDS_DEBUG
   fprintf (stderr, "gds_do_sref\n");
#endif
   calling_trans = trans;
   /* get SNAME */
   if (!(item = gds_get_item ()))
      return;
   if (item -> len < 1 || item -> code != GDS_SNAME)
      fprintf (stderr, "Error: expected SNAME in gds_do_sref\n");
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[GDS_NAMELEN-1] = 0;
   name[item->len]=0;
#ifdef GDS_DEBUG
   fprintf (stderr, "do_sref %s\n", name);
#endif
   if (item->len < GDS_NAMELEN) {
      name[item->len]=0;
   }
   else if (dowarning) {
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);
   }
   /* preserve mirror only */
   trans.strans &= 0x8000;
   /* save translate for origin */
   otrans = trans;
   istrans = trans.strans;
   x0 = y0 = 0L;
   st = 0;
   iangle = 0.0;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL) {
#ifdef GDS_DEBUG
      if (item->code == GDS_STRANS)
         fprintf (stderr, "fSTR2e %d %04x %04x %08llx\n", item->len, item->data.sptr[0], item->data.sptr[1], (long long) item->data.sptr);
#endif
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_PROPATTR:
         break;
      case GDS_STRANS:
         if (item -> len != 2)
            fprintf (stderr, "Error: expected STRANS in SREF %s\n", name);
         st = *item -> data.sptr;
         if (st & 0x8000) /* reflection */
            trans.strans ^= 0x8000;
         trans.strans |= st & 0x60;
         break;
      case GDS_ANGLE:
         iangle = item -> data.dptr[0];
         if (trans.strans & 0x2)
            trans.angle = iangle;
         break;
      case GDS_MAG:
         if (trans.strans & 0x4)
            trans.mag = item -> data.dptr[0];
         else
            trans.mag *= item -> data.dptr[0];
         break;
      case GDS_XY:
         if (item -> len != 8)
            fprintf (stderr, "Error: expected XY in SREF %s\n", name);
         x0 = item -> data.lptr[0];
         y0 = item -> data.lptr[1];
         break;
      default:
         fprintf (stderr, "Error: Illegal code for SREF %s\n",
            gds_lookup[item -> code >> 8].name);
         break;
      }
   }
   int from = __LINE__;
   push (from);
   if (!(item = (GDS_ITEM *) gds_find_structure (name))) {
      fprintf (stderr, "Error: No structure %s in library\n", name);
      pop(from);
      return;
   }
   gds_transform (otrans, &x0, &y0);
   if (!(st & 0x2)) {
      if (calling_trans.strans & 0x8000)
	 trans.angle = trans.angle - iangle;
      else
	 trans.angle += iangle;
      while (trans.angle >= 360.0)
	 trans.angle -= 360.0;
      while (trans.angle < 0.0)
	 trans.angle += 360.0;
   }
   trans.x = x0;
   trans.y = y0;
/*
   if (is_use_cell (name) == 1)
 */
   gds_new_ref (name, trans);
   gds_item_ptr = item;
   gds_read_structure (trans, name);
   *gds_item_ptr = *pop(from);
}

static void gds_do_aref (GDS_TRANS trans, GDS_STRUCTURE *srf)
{
   GDS_ITEM *item;
   GDS_TRANS calling_trans;
   char name[GDS_NAMELEN];
   int  col, row;
   int  iangle;
   double dtmp;
   LONGINT x0, y0;
   LONGINT x1, y1;
   LONGINT x2, y2;
   LONGINT dx, dy;
   int  i, j;
   int  st;

   calling_trans = trans;
   /* get SNAME */
   if (!(item = gds_get_item ()))
      return;
   if (item -> len < 1 || item -> code != GDS_SNAME)
      fprintf (stderr, "Error: expected SNAME in gds_do_aref\n");
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[GDS_NAMELEN-1] = 0;
   name[item->len]=0;
#ifdef GDS_DEBUG
   fprintf (stderr, "do_aref %s\n", name);
#endif
   if (item->len < GDS_NAMELEN) {
      name[item->len]=0;
   }
   else if (dowarning) {
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "AREF %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   /* preserve mirror only */
   trans.strans &= 0x8000;
   /* initialize local translation */
   st = iangle = col = row = x0 = y0 = x1 = y1 = x2 = y2 = 0;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL)
   {
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_PROPATTR:
         break;
      case GDS_COLROW:
         if (item -> len != 4)
            fprintf (stderr, "Error: expected COLROW in AREF %s\n", name);
         col = item -> data.sptr[0];
         row = item -> data.sptr[1];
         break;
      case GDS_STRANS:
         if (item -> len != 2)
            fprintf (stderr, "Error: expected STRANS in AREF %s\n", name);
         st = *item -> data.sptr;
         if (st & 0x8000) /* reflection */
         {
            trans.strans ^= 0x8000;
         }
         trans.strans |= st & 0x60;
         break;
      case GDS_ANGLE:
         dtmp = item -> data.dptr[0];
         iangle = (int) (dtmp + 0.5);
         if (trans.strans & 0x2)
            trans.angle = dtmp;
         break;
      case GDS_MAG:
         dtmp = item -> data.dptr[0];
         if (trans.strans & 0x4)
            trans.mag = dtmp;
         else
            trans.mag *= dtmp;
         break;
      case GDS_XY:
         if (item -> len != 24)
            fprintf (stderr, "Error: expected XY in AREF %s\n", name);
         x0 = item -> data.lptr[0];
         y0 = item -> data.lptr[1];
         x1 = item -> data.lptr[2];
         y1 = item -> data.lptr[3];
         x2 = item -> data.lptr[4];
         y2 = item -> data.lptr[5];
         break;
      default:
         fprintf (stderr, "Error: Illegal code for AREF %s\n",
            gds_lookup[item -> code >> 8].name);
         break;
      }
   }
   int from=__LINE__;
   push (from);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
   {
      fprintf (stderr, "Error: No structure %s in library\n", name);
      pop(from);
      return;
   }
   if (col == 0 || row == 0)
   {
      fprintf (stderr, "Error: No COLROW in aref %s\n", name);
      pop(from);
      return;
   }
   if (x1 - x0)
      dx = x1 - x0;
   else
      dx = x2 - x0;
   if (y1 - y0)
      dy = y1 - y0;
   else
      dy = y2 - y0;
   /* the row/column "arrangement" can be switched! */
   switch (iangle)
   {
   case 0:
   case 180:
      break;
   case 90: /* row and col for 90 and 270 */
   case 270:
      i = col;
      col = row;
      row = i;
      break;
   default:
      fprintf (stderr,
         "Severe Warning: Do not know how to do AREF at angle %d\n", iangle);
      pop(from);
      return;
      break;
   }
   dx /= col;
   dy /= row;
#ifdef GDS_DEBUG
   fprintf (stderr, "AREF %s, col %d, row %d, dx=%ld, dy=%ld\n",
      name, col, row, (long) dx, (long) dy);
#endif
   /* x0, y0 is AREF origin with respect to calling SREF */
   if (!(st & 0x2))
   {
      if (calling_trans.strans & 0x8000)
      {
	 trans.angle = trans.angle - iangle;
      }
      else
      {
	 trans.angle += iangle;
      }
      while (trans.angle >= 360.0)
	 trans.angle -= 360.0;
      while (trans.angle < 0.0)
	 trans.angle += 360.0;
   }
   if (is_use_cell (name) == 1 && aref_mode)
   {
      trans.x += x0;
      trans.y += y0;
      gds_new_aref (name, trans, col, row, dx, dy);
   }
   else
   {
      for (i = 0; i < row; i++)
      for (j = 0; j < col; j++)
      {
         x1 = x0 + dx * j;
         y1 = y0 + dy * i;
         gds_transform (calling_trans, &x1, &y1);
         trans.x = x1;
         trans.y = y1;
         if (is_use_cell (name) == 1)
            gds_new_ref (name, trans);
         gds_item_ptr = item;
#ifdef GDS_DEBUG
	 fprintf (stderr, "AREF trans=(%d,%d:%4.4x,%.0f)\n",
	    trans.x, trans.y, (unsigned)(trans.strans&0xffff),trans.angle);
#endif
         gds_read_structure (trans, name);
      }
   }
   *gds_item_ptr = *pop(from);
}

void gds_eoferr (char *s)
{
   fprintf (stdout, "Unexpected end of file in %s\n", s);
   exit (1);
}

static int gds_mgetlen (int fp)
{
   int           sign;
   int           a, b;

   a = *(gds_files[fp].ptr++);
   b = *(gds_files[fp].ptr++);
   if (b < 0)
      return (-1);
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 8) + b) + (sign - 1) / 2);
}

int gds_sgetint2 (char *s)
{
   unsigned short a,b;
   int sign;

   a = (*s++) & 0xff;
   b = (*s++) & 0xff;
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 8) + b) + (sign - 1) / 2);
}

LONGINT gds_sgetint4 (char *s)
{
   ULONGINT a,b,c,d;
   int sign;

   a = (*s++) & 0xff;
   b = (*s++) & 0xff;
   c = (*s++) & 0xff;
   d = (*s++) & 0xff;
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      c = (~c) & 0xff;
      d = (~d) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 24) + (b << 16) + (c << 8) + d) + (sign - 1) / 2);
}

double gds_sgetreal4 (char *s)
{
   int a, b, c, d, sign, exp;
   double mant;

   a = (*s++) & 0xff;
   b = (*s++) & 0xff;
   c = (*s++) & 0xff;
   d = (*s++) & 0xff;
   sign = (a & 0x80) ? -1 : 1;
   exp = (a & 0x7f) - 64;
   mant = (((256.0 + d ) /
                  256.0 + c ) / 256.0 + b ) / 256.0;
   while (exp > 0)
   {
      mant *= 16.0;
      exp--;
   }
   while (exp < 0)
   {
      mant /= 16.0;
      exp++;
   }
   return (sign * mant);
}

double gds_sgetreal8 (char *s)
{
   int a, b, c, d, e, f, g, h, sign, exp;
   double mant;

   a = (*s++) & 0xff;
   b = (*s++) & 0xff;
   c = (*s++) & 0xff;
   d = (*s++) & 0xff;
   e = (*s++) & 0xff;
   f = (*s++) & 0xff;
   g = (*s++) & 0xff;
   h = (*s++) & 0xff;
   sign = (a & 0x80) ? -1 : 1;
   exp = (a & 0x7f) - 64;
   mant = ((((((h / 256.0 + g ) /
            256.0 + f ) / 256.0 + e ) / 256.0 + d ) /
                  256.0 + c ) / 256.0 + b ) / 256.0;
   while (exp > 0)
   {
      mant *= 16.0;
      exp--;
   }
   while (exp < 0)
   {
      mant /= 16.0;
      exp++;
   }
   return (sign * mant);
}

static int gds_mgetint2 (int fp)
{
   int a,b,sign;

   a = *(gds_files[fp].ptr++);
   b = *(gds_files[fp].ptr++);
   if (a < 0 || b < 0)
      gds_eoferr ("getint2");
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 8) + b) + (sign - 1) / 2);
}

LONGINT gds_mgetint4 (int fp)
{
   LONGINT a,b,c,d;
   int sign;

   a = *(gds_files[fp].ptr++);
   b = *(gds_files[fp].ptr++);
   c = *(gds_files[fp].ptr++);
   d = *(gds_files[fp].ptr++);
   if (d < 0)
      gds_eoferr ("fgetint4");
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      c = (~c) & 0xff;
      d = (~d) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 24) + (b << 16) + (c << 8) + d) + (sign - 1) / 2);
}

void gds_clear (void)
{
   gds_free_bndy ();
   gds_free_text ();
   gds_free_org ();
   gds_free_ref ();
   gds_set_cell ("");
   aref_mode = 0;
   do_ref = 1;
   do_text = 1;
   do_bndy = 1;
   do_org = 1;
   gds_clear_rect ();
}

void gds_init (void)
{
   if (!initialized)
   {
      pi = atan2 (1.0, 0.0) * 2;
      deg2rad = pi / 180.0;
      rad2deg = 1.0 / deg2rad;
      gds_set_layer_mask ();
      global_tmp.dptr = g_tmp;
   }
   else
   {
      btfree (&bt_structure);
      btfree (&bt_flat_structure);
   }
   
   btinit (&bt_structure, GDS_NAMELEN, sizeof (GDS_STRUCTURE), strcmp);
   // optional list of structures not to descend into when flattening
   btinit (&bt_flat_structure,GDS_NAMELEN,4,strcmp);
   gds_free_items ();
   gds_clear ();
   initialized = 1;
}

/***********************************************************************
* Use this function in replacement for gds_load (fn). 
* In future we can get rid of global bt_structure.
*
* Function: this can be called repeatedly with the same gds_tree. Hence,
*           gds_tree is part of an argument to the function for future use,eg.
*           (*gds_tree) = calloc( 1, ........);
*           Right now, it's just returning global bt_structure.
*
************************************************************************/
int gds_loadfile(BTTREE **gds_tree, char *filename)
{
  if(!gds_load(filename)){
    (*gds_tree) = &bt_structure;
    return(TRUE);
  }
  else{
    return(FALSE);
  }
}

int gds_load (char *fn)
{
   GDS_ITEM *item = NULL;
   GDS_STRUCTURE *srf;
   char   gds_fn[1024];
   char   name[GDS_NAMELEN];
   int    gds;
   int    is_endlib;
   int    fp;
   unsigned char  *prevptr;
   double database_unit;
   double physical_unit;

   if (!fn || !*fn)
      return (1);
   if (gds_next_file >= MAX_FILES) {
      fprintf (stderr, "Error: Too many files, recompile to exceed %d files\n", MAX_FILES);
      return (1);
   }
   strcpy (gds_fn, fn);
   gds = -1;
   gds_files[gds_next_file].map=NULL;
   gds_files[gds_next_file].ptr=NULL;
   gds_files[gds_next_file].length=0;
   gds_files[gds_next_file].scale=1.0;
   if ((gds = open(gds_fn, O_RDONLY)) < 0)
   {
      fprintf (stderr, "Error: Cannot open gds file '%s'.\n", gds_fn);
      return 1;
   }
   gds_files[gds_next_file].name=malloc(strlen(gds_fn)+1);
   if (! gds_files[gds_next_file].name ) {
      out_of_memory(__LINE__);
   }
   strcpy (gds_files[gds_next_file].name, gds_fn);
   if (!initialized)
      gds_init ();

   gds_files[gds_next_file].length = lseek(gds, 0L, SEEK_END);
   if (gds_files[gds_next_file].length == 0) {
      fprintf (stderr, "Error: %s zero length\n", gds_fn);
      return (1);
   }
   gds_files[gds_next_file].map =
      mmap (NULL, (size_t) gds_files[gds_next_file].length, PROT_WRITE | PROT_READ, MAP_PRIVATE, gds, (off_t) 0);
   if ( gds_files[gds_next_file].map == (unsigned char *) -1 || gds_files[gds_next_file].map == NULL ) {
      fprintf (stderr, "Error: mmap failed, error: %d\n", errno);
      return (1);
   }
   fp = gds_next_file++;
   gds_files[fp].ptr = gds_files[fp].map;
   prevptr = gds_files[fp].ptr;
   is_endlib = 0;
   while (!is_endlib)
   {
      prevptr = gds_files[fp].ptr;
      item = gds_next_item(fp);
      if (! item || item -> code == GDS_ENDLIB)
         is_endlib = 1;
      while (item && !is_endlib && item -> code != GDS_BGNSTR)
      {
         gds_convert(item);
         if (item -> code == GDS_UNITS)
         {
            database_unit = item -> data.dptr[0];
            physical_unit = item -> data.dptr[1];
            gds_files[fp].scale = database_unit*1e3;
            if (gds_files[fp].scale != 1.0 && dowarning && !dorescale)
                fprintf(stderr, "Warning: scale not default and no conversion being done.\n");
            if ((LONGINT)(database_unit / physical_unit + 0.5) != 1000000L)
               fprintf (stderr, "Error: User units are not Micro-meters!\n");
            LONGINT_units = (LONGINT) (database_unit * 1e3 + 0.5);
#ifdef GDS_DEBUG
            fprintf (stderr, "Data base unit is %ld nano-meter%s\n",
               (long) LONGINT_units, LONGINT_units > 1L ? "s" : "");
#endif 
         }
         prevptr = gds_files[fp].ptr;
         item = gds_next_item(fp);
         if (! item || item -> code == GDS_ENDLIB)
            is_endlib = 1;
         else
            gds_convert(item);
      }
      if (!item)
         break;
      if (is_endlib)
         break;
      gds_convert(item);
#ifdef GDS_DEBUG
      fprintf (stderr, "item %s\n", gds_lookup[item->code>>8].name);
#endif
      item = gds_next_item (fp);
      gds_convert(item);
      if (!item || item -> code != GDS_STRNAME || item -> len < 1)
         fprintf (stderr, "Error: No structure name.\n");
      memcpy (name, item -> data.cptr, (int) item -> len);
      name[GDS_NAMELEN-1] = 0;
      if (item->len < GDS_NAMELEN) {
         name[item->len]=0;
      }
      else if (dowarning) {
         fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
            GDS_NAMELEN, name);
      }
      gds_add_structure (name, fp, prevptr);
      if (! (srf = (GDS_STRUCTURE *) btloc (name, &bt_structure)))
      {
         fprintf (stderr, "Error: Cannot locate inserted structure %s\n", name);
         exit(1);
      }
      /* read until end of structure */
      while ((item = gds_next_item (fp)) && item -> code != GDS_ENDSTR)
      {
         gds_convert(item);
         if (item -> code == GDS_SNAME)
         {
            GDS_STRUCTURE *child;
            char name[GDS_NAMELEN+2];

            memcpy (name, item->data.cptr, item->len);
            name[item->len]=0;
            child = (GDS_STRUCTURE *) btloc (name, &bt_structure);
            if ( ! child ) {
               child = (GDS_STRUCTURE *) btins (name, &bt_structure);
               child -> fp = -1;
            }
            child -> parents++;
         }
      }
      if (!item)
      {
         fprintf (stderr, "Error: Unexpected end of file\n");
         break;
      }
   }
   return (0);
}

double gds_scale (void)
{
   /* all units are scaled to this */
   return (1e-3);
}

void gds_free_org (void)
{
   GDS_ORG *org, *tmp;

   for (org = org_head; org; org = tmp)
   {
      if (org -> structure)
         free (org -> structure);
      tmp = org -> next;
      free ((char *) org);
   }
   org_head = org_tail = NULL;
}

void gds_free_text (void)
{
   GDS_TXT *text, *tmp;

   for (text = text_head; text; text = tmp)
   {
      if (text -> string)
         free (text -> string);
      tmp = text -> next;
      free ((char *) text);
   }
   text_head = text_tail = NULL;
}

void gds_free_bndy (void)
{
   GDS_BNDY *bndy, *tmp;

   for (bndy = bndy_head; bndy; bndy = tmp)
   {
      if (bndy -> x)
         free ((char *) bndy -> x);
      if (bndy -> y)
         free ((char *) bndy -> y);
      tmp = bndy -> next;
      free ((char *) bndy);
   }
   bndy_head = bndy_tail = NULL;
}

void gds_minmax (GDS_BNDY *item, LONGINT *minx, LONGINT *maxx, LONGINT *miny, LONGINT *maxy)
{
   LONGINT x, y;
   int i;

   for (i = 0; i < item -> len; i++)
   {
      x = item -> x[i];
      y = item -> y[i];
      if (i == 0)
      {
         *minx = *maxx = x;
         *miny = *maxy = y;
      }
      else
      {
         if (*minx > x)
            *minx = x;
         else if (*maxx < x)
            *maxx = x;
         if (*miny > y)
            *miny = y;
         else if (*maxy < y)
            *maxy = y;
      }
   }
}

GDS_BNDY *gds_bndy_head (void)
{
   return (bndy_head);
}

GDS_ORG *gds_org_head (void)
{
   return (org_head);
}

GDS_TXT *gds_text_head (void)
{
   return (text_head);
}

GDS_ITEM *gds_item_head (void)
{
   return (item_head);
}

void gds_fputint2 (unsigned num, FILE *fp)
{
   fputc ((int) ((num >> 8) & 0xff), fp);
   fputc ((int) (num & 0xff), fp);
}

void gds_sputint2 (char *s, int num)
{
   *s++ = (num >> 8) & 0xff;
   *s = num & 0xff;
}

void gds_fputint4 (LONGINT num, FILE *fp)
{
   fputc((num >> 24) & 0xff, fp);
   fputc((num >> 16) & 0xff, fp);
   fputc((num >> 8) & 0xff, fp);
   fputc(num & 0xff, fp);
}

void gds_sputint4 (char *s, LONGINT num)
{
   *s++ = (num >> 24) & 0xff;
   *s++ = (num >> 16) & 0xff;
   *s++ = (num >> 8) & 0xff;
   *s = num & 0xff;
}

void gds_fputreal4 (double num, FILE *fp)
{
   char val[4];

   gds_sputreal4 (val, num);
   gds_fwrite (val, 1, 4, fp);
}

void gds_sputreal4 (char *s, double num)
{
   int exp;
   double mantissa;
   int    sign;
   int    i;

   exp = 64;
   mantissa = num;
   sign = 0;
   if (mantissa < 0)
   {
      mantissa = -mantissa;
      sign = 1;
   }
   else if (mantissa == 0)
   {
      for (i = 0; i < 4; i++)
         *s++ = 0;
      return;
   }
   while (mantissa >= 1.0)
   {
      mantissa /= 16;
      exp++;
   }
   while (mantissa < 1/16.0)
   {
      mantissa *= 16;
      exp--;
   }
   if (exp < 0 || exp > 127)
      fprintf (stderr, "Error: FP overflow %e\n", num);
   exp &= 0x7f;
   if (sign)
      exp |= 0x80;
   *s++ = exp;
   for (i = 0; i < 3; i++)
   {
      mantissa *= 256.0;
      *s++ = (int) mantissa;
      mantissa =  mantissa - floor (mantissa);
   }
}

void gds_fputreal8 (double num, FILE *fp)
{
   char val[8];

   gds_sputreal8 (val, num);
   gds_fwrite (val, 1, 8, fp);
}

static struct real8table {
    char *string;
    unsigned char binary[8];
} real8table[] = {
    // cadence and my algorithms disagree on these numbers
    {"1.000000e-03", {0x3e,0x41,0x89,0x37,0x4b,0xc6,0xa7,0xef} },
    {"1.000000e-09", {0x39,0x44,0xb8,0x2f,0xa0,0x9b,0x5a,0x51} },
// on this number, cadence generates two different values!
//    {"1.000000e-01", {0x40,0x19,0x99,0x99,0x99,0x99,0x99,0x9a} },
    {NULL},
};

void gds_sputreal8 (char *s, double num)
{
   int exp;
   double mantissa;
   int sign;
   int i;

   if (fabs(num) < 1 && num != 0) {
   char string[32];
   sprintf (string, "%le", num);
   for (i = 0; real8table[i].string; i++) {
      if (!strcmp (string, real8table[i].string)) {
         memcpy (s, real8table[i].binary, 8);
         return;
      }
   }
   }
   exp = 64;
   mantissa = num;
   sign = 0;
   if (mantissa < 0)
   {
      mantissa = -mantissa;
      sign = 1;
   }
   else if (mantissa == 0.0)
   {
      for (i = 0; i < 8; i++)
         *s++ = 0;
      return;
   }
   while (mantissa >= 1.0)
   {
      mantissa /= 16.0;
      exp++;
   }
   while (mantissa < 1/16.0)
   {
      mantissa *= 16.0;
      exp--;
   }
   if (exp < 0 || exp > 127)
      fprintf (stderr, "Error: FP overflow %e\n", num);
   exp &= 0x7f;
   if (sign)
      exp |= 0x80;
   *s++ = exp;
   for (i = 0; i < 7; i++)
   {
      mantissa *= 256.0;
      *s++ = (int) mantissa;
      mantissa =  mantissa - floor (mantissa);
   }
}

void gds_sputreal8h (char *s, double num, unsigned lb)
{
   gds_sputreal8 (s, num);
   if (lb != 0)
      s[7] = lb;
}

void gds_sputstring (char *s, char *t, unsigned len)
{
   strcpy (s, t);
   s += strlen (t);
   while (len-- > strlen (t))
      *s++ = 0;
}

double gds_mgetreal8 (int fp)
{
   char val[8];

   memcpy (val, gds_files[fp].ptr, 8);
   gds_files[fp].ptr += 8;
   return (gds_sgetreal8 (val));
}

void gds_mgetstring (char *s, int len, int fp)
{
   memcpy (s, gds_files[fp].ptr, len);
   gds_files[fp].ptr += len;
   s[len] = 0;
}

void gds_putitem (GDS_ITEM *item, FILE *fp)
{
   int i;

   gds_fputint2 (item -> len + 4, fp);
   gds_fputint2 (item -> code, fp);
   if (item -> len)
   {
      switch (item -> code & 0xff)
      {
      case GDS_NODATA:
         break;
      case GDS_BITARRAY:
      case GDS_INT2:
         for (i = 0; i < item -> len / 2; i++)
            gds_fputint2 (item -> data.sptr[i], fp);
         break;
      case GDS_INT4:
         for (i = 0; i < item -> len / 4; i++)
            gds_fputint4 (item -> data.lptr[i], fp);
         break;
      case GDS_REAL4:
         for (i = 0; i < item -> len / 4; i++)
            gds_fputreal4 (item -> data.dptr[i], fp);
         break;
      case GDS_REAL8:
         for (i = 0; i < item -> len / 8; i++)
            gds_fputreal8 (item -> data.dptr[i], fp);
         break;
      case GDS_ASCII:
         gds_fwrite (item -> data.cptr, 1, (int) item -> len, fp);
         break;
      default:
         fprintf (stderr, "Error: gds_putitem: Illegal GDS data type %d\n",
            item -> code & 0xff);
         break;
      }
   }
}

GDS_ITEM *gds_find_structure (char *name)
{
   GDS_STRUCTURE *srf;
   GDS_ITEM *item;

   if ((srf = (GDS_STRUCTURE *) btloc (name, &bt_structure)) && srf->fp >= 0)
   {
      gds_files[srf->fp].ptr = srf->ptr;
      item = gds_next_item(srf->fp);
      /*
      fprintf (stderr, "Error: FS %s %lld %lld %lld\n",
         gds_lookup[item->code>>8].name,
         (long long) item->next_ptr,
         (long long) gds_files[srf->fp].ptr,
         (long long) item->next_ptr - (long long) gds_files[srf->fp].ptr
         );
         */
      item -> fp = srf->fp;
      item -> next_ptr = srf->ptr;
      gds_item_ptr=item;
      return item;
   }
   else
      return (NULL);
}

void gds_write_flat (GDS_TRANS trans, char *name, FILE *fp)
{
   GDS_ITEM tmp, *item;
   if (!(item = gds_find_structure (name))) {
      fprintf (stderr, "Error: structure %s not found\n", name);
      return;
   }
   wrerr = 0;
   /* preserve mirror only */
   trans.strans &= 0x8000;
   gds_write_header (name, fp);
   /* BGNSTR */
   gds_item_ptr = item;
   gds_putitem (gds_item_ptr, fp);
   gds_item_ptr=gds_next_item(item->fp);
   gds_putitem (gds_item_ptr, fp);
   gds_flat_structure (trans, name, fp);
   /* ENDSTR */
   tmp.len = 0;
   tmp.code = GDS_ENDSTR;
   gds_putitem (&tmp, fp);

   gds_loc_structure (name);
   /* ENDLIB */
   tmp.code = GDS_ENDLIB;
   gds_putitem (&tmp, fp);
   gds_fill_space(fp);
}

static void gds_flat_structure (GDS_TRANS trans, char *name, FILE *fp)
{
   GDS_ITEM tmp, *item;
   int  datasize=(GDS_MAX_POINTS+1)*4*2;
   char *data;
   int i;
   int text_processing = 0;
   static int level = 0;
   level++;
#ifdef GDS_DEBUG
   fprintf (stderr, "Structure %s  entering recursion level - %d\n",name,level);
#endif 
   if (!(item = (GDS_ITEM *) gds_find_structure (name))) {
      fprintf (stderr, "Error: structure %s not found\n", name);
      return;
   }

   int from = __LINE__;
   push (from);
   memset ((char *) &tmp, sizeof (tmp), 0);
   data = malloc(datasize);
   if (! data ) {
      out_of_memory(__LINE__);
   }
   tmp.data.cptr = data;
   gds_item_ptr = item;
#ifdef GDS_DEBUG
   fprintf (stderr, "fSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   // get BGNSTR
   gds_get_item();
   // get STRNAME
   gds_get_item();
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR)
   {
      // to handle illegal gds with > 200 (GDS_MAX_POINTS) vertices
      if (item->len >= datasize)
      {
         while (datasize <= item->len) {
            datasize *= 2;
         }
         data = realloc(data, datasize);
         if (! data ) {
            out_of_memory(__LINE__);
         }
         tmp.data.cptr = data;
      }
      switch (item -> code)
      {
      case GDS_STRCLASS:
      case GDS_STRTYPE:
      case GDS_BOUNDARY:
      case GDS_BOX:
      case GDS_PATH:
      case GDS_BGNEXTN:
      case GDS_ENDEXTN:
	gds_putitem (item, fp);
	text_processing = 0;
	break;
      case GDS_ELKEY:
	if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
	break;
      case GDS_PROPATTR:
	if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
	break;
      case GDS_PROPVALUE:
	if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
	break;
      case GDS_DATATYPE:
	if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
	break;
      case GDS_PATHTYPE:
	if(!text_processing || (text_processing && (level == 1) ))
         gds_putitem (item, fp);
        break;
      case GDS_TEXT:
	text_processing = 1;
	if((level == 1))
          gds_putitem (item, fp);
	break;
      case GDS_WIDTH:
	if(!text_processing || (text_processing && (level == 1) ))
          gds_putitem (item, fp);
        break;
      case GDS_ENDEL:
        if(!text_processing || (text_processing && (level == 1) ))
            gds_putitem (item, fp);
	if(text_processing)
	  text_processing = 0;    
	break;
      case GDS_LAYER:
	if(!text_processing || (text_processing && (level == 1) ))
	   gds_putitem (item, fp);
	break;
      case GDS_PRESENTATION:
        if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
        break;
      case GDS_STRANS:
	if(!text_processing || (text_processing && (level == 1) ))
            gds_putitem (item, fp);
        break;
      case GDS_STRING:
	if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
        break;
      case GDS_TEXTTYPE:
	if(!text_processing || (text_processing && (level == 1) ))
           gds_putitem (item, fp);
        break;
      case GDS_SREF:
         if(check_flat_list())
	 {
            gds_putitem (item, fp);
            gds_hier_sref_aref(trans, fp);
	 }
         else
            gds_flat_sref (trans, fp);
         text_processing = 0;
         break;
      case GDS_AREF:
         if(check_flat_list())
	 {
           gds_putitem (item, fp);
           gds_hier_sref_aref(trans, fp); 
	 }
         else
           gds_flat_aref (trans, fp);
	 text_processing = 0;
         break;
      case GDS_MAG: /* should only occur on text */
	if(!text_processing || (text_processing && (level == 1) ))
        {
            tmp.data.dptr[0] = item -> data.dptr[0];
            tmp.data.dptr[0] *= trans.mag;
            tmp.len = item -> len;
            tmp.code = item -> code;
            tmp.flags=0;
            gds_putitem (&tmp, fp);
        }
        break;
      case GDS_ANGLE: /* should only occur on text */
        if(!text_processing || (text_processing && (level == 1) ))
        {
            tmp.data.dptr[0] = item -> data.dptr[0];
            tmp.data.dptr[0] += trans.angle;
            tmp.len = item -> len;
            tmp.code = item -> code;
            tmp.flags = 0;
            gds_putitem (&tmp, fp);
        }
        break;
      case GDS_XY:
	if(!text_processing || (text_processing && (level == 1) ))
        {
            tmp.len = item -> len;
            tmp.code = item -> code;
            tmp.flags = 0;
            memcpy (tmp.data.cptr, item->data.cptr, item->len);
            for (i = 0; i < tmp.len / 8; i++)
                gds_transform (trans, &tmp.data.lptr[i*2], &tmp.data.lptr[i*2+1]);
            gds_putitem (&tmp, fp);
        }
        break;
      default:
         fprintf (stderr,
            "Error: Illegal GDS element code %s in structure %s line %d\n",
            gds_lookup[item -> code >> 8].name, name, __LINE__);
         break;
      }
   }
   level--;
#ifdef GDS_DEBUG
   fprintf(stderr,"Structure %s  returning to recursion level - %d\n",name,level);
#endif 
   pop(from);
   free(data);
}


static void gds_hier_sref_aref(GDS_TRANS trans, FILE *fp)
{
   GDS_ITEM tmp, *item;
   char data[2048];
   int i;
   tmp.data.cptr = data;
   tmp.flags = 0;

   /* output only the SREF or AREF elements */
   while ((item = gds_get_item ()) && 
          ((item -> code == GDS_ELFLAGS) ||
           (item -> code == GDS_PLEX)    ||
           (item -> code == GDS_SNAME)   ||
           (item -> code == GDS_STRANS)  ||
           (item -> code == GDS_COLROW)  ||
           (item -> code == GDS_XY)))
   {
      if(item -> code == GDS_XY)
      {
         tmp.len = item -> len;
         tmp.code = item -> code;
         tmp.flags = 0;
         memcpy (tmp.data.cptr, item->data.cptr, item->len);
         for (i = 0; i < tmp.len / 8; i++)
             gds_transform (trans, &tmp.data.lptr[i*2], &tmp.data.lptr[i*2+1]);
         gds_putitem (&tmp, fp);
      }
      else
      {
         gds_putitem (item, fp);
      }
  }
}

static int check_flat_list()
{

   GDS_ITEM *item;
   char name[GDS_NAMELEN];
   BOOL flag = FALSE;


   /* get sname */
   int from = __LINE__;
   push (from);
   if (!(item = gds_get_item ())) {
      gds_item_ptr = pop(from);
      return FALSE;
   }
   if (item -> len < 1 || item -> code != GDS_SNAME)
      fprintf (stderr, "Error: expected SNAME in check_flat_list\n");
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[GDS_NAMELEN-1] = 0;
   if (item->len < GDS_NAMELEN)
      name[item->len]=0;
   else if (dowarning)
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);

   if( btloc(name,&bt_flat_structure))
      flag = TRUE;
   *gds_item_ptr = *pop(from);
   return flag;
 }


static void gds_flat_sref (GDS_TRANS trans, FILE *fp)
{
   GDS_TRANS otrans;
   GDS_TRANS calling_trans;
   GDS_ITEM *item, sname, sref;
   char name[GDS_NAMELEN];
   int  st;
   double iangle;
   int exists=1;
   char a[8];
   memset(a,8,0);

   calling_trans = trans;
   /* get sname */
   if (!(item = gds_get_item ())) {
      return;
   }
   sname=*item;
   if (item -> len < 1 || item -> code != GDS_SNAME)
   {
      fprintf (stderr, "Error: expected SNAME, got %s in gds_flat_sref\n",
        gds_lookup[item->code>>8].name);
   }
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[item->len]=0;
   name[GDS_NAMELEN-1] = 0;
   if (item->len < GDS_NAMELEN) {
      name[item->len]=0;
   }
   else if (dowarning) {
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "fsSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   /* preserve mirror only */
   trans.strans &= 0x8000;
   /* save translate for origin */
   otrans = trans;
   st = 0;
   iangle = 0;
   int from=__LINE__;
   push(from);
   if ((! gds_find_structure(name))) {
    sref.len=0;
    sref.code=GDS_SREF;
    sref.data.cptr=a;
    gds_putitem(&sref, fp);
    gds_putitem(&sname, fp);
    exists=0;
   }
   pop(from);
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL)
   {
      LONGINT x, y;

#ifdef GDS_DEBUG
      if (item->code == GDS_STRANS)
         fprintf (stderr, "fSTR2h %d %04x %04x %08llx\n", item->len, item->data.sptr[0], item->data.sptr[1], (long long) item->data.sptr);
#endif
      if (exists) {
          switch (item -> code)
          {
          case GDS_ELFLAGS:
          case GDS_PLEX:
          case GDS_PROPVALUE:
          case GDS_PROPATTR:
             break;
          case GDS_STRANS:
             if (item -> len != 2)
                fprintf (stderr, "Error: expected STRANS in SREF %s\n", name);
             st = *item -> data.sptr;
             if (st & 0x8000) /* reflection */
                trans.strans ^= 0x8000;
             trans.strans |= st & 0x60;
             break;
          case GDS_ANGLE:
             iangle = item -> data.dptr[0];
             if (trans.strans & 0x2)
                trans.angle = iangle;
             break;
          case GDS_MAG:
             if (trans.strans & 0x4)
                trans.mag = item -> data.dptr[0];
             else
                trans.mag *= item -> data.dptr[0];
             break;
          case GDS_XY:
             if (item -> len != 8)
                fprintf (stderr, "Error: expected XY in SREF %s\n", name);
             x = item -> data.lptr[0];
             y = item -> data.lptr[1];
             gds_transform (otrans, &x, &y);
             trans.x = x;
             trans.y = y;
             break;
          default:
             fprintf (stderr, "Error: Illegal code for SREF %s in %s\n",
                gds_lookup[item -> code >> 8].name, name);
             break;
          }
      }
      else {
        gds_putitem(item,fp);
      }
   }
   if (! exists) {
      sref.code=GDS_ENDEL;
      gds_putitem(&sref, fp);
      return;
   }
   from = __LINE__;
   push (from);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
   {
      fprintf (stderr, "Error: No structure %s in library\n", name);
      pop(from);
      return;
   }
   gds_item_ptr = item;
   if (!(st & 0x2))
   {
      if (calling_trans.strans & 0x8000)
      {
	 trans.angle = trans.angle - iangle;
      }
      else
      {
	 trans.angle += iangle;
      }
      while (trans.angle >= 360.0)
	 trans.angle -= 360.0;
      while (trans.angle < 0.0)
	 trans.angle += 360.0;
   }
#ifdef GDS_DEBUG
   fprintf (stderr, " st=%04X angle=%.0lf strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf %s\n",
      st & 0xffff, iangle, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag, name);
#endif
   gds_flat_structure (trans, name, fp);
   *gds_item_ptr = *pop(from);
}

static void gds_flat_aref (GDS_TRANS trans, FILE *fp)
{
   GDS_TRANS otrans, /* translate origin */
         ltrans; /* translate local array */
   GDS_TRANS calling_trans;
   GDS_ITEM *item, sname, sref;
   char name[GDS_NAMELEN];
   int  col, row;
   LONGINT x0, y0;
   LONGINT x1, y1;
   LONGINT x2, y2;
   LONGINT dx, dy;
   double  dtmp;
   int  iangle;
   int  i, j;
   int  st;
   int  from;
   int  exists=1;
   char a[8];
   memset(a,8,0);

   calling_trans = trans;
   /* get sname */
   if (!(item = gds_get_item ())) {
      return;
   }
   sname=*item;
   if (item -> len < 1 || item -> code != GDS_SNAME)
      fprintf (stderr, "Error: expected SNAME\n");
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[GDS_NAMELEN-1] = 0;
   if (item->len < GDS_NAMELEN) {
      name[item->len]=0;
   }
   else if (dowarning) {
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "faSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   /* preserve mirror only */
   trans.strans &= 0x8000;
   /* save translate for origin */
   otrans = trans;
   ltrans = trans;
   /* initialize local translation */
   ltrans.x = ltrans.y = 0L;
   st = iangle = col = row = x0 = y0 = x1 = y1 = x2 = y2 = 0;
   push(from);
   if ((! gds_find_structure(name))) {
    sref.len=0;
    sref.code=GDS_AREF;
    sref.data.cptr=a;
    gds_putitem(&sref, fp);
    gds_putitem(&sname, fp);
    exists=0;
   }
   pop(from);
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL)
   {
      if (exists) {
          switch (item -> code)
          {
          case GDS_ELFLAGS:
          case GDS_PLEX:
          case GDS_PROPVALUE:
          case GDS_PROPATTR:
             break;
          case GDS_COLROW:
             if (item -> len != 4)
                fprintf (stderr, "Error: expected COLROW in AREF %s\n", name);
             col = item -> data.sptr[0];
             row = item -> data.sptr[1];
             break;
          case GDS_STRANS:
             if (item -> len != 2)
                fprintf (stderr, "Error: expected STRANS in AREF %s\n", name);
             st = *item -> data.sptr;
             if (st & 0x8000) /* reflection */
                trans.strans ^= 0x8000;
             trans.strans |= st & 0x60;
             break;
          case GDS_ANGLE:
             dtmp = item -> data.dptr[0];
             iangle = (int) (dtmp + 0.5);
             if (trans.strans & 0x2)
                trans.angle = dtmp;
             break;
          case GDS_MAG:
             if (trans.strans & 0x4)
                trans.mag = item -> data.dptr[0];
             else
                trans.mag *= item -> data.dptr[0];
             break;
          case GDS_XY:
             if (item -> len != 24)
                fprintf (stderr, "Error: expected XY in AREF %s\n", name);
             x0 = item -> data.lptr[0];
             y0 = item -> data.lptr[1];
             x1 = item -> data.lptr[2];
             y1 = item -> data.lptr[3];
             x2 = item -> data.lptr[4];
             y2 = item -> data.lptr[5];
             break;
          default:
             fprintf (stderr, "Error: Illegal code for AREF %s\n",
                gds_lookup[item -> code >> 8].name);
             break;
          }
      }
      else {
        gds_putitem(item,fp);
      }
   }
   if ( ! exists) {
      sref.code=GDS_ENDEL;
      gds_putitem(&sref, fp);
      return;
   }
   from = __LINE__;
   push (from);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
   {
      fprintf (stderr, "Error: No structure %s in library\n", name);
      pop(from);
      return;
   }
   if (col == 0 || row == 0)
   {
      fprintf (stderr, "Error: No COLROW in aref %s\n", name);
      pop(from);
      return;
   }
   if (x1 - x0)
      dx = x1 - x0;
   else
      dx = x2 - x0;
   if (y1 - y0)
      dy = y1 - y0;
   else
      dy = y2 - y0;
   /* the row/column "arrangement" can be switched! */
   switch (iangle)
   {
   case 0:
   case 180:
      break;
   case 90: /* row and col for 90 and 270 */
   case 270:
      i = col;
      col = row;
      row = i;
      break;
   default:
      fprintf (stderr,
         "Severe Warning: Do not know how to do AREF at angle %d\n", iangle);
      pop(from);
      return;
      break;
   }
   dx /= col;
   dy /= row;
   gds_transform (otrans, &x0, &y0);
   if (!(st & 0x2))
   {
      if (calling_trans.strans & 0x8000)
      {
	 trans.angle = trans.angle - iangle;
      }
      else
      {
	 trans.angle += iangle;
      }
      while (trans.angle >= 360.0)
	 trans.angle -= 360.0;
      while (trans.angle < 0.0)
	 trans.angle += 360.0;
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "AREF %s, col %d, row %d, dx=%ld, dy=%ld\n",
      name, col, row, (long) dx, (long) dy);
   fprintf (stderr, "     incoming trans=(%d,%d) %04x, %.0lf\n",
      calling_trans.x, calling_trans.y, calling_trans.strans, calling_trans.angle);
   fprintf (stderr, "     result   trans=(%d,%d) %04x, %.0lf\n",
      trans.x+x0, trans.y+y0, trans.strans, trans.angle);
#endif
   for (i = 0; i < row; i++)
   for (j = 0; j < col; j++)
   {
      LONGINT x, y;

      x = dx * j;
      y = dy * i;
      gds_transform (ltrans, &x, &y);
      trans.x = x + x0;
      trans.y = y + y0;
      gds_item_ptr = item;
      gds_flat_structure (trans, name, fp);
   }
   pop(from);
}

void gds_free_item (GDS_ITEM *item)
{
    if (item -> data.cptr)
        free (item -> data.cptr);
    free ((char *) item);
}

static void gds_free_items (void)
{
   item_head = item_tail = NULL;
}



void gds_write_cell (GDS_TRANS trans, char *name, FILE *fp)
{
   GDS_ITEM tmp;
   char stru[GDS_NAMELEN];
   char  *dummy;
   int    convt;

   wrerr = 0;
   btinit (&bt_list, GDS_NAMELEN, 4, strcmp);
   gds_write_header (name, fp);
   convt = gds_do_convert (0);
   gds_loc_structure (name);
   gds_do_convert (convt);
   gds_write_structure (trans, name, fp);
   gds_clear_trans (&trans);
   btselect ("", "zzzzzzz", &bt_list);
   while (btget ((void *) stru, (void **) &dummy, &bt_list))
   {
      if (!strcmp (name, stru))
         continue;
      gds_write_structure (trans, stru, fp);
   }
   tmp.len = 0;
   tmp.code = GDS_ENDLIB;
   tmp.data.cptr = NULL;
   tmp.flags = 0;
   gds_putitem (&tmp, fp);
   btfree (&bt_list);
   gds_fill_space(fp);
}

void gds_write_lib (char *name, FILE *fp)
{
   GDS_ITEM tmp;
   char stru[GDS_NAMELEN];
   GDS_TRANS trans;
   char *dummy;

   wrerr = 0;
   gds_write_header (name, fp);
   gds_clear_trans (&trans);
   dummy = NULL;
   btselect ("", "zzzzzzz", &bt_structure);
   while (btget ((void *) stru, (void **) &dummy, &bt_structure))
      gds_write_structure (trans, stru, fp);
   tmp.len = 0;
   tmp.code = GDS_ENDLIB;
   tmp.data.cptr = NULL;
   tmp.flags = 0;
   gds_putitem (&tmp, fp);
   gds_fill_space(fp);
}

static void gds_write_structure (GDS_TRANS trans, char *name, FILE *fp)
{
   GDS_ITEM tmp, *item;
   int  datasize=(GDS_MAX_POINTS+1)*4*2;
   char *data;
   int i;

   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
      return;
   
   tmp.flags = 0;
   data = malloc(datasize);
   if (! data ) {
      out_of_memory(__LINE__);
   }
   tmp.data.cptr = data;
#ifdef GDS_DEBUG
   fprintf (stderr, "wSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   /* preserve mirror only */
   trans.strans &= 0x8000;
   while (item -> code != GDS_ENDSTR && (item = gds_get_item ()))
   {
#ifdef GDS_DEBUG
      fprintf (stderr, "wSTRU code %s\n", gds_lookup[item->code>>8].name);
#endif
      // to handle illegal gds with > 200 (GDS_MAX_POINTS) vertices
      if (item->len >= datasize)
      {
         while (datasize <= item->len) {
            datasize *= 2;
         }
         data = realloc(data, datasize);
         if (! data ) {
            out_of_memory(__LINE__);
         }
         tmp.data.cptr = data;
      }
      if (item -> code == GDS_XY) // do transformation if needed
      {
         tmp.len = item -> len;
         tmp.code = item -> code;
         tmp.flags = 0;
         memcpy (tmp.data.cptr, item->data.cptr, item->len);
         for (i = 0; i < tmp.len / 8; i++)
             gds_transform (trans, &tmp.data.lptr[i*2], &tmp.data.lptr[i*2+1]);
         gds_putitem (&tmp, fp);
      }
      else {
         gds_putitem (item, fp);
      }
   }
   free(data);
}

/* gdsII spec page 6, a "null word" is 2 consecutive zero bytes. */ 
static void gds_fill_space(FILE *fp)
{
#ifdef GDS_FILL_SPACE
   unsigned char prt[2];

   memset((void *)prt, 0, 2);

   while (ftello(fp) % 2048)
      fwrite(prt, 1, 2, fp);
#endif
}


static void gds_write_header (char *name, FILE *fp)
{
   GDS_ITEM tmp;
   char data[2048];

   tmp.data.cptr = data;
   tmp.flags = 0;
   /* HEADER 3 */
   tmp.len = 2;
   tmp.code = GDS_HEADER;
   tmp.data.sptr[0] = 3;
   gds_putitem (&tmp, fp);
   /* BGNLIB */
   gds_time (tmp.data.cptr);
   tmp.len = 24;
   tmp.code = GDS_BGNLIB;
   gds_putitem (&tmp, fp);
   /* LIBNAME */
   tmp.len = strlen (name);
   tmp.len += tmp.len % 2;
   tmp.code = GDS_LIBNAME;
   strcpy (tmp.data.cptr, name);
   gds_putitem (&tmp, fp);
   /* UNITS */
   tmp.code = GDS_UNITS;
   tmp.len  = 16;
   tmp.data.dptr[0] = 1e-3;
   tmp.data.dptr[1] = 1e-9;
   gds_putitem (&tmp, fp);
}

/*************************************************************************
   This function hierarchically searches a tree and adds any
   structure references to a btree (bt_list)
*************************************************************************/
static void gds_loc_structure (char *name)
{
   GDS_ITEM *item;
   char stru[GDS_NAMELEN];

   if (!(item = gds_find_structure (name)))
   {
      fprintf (stderr, "Error: Cannot locate %s in library(s)\n", name);
      return;
   }
   if (bt_list.cmpfunc && !(btins (name, &bt_list)))
      return;
   int from=__LINE__;
   push (from);
   gds_item_ptr = item;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR)
   {
      if (item -> code == GDS_SNAME)
      {
      /* SNAME only occurs on a structure reference */
         memcpy (stru, item -> data.cptr, (int) item -> len);
         stru[GDS_NAMELEN-1] = 0;
         if (item->len < GDS_NAMELEN) {
            stru[item->len]=0;
         }
         else if (dowarning) {
            fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
               GDS_NAMELEN, stru);
         }
         int from = __LINE__;
         push(from);
         gds_loc_structure (stru);
         pop(from);
      }
   }
   gds_item_ptr = pop(from);
}

/* sets no layers */
void gds_clear_layer_mask (void)
{
   int i;

   for (i = 0; i < MAX_LAYER; i++)
      layer_mask[i] = 0;
   mask_active = 0;
}

/* 
   string syntax:
      for all datatypes: layer,layer,layer
      for selected datatypes: layer;type,layer;type
*/
void gds_set_layer (char *string)
{
   int layer;
   int mask;

   if (string) {
       while (*string)
       {
          while (isspace ((int)*string) || *string == ',') string++;
          if (*string && (layer = atoi (string)) > 0 && layer < MAX_LAYER)
          {
             char *s;
             for (s = string; isdigit(*s) && *s; s++)
                ;
             mask = (ULONGINT)-1;
             if (*s == ';')
                mask = (1 << atoi(s+1));
#ifdef GDS_DEBUG
             fprintf (stderr, "Setting %d;%d;%x\n", layer, atoi(s+1), mask); 
#endif
             layer_mask[layer] = mask;
          }
          while (!isspace ((int)*string) && *string != ',' && *string) string++;
       }
       mask_active = 1;
   }
}

void gds_set_layer_mask (void)
{
   int i;

   for (i = 0; i < MAX_LAYER; i++)
      layer_mask[i] = (ULONGINT)-1;
   mask_active = 0;
}

void gds_clear_trans (GDS_TRANS *trans)
{
   trans -> angle = 0.0;
   trans -> mag = 1.0;
   trans -> x = trans -> y = 0L;
   trans -> strans = 0;
}

static void gds_time (char *data)
{
   time_t now, time ();
   struct tm *tms;
   GDS_DATA dp;

   dp.cptr = data;
   now = time ((time_t *) 0);
   tms = localtime (&now);
   dp.sptr[0] = tms->tm_year;
   dp.sptr[1] = tms -> tm_mon + 1;
   dp.sptr[2] = tms -> tm_mday;
   dp.sptr[3] = tms -> tm_hour;
   dp.sptr[4] = tms -> tm_min;
   dp.sptr[5] = tms -> tm_sec;
   memcpy (data + 12, data, 12);
}

int gds_getsize (char *name, LONGINT *xx1, LONGINT *yy1, LONGINT *xx2, LONGINT *yy2)
{
   GDS_TRANS trans;
   GDS_BNDY *bndy;
   int       i, first;
   LONGINT   x1 = 0, y1 = 0, x2 = 0, y2 = 0;

   gds_clear_trans (&trans);
   gds_clear_layer_mask ();
   gds_set_layer ("16");
   gds_free_bndy ();
   if (!gds_read_structure (trans, name))
      return (0);
   first = 1;
   for (bndy = bndy_head; bndy; bndy = bndy -> next)
   {
      if (first)
      {
         x1 = x2 = bndy -> x[0];
         y1 = y2 = bndy -> y[0];
         first = 0;
      }
      for (i = 0; i < bndy -> len - 1; i++)
      {
         if (x1 > bndy -> x[i])
            x1 = bndy -> x[i];
         if (y1 > bndy -> y[i])
            y1 = bndy -> y[i];
         if (x2 < bndy -> x[i])
            x2 = bndy -> x[i];
         if (y2 < bndy -> y[i])
            y2 = bndy -> y[i];
      }
   }
   *xx1 = x1;
   *yy1 = y1;
   *xx2 = x2;
   *yy2 = y2;
   return (1);
}

void gds_set_cell (char *name)
{
   int i;

   if (!*name)
      use_cell[0][0] = 0;
   else
   {
      for (i = 0;
         use_cell[i][0] && i < USE_CELL_CNT && strcmp (use_cell[i], name);
            i++);
      if (i < USE_CELL_CNT)
         strncpy (use_cell[i], name, 63);
      if (i < USE_CELL_CNT - 1)
         use_cell[i+1][0] = 0;
   }
}

void gds_set_ptr (GDS_ITEM *item)
{
   gds_item_ptr = item;
}

GDS_REF *gds_ref_head (void)
{
   return (ref_head);
}

void gds_free_ref (void)
{
   GDS_REF *ref, *next;

   for (ref = ref_head; ref; ref = next)
   {
      next = ref -> next;
      free ((char *) ref);
   }
   ref_head = NULL;
}

static void gds_new_ref (char *name, GDS_TRANS trans)
{
   GDS_REF *ref;

   if (!do_ref)
      return;
   if (!(ref = (GDS_REF *) calloc (1, (unsigned) sizeof (GDS_REF))))
      out_of_memory (__LINE__);
   strcpy (ref -> strname, name);
   ref -> trans = trans;
   ref -> next = NULL;
   if (!ref_head)
      ref_head = ref;
   else
      ref_tail -> next = ref;
   ref_tail = ref;
}

static void gds_new_aref (char *name, GDS_TRANS trans, int col, int row, LONGINT dx, LONGINT dy)
{
   GDS_REF *ref;

   if (!do_ref)
      return;
   if (!(ref = (GDS_REF *) calloc (1, (unsigned) sizeof (GDS_REF))))
      out_of_memory (__LINE__);
   strcpy (ref -> strname, name);
   ref -> trans = trans;
   ref -> col = col;
   ref -> row = row;
   ref -> dx = dx;
   ref -> dy = dy;
   ref -> next = NULL;
   if (!ref_head)
      ref_head = ref;
   else
      ref_tail -> next = ref;
   ref_tail = ref;
}

typedef struct regexlist REGEXLIST;

struct regexlist {
    union {
      regex_t regex;
      char   *cptr;
    } data;
    REGEXLIST *next;
};

static REGEXLIST *regexlist;
static REGEXLIST *exclude_list;

void gds_clear_exclude_regex (void)
{
   REGEXLIST *ptr, *next_ptr;

   for (ptr = regexlist; ptr; ptr = next_ptr) {
      next_ptr = ptr -> next;
      free ((char *) ptr);
   }
   regexlist = NULL;
}

static int gds_excluded_regex (char *name)
{
   REGEXLIST *ptr;

   for (ptr = regexlist; ptr; ptr = ptr -> next)
      if (!regexec(&ptr->data.regex, name, 0, NULL, 0))
          return (1);
   return (0);
}

void gds_exclude_regex (char *name)
{
   REGEXLIST *ptr;
   regex_t   regex;

   // some people seem to inadvertently add quotes incorrectly
   if (*name == '"' and name[strlen(name)-1] == '"') {
      memmove (name, name + 1, strlen(name));
      name[strlen(name)-1] = 0;
   }
   if (*name == '\'' and name[strlen(name)-1] == '\'') {
      memmove (name, name + 1, strlen(name));
      name[strlen(name)-1] = 0;
   }
   if (regcomp(&regex, name, 0)) {
      fprintf (stderr, "Error: Illegal regular expression '%s'\n", name);
      return;
   }
   if (!(ptr = (REGEXLIST *) malloc ((unsigned) sizeof (REGEXLIST))))
      out_of_memory (__LINE__);
#ifdef GDS_DEBUG
   fprintf (stderr, "Adding %s to exclude regex\n", name);
#endif
   ptr -> data.regex = regex; /* structure copy */
   ptr -> next = regexlist;
   regexlist = ptr;
}

void gds_clear_exclude (void)
{
   REGEXLIST *ptr, *next_ptr;

   for (ptr = exclude_list; ptr; ptr = next_ptr)
   {
      next_ptr = ptr -> next;
      free (ptr -> data.cptr);
      free ((char *) ptr);
   }
   exclude_list = NULL;
}

static int gds_excluded (char *name)
{
   REGEXLIST *ptr;

   for (ptr = exclude_list; ptr; ptr = ptr -> next)
      if (!strcmp (name, ptr -> data.cptr))
         return (1);
   return (0);
}

void gds_exclude (char *name)
{
   REGEXLIST *ptr;

   // some people seem to inadvertently add quotes incorrectly
   if (*name == '"' and name[strlen(name)-1] == '"') {
      memmove (name, name + 1, strlen(name));
      name[strlen(name)-1] = 0;
   }
   if (*name == '\'' and name[strlen(name)-1] == '\'') {
      memmove (name, name + 1, strlen(name));
      name[strlen(name)-1] = 0;
   }
   if (gds_excluded (name)) // already added to list
      return;
   if (!(ptr = (REGEXLIST *) malloc ((unsigned) sizeof (REGEXLIST))) ||
         !(ptr -> data.cptr = malloc ((unsigned) strlen (name) + 1)))
      out_of_memory (__LINE__);
#ifdef GDS_DEBUG
   fprintf (stderr, "Adding %s to exclude\n", name);
#endif
   strcpy (ptr -> data.cptr, name);
   ptr -> next = exclude_list;
   exclude_list = ptr;
}

void gds_set_aref_mode (int val)
{
   aref_mode = val;
}

void gds_set_do_org (int val)
{
   do_org = val;
}

void gds_set_do_ref (int val)
{
   do_ref = val;
}

void gds_set_do_text (int val)
{
   do_text = val;
}

void gds_set_do_bndy (int val)
{
   do_bndy = val;
}

int gds_delete_structure (char *name)
{
   return (btdel (name, &bt_structure));
}

void gds_null_org (void)
{
   org_head = org_tail = NULL;
}

void gds_null_ref (void)
{
   ref_head = ref_tail = NULL;
}

void gds_null_text (void)
{
   text_head = text_tail = NULL;
}

void gds_null_bndy (void)
{
   bndy_head = bndy_tail = NULL;
}

void gds_clear_rect (void)
{
   GDS_RECT *rect, *next;

   for (rect = incl_rect; rect; rect = next)
   {
      next = rect -> next;
      free ((char *) rect);
   }
   incl_rect = NULL;
   for (rect = excl_rect; rect; rect = next)
   {
      next = rect -> next;
      free ((char *) rect);
   }
   excl_rect = NULL;
}

void gds_include_rect (LONGINT minx, LONGINT miny, LONGINT maxx, LONGINT maxy)
{
   GDS_RECT *rect;

   if (!(rect = (GDS_RECT *) malloc ((unsigned) sizeof (GDS_RECT))))
      out_of_memory (__LINE__);
   rect -> minx = minx;
   rect -> miny = miny;
   rect -> maxx = maxx;
   rect -> maxy = maxy;
   rect -> next = incl_rect;
   incl_rect = rect;
}

void gds_exclude_rect (LONGINT minx, LONGINT miny, LONGINT maxx, LONGINT maxy)
{
   GDS_RECT *rect;

   if (!(rect = (GDS_RECT *) malloc ((unsigned) sizeof (GDS_RECT))))
      out_of_memory (__LINE__);
   rect -> minx = minx;
   rect -> miny = miny;
   rect -> maxx = maxx;
   rect -> maxy = maxy;
   rect -> next = excl_rect;
   excl_rect = rect;
}

static GDS_BNDY *layer_head[MAX_LAYER];

GDS_BNDY *gds_layer_head (int layer)
{
   if (layer >= 0 && layer < MAX_LAYER)
      return layer_head[layer];
   return (GDS_BNDY *) NULL;
}

void gds_free_layers (void)
{
   GDS_BNDY *bndy, *next;
   int i;

   for (i = 0; i < MAX_LAYER; i++)
   {
      for (bndy = layer_head[i]; bndy; bndy = next)
      {
         if (bndy -> x)
            free ((char *) bndy -> x);
         if (bndy -> y)
            free ((char *) bndy -> y);
         next = bndy -> next;
         free ((char *) bndy);
      }
      layer_head[i] = NULL;
   }
}

void gds_separate_layers (void)
{
   GDS_BNDY *bndy, *next;;

   for (bndy = gds_bndy_head (); bndy; bndy = next)
   {
      next = bndy -> next;
      if (bndy -> layer > 0 && bndy -> layer < MAX_LAYER)
      {
         bndy -> next = layer_head[bndy -> layer];
         layer_head[bndy -> layer] = bndy;
      }
   }
   /* MUST make original head NULL, else gds_clear is broken */
   gds_null_bndy ();
}

void gds_path2boundary (int val)
{
   path2boundary = val;
}

void gds_do_add_prop(GDS_ITEM *endel, short key, char *string)
{
   int length;
   GDS_ITEM *attr, *value;


   /* do attribute first *********************************/
   if (!(attr = (GDS_ITEM *)calloc(1, sizeof(GDS_ITEM)))) {
       out_of_memory(__LINE__);
   } 
   if (!(attr->data.cptr = malloc(sizeof(key)))) {
       out_of_memory(__LINE__);
   } 
   attr->len = 2;
   attr->code = GDS_PROPATTR;
   attr->flags = 0;
   gds_sputint2(attr->data.cptr, key);


   /* now do value ****************************************/
   if (!(value = (GDS_ITEM *)calloc(1, sizeof(GDS_ITEM)))) {
       out_of_memory(__LINE__);
   } 
   length = strlen(string);
   if (length % 2) {
       length++; /* pad if uneven (calma pg. 6) */
   } 
   if (length > 126) {
       fprintf(stderr, "Error: Truncating property value longer than 126 bytes: '%s'\n", string);
       length = 126;
   }
   if (!(value->data.cptr = (char *)calloc(length, sizeof(char)))) {
       out_of_memory(__LINE__);
   }
   value->len = length;
   value->code = GDS_PROPVALUE; 
   memcpy(value->data.cptr, string, length);

   /* Insert these PROPERTY ITEM nodes into the dbly linked list
    * right in front of the ENDEL ITEM node.  
    */
   // FIX
   /*
   attr->next = value;
   value->prev = attr;
   endel->prev->next = attr;
   attr->prev = endel->prev;
   endel->prev = value;
   value->next = endel;
   */
}

int gds_add_property(GDS_ITEM *element, short key, char *string)
{
   if (!element) return 0;

   switch (element->code) {
           case GDS_BOUNDARY:
           case GDS_PATH:
           case GDS_SREF:
           case GDS_AREF:
           case GDS_TEXT:
           case GDS_NODE:
           case GDS_BOX:
              // FIX
//              for (item = element->next; item; item = item->next) {
//                   /* This attribute already exists... */
//                   if (item->code == GDS_PROPATTR && *(item->data.sptr) == key) {
//                       fprintf(stderr, "This property attribute already exists: %d\n", *(item->data.sptr));
//                       return(0);
//                   }
//                   if (item->code == GDS_ENDEL) {
//                       gds_do_add_prop(item, key, string); 
//                       return 1;
//                   } 
//              } 
              break;
           default:
              fprintf(stderr, "Error: Attempt to add property to %s not allowed.\n", gds_lookup[element->code >> 8].name);
              break;
   }

   return 0;
}


/*******************************************************************************
 * A Fast 2D Point-On-Line Test by Alan Paeth
 * from "Graphics Gems", Academic Press, 1990
 * Point T:(tx,ty)  Line P:(px,py) Q:(qx,qy)
 * return 0 if T is not on the line through        <--P--Q-->
 *        1 if T is on the open ray ending at P:   <--P
 *        2 if T is on the closed interior along:     P--Q
 *        3 if T is on the open ray beginning at Q:      Q-->
 *
 * Added the casts to float to prevent overflow with gds size numbers. -cedwards
 ******************************************************************************/
int gds_pt_on_line(long tx, long ty, long px, long py, long qx, long qy)
{
   if ((px == qx) && (py == qy)) {
       if ((tx == px) && (ty == py)) {
           return 2;
       } else {
           return 0;
       }
   }

   if ( ABS(((float)qy-py)*((float)tx-px)-((float)ty-py)*((float)qx-px) ) >= 
       (MAX(ABS((float)qx-px), ABS((float)qy-py))))
       return 0;

   if (((qx<px)&&(px<tx)) || ((qy<py)&&(py<ty)))
       return 1;
 
   if (((tx<px)&&(px<qx)) || ((ty<py)&&(py<qy)))
       return 1;
 
   if (((px<qx)&&(qx<tx)) || ((py<qy)&&(qy<ty))) 
       return 3;

   if (((tx<qx)&&(qx<px)) || ((ty<qy)&&(qy<py))) 
       return 3;


   return 2;
}



/*
 * The following functions are from:
 * Algorithms in C++ by Robert Sedgewick pg 350
 * ftp://ftp.aw.com/cseng/authors/sedgewick/AlgorithmsCC
 */

/*******************************************************************************
 * return -1 if the path p0 to p1 to p2 is clockwise
 * return  1 if the path p0 to p1 to p2 is counter-clockwise
 * return -1 if the points are collinear and p0 is between p1 and p2
 * return  0 if the points are collinear and p2 is between p0 and p1
 * return  1 if the points are collinear and p1 is between p0 and p2
 ******************************************************************************/
int gds_ccw(long x0, long y0, long x1, long y1, long x2, long y2)
{
    /* Increase to double to avoid overflow with gds coordinates. */
    double dx1, dy1, dx2, dy2;


    dx1 = x1 - x0;
    dy1 = y1 - y0;
    dx2 = x2 - x0;
    dy2 = y2 - y0;


    if (dx1*dy2 > dy1*dx2) return +1;

    if (dx1*dy2 < dy1*dx2) return -1;

    if ((dx1*dx2 < 0) || (dy1*dy2 < 0)) return -1;

    if ((dx1*dx1+dy1*dy1) < (dx2*dx2+dy2*dy2)) return +1;


    return 0;
}

/*******************************************************************************
 * Return 1 if the two line segments intersect, 0 otherwise.
 ******************************************************************************/
int gds_intersect(long x0, long y0, long x1, long y1,
                  long x2, long y2, long x3, long y3)
{
    return ((gds_ccw(x0, y0, x1, y1, x2, y2) * 
             gds_ccw(x0, y0, x1, y1, x3, y3)) <= 0)
        && ((gds_ccw(x2, y2, x3, y3, x0, y0) * 
             gds_ccw(x2, y2, x3, y3, x1, y1)) <= 0);
}



/*
 * The following functions are from:
 * "Computational Geometry in C".
 * by Joseph O'Rourke   http://cs.smith.edu/~orourke/
 */

int gds_area_sign(long x0, long y0, long x1, long y1, long x2, long y2)
{
    double area2;


    area2 = (x1 - x0) * (double)(y2 - y0) - (x2 - x0) * (double)(y1 - y0);

    /* The area should be an integer. */
    if      ( area2 >  0.5 ) return  1;
    else if ( area2 < -0.5 ) return -1;
    else                     return  0;
}

int gds_collinear(long x0, long y0, long x1, long y1, long x2, long y2)
{
   return gds_area_sign(x0, y0, x1, y1, x2, y2) == 0;
}

/*******************************************************************************
 * Returns TRUE iff point c lies on the closed segement ab.
 * Assumes it is already known that abc are collinear.
 ******************************************************************************/
int gds_between(long x0, long y0, long x1, long y1, long x2, long y2)
{
   /* If ab not vertical, check betweenness on x; else on y. */
   if (x0 != x1)
       return ((x0 <= x2) && (x2 <= x1)) || ((x0 >= x2) && (x2 >= x1));
   else
       return ((y0 <= y2) && (y2 <= y1)) || ((y0 >= y2) && (y2 >= y1));
}

/*******************************************************************************
 * Find the intersection of two seemingly parallel lines 
 ******************************************************************************/
char gds_parallel_int(long x0, long y0, long x1, long y1,
                      long x2, long y2, long x3, long y3, long *x, long *y)
{
   if (!gds_collinear(x0, y0, x1, y1, x2, y2)) {
       return '0';
   }
   if (gds_between(x0, y0, x1, y1, x2, y2 ) ) {
       *x = x2;
       *y = y2;
       return 'e';
   }
   if (gds_between(x0, y0, x1, y1, x3, y3)) {
       *x = x3;
       *y = y3;
       return 'e';
   }
   if (gds_between(x2, y2, x3, y3, x0, y0 )) {
       *x = x0;
       *y = y0;
       return 'e';
   }
   if (gds_between(x2, y2, x3, y3, x1, y1 )) {
       *x = x1;
       *y = y1;
       return 'e';
   }

   return '0';
}

/*******************************************************************************
 * Finds the point of intersection p between two closed
 * segments ab and cd.  Returns p and a char with the following meaning:
 *   'e': The segments collinearly overlap, sharing a point.
 *   'v': An endpoint (vertex) of one segment is on the other segment,
 *        but 'e' doesn't hold.
 *   '1': The segments intersect properly (i.e., they share a point and
 *        neither 'v' nor 'e' holds).
 *   '0': The segments do not intersect (i.e., they share no points).
 ******************************************************************************/
char gds_seg_seg_int(long x0, long y0, long x1, long y1, 
                     long x2, long y2, long x3, long y3, long *x, long *y)
{
   double  s, t;       /* The two parameters of the parametric eqns. */
   double num, denom;  /* Numerator and denoninator of equations. */
   char code = '?';    /* Return char characterizing intersection. */

   denom = x0 * (double)( y3 - y2 ) +
           x1 * (double)( y2 - y3 ) +
           x3 * (double)( y1 - y0 ) +
           x2 * (double)( y0 - y1 );

   /* If denom is zero, then segments are parallel: handle separately. */
   if (denom == 0.0)
       return gds_parallel_int(x0, y0, x1, y1, x2, y2, x3, y3, x, y);

   num =    x0 * (double)( y3 - y2 ) +
            x2 * (double)( y0 - y3 ) +
            x3 * (double)( y2 - y0 );

   if ( (num == 0.0) || (num == denom) )
       code = 'v';

   s = num / denom;

   num = -( x0 * (double)( y2 - y1 ) +
            x1 * (double)( y0 - y2 ) +
            x2 * (double)( y1 - y0 ) );

   if ( (num == 0.0) || (num == denom) )
       code = 'v';

   t = num / denom;

   if ( (0.0 < s) && (s < 1.0) && (0.0 < t) && (t < 1.0) )
       code = '1';
   else if ( (0.0 > s) || (s > 1.0) || (0.0 > t) || (t > 1.0) )
       code = '0';

   *x = x0 + s * ( x1 - x0 );
   *y = y0 + s * ( y1 - y0 );


   return code;
}


/*******************************************************************************
 * Determine if a point is on or inside a triangle by checking which way
 * each line segment triplet is turning.  If the point is on the same side of
 * all three turns it must be inside the triangle.
 *
 * See Mastering Algorithms with Perl pg. 446
 ******************************************************************************/
int gds_pt_in_triangle(long px, long py,
                       long x1, long y1, long x2, long y2, long x3, long y3)
{
   int ccw1, ccw2, ccw3;


   if ((ccw1 = gds_ccw(x1, y1, x2, y2, px, py)) == 0) {
       return 1;
   }

   if ((ccw2 = gds_ccw(x2, y2, x3, y3, px, py)) == 0) {
       return 1;
   }

   if ((ccw1 < 0 && ccw2 > 0) || (ccw1 > 0 && ccw2 < 0)) {
       return 0;
   }

   if ((ccw3 = gds_ccw(x3, y3, x1, y1, px, py)) == 0) {
       return 1;
   }

   if ((ccw1 < 0 && ccw3 > 0) || (ccw1 > 0 && ccw3 < 0)) {
       return 0;
   }


   return 1;
}

/*******************************************************************************
 * Use the same algoritm as gds_set_layer to see if a layer number is in a
 * c string of layers separated by commas.
 ******************************************************************************/
int gds_layer_match(int layer, char *string)
{
    if (string) {
        while (*string) {
            while (isspace((int)*string) || *string == ',') {
                string++;
            }
            if (*string && layer == atoi(string)) {
                return 1;
            }
            while (!isspace((int)*string) && *string != ',' && *string) {
                string++;
            }
        }
    }

    return 0;
}

/*******************************************************************************
 * Determine if a gds boundary is a rectangle by knowing from the gds spec
 * that it will only have 4 vertices (the first and last are recorded twice).
 * And the first side is a straight line segment.
 ******************************************************************************/
int gds_is_rectangle(GDS_BNDY *b)
{
   return(b->len == 5 && (b->x[0] == b->x[1] || b->y[0] == b->y[1]));
}

/*******************************************************************************
 * Return the euclidean distance between two points.
 ******************************************************************************/
double gds_euclidean(long x1, long y1, long x2, long y2)
{
   double x, y;


   x = x2 - x1;
   y = y2 - y1;

   return sqrt(x * x + y * y);
}

int gds_pt_in_rect(long px, long py,
                   long rminx, long rmaxx, long rminy, long rmaxy)
{
   return px >= rminx && py >= rminy && px <= rmaxx && py <= rmaxy;
}

int gds_bndy_in_rect(GDS_BNDY *bndy, 
                     long rminx, long rmaxx, long rminy, long rmaxy)
{
   LONGINT bminx=0, bmaxx=0, bminy=0, bmaxy=0;


   gds_minmax(bndy, &bminx, &bmaxx, &bminy, &bmaxy);
   
   return bminx >= rminx && bminy >= rminy && bmaxx <= rmaxx && bmaxy <= rmaxy;

}

/*******************************************************************************
 * See if two polygons intersect by checking each vertice for inclusion in
 * the other polygon.
 * Notice with enough distance between vertices you could easily miss an
 * intersection, see gds_bndy_segs_intersect().
 ******************************************************************************/
int gds_bndy_pts_intersect(GDS_BNDY *from, GDS_BNDY *to)
{
   unsigned short i;
   long poly[GDS_MAX_POINTS+1][2];


   /* create the 'to' polygon */
   assert(to->len > 0 && to->len < GDS_MAX_POINTS + 1);
   for (i = 0; i < to->len; i++) {
        poly[i][X] = to->x[i];
        poly[i][Y] = to->y[i];
   }
   /* See if any of the vertices of 'from' are inside the 'to' polygon. */
   for (i = 1; i < from->len; i++) {
        if (gds_pt_in_poly(from->x[i], from->y[i], poly, to->len)) {
            return 1;
        }
   }


   /* See if any of the vertices of 'to' are in 'from' */
   assert(from->len > 0 && from->len < (GDS_MAX_POINTS+1));
   for (i = 0; i < from->len; i++) {
        poly[i][X] = from->x[i];
        poly[i][Y] = from->y[i];
   }
   for (i = 1; i < to->len; i++) {
        if (gds_pt_in_poly(to->x[i], to->y[i], poly, from->len)) {
            return 1;
        }
   }


   return 0;
}

/*******************************************************************************
 * If the line segment made of two consecutive verticies in polygon 1
 * intersects with a line segment from the other we have an intersection.
 * Notice this will miss one polygon completely inside another, see 
 * gds_bndy_pts_intersect().
 ******************************************************************************/
int gds_bndy_segs_intersect(GDS_BNDY *from, GDS_BNDY *to)
{
   unsigned short i;
   unsigned short j;


   for (i = 1; i < from->len; i++) {

        /* build the line segment for the 'from' polygon */

        for (j = 1; j < to->len; j++) {

             /* build the line segment for the 'to' polygon */

             if (gds_intersect(to->x[j-1], to->y[j-1], to->x[j], to->y[j],
                          from->x[i-1], from->y[i-1], from->x[i], from->y[i])) {
                 return 1;
             }
        } 
   }


   return 0;
}

/*******************************************************************************
 * A polygon intersects another if either:
 * One contains a point from the other in it's interior OR
 * a segment drawn from consecutive vertices intersect.
 *
 * Which function should be called first for efficiency?? TODO
 ******************************************************************************/
int gds_bndy_intersect(GDS_BNDY *from, GDS_BNDY *to)
{
   if (gds_bndy_segs_intersect(from, to)) {
       return 1;
   }

   if (gds_bndy_pts_intersect(from, to)) {
       return 1;
   } 

   return 0;
}

/*******************************************************************************
 * Count the number of times a ray drawn from the point in question
 * intersects with the polygon boundary.  Even=Outside and Odd=Inside.
 *
 * Special cases:  1) point is on the boundary (considered inside).
 *                 2) ray intersects polygon vertices (move test line).
 * 
 * This is a rewrite of Tom's is_inside function from map_abstract.c
 *
 * TODO: The far away endpoint of the ray is computed for each call, if your
 * application has a global max x,y I should use be able to use it.
 ******************************************************************************/
static int gds_pt_in_poly(long px, long py, long poly[][2], int nverts)
{
   unsigned short i;
   int count = 0;
   long x, y, maxx, minx, maxy, miny;


   /* for gds use, the first and last vertice are stored twice */
   assert(poly[0][X] == poly[nverts-1][X] && poly[0][Y] == poly[nverts-1][Y]);

   /* calculate the maximum x and y values of the polygon in question */
   minx = maxx = poly[0][X];
   miny = maxy = poly[0][Y];
   for (i = 0; i < nverts; i++) {
        x = poly[i][X];
        y = poly[i][Y];
        if (minx > x) {
            minx = x;
        } else if (maxx < x) {
           maxx = x;
        }
        if (miny > y) {
            miny = y;
        } else if (maxy < y) {
            maxy = y;
        } 
   }

   /* test segment that extends from the pt to the extrema plus "a little". */
   maxx += 5000; 
   maxy += 3000; 

   /* get out quick if were are not even in the bounding box */
   if (px < minx || px > maxx || py < miny || py > maxy) {
       return 0;
   } 

   for (i = 1; i < nverts; i++) {

        /* Point is on the polygon boundary segment, so we call it inside. */
        if (2 == gds_pt_on_line(px, py,
                            poly[i-1][X], poly[i-1][Y], poly[i][X], poly[i][Y])) {
            return 1; /* return 1 (odd) to signify point is inside polygon */
        }

        /* Check if test line segment intersects a polygon vertice.
         * (this will also catch collinear with one of the segments)
         * If so move the endpoint of the test segment a little.
         */ 
        if (2 == gds_pt_on_line(poly[i-1][X], poly[i-1][Y], 
                                px, py, maxx, maxy)) {
            maxx += 2000; /* move ray endpoint */
            maxy += 1000; 

            i = 0;      /* Restart, NOTE: for loop does an increment so i=1 */
            count = 0;  /* reset even odd count */ 
            continue;
        } 

        if (gds_intersect(px, py, maxx, maxy, 
                          poly[i-1][X], poly[i-1][Y], poly[i][X], poly[i][Y])) {
            count++;
        }
   }


   return(count % 2); /* If count is even the remainder=0 to signal outside. */ 
}

void gds_bounding_rectangle (LONGINT *mx, LONGINT *my, LONGINT *xx, LONGINT *xy)
{
    *mx = minx;
    *my = miny;
    *xx = maxx;
    *xy = maxy;
}

void gds_notext (int v)
{
    skip_text=v;
}

char *gds_topcell (void)
{
    static char name[2048];
    int   count=0;
    GDS_STRUCTURE *srf;
    char cmd[2048];

    btselect ("", "zzzzzzzz", &bt_structure);
    while (btget (cmd, (void * *) &srf, &bt_structure))
    {
        if (! srf -> parents )
        {
            strcpy (name, cmd);
            count++;
        }
    }
    if (count != 1) return NULL;
    return name;
}

int gds_fgetlen (FILE *fp)
{
   int a,b,sign;

   a = fgetc (fp);
   b = fgetc (fp);
   if (b < 0)
      return (-1);
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 8) + b) + (sign - 1) / 2);
}

int gds_fgetint2 (FILE *fp)
{
   int a,b,sign;

   a = fgetc (fp);
   b = fgetc (fp);
   if (a < 0 || b < 0)
      gds_eoferr ("getint2");
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 8) + b) + (sign - 1) / 2);
}

LONGINT gds_fgetint4 (FILE *fp)
{
   LONGINT a,b,c,d;
   int sign;

   a = fgetc (fp);
   b = fgetc (fp);
   c = fgetc (fp);
   d = fgetc (fp);
   if (d < 0)
      gds_eoferr ("fgetint4");
   if (a & 0x80)
   {
      a = (~a) & 0xff;
      b = (~b) & 0xff;
      c = (~c) & 0xff;
      d = (~d) & 0xff;
      sign = -1;
   }
   else
      sign = 1;
   return (sign * ((a << 24) + (b << 16) + (c << 8) + d) + (sign - 1) / 2);
}

double gds_fgetreal8 (FILE *fp)
{
   char val[8];

   if (fread (val, 1, 8, fp) != 8)
      return (0.0);
   return (gds_sgetreal8 (val));
}

void gds_fgetstring (char *s, int len, FILE *fp)
{
   if ((int) fread (s, 1, len, fp) != len)
      return;
   s[len] = 0;
}

int gds_loaded (char *filename)
{
    int i;
    for (i = 0; i < gds_next_file; i++)
        if (! strcmp (gds_files[i].name, filename))
            return 1;
    return 0;
}

static void gds_hier_sref_aref_size (GDS_TRANS trans, LONGINT *minx, LONGINT* miny, LONGINT* maxx, LONGINT* maxy)
{
   GDS_ITEM tmp, *item;
   char data[2048];
   int i;
   tmp.data.cptr = data;
   tmp.flags = 0;

   /* output only the SREF or AREF elements */
   while ((item = gds_get_item ()) && 
          ((item -> code == GDS_ELFLAGS) ||
           (item -> code == GDS_PLEX)    ||
           (item -> code == GDS_SNAME)   ||
           (item -> code == GDS_STRANS)  ||
           (item -> code == GDS_COLROW)  ||
           (item -> code == GDS_XY)))
   {
      if(item -> code == GDS_XY)
      {
         tmp.len = item -> len;
         tmp.code = item -> code;
         tmp.flags = 0;
         memcpy (tmp.data.cptr, item->data.cptr, item->len);
         for (i = 0; i < tmp.len / 8; i++) {
             gds_transform (trans, &tmp.data.lptr[i*2], &tmp.data.lptr[i*2+1]);
             if (tmp.data.lptr[i*2] < *minx) *minx = tmp.data.lptr[i*2];
             if (tmp.data.lptr[i*2+1] < *miny) *miny = tmp.data.lptr[i*2];
             if (tmp.data.lptr[i*2] > *maxx) *maxx = tmp.data.lptr[i*2];
             if (tmp.data.lptr[i*2+1] > *maxy) *maxy = tmp.data.lptr[i*2];
         }
      }
  }
}

static void gds_flat_sref_size (GDS_TRANS trans, LONGINT *minx, LONGINT* miny, LONGINT* maxx, LONGINT *maxy)
{
   GDS_TRANS otrans;
   GDS_TRANS calling_trans;
   GDS_ITEM *item;
   char name[GDS_NAMELEN];
   int  st;
   double iangle;

   calling_trans = trans;
   /* get sname */
   if (!(item = gds_get_item ())) {
      return;
   }
   if (item -> len < 1 || item -> code != GDS_SNAME)
   {
      fprintf (stderr, "Error: expected SNAME, got %s in gds_flat_sref_size\n",
        gds_lookup[item->code>>8].name);
   }
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[item->len]=0;
   name[GDS_NAMELEN-1] = 0;
   if (item->len < GDS_NAMELEN) {
      name[item->len]=0;
   }
   else if (dowarning) {
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "fsSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   /* preserve mirror only */
   trans.strans &= 0x8000;
   /* save translate for origin */
   otrans = trans;
   st = 0;
   iangle = 0;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL)
   {
      LONGINT x, y;

#ifdef GDS_DEBUG
      if (item->code == GDS_STRANS)
         fprintf (stderr, "fSTR2h %d %04x %04x %08llx\n", item->len, item->data.sptr[0], item->data.sptr[1], (long long) item->data.sptr);
#endif
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_PROPATTR:
         break;
      case GDS_STRANS:
         if (item -> len != 2)
            fprintf (stderr, "Error: expected STRANS in SREF %s\n", name);
         st = *item -> data.sptr;
         if (st & 0x8000) /* reflection */
            trans.strans ^= 0x8000;
         trans.strans |= st & 0x60;
         break;
      case GDS_ANGLE:
	 iangle = item -> data.dptr[0];
         if (trans.strans & 0x2)
            trans.angle = iangle;
         break;
      case GDS_MAG:
         if (trans.strans & 0x4)
            trans.mag = item -> data.dptr[0];
         else
            trans.mag *= item -> data.dptr[0];
         break;
      case GDS_XY:
         if (item -> len != 8)
            fprintf (stderr, "Error: expected XY in SREF %s\n", name);
         x = item -> data.lptr[0];
         y = item -> data.lptr[1];
         gds_transform (otrans, &x, &y);
         trans.x = x;
         trans.y = y;
         break;
      default:
         fprintf (stderr, "Error: Illegal code for SREF %s\n",
            gds_lookup[item -> code >> 8].name);
         break;
      }
   }
   int from = __LINE__;
   push (from);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
   {
      fprintf (stderr, "Error: No structure %s in library\n", name);
      pop(from);
      return;
   }
   gds_item_ptr = item;
   if (!(st & 0x2))
   {
      if (calling_trans.strans & 0x8000)
      {
	 trans.angle = trans.angle - iangle;
      }
      else
      {
	 trans.angle += iangle;
      }
      while (trans.angle >= 360.0)
	 trans.angle -= 360.0;
      while (trans.angle < 0.0)
	 trans.angle += 360.0;
   }
#ifdef GDS_DEBUG
   fprintf (stderr, " st=%04X angle=%.0lf strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf %s\n",
      st & 0xffff, iangle, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag, name);
#endif
   gds_flat_size (trans, name, minx, miny, maxx, maxy);
   *gds_item_ptr = *pop(from);
}

static void gds_flat_aref_size (GDS_TRANS trans, LONGINT *minx, LONGINT* miny, LONGINT* maxx, LONGINT *maxy)
{
   GDS_TRANS otrans, /* translate origin */
         ltrans; /* translate local array */
   GDS_TRANS calling_trans;
   GDS_ITEM *item;
   char name[GDS_NAMELEN];
   int  col, row;
   LONGINT x0, y0;
   LONGINT x1, y1;
   LONGINT x2, y2;
   LONGINT dx, dy;
   double  dtmp;
   int  iangle;
   int  i, j;
   int  st;
   int  from;

   calling_trans = trans;
   /* get sname */
   if (!(item = gds_get_item ())) {
      return;
   }
   if (item -> len < 1 || item -> code != GDS_SNAME)
      fprintf (stderr, "Error: expected SNAME\n");
   memcpy (name, item -> data.cptr, (int) item -> len);
   name[GDS_NAMELEN-1] = 0;
   if (item->len < GDS_NAMELEN) {
      name[item->len]=0;
   }
   else if (dowarning) {
      fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
         GDS_NAMELEN, name);
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "faSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   /* preserve mirror only */
   trans.strans &= 0x8000;
   /* save translate for origin */
   otrans = trans;
   ltrans = trans;
   /* initialize local translation */
   ltrans.x = ltrans.y = 0L;
   st = iangle = col = row = x0 = y0 = x1 = y1 = x2 = y2 = 0;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDEL)
   {
      switch (item -> code)
      {
      case GDS_ELFLAGS:
      case GDS_PLEX:
      case GDS_PROPVALUE:
      case GDS_PROPATTR:
         break;
      case GDS_COLROW:
         if (item -> len != 4)
            fprintf (stderr, "Error: expected COLROW in AREF %s\n", name);
         col = item -> data.sptr[0];
         row = item -> data.sptr[1];
         break;
      case GDS_STRANS:
         if (item -> len != 2)
            fprintf (stderr, "Error: expected STRANS in AREF %s\n", name);
         st = *item -> data.sptr;
         if (st & 0x8000) /* reflection */
            trans.strans ^= 0x8000;
         trans.strans |= st & 0x60;
         break;
      case GDS_ANGLE:
         dtmp = item -> data.dptr[0];
         iangle = (int) (dtmp + 0.5);
         if (trans.strans & 0x2)
            trans.angle = dtmp;
         break;
      case GDS_MAG:
         if (trans.strans & 0x4)
            trans.mag = item -> data.dptr[0];
         else
            trans.mag *= item -> data.dptr[0];
         break;
      case GDS_XY:
         if (item -> len != 24)
            fprintf (stderr, "Error: expected XY in AREF %s\n", name);
         x0 = item -> data.lptr[0];
         y0 = item -> data.lptr[1];
         x1 = item -> data.lptr[2];
         y1 = item -> data.lptr[3];
         x2 = item -> data.lptr[4];
         y2 = item -> data.lptr[5];
         break;
      default:
         fprintf (stderr, "Error: Illegal code for AREF %s\n",
            gds_lookup[item -> code >> 8].name);
         break;
      }
   }
   from = __LINE__;
   push (from);
   if (!(item = (GDS_ITEM *) gds_find_structure (name)))
   {
      fprintf (stderr, "Error: No structure %s in library\n", name);
      pop(from);
      return;
   }
   if (col == 0 || row == 0)
   {
      fprintf (stderr, "Error: No COLROW in aref %s\n", name);
      pop(from);
      return;
   }
   if (x1 - x0)
      dx = x1 - x0;
   else
      dx = x2 - x0;
   if (y1 - y0)
      dy = y1 - y0;
   else
      dy = y2 - y0;
   /* the row/column "arrangement" can be switched! */
   switch (iangle)
   {
   case 0:
   case 180:
      break;
   case 90: /* row and col for 90 and 270 */
   case 270:
      i = col;
      col = row;
      row = i;
      break;
   default:
      fprintf (stderr,
         "Severe Warning: Do not know how to do AREF at angle %d\n", iangle);
      pop(from);
      return;
      break;
   }
   dx /= col;
   dy /= row;
   gds_transform (otrans, &x0, &y0);
   if (!(st & 0x2))
   {
      if (calling_trans.strans & 0x8000)
      {
	 trans.angle = trans.angle - iangle;
      }
      else
      {
	 trans.angle += iangle;
      }
      while (trans.angle >= 360.0)
	 trans.angle -= 360.0;
      while (trans.angle < 0.0)
	 trans.angle += 360.0;
   }
#ifdef GDS_DEBUG
   fprintf (stderr, "AREF %s, col %d, row %d, dx=%ld, dy=%ld\n",
      name, col, row, (long) dx, (long) dy);
   fprintf (stderr, "     incoming trans=(%d,%d) %04x, %.0lf\n",
      calling_trans.x, calling_trans.y, calling_trans.strans, calling_trans.angle);
   fprintf (stderr, "     result   trans=(%d,%d) %04x, %.0lf\n",
      trans.x+x0, trans.y+y0, trans.strans, trans.angle);
#endif
   for (i = 0; i < row; i++)
   for (j = 0; j < col; j++)
   {
      LONGINT x, y;

      x = dx * j;
      y = dy * i;
      gds_transform (ltrans, &x, &y);
      trans.x = x + x0;
      trans.y = y + y0;
      gds_item_ptr = item;
      gds_flat_size (trans, name, minx, miny, maxx, maxy);
   }
   pop(from);
}

static void gds_flat_size (GDS_TRANS trans, char *name, LONGINT* minx, LONGINT* miny, LONGINT* maxx, LONGINT* maxy)
{
   GDS_ITEM tmp, *item;
   int  datasize=(GDS_MAX_POINTS+1)*4*2;
   char *data;
   int i;
   static int level = 0;
   int layer = 0;
   int dt = 0;
   char key[10];
   LONGINT nx,ny,mx,my;

   nx = *minx;
   ny = *miny;
   mx = *maxx;
   my = *maxy;
   level++;
#ifdef GDS_DEBUG
   fprintf (stderr, "Structure %s  entering recursion level - %d\n",name,level);
#endif 
   if (!(item = (GDS_ITEM *) gds_find_structure (name))) {
      fprintf (stderr, "Error: structure %s not found\n", name);
      return;
   }

   int from = __LINE__;
   push (from);
   memset ((char *) &tmp, sizeof (tmp), 0);
   data=malloc(datasize);
   if (! data ) {
      out_of_memory(__LINE__);
   }
   tmp.data.cptr = data;
   gds_item_ptr = item;
#ifdef GDS_DEBUG
   fprintf (stderr, "fSTRU %s, strans=%4.4x, (%ld, %ld), A=%.0lf, M=%.2lf\n",
      name, (unsigned) (trans.strans & 0xffff), (long) trans.x, (long) trans.y, trans.angle, trans.mag);
#endif
   // get BGNSTR
   gds_get_item();
   // get STRNAME
   gds_get_item();
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR)
   {
      // to handle illegal gds with > 200 (GDS_MAX_POINTS) vertices
      if (item->len >= datasize)
      {
         while (datasize <= item->len) {
            datasize *= 2;
         }
         data = realloc(data, datasize);
         if (! data ) {
            out_of_memory(__LINE__);
         }
         tmp.data.cptr=data;
      }
      switch (item -> code)
      {
      case GDS_STRCLASS:
      case GDS_STRTYPE:
      case GDS_BOUNDARY:
      case GDS_BOX:
      case GDS_PATH:
      case GDS_BGNEXTN:
      case GDS_ENDEXTN:
      case GDS_ELKEY:
      case GDS_PROPATTR:
      case GDS_PROPVALUE:
      case GDS_PATHTYPE:
      case GDS_TEXT:
      case GDS_WIDTH:
      case GDS_ENDEL:
      case GDS_PRESENTATION:
      case GDS_STRANS:
      case GDS_STRING:
      case GDS_MAG: /* should only occur on text */
      case GDS_ANGLE: /* should only occur on text */
         break;
      case GDS_DATATYPE:
         dt = *item -> data.sptr;
         break;
      case GDS_LAYER:
         layer = *item -> data.sptr;
         break;
      case GDS_TEXTTYPE:
         layer = dt = 0;
         break;
      case GDS_SREF:
         {
         LONGINT lnx, lny, lmx, lmy;

         lnx = lny = LONGINT_MAX;
         lmx = lmy = LONGINT_MIN;
         if(check_flat_list())
            gds_hier_sref_aref_size(trans, &lnx, &lny, &lmx, &lmy);
         else
            gds_flat_sref_size (trans, &lnx, &lny, &lmx, &lmy);
         if (lnx < nx) nx = lnx;
         if (lny < ny) ny = lny;
         if (lmx > mx) mx = lmx;
         if (lmy > my) my = lmy;
         }
         break;
      case GDS_AREF:
         {
         LONGINT lnx, lny, lmx, lmy;

         lnx = lny = LONGINT_MAX;
         lmx = lmy = LONGINT_MIN;
         if(check_flat_list())
           gds_hier_sref_aref_size(trans, &lnx, &lny, &lmx, &lmy); 
         else
           gds_flat_aref_size (trans, &lnx, &lny, &lmx, &lmy);
         if (lnx < nx) nx = lnx;
         if (lny < ny) ny = lny;
         if (lmx > mx) mx = lmx;
         if (lmy > my) my = lmy;
         }
         break;
      case GDS_XY:
        tmp.len = item -> len;
        tmp.code = item -> code;
        tmp.flags = 0;
        memcpy (tmp.data.cptr, item->data.cptr, item->len);
        for (i = 0; i < tmp.len / 8; i++)
        {
//            fprintf (stderr, "%.0f,%.0f (%d,%d %d,%d) => ", (double) tmp.data.lptr[i*2], (double) tmp.data.lptr[i*2+1], nx, ny, mx, my);
            gds_transform (trans, &tmp.data.lptr[i*2], &tmp.data.lptr[i*2+1]);
            if (tmp.data.lptr[i*2] < nx) nx = tmp.data.lptr[i*2];
            if (tmp.data.lptr[i*2+1] < ny) ny = tmp.data.lptr[i*2+1];
            if (tmp.data.lptr[i*2] > mx) mx = tmp.data.lptr[i*2];
            if (tmp.data.lptr[i*2+1] > my) my = tmp.data.lptr[i*2+1];
            if (layer > 0) {
                SIZE_STRUCTURE *st;

                sprintf(key, "%d:%d", layer, dt);
//                fprintf (stderr, "%d:%d %d,%d => ", layer, dt, tmp.data.lptr[i*2], tmp.data.lptr[i*2+1]);
                if ((st = btins (key, &bt_layersize))) {
//                    fprintf (stderr, "new ");
                    st->minx = st->maxx = tmp.data.lptr[i*2];
                    st->miny = st->maxy = tmp.data.lptr[i*2+1];
                }
                else {
                    if ((st = btloc (key, &bt_layersize))) {
//                        fprintf (stderr, "mod ");
                        if (tmp.data.lptr[i*2] < st->minx) st->minx = tmp.data.lptr[i*2];
                        if (tmp.data.lptr[i*2+1] < st->miny) st->miny = tmp.data.lptr[i*2+1];
                        if (tmp.data.lptr[i*2] > st->maxx) st->maxx = tmp.data.lptr[i*2];
                        if (tmp.data.lptr[i*2+1] > st->maxy) st->maxy = tmp.data.lptr[i*2+1];
                    }
                }
//                fprintf (stderr, "%d,%d (%d,%d %d,%d)\n", tmp.data.lptr[i*2], tmp.data.lptr[i*2+1], st->minx, st->miny, st->maxx, st->maxy);
            }
        }
        break;
      default:
         fprintf (stderr,
            "Error: Illegal GDS element code %s in structure %s line %d\n",
            gds_lookup[item -> code >> 8].name, name, __LINE__);
         break;
      }
   }
   level--;
#ifdef GDS_DEBUG
   fprintf(stderr,"Structure %s  returning to recursion level - %d\n",name,level);
#endif 
   *minx = nx;
   *miny = ny;
   *maxx = mx;
   *maxy = my;
   pop(from);
   free(data);
}

void gds_size_flat (GDS_TRANS trans, char *name, LONGINT* minx, LONGINT* miny, LONGINT* maxx, LONGINT* maxy)
{
   GDS_ITEM *item;
   SIZE_STRUCTURE *sz;
   char key[10];

   if (!(item = gds_find_structure (name))) {
      fprintf (stderr, "Error: structure %s not found\n", name);
      return;
   }
   wrerr = 0;
   *minx = *miny = LONGINT_MAX;
   *maxx = *maxy = LONGINT_MIN;
   /* preserve mirror only */
   trans.strans &= 0x8000;
   btinit (&bt_layersize, 10, sizeof (SIZE_STRUCTURE), strcmp);
   gds_flat_size (trans, name, minx, miny, maxx, maxy);
   btselect ("", "zzzzzzzz", &bt_layersize);
   while (btget (key, (void * *) &sz, &bt_layersize))
   {
       fprintf (stderr, "%-9.9s %d,%d %d,%d\n", key,
           sz->minx, sz->miny, sz->maxx, sz->maxy);
   }
}

static BTTREE bt_lay;

typedef struct layer_structure LAYSTRUCT;

struct layer_structure {
    int layer;
    int datatype;
}; 

typedef struct count_structure CNTSTRUCT;

struct count_structure {
    long geom;
    long text;
};

static int laycmp (LAYSTRUCT *a, LAYSTRUCT *b) {
    if (a->layer > b->layer) return 1;
    if (a->layer < b->layer) return -1;
    if (a->datatype > b->datatype) return 1;
    if (a->datatype < b->datatype) return -1;
    return 0;
}

static void add_layer_info (int layer, int datatype, int geomcnt, int textcnt)
{
    struct layer_structure l;
    struct count_structure *c;

    if (! geomcnt && ! textcnt) return;
    l.layer = layer;
    l.datatype = datatype;
    if ((c=btloc(&l, &bt_lay)))
    {
        c->geom += geomcnt;
        c->text += textcnt;
    }
    else if ((c=btins(&l, &bt_lay))) {
        c->geom=geomcnt;
        c->text=textcnt;
    }
}

static void gds_getlayers (char *name)
{
   GDS_ITEM *item;
   static int level = 0;
   int layer = 0;
   int dt = 0;

   if (!(item = gds_find_structure (name))) {
      fprintf (stderr, "Error: structure %s not found\n", name);
      return;
   }
   level++;
#ifdef GDS_DEBUG
   fprintf (stderr, "Structure %s  entering recursion level - %d\n",name,level);
#endif 

   gds_item_ptr = item;
#ifdef GDS_DEBUG
   fprintf (stderr, "fSTRU %s\n", name);
#endif
   // get BGNSTR
   gds_get_item();
   // get STRNAME
   gds_get_item();
   int text=0;
   int geom=0;
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR)
   {
//      fprintf (stderr, "item %s\n", gds_lookup[item -> code >> 8].name);
      switch (item -> code)
      {
      case GDS_TEXT:
          text=1;
          geom=0;
          break;
      case GDS_BOUNDARY:
      case GDS_BOX:
      case GDS_PATH:
          text=0;
          geom=1;
          break;
      case GDS_STRNAME:
      case GDS_STRCLASS:
      case GDS_STRTYPE:
      case GDS_BGNEXTN:
      case GDS_ENDEXTN:
      case GDS_ELKEY:
      case GDS_PROPATTR:
      case GDS_PROPVALUE:
      case GDS_PATHTYPE:
      case GDS_WIDTH:
      case GDS_PRESENTATION:
      case GDS_STRANS:
      case GDS_STRING:
      case GDS_MAG: /* should only occur on text */
      case GDS_ANGLE: /* should only occur on text */
      case GDS_SREF:
      case GDS_AREF:
      case GDS_XY:
      case GDS_COLROW:
         break;
      case GDS_DATATYPE:
         dt = *item -> data.sptr;
         break;
      case GDS_LAYER:
         layer = *item -> data.sptr;
         break;
      case GDS_TEXTTYPE:
         dt = *item -> data.sptr;
         break;
      case GDS_ENDEL:
         add_layer_info(layer, dt, geom, text);
         geom=text=layer=dt=0;
         break;
      case GDS_SNAME:
         break;
      default:
         fprintf (stderr,
            "Error: Illegal GDS element code %s in structure %s line %d\n",
            gds_lookup[item -> code >> 8].name, name, __LINE__);
         break;
      }
   }
   level--;
#ifdef GDS_DEBUG
   fprintf(stderr, "Structure %s  returning to recursion level - %d\n",name,level);
#endif 
//   pop(from);
}

void gds_listlayers (char *name, int with_structures, char *file)
{
   struct layer_structure m, x, v;
   struct count_structure *c;
   GDS_STRUCTURE *srf;
   char cmd[2048];
   FILE *fp;

   m.layer = 0;
   m.datatype = 0;
   x.layer=256;
   x.datatype=256;

   if (! *file || ! (fp = fopen (file, "w")))
      fp = stdout;
   if (! with_structures)
       btinit (&bt_lay, sizeof (struct layer_structure), sizeof (struct count_structure), laycmp);
   btselect ("", "zzzzzzzz", &bt_structure);
   while (btget (cmd, (void * *) &srf, &bt_structure))
   {
       if (with_structures) btinit (&bt_lay, sizeof (struct layer_structure), sizeof (struct count_structure), laycmp);
      gds_getlayers (cmd);
      if (with_structures) {
          fprintf (fp, "STRNAME %s\n", cmd);
          btselect (&m, &x, &bt_lay);
          while (btget (&v, (void * *) &c, &bt_lay)) {
             fprintf (fp, "%3d %3d %6ld %6ld\n", v.layer, v.datatype, c->geom, c->text);
          }
      }
   }
   if (! with_structures)
   {
       btselect (&m, &x, &bt_lay);
       while (btget (&v, (void * *) &c, &bt_lay)) {
          fprintf (fp, "%3d %3d %6ld %6ld\n", v.layer, v.datatype, c->geom, c->text);
       }
   }
   if (fp != stdout) fclose (fp);
}

typedef struct pg_list PG_LIST;

struct pg_list {
    BTTREE bt_slist;
    };

static void loc_structure (PG_LIST *tree, char *name, int level, int max_level);
static void write_top (PG_LIST *top, int level);

void gds_print_hier (char *name, int max_level)
{
   PG_LIST  *top;

   if (!(top = (PG_LIST *) calloc (1, sizeof (PG_LIST)))) {
      out_of_memory(__LINE__);
   }
   btinit (&top -> bt_slist, GDS_NAMELEN, sizeof (PG_LIST), strcmp);
   loc_structure (top, name, 1, max_level);
   fprintf (stderr, "%s\n", name);
   write_top (top, 1);
}

/*************************************************************************
   This function hierarchically searches a tree and adds any
   structure references to a btree (bt_list)
*************************************************************************/
static void loc_structure (PG_LIST *tree, char *name, int level, int max_level)
{
   GDS_ITEM *item;
   PG_LIST *pg;
   char stru[GDS_NAMELEN + 1];
   int  from = __LINE__;


   if (!(item = gds_find_structure (name)))
   {
      fprintf (stderr, "Error: Cannot locate %s in library(s)\n", name);
      return;
   }
//   fprintf (stderr, "loc_structure %s %d\n", name, level);
   while ((item = gds_get_item ()) && item -> code != GDS_ENDSTR)
   {
      if (item -> code == GDS_SNAME)
      {
      /* SNAME only occurs on a structure reference */
         strncpy (stru, item -> data.cptr, (int) item -> len);
         if (item -> len > GDS_NAMELEN) {
            stru[GDS_NAMELEN]=0;
            if (dowarning)
                fprintf (stderr, "Warning: GDS Name exceeds %d characters\n %s\n",
                    GDS_NAMELEN, stru);
         }
         else {
             stru[item -> len] = 0;
         }
         if ((pg = (PG_LIST *) btins (stru, &tree -> bt_slist)))
         {
            btinit (&pg -> bt_slist, GDS_NAMELEN, sizeof (PG_LIST), strcmp);
//            fprintf (stderr, "Adding %s\n", stru);
            push (from);
            if (level < max_level)
               loc_structure (pg, stru, level + 1, max_level);
            pop (from);
         }
      }
   }
//   fprintf (stderr, "rtn %s\n", name);
}

void write_top (PG_LIST *top, int level)
{
   int i;
   PG_LIST *pg;
   char key[GDS_NAMELEN];

   btselect ("", "zzzzzz", &top -> bt_slist);
   while (btget (key, (void * *) &pg, &top -> bt_slist))
   {
      for (i = 0; i < level; i++)
         fprintf (stderr, " ");
      fprintf (stderr, "%s\n", key);
      write_top (pg, level+1);
   }
   btfree (&top -> bt_slist);
}

