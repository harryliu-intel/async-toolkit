// $Id$ AAG

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "imag.h"

/* include files in /usr/include/X11 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Paned.h>
//#include <X11/Xaw/Scroll.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Viewport.h>

#include <X11/Xmu/Xmu.h>
//#define  NORECTS

/*
#include <X11/Grip.h>
#include <X11/Composite.h>
#include <X11/Constraint.h>
#include <X11/Core.h>
#include <X11/VPaned.h>
#include <X11/Simple.h>
#include <X11/Scroll.h>
#include <X11/Logo.h>
#include <X11/Mailbox.h>
#include <X11/Text.h>
#include <X11/cursorfont.h>
#include <X11/bitmaps/left_ptr>
#include <X11/AsciiText.h>
*/

#define HSIZE   ((3300 + (xtopr)/2)/(xtopr))
#define VSIZE   ((2550 + (xtopr)/2)/(xtopr))

static   double magnification = 1.0;
static   double offsetx       = 0.0;
static   double offsety       = 0.0;
static   double original_magnification;
static   double original_offsetx;
static   double original_offsety;
static   int  xtopr = 4;
static   char *argv[1];
static   int   argc;
static   int   page_nr = 0;
static   int   current_font;
static   int   rect_built;
static   long  maxrequests;
static   char  filename[1024];
static   int   text_toggle = 1;
static   int   color_toggle = 1;
static   int   texture_fill = 0;

#define COLOR_TABLE_SIZE  1024
static   int   color_table_size=14;

static   XRectangle *rectlist[COLOR_TABLE_SIZE];
static   unsigned    rect_cnt[COLOR_TABLE_SIZE];
static   unsigned    rect_len[COLOR_TABLE_SIZE];
static   XRectangle *fillrlist[COLOR_TABLE_SIZE];
static   unsigned    fill_cnt[COLOR_TABLE_SIZE];
static   unsigned    fill_len[COLOR_TABLE_SIZE];
static   unsigned long  current_color;

static void load_all_fonts (void);

void dwg_setmag (double mag)
{
   if (mag > 0.0)
      magnification = mag;
}

void dwg_setoffset (double x, double y)
{
   offsetx = x * 300;
   offsety = y * 300;
}

#ifndef NORECTS
static void free_rects (void)
{
   int  i;

#ifdef DEBUG
   fprintf (stderr, "free_rects\n");
#endif
   for (i = 0; i < COLOR_TABLE_SIZE; i++)
      rect_cnt[i] = fill_cnt[i] = 0;
}

static void add_rect (int fill, int x, int y, int width, int height)
{
   unsigned *cnt, *len;
   XRectangle **list;

#ifdef DEBUG
   fprintf (stderr, "add_rect(%d,%d,%d,%d,%d)\n",fill,x,y,width,height);
#endif
   if (fill && color_toggle)
   {
      len = &fill_len[current_color];
      cnt = &fill_cnt[current_color];
      list = &fillrlist[current_color];
   }
   else if (!fill && color_toggle && !texture_fill)
   {
      len = &rect_len[current_color];
      cnt = &rect_cnt[current_color];
      list = &rectlist[current_color];
   }
   else
   {
      len = &rect_len[0];
      cnt = &rect_cnt[0];
      list = &rectlist[0];
   }
   if (*cnt >= *len)
   {
      *len += 32;
      if (!(*list))
      {
         *len = 32;
         if (!(*list = (XRectangle *) malloc (sizeof (XRectangle) * (*len))))
         {
            fprintf (stderr, "Out of memory at %d %s\n",
               __LINE__, __FILE__);
            exit (1);
         }
      }
      else
      {
         if (!(*list =
            (XRectangle *)
               realloc ((char *) *list, sizeof (XRectangle) * (*len))))
         {
            fprintf (stderr, "Out of memory at %d %s\n",
               __LINE__, __FILE__);
            exit (1);
         }
      }
   }
   (*list)[*cnt].x = x;
   (*list)[*cnt].y = y;
   (*list)[*cnt].width = width;
   (*list)[*cnt].height = height;
   (*cnt)++;
}
#endif /* norects */

#define NUMCOLORS COLOR_TABLE_SIZE
#define NUMFONTS  32
static   unsigned long color_pixel[NUMCOLORS];

static char *cnames[NUMCOLORS] = {
   "",
   "black",
   "lightgray",
   "lightblue",
   "gray",
   "yellow",
   "red",
   "mediumblue",
   "olivedrab",
   "green",
   "aquamarine",
   "purple",
   "pink",
   "violet",
   "orange",
   NULL,
   };

static   int   adv_dir = 0;

static int  fam_created[] =
{
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static char *fam_fontname[] =
{
   "-*-fixed-*-*-*-*-*-60-*-*-*-*-*-*",
   "6x10",
   "7x13",
   "9x15",
   "-adobe-helvetica-bold-r-*-*-20-140-*-*-*-*-*-*",
   "7x13bold",
   "9x15bold",
   NULL
};

static XFontStruct *font_list[NUMFONTS];

static Arg   arglist[13];
static Display *mydisplay;
static Window mywindow;
//static Window myno;
static XWindowAttributes myattributes;
static int mywidth, myheight;
static GC mygc;
static GC zoom_gc;

static char hello[] = {"Hello"};

static  Widget wtoplevel,
        wpane,
        menu,
        wform;

static  XGCValues values;
static  int i;
static  unsigned long myforeground, mybackground;
static  int myscreen;
static  XSizeHints myhint;
static  XEvent myevent;

static int  xb = 300,
            yb = 300;


#define SETPEN        1
#define DRAW_BOX      2
#define FILL_BOX      3
#define DRAW_LINE     4
#define MKBGLY        5
#define SET_TEXTURE   6
#define DRAW_POINT    7
#define SET_FAMILY    8
#define SET_SP        9
#define SMOVE        10
#define MMOVE        11
#define CRLF         12
#define TEXT_ITEM    13
#define DRAW_POLY    14
#define FILL_POLY    15
#define SET_ADV_DIRS 16
#define DRAW_CIRCLE  17
#define FILL_CIRCLE  18
#define DRAW_ARC     19
#define LAST_FUNCT   19

typedef struct dwg_item DWG_ITEM;
typedef union  dwg_union DWG_UNION;
typedef struct dwg_point DWG_POINT;

struct dwg_point {
   int x, y;
   };

union dwg_union {
   int ival;
   char *sval;
   DWG_POINT pval;
   };

struct dwg_item {
   int function;
   DWG_UNION *args;
   int   argcnt;
   DWG_ITEM *next;
   };

static DWG_ITEM *dwglist, *dwgtail;
static DWG_ITEM *textlist, *texttail;

static int portrait;
static int prg_portrait = 1;
static unsigned long fill_pixel;

static int ConvertColor();

static unsigned long pixel_value (int i)
{
   if (color_pixel[i] == 0xffffffffL && wform)
   {
      if (cnames[i] && strlen (cnames[i]))
          color_pixel[i] = ConvertColor (wform, cnames[i]);
      else
          color_pixel[i] = 0;
   }
   return color_pixel[i];
}

static void cnvt_to_screen (int *x, int *y)
{
   double tmp;

   tmp = magnification * (*x - offsety) + 0;
   if (tmp < -2e4)
      *x = -20000;
   else if (tmp > 2e4)
      *x = 20000;
   else
      *x = (tmp + yb) / xtopr;
   tmp = magnification * (*y - offsetx) + 0;
   if (tmp < -2e4)
      *y = -20000;
   else if (tmp > 2e4)
      *y = 20000;
   else
      *y = (tmp + xb) / xtopr;
}

#if 0
static void MenuSelect (Widget w, XtPointer junk, XtPointer garbage)
{
#ifdef DEBUG
   fprintf (stderr, "Menu item `%s' has been selected.\n", XtName (w));
#endif
   if (strcmp (XtName (w), "Destroy") == 0)
   {
      XtDestroyApplicationContext (XtWidgetToApplicationContext (w));
      exit (0);
   }
}
#endif

static void execute_item (DWG_ITEM *dwg, Region region)
{
   int Gx1, Gy1, Gx2,  Gy2;
   int x, y, w, h;
   int a1, a2;
   int i;
   int startx = 0, starty = 0;
   int maxx = 0, maxy = 0;
   int delta;
   static XPoint   *plist;
   static unsigned  plist_len;

#ifdef DEBUG
   fprintf (stderr, "execute_item(%d)\n", dwg->function);
#endif
   if (dwg -> function < 1 || dwg -> function > LAST_FUNCT)
      return;
   switch (dwg -> function)
   {
   case FILL_POLY:
   case DRAW_POLY:
      if (plist_len < (unsigned) dwg -> argcnt)
      {
         if (plist_len == 0)
         {
            plist_len = dwg -> argcnt;
            if (!(plist = (XPoint *) malloc (sizeof (XPoint) * plist_len)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
         else
         {
            plist_len = dwg -> argcnt;
            if (!(plist =
                     (XPoint *)
                        realloc ((char *) plist, sizeof (XPoint) * plist_len)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
      }
      for (i = 0; i < dwg -> argcnt; i++)
      {
         /* assume portrait */
         Gx1 = dwg -> args[i].pval.y;
         Gy1 = dwg -> args[i].pval.x;
         cnvt_to_screen (&Gx1, &Gy1);
         if (portrait)
         {
            plist[i].x = Gx1;
            plist[i].y = Gy1;
         }
         else
         {
            /* if not portrait, correct coords */
            plist[i].x = Gy1;
            plist[i].y = myheight - Gx1;
         }
         if (i == 0)
         {
            startx = maxx = plist[i].x;
            starty = maxy = plist[i].y;
         }
         else
         {
            if (startx > plist[i].x)
               startx = plist[i].x;
            if (starty > plist[i].y)
               starty = plist[i].y;
            if (maxx < plist[i].x)
               maxx = plist[i].x;
            if (maxy < plist[i].y)
               maxy = plist[i].y;
         }
      }
      if (XRectInRegion (region, startx, starty,
            (unsigned) maxx - startx, (unsigned) maxy - starty))
      {
         if (dwg -> function == FILL_POLY && color_toggle)
         {
            XSetForeground(mydisplay, mygc, fill_pixel);
            XFillPolygon (mydisplay, mywindow, mygc,
               plist, dwg -> argcnt, Complex, CoordModeOrigin);
            XDrawLines (mydisplay, mywindow, mygc,
               plist, dwg -> argcnt, CoordModeOrigin);
            XSetForeground(mydisplay, mygc, myforeground);
         }
         else
         {
	    if (color_toggle && !texture_fill)
	       XSetForeground(mydisplay, mygc, fill_pixel);
	    else
	       XSetForeground(mydisplay, mygc, myforeground);
            XDrawLines (mydisplay, mywindow, mygc,
               plist, dwg -> argcnt, CoordModeOrigin);
         }
      }
      break;
   case SETPEN:
      Gx1 = (dwg -> args[0].ival * magnification) / xtopr + 0.5;
      if (Gx1 <= 0)
         Gx1 = 1;
      XSetLineAttributes(mydisplay, mygc, Gx1, LineSolid, CapButt, JoinMiter);
      break;
   case DRAW_LINE:
      Gy1 = dwg -> args[0].ival;
      Gx1 = dwg -> args[1].ival;
      Gy2 = dwg -> args[2].ival;
      Gx2 = dwg -> args[3].ival;
      cnvt_to_screen (&Gx1, &Gy1);
      cnvt_to_screen (&Gx2, &Gy2);
      if (color_toggle && !texture_fill)
	 XSetForeground(mydisplay, mygc, fill_pixel);
      if (portrait)
      {
         if (Gx1 > Gx2)
         {
            x = Gx2;
            w = Gx1 - Gx2;
         }
         else
         {
            x = Gx1;
            w = Gx2 - Gx1;
         }
         if (Gy1 > Gy2)
         {
            y = Gy2;
            h = Gy1 - Gy2;
         }
         else
         {
            y = Gy1;
            h = Gy2 - Gy1;
         }
         if (XRectInRegion (region, x, y, (unsigned) w, (unsigned) h))
            XDrawLine(mydisplay, mywindow, mygc, Gx1, Gy1, Gx2, Gy2);
      }
      else
      {
         if (Gx1 > Gx2)
         {
            y = myheight - Gx1;
            h = Gx1 - Gx2;
         }
         else
         {
            y = myheight - Gx2;
            h = Gx2 - Gx1;
         }
         if (Gy1 > Gy2)
         {
            x = Gy2;
            w = Gy1 - Gy2;
         }
         else
         {
            x = Gy1;
            w = Gy2 - Gy1;
         }
         if (XRectInRegion (region, x, y, (unsigned) w, (unsigned) h))
            XDrawLine(mydisplay, mywindow, mygc,
               Gy1, myheight - Gx1, Gy2, myheight - Gx2);
      }
      break;
   case DRAW_BOX:
      Gy1 = dwg -> args[0].ival;
      Gx1 = dwg -> args[1].ival;
      Gy2 = dwg -> args[2].ival + Gy1;
      Gx2 = dwg -> args[3].ival + Gx1;
      cnvt_to_screen (&Gx1, &Gy1);
      cnvt_to_screen (&Gx2, &Gy2);
      Gx2 -= Gx1;
      Gy2 -= Gy1;
      if (color_toggle)
	 XSetForeground(mydisplay, mygc, fill_pixel);
      if (portrait)
      {
#ifdef NORECTS
         if (XRectInRegion (region, Gx1, Gy1, (unsigned) Gx2, (unsigned) Gy2))
            XDrawRectangle(mydisplay, mywindow, mygc, Gx1, Gy1, Gx2, Gy2);
#else
         if (!rect_built)
            add_rect (0, Gx1, Gy1, Gx2, Gy2);
#endif
      }
      else
      {
#ifdef NORECTS
         if (XRectInRegion (region, Gy1, myheight - Gx1 -  Gx2,
               (unsigned) Gy2, (unsigned) Gx2))
         {
            XDrawRectangle(mydisplay, mywindow, mygc,
               Gy1, myheight - Gx1 - Gx2, Gy2, Gx2);
         }
#else
         if (!rect_built)
            add_rect (0, Gy1, myheight - Gx1 - Gx2, Gy2, Gx2);
#endif
      }
      break;
   case FILL_BOX:
      if (!texture_fill)
	 break;
      Gy1 = dwg -> args[0].ival;
      Gx1 = dwg -> args[1].ival;
      Gy2 = dwg -> args[2].ival + Gy1;
      Gx2 = dwg -> args[3].ival + Gx1;
      cnvt_to_screen (&Gx1, &Gy1);
      cnvt_to_screen (&Gx2, &Gy2);
      Gx2 -= Gx1;
      Gy2 -= Gy1;
      if (portrait)
      {
#ifdef NORECTS
         if (XRectInRegion (region, Gx1, Gy1,
               (unsigned) Gx2 + 1, (unsigned) Gy2 + 1))
         {
            XSetForeground(mydisplay, mygc, fill_pixel);
            XFillRectangle(mydisplay, mywindow, mygc, Gx1, Gy1, Gx2+1, Gy2+1);
            /*
            XDrawRectangle(mydisplay, mywindow, mygc, Gx1, Gy1, Gx2, Gy2);
            */
         }
#else
         if (!rect_built)
         {
            add_rect (1, Gx1, Gy1, Gx2+1, Gy2+1);
         }
#endif
      }
      else
      {
#ifdef NORECTS
         if (XRectInRegion (region, Gy1, myheight - Gx1 - Gx2,
               (unsigned) Gy2+1, (unsigned) Gx2+1))
         {
            XSetForeground(mydisplay, mygc, fill_pixel);
            XFillRectangle(mydisplay, mywindow, mygc,
               Gy1, myheight - Gx1 - Gx2, Gy2+1, Gx2+1);
            /*
            XDrawRectangle(mydisplay, mywindow, mygc,
               Gy1, myheight - Gx1 - Gx2, Gy2, Gx2);
            */
         }
#else
         if (!rect_built)
         {
            add_rect (1, Gy1, myheight - Gx1 - Gx2, Gy2+1, Gx2+1);
         }
#endif
      }
      XSetForeground(mydisplay, mygc, myforeground);
      break;
   case DRAW_POINT:
      Gy1 = dwg -> args[0].ival;
      Gx1 = dwg -> args[1].ival;
      cnvt_to_screen (&Gx1, &Gy1);
      if (color_toggle && !texture_fill)
	 XSetForeground(mydisplay, mygc, fill_pixel);
      if (portrait)
      {
         if (XPointInRegion (region, Gx1, Gy1))
            XDrawPoint(mydisplay, mywindow, mygc, Gx1, Gy1);
      }
      else
      {
         if (XPointInRegion (region, Gy1, myheight - Gx1))
            XDrawPoint(mydisplay, mywindow, mygc, Gy1, myheight - Gx1);
      }
      break;
   case DRAW_ARC:
      Gy1 = dwg -> args[0].ival;
      Gx1 = dwg -> args[1].ival;
      Gy2 = dwg -> args[2].ival;
      Gx2 = dwg -> args[3].ival;
      cnvt_to_screen (&Gx1, &Gy1);
      Gx2 = (magnification * Gx2 / xtopr + 0.5);
      Gy2 = (magnification * Gy2 / xtopr + 0.5);
      a1 = dwg -> args[4].ival;
      a2 = dwg -> args[5].ival;
      if (color_toggle && !texture_fill)
	 XSetForeground(mydisplay, mygc, fill_pixel);
      if (portrait)
      {
         if (XRectInRegion (region, Gx1, Gy1, (unsigned) Gx2, (unsigned) Gy2))
            XDrawArc(mydisplay, mywindow, mygc, Gx1, Gy1, Gx2, Gy2,
               a1, a2);
      }
      else
      {
         if (XRectInRegion (region, Gy1, myheight - Gx1 - Gx2,
               (unsigned) Gy2, (unsigned) Gx2))
         {
            a1 += 360 * 16;
            while (a1 > 360 * 64)
               a1 -= 360 * 64;
            XDrawArc(mydisplay, mywindow, mygc,
               Gy1, myheight - Gx1 - Gx2, Gy2, Gx2,
               a1, a2);
         }
      }
      break;
   case TEXT_ITEM:
      if (color_toggle && !texture_fill)
	 XSetForeground(mydisplay, mygc, fill_pixel);
      if (text_toggle)
      {
         Gy1 = dwg -> args[0].ival;
         Gx1 = dwg -> args[1].ival;
         cnvt_to_screen (&Gx1, &Gy1);
         if (portrait)
         {
            if (adv_dir)
            {
               /* wrong way text */
               startx = Gx1;
               starty = Gy1;
               /*
               startx -= font_list[current_font] -> max_bounds.width;
               */
               delta = font_list[current_font] -> max_bounds.ascent + 1;
                     /*
                     font_list[current_font] -> max_bounds.descent;
                     */
               starty += delta;
               /* wrong way text */
               for (i = dwg -> args[3].ival - 1; i >= 0; i--)
               {
                  XDrawString (mydisplay, mywindow, mygc,
                     startx, starty + i * delta, dwg -> args[2].sval + i, 1);
               }
            }
            else
            {
               /* right way text */
               XDrawString (mydisplay, mywindow, mygc,
                  Gx1, Gy1, dwg -> args[2].sval, dwg -> args[3].ival);
            }
         }
         else
         {
            if (adv_dir)
            {
               /* right way text */
               XDrawString (mydisplay, mywindow, mygc,
                  Gy1, myheight - Gx1, dwg -> args[2].sval, dwg -> args[3].ival);
            }
            else
            {
               startx = Gy1;
               starty = myheight - Gx1;
               startx -= font_list[current_font] -> max_bounds.width;
               delta = font_list[current_font] -> max_bounds.ascent + 1;
                     /*
                     font_list[current_font] -> max_bounds.descent;
                     */
               starty -= (dwg -> args[3].ival - 1) * delta;
               /* wrong way text */
               for (i = dwg -> args[3].ival - 1; i >= 0; i--)
               {
                  XDrawString (mydisplay, mywindow, mygc,
                     startx, starty + i * delta, dwg -> args[2].sval + i, 1);
               }
            }
         }
      }
      break;
   case SET_FAMILY:
      if (font_list[dwg -> args[0].ival])
         XSetFont (mydisplay, mygc, font_list[dwg -> args[0].ival] -> fid);
      current_font = dwg -> args[0].ival;
      if (current_font >= NUMFONTS)
          current_font = NUMFONTS-1;
      break;
   case SET_TEXTURE:
      if (dwg -> args[0].ival == 0)
      {
         fill_pixel = pixel_value (abs(dwg -> args[1].ival));
         current_color = abs(dwg -> args[1].ival);
	 texture_fill = dwg -> args[1].ival > 0;
      }
      else
      {
         fill_pixel = myforeground;
         current_color = 0;
      }
      break;
   case SET_ADV_DIRS:
      adv_dir = dwg -> args[0].ival;
      break;
   }
}

static DWG_ITEM *newdwgitem (int function, DWG_UNION *args, int argcnt)
{
   DWG_ITEM *dwg;
   int       i;

#ifdef DEBUG
   fprintf (stderr, "newdwgitem (%d)\n", function);
#endif
   if (dwglist && !dwgtail)
      dwg = dwglist;
   else if (dwgtail && dwgtail -> next)
      dwg = dwgtail -> next;
   else
   {
      if (!(dwg = (DWG_ITEM *) calloc (1, sizeof (DWG_ITEM))))
      {
         fprintf (stderr, "Out of memory at %d %s\n",
            __LINE__, __FILE__);
         exit (1);
      }
   }
   dwg -> function = function;
   if (argcnt > 0)
   {
      if (dwg -> argcnt != argcnt)
      {
         if (dwg -> args)
         {
            if (!(dwg -> args = (DWG_UNION *)
               realloc ((char *) dwg -> args,
                              sizeof (DWG_UNION) * (unsigned) argcnt)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
         else
         {
            if (!(dwg -> args =
               (DWG_UNION *) malloc (sizeof (DWG_UNION) * (unsigned) argcnt)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
      }
   }
   else
   {
      if (dwg -> args)
         free ((char *) dwg -> args);
   }
   for (i = 0; i < argcnt; i++)
      dwg -> args[i] = args[i];
   dwg -> argcnt = argcnt;
   if (dwgtail)
      dwgtail -> next = dwg;
   else if (!dwglist)
      dwglist = dwg;
   dwgtail = dwg;
   return dwg;
}

static DWG_ITEM *newtextitem (int function, DWG_UNION *args, int argcnt)
{
   DWG_ITEM *text;
   int       i;

#ifdef DEBUG
   fprintf (stderr, "newtextitem (%d)\n", function);
#endif
   if (textlist && !texttail)
      text = textlist;
   else if (texttail && texttail -> next)
      text = texttail -> next;
   else
   {
      if (!(text = (DWG_ITEM *) calloc (1, sizeof (DWG_ITEM))))
      {
         fprintf (stderr, "Out of memory at %d %s\n",
            __LINE__, __FILE__);
         exit (1);
      }
   }
   text -> function = function;
   if (argcnt > 0)
   {
      if (text -> argcnt != argcnt)
      {
         if (text -> args)
         {
            if (!(text -> args = (DWG_UNION *)
               realloc ((char *) text -> args,
                              sizeof (DWG_UNION) * (unsigned) argcnt)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
         else
         {
            if (!(text -> args =
               (DWG_UNION *) malloc (sizeof (DWG_UNION) * (unsigned) argcnt)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
      }
   }
   else
   {
      if (text -> args)
         free ((char *) text -> args);
   }
   for (i = 0; i < argcnt; i++)
      text -> args[i] = args[i];
   text -> argcnt = argcnt;
   if (texttail)
      texttail -> next = text;
   else if (!textlist)
      textlist = text;
   texttail = text;
   return text;
}

void set_abs_h (int x)
{
    x = x;
}

void set_abs_v (int y)
{
    y = y;
}

int printer_type (void)
{
   return (2);
 /* 2 means xwindow */
}

#ifndef NORECTS

static int rects_initd;

static void draw_rects (Region region)
{
   int i, j;
   long st, nd;
   static XRectangle *rects;
   static unsigned int save_cnt;
   int    unsigned cnt;

#ifdef DEBUG
   fprintf (stderr, "draw_rects rect_initd=%d\n", rects_initd);
#endif
   if (rects_initd == -1) /* no rectangles! */
      return;
   if (rects_initd == 0)
   {
      cnt = rect_cnt[0];
      for (i = 1; i < COLOR_TABLE_SIZE; i++)
         if (cnt < rect_cnt[i])
            cnt = rect_cnt[i];
      for (i = 0; i < COLOR_TABLE_SIZE; i++)
         if (cnt < fill_cnt[i])
            cnt = fill_cnt[i];
      if (save_cnt && cnt)
      {
         if (save_cnt < cnt)
         {
            if (!(rects = (XRectangle *)
               realloc ((char *) rects, sizeof (XRectangle) * cnt)))
            {
               fprintf (stderr, "Out of memory at %d %s\n",
                  __LINE__, __FILE__);
               exit (1);
            }
         }
      }
      else if (cnt)
      {
         if (!(rects = (XRectangle *) malloc (sizeof (XRectangle) * cnt)))
         {
            fprintf (stderr, "Out of memory at %d %s\n",
               __LINE__, __FILE__);
            exit (1);
         }
      }
      if (cnt > 0)
         rects_initd = 1;
      else
         rects_initd = -1;
      if (cnt > save_cnt)
         save_cnt = cnt;
   }
   for (i = 0; i < COLOR_TABLE_SIZE; i++)
   {
      if (rect_cnt[i])
      {
         cnt = 0;
         for (j = 0; j < (int) rect_cnt[i]; j++)
         {
            if (XRectInRegion (region,
                  rectlist[i][j].x, rectlist[i][j].y,
                  rectlist[i][j].width, rectlist[i][j].height))
               rects[cnt++] = rectlist[i][j];
         }
         if (cnt)
         {
            XSetForeground(mydisplay, mygc, pixel_value (i));
            nd = cnt;
            st = 0;
            while (nd > 0)
            {
               XDrawRectangles(mydisplay, mywindow, mygc,
                  rects + st,
                  nd  > maxrequests ? maxrequests : nd);
               st += maxrequests;
               nd -= maxrequests;
            }
         }
      }
   }
   for (i = color_table_size; i >= 0; i--)
   {
      if (fill_cnt[i])
      {
         cnt = 0;
         for (j = 0; j < (int) fill_cnt[i]; j++)
         {
            if (XRectInRegion (region,
                  fillrlist[i][j].x, fillrlist[i][j].y,
                  fillrlist[i][j].width, fillrlist[i][j].height))
               rects[cnt++] = fillrlist[i][j];
         }
         if (cnt)
         {
            XSetForeground(mydisplay, mygc, pixel_value (i));
            nd = cnt;
            st = 0;
            while (nd > 0)
            {
               XFillRectangles(mydisplay, mywindow, mygc,
                  rects + st,
                  nd  > maxrequests ? maxrequests : nd);
               st += maxrequests;
               nd -= maxrequests;
            }
         }
      }
   }
   XSetForeground(mydisplay, mygc, myforeground);
}
#endif /* NORECTS */

#if 0
static void EventExit(Widget w, XtPointer data, XEvent event, Boolean cont)
{
#ifdef DEBUG
   fprintf (stderr, "EventExit\n");
#endif
   XSetFont (mydisplay, mygc, font_list[5] -> fid);
   XDrawImageString (mydisplay, myno, mygc, 3, 15, "Quit", 4);
}
#endif

static int zoom_startx;
static int zoom_starty;
static int zoom_endx;
static int zoom_endy;
static int button;

static void EventDwg(Widget w, XtPointer data, XEvent *event, Boolean *cont)
{
   DWG_ITEM *dwg;
   static Region   region;
   XRectangle      rect;

   w = w;
   data = data;
   cont = cont;
#ifdef DEBUG
   fprintf (stderr, "EventDwg %d:%d,%d,%d,%d\n",
      event -> xexpose.count,
      event -> xexpose.x,
      event -> xexpose.y,
      event -> xexpose.width,
      event -> xexpose.height);
#endif
   /* looks like only last event is important, so throw away the rest! */
   if (!region)
      region = XCreateRegion ();
   rect.x = event -> xexpose.x;
   rect.y = event -> xexpose.y;
   rect.width = event -> xexpose.width;
   rect.height = event -> xexpose.height;
   XClearArea (mydisplay, mywindow, rect.x, rect.y, rect.width, rect.height, False);
   XSetForeground(mydisplay, mygc, mybackground);
   XFillRectangle(mydisplay, mywindow, mygc, rect.x, rect.y, rect.width, rect.height);
   if (event -> xexpose.count == 0) // last event in tree
   {
      XGetWindowAttributes (mydisplay, mywindow, &myattributes);
      if (myattributes.height != myheight)
      {
#ifdef DEBUG
	 fprintf (stderr, "Reset rect_built\n");
#endif
         rect_built = 0;
#ifndef NORECTS
         free_rects ();
#endif
      }
      myheight = myattributes.height;
      mywidth = myattributes.width;
   }
   XUnionRectWithRegion (&rect, region, region);
   if (event -> xexpose.count > 0)
      return;
   XSetRegion (mydisplay, mygc, region);
   XSetRegion (mydisplay, zoom_gc, region);
   xb = yb = 300;
   adv_dir = 0;
   fill_pixel = myforeground;
   if (prg_portrait & 0x80)
      portrait = (prg_portrait & 0x7f);
#ifndef NORECTS
   if (rect_built)
      draw_rects (region);
#endif
   for (dwg = dwglist; dwg && dwg != dwgtail; dwg = dwg -> next)
      execute_item (dwg, region);
   if (dwg)
      execute_item (dwg, region);
#ifndef NORECTS
   if (!rect_built)
      draw_rects (region);
#endif
   if (text_toggle)
   {
      for (dwg = textlist; dwg && dwg != texttail; dwg = dwg -> next)
         execute_item (dwg, region);
      if (dwg)
         execute_item (dwg, region);
   }
   XDestroyRegion (region);
   region = XCreateRegion ();
   rect_built = 1;
}

void ReDraw (Widget w, XtPointer data, Boolean *cont, double mag, double offx, double offy)
{
   XEvent dwg_event;
   int i;

#ifdef DEBUG
   fprintf (stderr, "ReDraw\n");
#endif
   XGetWindowAttributes (mydisplay, mywindow, &myattributes);
   magnification = mag;
   offsetx = offx;
   offsety = offy;
   XClearWindow (mydisplay, mywindow);
   dwg_event.xexpose.x = 0;
   dwg_event.xexpose.y = 0;
   dwg_event.xexpose.width = mywidth;
   dwg_event.xexpose.height = myheight;
   dwg_event.xexpose.count = 0;
   rect_built = 0;
   for (i = 0; i < COLOR_TABLE_SIZE; i++)
      rect_cnt[i] = fill_cnt[i]  = 0;
   EventDwg (w, data, &dwg_event, cont);
}

static void EventKeyPress(Widget w, XtPointer data, XEvent *event, Boolean *cont)
{
   char c;
   int  count;
   KeySym keycode;
   double magx, offx, offy;
   int    width, height;

   count = XLookupString ((XKeyEvent *) event, &c, 1, &keycode, 0);
   if (count == 0 || !isascii (c))
      keycode = XKeycodeToKeysym (mydisplay, event -> xkey.keycode, 0);
   else
      keycode = c;
#ifdef DEBUG
   fprintf (stderr,
      "Key   sn %ld (%d,%d) (%d,%d) st 0x%x k 0x%x cnt %d same %d\n",
      event -> xkey.serial,
      event -> xkey.x,
      event -> xkey.y,
      event -> xkey.x_root,
      event -> xkey.y_root,
      event -> xkey.state,
      count ? (unsigned char) c : (int) keycode,
      count,
      event -> xkey.same_screen);
   switch (keycode)
   {
      case XK_Up: fprintf (stderr, "XK_Up\n"); break;
      case XK_Down: fprintf (stderr, "XK_Down\n"); break;
      case XK_Left: fprintf (stderr, "XK_Left\n"); break;
      case XK_Right: fprintf (stderr, "XK_Right\n"); break;
   }
#endif
   if (portrait)
   {
      width = myheight;
      height = mywidth;
   }
   else
   {
      height = myheight;
      width = mywidth;
   }
   switch (keycode)
   {
   case 'q':
   case 'C' - 'A' + 1:
      /* quit */
      XtDestroyApplicationContext (XtWidgetToApplicationContext (w));
      exit (0);
      break;
   case 'c':
      color_toggle = ! color_toggle;
#ifndef NORECTS
      rects_initd = 0;
#endif
      ReDraw (w, data, cont, magnification, offsetx, offsety);
      break;
   case 'r':
   case 'R' - 'A' + 1:
      /* revert */
      magnification = original_magnification;
      ReDraw (w, data, cont, magnification, original_offsetx, original_offsety);
      break;
   case 't':
      text_toggle = ! text_toggle;
      ReDraw (w, data, cont, magnification, offsetx, offsety);
      break;
   case XK_R7:
      ReDraw (w, data, cont, magnification, original_offsetx, original_offsety);
      break;
   case XK_Up:
   case 'k':
      if (portrait)
      {
         offy = offsety;
         offx = offsetx + (myheight*xtopr) * 0.20 / magnification;
      }
      else
      {
         offx = offsetx;
         offy = offsety + (myheight*xtopr) * 0.20 / magnification;
      }
      ReDraw (w, data, cont, magnification, offx, offy);
      break;
   case XK_Down:
   case 'j':
      if (portrait)
      {
         offy = offsety;
         offx = offsetx - (myheight*xtopr) * 0.20 / magnification;
      }
      else
      {
         offx = offsetx;
         offy = offsety - (myheight*xtopr) * 0.20 / magnification;
      }
      ReDraw (w, data, cont, magnification, offx, offy);
      break;
   case XK_Left:
   case 'h':
      if (portrait)
      {
         offx = offsetx;
         offy = offsety - (mywidth*xtopr) * 0.20 / magnification;
      }
      else
      {
         offy = offsety;
         offx = offsetx - (mywidth*xtopr) * 0.20 / magnification;
      }
      ReDraw (w, data, cont, magnification, offx, offy);
      break;
   case XK_Right:
   case 'l':
      if (portrait)
      {
         offx = offsetx;
         offy = offsety + (mywidth*xtopr) * 0.20 / magnification;
      }
      else
      {
         offy = offsety;
         offx = offsetx + (mywidth*xtopr) * 0.20 / magnification;
      }
      ReDraw (w, data, cont, magnification, offx, offy);
      break;
   case 'Z':
      magx = 0.5;
      offx = ((width*xtopr)/2 - xb)/magnification + offsetx;
      offx = (offsetx - offx) / magx + offx;
      offy = ((height*xtopr)/2 - yb)/magnification + offsety;
      offy = (offsety - offy) / magx + offy;
      ReDraw (w, data, cont, magx * magnification, offx, offy);
      break;
   case 'z':
      magx = 2.0;
      offx = ((width*xtopr)/2 - xb)/magnification + offsetx;
      offx = (offsetx - offx) / magx + offx;
      offy = ((height*xtopr)/2 - yb)/magnification + offsety;
      offy = (offsety - offy) / magx + offy;
      ReDraw (w, data, cont, magx * magnification, offx, offy);
      break;
   }

   /*
   if (event -> xbutton.button == 1)
   {
      zoom_startx = zoom_endx = event -> xbutton.x;
      zoom_starty = zoom_endy = event -> xbutton.y;
      button = event -> xbutton.button;
   }
   */
}

static void EventButtonPress(Widget w, XtPointer data, XEvent *event, Boolean *cont)
{
   int i;

   data = data;
   cont = cont;
#ifdef DEBUG
   fprintf (stderr, "Press sn %ld (%d,%d) (%d,%d) state 0x%x b %d same %d\n",
      event -> xbutton.serial,
      event -> xbutton.x,
      event -> xbutton.y,
      event -> xbutton.x_root,
      event -> xbutton.y_root,
      event -> xbutton.state,
      event -> xbutton.button,
      event -> xbutton.same_screen);
#endif
   if (w == wform && event -> xbutton.button == 1)
   {
      zoom_startx = zoom_endx = event -> xbutton.x;
      zoom_starty = zoom_endy = event -> xbutton.y;
   }
   if (menu && w == wform && event -> xbutton.button == 2)
   {
      i = 0;
      XtSetArg (arglist[i], XtNx, event -> xbutton.x_root); i++;
      XtSetArg (arglist[i], XtNy, event -> xbutton.y_root); i++;
      XtSetValues (menu, arglist, i);
      XtPopup (menu, XtGrabNonexclusive);
   }
   button = event -> xbutton.button;
}

static void EventRelease(Widget w, XtPointer data, XEvent *event, Boolean *cont)
{
   int  minx, maxx, miny, maxy;
   double magx, magy;
   double offx, offy;

#ifdef DEBUG
   fprintf (stderr, "Relse wd %ld (%d,%d) (%d,%d) state 0x%x b %d same %d\n",
      w,
      event -> xbutton.x,
      event -> xbutton.y,
      event -> xbutton.x_root,
      event -> xbutton.y_root,
      event -> xbutton.state,
      event -> xbutton.button,
      event -> xbutton.same_screen);
#endif
   if (w == menu && button == 2 && event -> xbutton.button == 2)
   {
      XtPopdown (menu);
   }
   if (w == wform && button == 1 && event -> xbutton.button == 1)
   {
      XGetWindowAttributes (mydisplay, mywindow, &myattributes);
      minx = maxx = zoom_startx;
      if (minx > zoom_endx)
         minx = zoom_endx;
      if (maxx < zoom_endx)
         maxx = zoom_endx;
      miny = maxy = zoom_starty;
      if (miny > zoom_endy)
         miny = zoom_endy;
      if (maxy < zoom_endy)
         maxy = zoom_endy;
      XDrawRectangle(mydisplay, mywindow, zoom_gc,
         minx, miny, maxx - minx, maxy - miny);
      if (abs (zoom_startx - event -> xbutton.x) > 10 ||
         abs (zoom_starty - event -> xbutton.y) > 10)
      {
         if (prg_portrait & 0x80)
            portrait = (prg_portrait & 0x7f);
         if (portrait)
         {
            magx = (double)(maxx - minx) / mywidth;
            magy = (double)(maxy - miny) / myheight;
            /* x = (magnification * (x - offsetx) + xb / xtopr */
            if (magy > magx)
               magx = magy;
            if (zoom_startx > event -> xbutton.x)
            {
               /* zoom out around center */
               /* The global coordinate of screen center */
               offx = ((myheight*xtopr)/2 - xb)/magnification + offsetx;
               offx = (offsetx - offx) / magx + offx;
               offy = ((mywidth*xtopr)/2 - yb)/magnification + offsety;
               offy = (offsety - offy) / magx + offy;
               magx = magnification * magx;
            }
            else
            {
               /* zoom in to view */
               magx = magnification / magx;
               offx = (miny * xtopr - xb) / magnification + offsetx + xb / magx;
               offy = (minx * xtopr - yb) / magnification + offsety + yb / magx;
            }
         }
         else
         {
            magx = (double)(maxx - minx) / mywidth;
            magy = (double)(maxy - miny) / myheight;
            if (magy > magx)
               magx = magy;
            if (zoom_startx > event -> xbutton.x)
            {
               /* zoom out around center */
               offx = ((mywidth*xtopr)/2 - xb)/magnification + offsetx;
               offx = (offsetx - offx) / magx + offx;
               offy = ((myheight*xtopr)/2 - yb)/magnification + offsety;
               offy = (offsety - offy) / magx + offy;
               magx = magnification * magx;
            }
            else
            {
               /* zoom in to view */
               magx = magnification / magx;
               offx = (minx * xtopr - xb) / magnification + offsetx + xb / magx;
               offy = ((myheight - maxy) * xtopr - yb) / magnification +
                  offsety + yb / magx;
            }
         }
         ReDraw (w, data, cont, magx, offx, offy);
      }
   }
   button = 0;
}

static void EventMotion(Widget w, XtPointer data, XEvent *event, Boolean *cont)
{
   int minx, miny, maxx, maxy;

    data = data;
    cont = cont;
#ifdef DEBUG
   fprintf (stderr, "Moton wd %ld (%d,%d) (%d,%d) state 0x%x b X same %d\n",
      w,
      event -> xmotion.x,
      event -> xmotion.y,
      event -> xmotion.x_root,
      event -> xmotion.y_root,
      event -> xmotion.state,
      event -> xmotion.same_screen);
#endif
   if (w == wform && button == 1)
   {
      minx = maxx = zoom_startx;
      if (minx > zoom_endx)
         minx = zoom_endx;
      if (maxx < zoom_endx)
         maxx = zoom_endx;
      miny = maxy = zoom_starty;
      if (miny > zoom_endy)
         miny = zoom_endy;
      if (maxy < zoom_endy)
         maxy = zoom_endy;
      XDrawRectangle(mydisplay, mywindow, zoom_gc,
         minx, miny, maxx - minx, maxy - miny);
      zoom_endx = event -> xmotion.x;
      zoom_endy = event -> xmotion.y;
      minx = maxx = zoom_startx;
      if (minx > zoom_endx)
         minx = zoom_endx;
      if (maxx < zoom_endx)
         maxx = zoom_endx;
      miny = maxy = zoom_starty;
      if (miny > zoom_endy)
         miny = zoom_endy;
      if (maxy < zoom_endy)
         maxy = zoom_endy;
      XDrawRectangle(mydisplay, mywindow, zoom_gc,
         minx, miny, maxx - minx, maxy - miny);
   }
}

#if 0
static void CallbackNo(Widget w, caddr_t closure, caddr_t call_data)
{
  XtDestroyApplicationContext (XtWidgetToApplicationContext (w));
  exit (0);
}
#endif

static int ConvertColor(Widget w, char *color_name)
{
  XrmValue from, to;

#ifdef DEBUG
  fprintf (stderr, "ConvertColor %s\n", color_name);
#endif
  from.size = strlen(color_name) + 1;
  from.addr = color_name;

  XtConvert(w, XtRString, (XrmValuePtr) &from, XtRPixel, (XrmValuePtr) &to);
  if (to.addr == NULL)
  {
    return 1;
  }

  return( (int) *((Pixel *) to.addr) );
}

void free_dwglist (void)
{
   dwgtail = NULL;
   texttail = NULL;
}

void end_page (void)
{
#define FILELEN 128
   char  wname[FILELEN];
   Arg   shellargs[20];
   char *s;

   page_nr++;
   if (dwgtail)
   {
      argc = 1;
      argv[0] = "xdwg";
      if (prg_portrait & 0x80)
         portrait = (prg_portrait & 0x7f);
      if (!fork ())
      {
         original_magnification = magnification;
         original_offsetx = offsetx;
         original_offsety = offsety;
         wtoplevel = XtInitialize("main","XAsk",NULL,0,&argc,argv);
         if (*filename)
         {
            if (strlen (filename) > FILELEN-1)
            {
               if ((s = strrchr (filename, '/')) && strlen (s) < FILELEN)
                  strcpy (wname, s + 1);
               else
                  strcpy (wname, s + strlen (filename) - FILELEN-1);
            }
            else
               strcpy (wname, filename);
         }
         else
            sprintf (wname, "XDwg Page %d", page_nr);
         i = 0;
         XtSetArg (shellargs[i], XtNiconName, wname); i++;
         XtSetArg (shellargs[i], XtNtitle, wname); i++;
         XtSetArg (shellargs[i], XtNsaveUnder, TRUE); i++;
         XtSetArg (shellargs[i], XtNx, 20); i++;
         XtSetArg (shellargs[i], XtNy, 20); i++;
         XtSetValues(wtoplevel,shellargs,i);

         i = 0;
         XtSetArg (arglist[i], XtNshowGrip, FALSE); i++;

         wpane = XtCreateManagedWidget ("outer", panedWidgetClass,
            wtoplevel, arglist, i);

         /*
         XtSetArg (arglist[i], XtNshapeStyle, XawShapeOval); i++;
         command = XtCreateManagedWidget ("Menu", formWidgetClass,
            wpane, arglist, i);

         entry = NULL;
         for (i = 0; i < 1; i++)
         {
            static char *menu_items[] = {
               "Destroy", NULL};
            char *item = menu_items[i];

            XtSetArg (arglist[0], XtNshapeStyle, XawShapeOval);
            XtSetArg (arglist[1], XtNheight, 14);
            XtSetArg (arglist[2], XtNfromHoriz, entry);

            entry = XtCreateManagedWidget (item, commandWidgetClass, command,
                        arglist, i ? 3 : 2);
            XtAddCallback (entry, XtNcallback, MenuSelect, NULL);
         }
         */

         i = 0;
         XtSetArg(arglist[i], XtNborderWidth, 1); i++;
         XtSetArg(arglist[i],XtNx,0); i++;
         XtSetArg(arglist[i],XtNy,0); i++;
         if (portrait)
         {
            XtSetArg(arglist[i],XtNwidth,VSIZE); i++;
            XtSetArg(arglist[i],XtNheight,HSIZE); i++;
         }
         else
         {
            XtSetArg(arglist[i],XtNwidth,HSIZE); i++;
            XtSetArg(arglist[i],XtNheight,VSIZE); i++;
         }
	 // set background to white, I think! seems to fix a problem 12/15/98
	 XtSetArg(arglist[i],XtNbackground,0); i++;

         wform= XtCreateManagedWidget("formWidget",stripChartWidgetClass,
                                      wpane, arglist,i);
         /*
         menu = XtCreatePopupShell ("Commands", simpleMenuWidgetClass, wform,
            (Arg *) NULL, 0);

         entry = NULL;
         for (i = 0; i < 3; i++)
         {
            static char *menu_items[] = {
               "Zoom In", "Zoom Out", "Kill", NULL};
            char *item = menu_items[i];

            entry = XtCreateManagedWidget (item, smeBSBObjectClass, menu,
                        arglist, 0);
            XtAddCallback (entry, XtNcallback, MenuSelect, NULL);
         }
         XtAddEventHandler(menu,ButtonReleaseMask,FALSE,EventRelease,
            (XtPointer) page_nr);
         XtAddEventHandler(menu,Button1MotionMask,FALSE,EventMotion,
            (XtPointer) page_nr);
         */

         XtRealizeWidget(wtoplevel);

         mydisplay = XtDisplay(wform);
         myscreen = DefaultScreen(mydisplay);

         mybackground = WhitePixel(mydisplay,myscreen);
         myforeground = BlackPixel(mydisplay,myscreen);
         color_pixel[0] = myforeground;
         color_pixel[1] = myforeground;

         /* window position and size */
         myhint.x = 20; myhint.y = 30;
         if (portrait)
         {
            myhint.width = myhint.max_width = VSIZE;
            myhint.height = myhint.max_height = HSIZE;
         }
         else
         {
            myhint.width = myhint.max_width = HSIZE;
            myhint.height = myhint.max_height = VSIZE;
         }
         myhint.flags = PPosition | PSize | PMaxSize;

         mywindow = XtWindow(wform);
        /* window creation */

         XSetStandardProperties(mydisplay, mywindow, hello, hello, None,
                               argv, argc, &myhint);

         mygc = XtGetGC(wform,0,&values);

         /* setup zoom graphics context */
         zoom_gc = XCreateGC(mydisplay,mywindow,0,0);

         XSetForeground(mydisplay,zoom_gc,myforeground);
         XSetBackground(mydisplay,zoom_gc,mybackground);
         XSetFunction(mydisplay,zoom_gc,GXxor);
         XSetLineAttributes (mydisplay, zoom_gc, 1, LineOnOffDash, 0, 0);

         XSetPlaneMask(mydisplay,zoom_gc,
           (BlackPixel(mydisplay,myscreen) ^ WhitePixel(mydisplay,myscreen)));

         XSetBackground(mydisplay, mygc, mybackground);
         XSetForeground(mydisplay, mygc, myforeground);

         XMapRaised(mydisplay, mywindow);

         XSelectInput(mydisplay, mywindow, ButtonReleaseMask | ButtonPressMask |
                      KeyPressMask | ExposureMask);

         XtNextEvent (&myevent);

         /*  something like this might work
         XtMapWidget (mywindow);
         */
         set_family (6);
         load_all_fonts ();
         XtAddEventHandler(wform,ExposureMask,FALSE,EventDwg,
            (XtPointer) page_nr);
         XtAddEventHandler(wform,ButtonPressMask,FALSE,EventButtonPress,
            (XtPointer) page_nr);
         XtAddEventHandler(wform,KeyPressMask,FALSE,EventKeyPress,
            (XtPointer) page_nr);
         XtAddEventHandler(wform,ButtonReleaseMask,FALSE,EventRelease,
            (XtPointer) page_nr);
         XtAddEventHandler(wform,Button1MotionMask,FALSE,EventMotion,
            (XtPointer) page_nr);
         for (i = 2; i < NUMCOLORS; i++)
         {
            color_pixel[i] = 0xffffffffL;
         }
         maxrequests = (XMaxRequestSize (mydisplay) - 3) / 2;
         while (1)
         {
            XtNextEvent(&myevent);
            XtDispatchEvent(&myevent);
         }
      }
   }
   xb = yb = 300;
   magnification = 1.0;
   offsetx = offsety = 0.0;
   free_dwglist ();
   setpen (-1);
   set_texture (-1, -1);
   text (0, 0, (char *) NULL, (-1));
   portrait = 1;
}

FILE *set_file (FILE *f)
{
   portrait = 1;
   filename[0] = 0;
   return (f);
}

void dwg_docolor (int arg)
{
   color_toggle = (arg != 0);
}

void dwg_filename (char *name)
{
   strcpy (filename, name);
}

void setpen (int i)
{
   DWG_UNION v;
   static int  p = -1;

   if (i < 1)
   {
      p = (-1);
      return;
   }
   if (p != i)
   {
      p = i;
   }
   v.ival = p;
   if (v.ival == 0)
      v.ival = 1;
   newdwgitem (SETPEN, &v, 1);
}

void set_border (int x, int y)
{
   xb = x;
   yb = y;
}

void draw_box (int x1, int y1, int x2, int y2, int fill)
{
   DWG_UNION v[4];

//   if (abs(fill) >= COLOR_TABLE_SIZE)
//      fill = 0;
   if (x1 < x2)
   {
      v[0].ival = x1;
      v[2].ival = x2 - x1;
   }
   else
   {
      v[0].ival = x2;
      v[2].ival = x1 - x2;
   }
   if (y1 < y2)
   {
      v[1].ival =  y1;
      v[3].ival = y2 - y1;
   }
   else
   {
      v[1].ival = y2;
      v[3].ival = y1 - y2;
   }
   set_texture (0, fill);
   if (fill > 0)
      newdwgitem (FILL_BOX, v, 4);
   else
      newdwgitem (DRAW_BOX, v, 4);
}

void draw_line (int x1, int y1, int x2, int y2, int pen)
{
   DWG_UNION v[4];

   v[0].ival =  x1;
   v[1].ival =  y1;
   v[2].ival =  x2;
   v[3].ival  = y2;

   setpen (pen);
   newdwgitem (DRAW_LINE, v, 4);
}

int set_texture (int fam, int mem)
{
   static int  f = -1,
               m = -1;
   static int  init = 0;
   DWG_UNION   v[2];

   if (abs(mem) & 0xf000000)
   {
   // create a color map entry if none exists for this color
      char newcname[32];
      int i;

      sprintf (newcname, "#%06x", abs(mem)&0xffffff);
      // slow but ok since list will be relatively short
      for (i = 1; cnames[i] && strcmp (cnames[i], newcname); i++)
         ;
      if (! cnames[i])
      {
         cnames[i] = malloc (strlen (newcname)+1);
         strcpy (cnames[i],newcname);
         color_pixel[i] = abs(mem) & 0xffffff;
         color_table_size=i;
      }
      if (mem < 0)
         mem = -i;
      else
         mem = i;
   }
   // prevent empty windows
   if (!dwgtail)
      return mem;
   if (f != fam || m != mem || !init)
   {
      init = (fam != (-1));
      v[0].ival = fam;
      v[1].ival = mem;
      newtextitem (SET_TEXTURE, v, 2);
      newdwgitem (SET_TEXTURE, v, 2);
      f = fam;
      m = mem;
   }
   return mem;
}

void draw_point (int x, int y, int pen)
{
   DWG_UNION v[2];

   v[0].ival = x;
   v[1].ival = y;
   setpen (pen);
   newdwgitem (DRAW_POINT, v, 2);
}

static void load_all_fonts (void)
{
   int i;

   for (i = 0; fam_fontname[i]; i++)
      if (fam_created [i])
      {
         if (!(font_list[i] = XLoadQueryFont (mydisplay, fam_fontname[i])))
         {
            fam_created[i] = 0;
         }
      }
}

void set_family (int f)
{
   DWG_UNION v;

   if (f < 1)
   {
      for (f = 0; f < 9; f++)
         fam_created[f] = 0;
      return;
   }
   if (!fam_created[f - 1])
   {
      fam_created[f - 1] = 1;
   }
   v.ival = f - 1;
   newtextitem (SET_FAMILY, &v, 1);
}

void set_sp (int d)
{
    d = d;
}

void smove (int d)
{
    d = d;
}

void mmove (int d)
{
    d = d;
}

void crlf (void)
{
}

void text (int x, int y, char *s, int f)
{
   static int  style = (-1);
   DWG_UNION v[4];

   if (f < 0)
   {
      style = (-1);
      return;
   }
   if (!*s)
      return;
   if (style != f)
   {
      set_family (f);
      style = f;
   }
   v[0].ival = y - yb;
   v[1].ival = x - xb;
   v[3].ival = strlen (s);
   if (!(v[2].sval = malloc ((unsigned) strlen (s) + 1)))
   {
      fprintf (stderr, "Out of memory at %d %s\n",
         __LINE__, __FILE__);
      exit (1);
   }
   strcpy (v[2].sval, s);
   newtextitem (TEXT_ITEM, v, 4);
}

#if 0
static void str (char *s)
{
   char *t;
   for (t = s + strlen (s) - 1; t - s >= 0 && isspace (*t); t--);
   *(t + 1) = '\0';
}
#endif

void polygon (unsigned cnt, int *x, int *y, int fill, int line)
{
   int       ncnt;
   unsigned  u;
   DWG_UNION *v;

   if (cnt == 0)
      return;
   if (!(v = (DWG_UNION *) malloc (sizeof (DWG_UNION) * cnt)))
   {
      fprintf (stderr, "Out of memory at %d %s\n",
         __LINE__, __FILE__);
      exit (1);
   }
#ifdef SHORTEN_POLYGON
   /* This is good for plots where there may be co-linear points */
   for (ncnt = i = 0; i < 2; i++, ncnt++)
   {
      v[ncnt].pval.x = x[i];
      v[ncnt].pval.y = y[i];
   }
   for (; i < cnt; i++, ncnt++)
   {
      v[ncnt].pval.x = x[i];
      v[ncnt].pval.y = y[i];
      if ((v[ncnt].pval.x - v[ncnt-2].pval.x) *
                     (v[ncnt-1].pval.y - v[ncnt-2].pval.y) ==
            (v[ncnt].pval.y - v[ncnt-2].pval.y) *
                     (v[ncnt-1].pval.x - v[ncnt-2].pval.x) &&
         (v[ncnt].pval.x != v[ncnt-2].pval.x) &&
            (v[ncnt-1].pval.x != v[ncnt-2].pval.x))
      {
         v[ncnt - 1].pval = v[ncnt].pval;
         ncnt--;
      }
   }
#else
   ncnt = cnt;
   for (u = 0; u < cnt; u++)
   {
      v[u].pval.x = x[u];
      v[u].pval.y = y[u];
   }
#endif /* SHORTEN_POLYGON */
   setpen (line ? line : 2);
   if (abs(fill) >= COLOR_TABLE_SIZE)
      fill = 0;
   if (fill)
      set_texture (0, fill);
   if (fill>0)
      newdwgitem (FILL_POLY, v, ncnt);
   else
      newdwgitem (DRAW_POLY, v, ncnt);
   free ((char *) v);
}

void set_adv_dirs (int main, int secondary)
{
   DWG_UNION v;
   if (main == 1)
      portrait = 0;
   else
      portrait = 1;
   secondary = secondary;

   v.ival = main;
   newtextitem (SET_ADV_DIRS, &v, 1);
}

void draw_circle (int x, int y, int r, int pen)
{
   circ_arc (r, x, y, 0, 16383, pen);
}

void circ_arc (int r, int x, int y, int a1, int a2, int pen)
{
   DWG_UNION v[6];

   setpen (pen);
   v[0].ival = x - r;
   v[1].ival = y - r;
   v[2].ival = 2 * r;
   v[3].ival = 2 * r;
   v[4].ival = -(a1 * 23040) / 16384;
   while (v[4].ival < 0)
      v[4].ival += 23040;
   v[5].ival = -((a2 -  a1) * 23040) / 16384;
   newdwgitem (DRAW_ARC, v, 6);
}

void end_file (void)
{
   if (dwglist)
      end_page ();
}

void force_portrait (int val)
{
   prg_portrait = (val & 0x7f) | 0x80;
}

void force_scale (int val)
{
   if (val < 3)
      val =  3;
   if (val > 32)
      val = 32;
   xtopr = val;
}
