// $Id$ AAG

/*
 This file is intended to the postscript printer
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "imag.h"

#define spt { if (! in_page) {fprintf (fp, "start_page\n"); in_page=1;}}

static int in_page = 0;
static FILE *fp;
static void draw_path(void) { spt; fprintf (fp, "sk\n");}
static void fill_path(void) { spt; fprintf (fp, "fill\n");}
static void newpath (void) { spt; fprintf (fp, "np\n");}
static void closepath (void) { spt; fprintf (fp, "closepath\n");}
static int xb=300,yb=300;
static int font_main = 0;
static int font_seco = 0;
static double magnification = 1.0;
static double offsetx       = 0.0;
static double offsety       = 0.0;
static int    docolor = 1;
static int    texture_fill = 1;

struct rgb {
   short r;
   short g;
   short b;
};

static int colorlistend = 0;
static struct rgb colorlist[1024] = {
   { 255,  255,  255}, // white
   {   0,    0,    0}, // black
   { 220,  220,  220}, // gray
   { 172,  220,  238}, // light blue
   { 205,  205,  205}, // medium gray
   { 255,  255,    0}, // yellow
   { 255,    0,    0}, // red
   {   0,    0,  205}, // dark blue
   { 102,  136,  333}, // olive
   {   0,  255,    0}, // green
   { 120,  255,  220}, // cyan 
   { 172,  333,  255}, // purple
   { 255,  205,  205}, // flesh
   { 238,  136,  238}, // light magenta
   { 255,  172,    0}, // orange
   { 255,  255,  255}, // 
   {-1, -1, -1}, // end of list
};

void dwg_docolor(int arg)
{
   docolor = (arg != 0);
}

void dwg_filename(char *s)
{
   /* do nothing */
   s = s;
}

void dwg_setmag(double mag)
{
   if (mag > 0.0)
      magnification = mag;
}

void dwg_setoffset(double valx, double valy)
{
   /* val in inches, offset in pixels */
   offsetx = valx * 300;
   offsety = valy * 300;
}

int printer_type (void)
{
   return (1);
}

static void moveto(int x, int y)
{
   spt;
   fprintf (fp, "%d %d mt\n", 2550 - x, y);
}

static void lineto(int x, int y)
{
   spt;
   fprintf (fp, "%d %d lt\n", 2550 - x, y);
}

void end_page(void) {fprintf (fp, "end_page\n"); in_page = 0;}

void setpen(int i)
{
   static int p = -1;
   if(i<1)
   {
      p=(-1);
      return;
   }
   if(p!=i)
   {
      /*
         So don't ask me why 0.8; it should be 0.24; but 0.8 matches
         the imagen line widths.  Zero is minimum for printer!
      */
      spt;
      p=i;
      i = (magnification * i + 0.5);
      if (i <= 0)
         i = 1;
      fprintf (fp, "%.3lf sl\n", i==1 ? 0.0 : i * 0.8);
   }
}

void end_file(void)
{
   fprintf (fp, "end_file\n");
//   fputs ("\033%-12345X@PJL EOJ", fp);
   fclose(fp);
   fp=NULL;
   setpen(-1);
   text(0,0,(char *)NULL,(-1));
}

FILE *set_file(FILE *f)
{
   fp=f;
//   fputs ("\033%-12345X@PJL JOB\n", fp);
//   fputs ("@PJL SET RESOLUTION = 300\n", fp);
//   fputs ("@PJL SET BITSPERPIXEL = 2\n", fp);
//   fputs ("@PJL SET ECONOMODE = OFF\n", fp);
//   fputs ("@PJL SET HOLDKEY = \"0000\"\n", fp);
//   fputs ("@PJL ENTER LANGUAGE = POSTSCRIPT \n", fp);
   fputs ("%!PS-Adobe-2.0\n", fp);
   fprintf (fp, "/start_file {/saved_state save def 1 setlinecap} def\n");
   fprintf (fp, "/start_page {0 setgray 1 setlinecap 0.24 0.24 scale} def\n");
   fprintf (fp, "/end_page {showpage} def\n");
   fprintf (fp, "/end_file {saved_state restore} def\n");
   fprintf (fp, "/sh {show} def\n");
   fprintf (fp, "/mt {moveto} def\n");
   fprintf (fp, "/lt {lineto} def\n");
   fprintf (fp, "/sa {save} def\n");
   fprintf (fp, "/re {restore} def\n");
   fprintf (fp, "/ro {rotate} def\n");
   fprintf (fp, "/sl {setlinewidth} def\n");
   fprintf (fp, "/np {newpath} def\n");
   fprintf (fp, "/ff {findfont} def\n");
   fprintf (fp, "/sf {scalefont} def\n");
   fprintf (fp, "/sk {stroke} def\n");
   fprintf (fp, "/bd{bind def}bind def\n");
   fprintf (fp, "/ld{load def}bd\n");
   fprintf (fp, "/:F/setrgbcolor ld\n");
   fprintf (fp, "start_file\n");
   fprintf (fp, "start_page\n");
   in_page = 1;
   return(f);
}

void set_border(int x, int y)
{
   xb=x; yb=y;
}

void draw_box(int x1, int y1, int x2, int y2, int fill)
{
   x1 = (magnification * (x1 - offsetx) + 0.5);
   x2 = (magnification * (x2 - offsetx) + 0.5);
   y1 = (magnification * (y1 - offsety) + 0.5);
   y2 = (magnification * (y2 - offsety) + 0.5);
   /*
   if (x1  < 0 && x2 < 0 || y1 < 0 && y2 < 0 ||
         x1 > 2550 && x2 > 2550 || y1 > 3300 && y2 > 3300)
      return;
   */
   if (fill == 0 || fill > 1)
      setpen (1);
   set_texture (0, fill);
   newpath ();
   moveto (y1+yb, x1+xb);
   lineto (y1+yb, x2+xb);
   lineto (y2+yb, x2+xb);
   lineto (y2+yb, x1+xb);
   lineto (y1+yb, x1+xb);
   closepath ();
   if (fill > 0)
      fill_path ();
   else
      draw_path ();
   /*
   if (fill > 1)
   {
      savemag = magnification;
      savex = offsetx;
      savey = offsety;
      magnification = 1.0;
      offsetx = 0.0;
      offsety = 0.0;
      draw_box(x1,y1,x2,y2,0);
      magnification = savemag;
      offsetx = savex;
      offsety = savey;
   }
   */
}

int set_texture (int fam, int mem)
{
   static int smem=-1;
   static int sfam=0;

   if (mem == smem)
     return smem;
   smem = mem;
   if (sfam != fam)
      sfam = fam;
   texture_fill = (mem > 0);
   mem = abs(mem);
   if (! colorlistend)
   {
      for (colorlistend = 0; colorlist[colorlistend].r >= 0;
            colorlistend++) colorlistend++;
   }
   if (docolor)
   {
      if (mem & 0xf000000)
      {
         int i;
         double r,g,b;

         r = (abs(mem) & 0xff0000) >> 16;
         g = (abs(mem) & 0xff00) >> 8;
         b = (abs(mem) & 0xff);

         for (i = 0; i < colorlistend &&
            (r != colorlist[i].r || g != colorlist[i].g || b != colorlist[i].b);
               i++)
            ;
         if ((unsigned) i >= sizeof (colorlist)/sizeof(struct rgb))
         {
            fprintf (stderr, "Colorlist too large\n");
            i = colorlistend-1;
         }
         else if (i == colorlistend)
         {
            colorlist[i].r = r;
            colorlist[i].g = g;
            colorlist[i].b = b;
            colorlistend = i+1;
         }
         fprintf (fp, "%.2f %.2f %.2f :F\n",
            colorlist[i].r/255.0, colorlist[i].g/255.0, colorlist[i].b/255.0);
         if (smem < 0)
            smem = mem = -i;
         else
            smem = mem = i;
      }
      else if (mem && mem < colorlistend)
         fprintf (fp, "%.2f %.2f %.2f :F\n",
            colorlist[mem].r/255.0, colorlist[mem].g/255.0, colorlist[mem].b/255.0);
      else
         fprintf (fp, "0 setgray\n");
   }
   else
   {
      switch (mem)
      {
      case 1:
         fprintf (fp, "0 setgray\n");
         break;
      case 2:
         fprintf (fp, "0.9843 setgray\n");
         break;
      case 3:
         fprintf (fp, "0.9375 setgray\n");
         break;
      case 4:
         fprintf (fp, "0.4 setgray\n");
         break;
      default:
         fprintf (fp, "0.18 setgray\n");
         break;
      }
   }
   return smem;
}

void draw_line(int x1, int y1, int x2, int y2, int pen)
{
   x1 = (magnification * (x1 - offsetx) + 0.5);
   x2 = (magnification * (x2 - offsetx) + 0.5);
   y1 = (magnification * (y1 - offsety) + 0.5);
   y2 = (magnification * (y2 - offsety) + 0.5);
   /*
   if (x1  < 0 && x2 < 0 || y1 < 0 && y2 < 0 ||
         x1 > 2550 && x2 > 2550 || y1 > 3300 && y2 > 3300)
      return;
   */
   setpen(pen);
   newpath ();
   moveto (y1+yb, x1+xb);
   lineto (y2+yb, x2+xb);
   draw_path();
}

void draw_point(int x, int y, int pen)
{
   x = (magnification * (x - offsetx) + 0.5);
   y = (magnification * (y - offsety) + 0.5);
   /*
   if (x  < 0 || y < 0 || x > 2550 || y > 3300)
      return;
   */
   setpen(pen);
   moveto (y+yb, x+xb);
   draw_path ();
}

void set_family(int f)
{
   static int font = -1;
   static char *n[] = {
      "Courier",
      "Courier",
      "Courier",
      "Courier",
      "Helvetica",
      "Courier-Bold",
      "Courier-Bold",
      NULL};
   static int s[] = { 7, 8, 10, 12, 20, 12, 14, 0};
   if (f < 1 || f > 7)
      return;
   if (f == font)
      return;
   font = f;
   spt;
   fprintf (fp, "/%s ff\n", n[f-1]);
   fprintf (fp, "%.1lf sf\n", s[f-1] / 0.24);
   fprintf (fp, "setfont\n");
}

/*
void smove(int d)
{
  spt;
  pb(134); pw(d);
}

void mmove(int d)
{
  spt;
  pb(133); pw(d);
}
*/

void crlf(void)
{
   spt;
   fprintf (fp, "(\\n)\n");
}

static void str(char *s)
{
   static int dir[4] = {180, 90, 0, 270};
   spt;
   if (dir[font_main & 3])
   {
      fprintf (fp, "sa %d ro\n", dir[font_main & 3]);
   }
   fputc ('(', fp);
   while (*s)
   {
      if (*s == '(' || *s == ')')
         fputc ('\\', fp);
      fputc (*s, fp);
      s++;
   }
   fputs (") sh\n", fp);
   if (dir[font_main & 3])
   {
      fprintf (fp, "re\n");
   }
}

void text(int x, int y, char *s, int f)
{
   static int style= (-1);
   x -= yb;
   y -= xb;
   x = (magnification * (x - offsety) + 0.5);
   y = (magnification * (y - offsetx) + 0.5);
   x += yb;
   y += xb;
   if(f<0)
   {
      style=(-1);
      return;
   }
   if(!*s) return;
   if(style!=f)
   {
      set_family(f);
      style=f;
   }
   set_texture (0, 0);
   moveto(x,y);
   str(s);
}

void polygon(unsigned cnt, int *x, int *y, int fill, int line)
{
   static long *xx, *yy;
   static unsigned save_cnt;
   int ncnt, i;
   if (cnt == 0)
      return;
   for (i = 0; (unsigned) i < cnt; i++)
   {
      x[i] = (magnification * (x[i] - offsetx) + 0.5);
      y[i] = (magnification * (y[i] - offsety) + 0.5);
   }
   if (save_cnt < cnt)
   {
      if (xx)
         free ((char *)xx);
      if (yy)
         free ((char *)yy);
      xx = (long *)malloc (sizeof (long) * cnt);
      yy = (long *)malloc (sizeof (long) * cnt);
      save_cnt = cnt;
   }
   for (ncnt = i = 0; i < 2; i++, ncnt++)
   {
      xx[ncnt] = x[i];
      yy[ncnt] = y[i];
   }
   for (; (unsigned) i < cnt; i++, ncnt++)
   {
      xx[ncnt] = x[i];
      yy[ncnt] = y[i];
      if ((xx[ncnt] - xx[ncnt - 2]) * (yy[ncnt - 1] - yy[ncnt - 2]) ==
            (yy[ncnt] - yy[ncnt - 2]) * (xx[ncnt - 1] - xx[ncnt - 2]) &&
         (xx[ncnt] != xx[ncnt - 2]) && (xx[ncnt - 1] != xx[ncnt - 2]))
      {
         xx[ncnt - 1] = xx[ncnt];
         yy[ncnt - 1] = yy[ncnt];
         ncnt--;
      }
   }
   if (fill <= 0)
      setpen(line ? line : 2);
   else
      setpen (1);
   set_texture (0, fill);
   moveto ((int)yy[0] + yb, (int)xx[0] + xb);
   for (i = 1;i < ncnt; i++)
      lineto ((int)yy[i] + yb, (int)xx[i] + xb);
   if (fill>0)
      fill_path();
   else
      draw_path();
}

void set_adv_dirs(int main, int secondary)
{
   /* main = 0:0, 1:90, 2:180, 3:270; secon = 0 normal, 1 revers */
   font_main = main;
   font_seco = secondary;
}

void draw_circle(int x, int y, int r, int pen)
{
   x = (magnification * (x - offsetx) + 0.5);
   y = (magnification * (y - offsety) + 0.5);
   r = (magnification * r + 0.5);
   /*
   if (x + r  < 0 || y + r < 0 || x - r > 2550 || y - r > 3300)
      return;
   */
   spt;
   setpen(pen);
   newpath ();
   fprintf (fp, "%d %d %d 0 360 arc\n", 2550 - (y+yb), x+xb, r);
   draw_path ();
}

void circ_arc (int r, int x, int y, int a1, int a2, int pen)
{
   int at;
/* angles are in units of 2^14 per circle */
   x = (magnification * (x - offsetx) + 0.5);
   y = (magnification * (y - offsety) + 0.5);
   r = (magnification * r + 0.5);
   /*
   if (x + r  < 0 || y + r < 0 || x - r > 2550 || y - r > 3300)
      return;
   */
   spt;
   setpen (pen);
   newpath ();
   /*
   fprintf (fp, "%d %d %d %.3lf %.3lf arc\n",
      2550 - (y+yb), x+xb, r, 180.0-a1/45.51111, 180.0-a2/45.5111);
   */
   if (a1 < a2)
   {
      at = a1;
      a1 = a2;
      a2 = at;
   }
   fprintf (fp, "%d %d %d %.3lf %.3lf arc\n",
      2550 - (y+yb), x+xb, r, 180.0-a1/45.51111, 180.0-a2/45.5111);
   draw_path ();
}

/* these are dummy functions only used in xdraw.c */
void force_portrait (int val)
{
    val = val;
}

void force_scale (int val)
{
    val = val;
}
