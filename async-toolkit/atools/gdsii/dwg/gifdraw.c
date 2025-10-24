// $Id$ AAG

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <memory.h>
#include <string.h>
#include "btutil.h"
#include "imag.h"
#include "afont.h"

void writegiffile (FILE *fp, int cols, int rows);

#define max(x,y) ((x)>(y)?(x):(y))
#define min(x,y) ((x)<(y)?(x):(y))

extern FONT courR08;
extern FONT courR10;
extern FONT courR12;
extern FONT courR14;
extern FONT courB14;
extern FONT courB10;
extern FONT courB12;

//#ifndef DRAW2X
//#define SIZEX    1320
//#define SIZEY    510
//#else /* DRAW2X */
//#define SIZEX    1320
//#define SIZEY    1020
//#endif /* DRAW2X */

#define SIZEX      2048
#define SIZEY      2048

static int initialized = 0;
static int sizex, sizey;
static int cursor_x, cursor_y;
static int portrait = 0;
static int optport = 0;
static int global_rows, global_cols;

static int in_page = 0;
static FILE *fp;
static int xb=60,yb=60;
static int font_main = 0;
static int font_seco = 0;
#ifndef DRAW2X
static double magnification = 1.0;
#else /* DRAW2X */
static double magnification = 2.0;
#endif /* DRAW2X */
static double offsetx       = 0.0;
static double offsety       = 0.0;
static int    docolor = 1;
static int    texture_fill;

typedef struct rgb RGB;

struct rgb {
   unsigned char r;
   unsigned char g;
   unsigned char b;
};
 
static struct rgb rgb[16] = {
   {255, 255, 255}, // white
   {  0,   0,   0}, // black
   {208, 208, 208}, // gray
   {160, 208, 224}, // light blue
   {192, 192, 192}, // medium gray
   {255, 255,   0}, // yellow
   {255,   0,   0}, // red
   {  0,   0, 192}, // dark blue
   { 96, 127,  30}, // olive
   {  0, 255,   0}, // green
   {113, 255, 208}, // cyan
   {160,  30, 255}, // purple
   {255, 192, 192}, // flesh
   {224, 127, 224}, // light magenta
   {255, 160,   0}, // orange
   {255, 255, 255}, //
};

static RGB current_color = {0, 0, 0};

RGB **map;

static RGB makergb (int value)
{
   RGB rgb;

   rgb.r = (value >> 16)&0xff;
   rgb.g = (value >> 8 ) & 0xff;
   rgb.b = value & 0xff;
//   fprintf (stderr, "#%02x%02x%02x\n", rgb.r, rgb.g, rgb.b);
   return rgb;
}
static void resize (int *newx, int *newy, int border)
{
   int minx, miny, maxx, maxy;
   int x, y;
   RGB **newmap, **tmp;

   minx = sizex - 1;
   miny = sizey - 1;
   maxx = 0;
   maxy = 0;
   for (x = 0; x < sizex; x++)
   for (y = 0; y < sizey; y++)
   {
      if ((x < minx || y < miny || x > maxx || y > maxy) &&
	    (map[y][x].r != 0xff || map[y][x].g != 0xff || map[y][x].b != 0xff))
      {
	 if (x < minx)
	    minx = x;
	 if (y < miny)
	    miny = y;
	 if (x > maxx)
	    maxx = x;
	 if (y > maxy)
	    maxy = y;
      }
   }
   minx -= border;
   miny -= border;
   maxx += border;
   maxy += border;
   if (minx < 0)
      minx = 0;
   if (miny < 0)
      miny = 0;
   if (maxx > sizex)
      maxx = sizex;
   if (maxy > sizey)
      maxy = sizey;
   *newx = maxx - minx + 1;
   *newy = maxy - miny + 1;
   if (sizex == *newx && sizey == *newy)
      return;
   if (sizex == *newx)
      newmap = map;
   else
   {
      if (!(newmap = calloc (*newy, sizeof (char *))))
      {
	 fprintf (stderr, "Out of memory in %s line %d\n", __FILE__, __LINE__);
	 exit (1);
      }
      newmap[0] = map[0];
      for (y = 1; y < *newy; y++)
	 newmap[y] = newmap[0] + y * (*newx);
   }
   for (y = 0; y < *newy; y++)
      memcpy (newmap[y], map[y+miny]+minx, (*newx) * sizeof (RGB));
   if (sizex != *newx)
   {
      tmp = map;
      map = newmap;
      free (tmp);
   }
}

static void initmap (int cols, int rows)
{
   int i;

   if (portrait)
   {
      i = cols;
      cols = rows;
      rows = i;
   }
   global_rows = rows;
   global_cols = cols;
   //fprintf (stderr, "initmap %d,%d\n", cols, rows);
   if (initialized)
   {
      if (sizex == cols && sizey == rows)
	 return;
      free (map[0]);
      free (map);
      initialized = 0;
   }
   if (!(map = calloc (rows, sizeof (char *))))
   {
      fprintf (stderr, "Out of memory in %s line %d\n", __FILE__, __LINE__);
      exit (1);
   }
   map[0] = malloc (rows * cols * sizeof (RGB));
   if (!map[0])
   {
      fprintf (stderr, "Out of memory in %s line %d\n", __FILE__, __LINE__);
      exit (1);
   }
   for (i = 1; i < rows; i++)
   {
      map[i] = map[0] + (i * cols);
   }
   memset (map[0], 0xff, rows * cols * sizeof (RGB));
   sizex = cols;
   sizey = rows;
   initialized = 1;
}

static void writemap (RGB color, int x, int y)
{
   if (!initialized)
   {
      fprintf (stderr, "Cannot write, not initialized.\n");
      exit (1);
   }
   if (y >= 0 && y < sizey && x >= 0 && x < sizex)
   {
      map[y][x] = color;
   }
}

static int writecharv (RGB color, int x, int y, int c, FONT *f)
{
   int i, j;

   if (c < 0 || c > 127 || c == ' ')
      return y-f->cmap[0x20].dwidth;
   for (j = 0; j <= f -> boxy; j++)
   {
      for (i = 0; i < f -> boxx; i++)
      {
         if ((f -> cmap[c].map[j] >> (31 - i)) & 1)
         {
            writemap (color, x-f->boxy*0.1+f->descent+j, y-i);
         }
      }
   }
   return y - f->cmap[c].dwidth;
}

static int writecharh (RGB color, int x, int y, int c, FONT *f)
{
   int i, j;

   if (c < 0 || c > 127)
      return x + f -> cmap[0x20].dwidth;
   for (j = 0; j <= f -> boxy; j++)
   {
      for (i = 0; i < f -> boxx; i++)
      {
         if ((f -> cmap[c].map[j] >> (31 - i)) & 1)
         {
            writemap (color, x+i, y-f->boxy+f->descent+j);
         }
      }
   }
   return x + f->cmap[c].dwidth;
}

static int writestring (RGB color, int x, int y, char *s, FONT *f)
{
   //fprintf (stderr, "writestring %d,%d '%s'\n", x, y, s);
   if ((!portrait && font_main) || (portrait && !font_main))
   {
      for (; *s; s++)
	 x = writecharh (color, x, y, *s, f);
   }
   else
   {
      x -= f->cmap['w'].dwidth;
      for (; *s; s++)
	 y = writecharv (color, x, y, *s, f);
   }
   return x;
}

static void writeline (RGB color, int x1, int y1, int x2, int y2)
{
   int x, y, lim, j, n;

   // always draw from left to right
   if (x1 > x2)
   {
      x = x1;
      x1 = x2;
      x2 = x;
      y = y1;
      y1 = y2;
      y2 = y;
   }
   else if (x1 == x2 && y1 > y2)
   {
      y = y1;
      y1 = y2;
      y2 = y;
   }
   if (x1 == x2) // Vertical Line
   {
      for (; y1 <= y2; y1++)
	 writemap (color, x1, y1);
   }
   else if (y1 == y2) // Horizontal Line
   {
      for (; x1 <= x2; x1++)
	 writemap (color, x1, y1);
   }
   else if (y1 <= y2) // going up (quadrant 1)
   {
      if (y2 - y1 >= x2 - x1) // >= 45 degrees, mul y pts per x pt
      {
	 lim = (y2 - y1) / (x2 - x1);
	 for (n = 0, j = 0; n <= (x2 - x1); n++)
	 {
	    for (; j < lim; j++)
	    {
	       writemap (color, x1 + n, y1 + j);
	    }
	    lim = ((n+1)*(y2-y1))/(x2-x1);
	 }
	 writemap (color, x2, y2);
      }
      else // < 45 degrees mul x pts per y pt
      {
	 for (n = 0; n <= (x2 - x1); n++)
	 {
	   writemap (color, x1 + n, y1 + (n * (y2 - y1))/(x2 - x1));
	 }
      }
   }
   else // down y1 > y2
   {
      if ((y1 - y2) >= (x2 - x1)) // >= 45 deg down
      {
	 lim = (y1 - y2) / (x2 - x1);
	 for (n = 0, j = 0; n <= (x2 - x1); n++)
	 {
	    for (; j < lim; j++)
	    {
	       writemap (color, x1 + n, y1 - j);
	    }
	    y = y1 - lim;
	    lim = ((n+1)*(y1-y2))/(x2-x1);
	 }
	 writemap (color, x2, y2);
      }
      else
      {
	 for (n = 0; n <= (x2 - x1); n++)
	 {
	   writemap (color, x1 + n, y1 - (n * (y1 - y2))/(x2 - x1));
	 }
      }
   }
}

#if 0
static void mwritemap (int x, int y)
{
   if (y >= 0 && y < sizey && x >= 0 && x < sizex)
   {
      map[y][x].r++;
   }
}

static void mwriteline (int x1, int y1, int x2, int y2)
{
   int x, y, lim, j, n;

   // always draw from left to right
   if (x1 > x2)
   {
      x = x1;
      x1 = x2;
      x2 = x;
      y = y1;
      y1 = y2;
      y2 = y;
   }
   else if (x1 == x2 && y1 > y2)
   {
      y = y1;
      y1 = y2;
      y2 = y;
   }
   if (x1 == x2) // Vertical Line
   {
      for (; y1 <= y2; y1++)
	 mwritemap (x1, y1);
   }
   else if (y1 == y2) // Horizontal Line
   {
      for (; x1 <= x2; x1++)
	 mwritemap (x1, y1);
   }
   else if (y1 <= y2) // going up (quadrant 1)
   {
      if (y2 - y1 >= x2 - x1) // >= 45 degrees, mul y pts per x pt
      {
	 lim = (y2 - y1) / (x2 - x1);
	 for (n = 0, j = 0; n <= (x2 - x1); n++)
	 {
	    for (; j < lim; j++)
	    {
	       mwritemap (x1 + n, y1 + j);
	    }
	    lim = ((n+1)*(y2-y1))/(x2-x1);
	 }
	 mwritemap (x2, y2);
      }
      else // < 45 degrees mul x pts per y pt
      {
	 for (n = 0; n <= (x2 - x1); n++)
	 {
	   mwritemap (x1 + n, y1 + (n * (y2 - y1))/(x2 - x1));
	 }
      }
   }
   else // down y1 > y2
   {
      if ((y1 - y2) >= (x2 - x1)) // >= 45 deg down
      {
	 lim = (y1 - y2) / (x2 - x1);
	 for (n = 0, j = 0; n <= (x2 - x1); n++)
	 {
	    for (; j < lim; j++)
	    {
	       mwritemap (x1 + n, y1 - j);
	    }
	    y = y1 - lim;
	    lim = ((n+1)*(y1-y2))/(x2-x1);
	 }
	 mwritemap (x2, y2);
      }
      else
      {
	 for (n = 0; n <= (x2 - x1); n++)
	 {
	   mwritemap (x1 + n, y1 - (n * (y1 - y2))/(x2 - x1));
	 }
      }
   }
}
#endif

static void writerect (RGB color, int x1, int y1, int x2, int y2, int fill)
{
   int x, y;

   if (x1 > x2)
   {
      x = x1;
      x1 = x2;
      x2 = x;
   }
   if (y1 > y2)
   {
      y = y1;
      y1 = y2;
      y2 = y;
   }
   if (fill)
   {
      if (x1 < 0)
	 x1 = 0;
      if (x2 >= sizex)
	 x2 = sizex - 1;
      if (y1 < 0)
	 y1 = 0;
      if (y2 > sizey - 1)
	 y2 = sizey - 1;
      for (x = x1; x <= x2; x++)
      {
	 for (y = y1; y <= y2; y++)
	 {
	    map[y][x] = color;
	 }
      }
   }
   else
   {
      writeline (color, x1, y1, x1, y2);
      writeline (color, x1, y2, x2, y2);
      writeline (color, x2, y2, x2, y1);
      writeline (color, x2, y1, x1, y1);
   }
}

static void writecircle (RGB color, int xc, int yc, int r, int fill)
{
   int x, y, rsq;

   rsq = r*r;
   if (fill)
   {
      for (x = r, y = 0; y <= x; y++)
      {
	 while (x*x > rsq - y*y)
	    x--;
	 if (x < y)
	    break;
	 writeline (color, xc + x, yc + y, xc - x, yc + y);
	 writeline (color, xc + x, yc - y, xc - x, yc - y);
	 writeline (color, xc + y, yc + x, xc - y, yc + x);
	 writeline (color, xc + y, yc - x, xc - y, yc - x);
      }
   }
   else
   {
      // write the four corners
      writemap (color, xc + r, yc);
      writemap (color, xc - r, yc);
      writemap (color, xc, yc + r);
      writemap (color, xc, yc - r);
      // write one octant and replicate around the circle
      for (x = r, y = 1; y < x; y++)
      {
	 while (x*x > rsq - y*y)
	    x--;
	 if (x < y)
	    break;
	 writemap (color, xc + x, yc + y);
	 writemap (color, xc + x, yc - y);
	 writemap (color, xc - x, yc + y);
	 writemap (color, xc - x, yc - y);
	 writemap (color, xc + y, yc + x);
	 writemap (color, xc + y, yc - x);
	 writemap (color, xc - y, yc + x);
	 writemap (color, xc - y, yc - x);
      }
   }
}

static void writearc (RGB color, int xc, int yc, int r, int a1, int a2, int fill)
{
   int x, y, rsq, a;

   if (abs(a1-a2)==16384)
   {
      writecircle (color, xc, yc, r, fill);
      return;
   }
   rsq = r*r;
   if (fill)
   {
      for (x = r, y = 0; y <= x; y++)
      {
	 while (x*x > rsq - y*y)
	    x--;
	 if (x < y)
	    break;
	 writeline (color, xc + x, yc + y, xc - x, yc + y);
	 writeline (color, xc + x, yc - y, xc - x, yc - y);
	 writeline (color, xc + y, yc + x, xc - y, yc + x);
	 writeline (color, xc + y, yc - x, xc - y, yc - x);
      }
   }
   else
   {
      // write the four corners
      if (a1 >= 4096 && a2 <= 4096)
      {
	 //writemap (color, xc + r, yc);
      }
      if (a1 >= 12288 && a2 <= 12288)
      {
	 //writemap (color, xc - r, yc);
      }
      if (a1 == 16384 || a2 == 0)
      {
	 //writemap (color, xc, yc + r);
      }
      if (a1 >= 8192 && a2 <= 8192)
      {
	 //writemap (color, xc, yc - r);
      }
      // write one octant and replicate around the circle
      for (x = r, y = 0; y < x; y++)
      {
	 while (x*x > rsq - y*y)
	    x--;
	 a = 4096 * 2 / 3.14159 * atan2 ((double)y, (double)x);
	 if (x < y)
	 {
	    break;
	 }
	 //1
	 if (a1 >= 4096 + a && a2 <= 4096 + a)
	    writemap (color, xc + x, yc + y);
	 //2
	 if (a1 >= 4096 - a && a2 <= 4096 - a)
	    writemap (color, xc + x, yc - y);
	 //3
	 if (a1 >= 12888 - a && a2 <= 12288 - a)
	    writemap (color, xc - x, yc + y);
	 //4
	 if (a1 >= 12288 + a && a2 <= 12288 + a)
	    writemap (color, xc - x, yc - y);
	 //5
	 if (a1 >= 8192 - a && a2 <= 8192 - a)
	    writemap (color, xc + y, yc + x);
	 //6
	 if (a1 >= 0 + a && a2 <= 0 + a)
	    writemap (color, xc + y, yc - x);
	 //7
	 if (a1 >= 8192 + a && a2 <= 8192 + a)
	    writemap (color, xc - y, yc + x);
	 //8
	 if (a1 >= 16384 - a && a2 <= 16384 - a)
	    writemap (color, xc - y, yc - x);
      }
   }
}

void writeppmfile (FILE *fppm)
{
   if (!initialized)
   {
      fprintf (stderr, "Cannot write uninitialized map.\n");
      return;
   }
   fprintf (fppm, "P6\n%d %d\n255\n", sizex, sizey);
   fwrite (map[0], 1, sizex * sizey * sizeof (RGB), fppm);
}

#define spt { if (! in_page) {initmap (SIZEX,SIZEY);  in_page=1;}}

static void convert_text_coordinates (int *x, int *y)
{
   int x1, y1;

   *x -= 5*xb;
   *y -= 5*yb;
   *x = (magnification * ((*x) - offsetx) + 0.5) / 5 + xb;
   *y = (magnification * ((*y) - offsety) + 0.5) / 5 + yb;
   if (! portrait)
   {
      x1 = *y;
      y1 = sizey-*x;
      *x = x1;
      *y = y1;
   }
}

static void convert_coordinates (int *x, int *y)
{
   int x1, y1;

   if (portrait)
   {
      x1 = (magnification * ((*x) - offsetx) + 0.5) / 5 + xb;
      y1 = (magnification * ((*y) - offsety) + 0.5) / 5 + yb;
   }
   else
   {
      y1 = (magnification * ((*x) - offsetx) + 0.5) / 5 + xb;
      x1 = sizey-(magnification * ((*y) - offsety) + 0.5) / 5 - yb;
   }
   /*
   fprintf (stderr, "convert %d,%d (%.3le,%.3le,%.3le) %d,%d\n",
      *x, *y, magnification, offsetx, offsety, x1, y1);
   */
   *x = x1;
   *y = y1;
}

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
#ifndef DRAW2X
      magnification = mag;
#else /* DRAW2X */
      magnification = mag*2;
#endif /* DRAW2X */
}

void dwg_setoffset(double valx, double valy)
{
   /* val in inches, offset in pixels */
   offsetx = valx * 60;
   offsety = valy * 60;
}

int printer_type (void)
{
   return (3);
}

static void moveto(int x, int y)
{
   cursor_x = x;
   cursor_y = y;
}

static void lineto(int x, int y)
{
   writeline (current_color, cursor_x, cursor_y, x, y);
   cursor_x = x;
   cursor_y = y;
}

void end_page(void)
{
   if (in_page)
   {
      int x, y;
      x = sizex;
      y = sizey;
      resize (&x, &y, 2);
      sizex = x;
      sizey = y;
      writegiffile (fp, x, y);
      in_page=0;
   }
}

// This needs fixing
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
      p=i;
      i = (magnification * i + 0.5);
      if (i <= 0)
         i = 1;
   }
}

void end_file(void)
{
   end_page ();
   fclose(fp);
   fp=NULL;
   setpen(-1);
   text(0,0,(char *)NULL,(-1));
}

FILE *set_file(FILE *f)
{
   fp=f;
   spt;
   return(f);
}

void set_border(int x, int y)
{
   xb=x/5; yb=y/5;
}

void draw_box(int x1, int y1, int x2, int y2, int fill)
{
   convert_coordinates (&x1, &y1);
   convert_coordinates (&x2, &y2);
   if (fill != 1)
      setpen (1);
   set_texture (0, fill);
   writerect (current_color, y1, x1, y2, x2, fill>0);
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
   if (mem && (unsigned) abs(mem) < sizeof (rgb)/sizeof (RGB))
   {
      current_color = rgb[abs(mem)];
   }
   else if (abs (mem) >= 16)
   {
      current_color = makergb (abs(mem));
   }
   else
   {
      current_color = rgb[1];
   }
   texture_fill = smem > 0;
   return smem;
}

void draw_line(int x1, int y1, int x2, int y2, int pen)
{
   convert_coordinates (&x1, &y1);
   convert_coordinates (&x2, &y2);
   /*
   if (x1  < 0 && x2 < 0 || y1 < 0 && y2 < 0 ||
         x1 > sizex && x2 > sizex || y1 > sizey && y2 > sizey)
      return;
   */
   setpen(pen);
   moveto (y1, x1);
   lineto (y2, x2);
}

void draw_point(int x, int y, int pen)
{
   convert_coordinates (&x, &y);
   if (x  < 0 || y < 0 || x > sizex || y > sizey)
      return;
   setpen(pen);
   moveto (y, x);
   lineto (y, x);
}

static FONT *current_font;

void set_family(int f)
{
   static int font = -1;
   static FONT *fontlist[] = {
      &courR08,
      &courR10,
      &courR12,
      &courR14,
      &courB14,
      &courB10,
      &courB12,
      NULL,
   };

   if (f < 1 || f > 7)
      return;
   if (f == font)
      return;
   font = f;
   current_font = fontlist[f-1];
}

/*
void smove(int d)
{
  pb(134); pw(d);
}

void mmove(int d)
{
  pb(133); pw(d);
}
*/

void crlf(void)
{
}

void text(int x, int y, char *s, int f)
{
   static int style= (-1);

   convert_text_coordinates (&x, &y);
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
   writestring (rgb[1], x, y, s, current_font);
}

int intcmp (const int *i1, const int *i2)
{
   return *i1 - *i2;
}

int dcmp (const double *i1, const double *i2)
{
    if (*i1 > *i2)
      return 1;
   if (*i1 < *i2)
      return -1;
   return 0;
}

void polygon(unsigned cnt, int *x, int *y, int fill, int line)
{
   static long *xx, *yy;
   static unsigned save_cnt;
   int ncnt, i, j;
   int minx, maxx, miny, maxy;

   if (cnt == 0)
      return;
   for (i = 0; (unsigned) i < cnt; i++)
   {
      convert_coordinates (&x[i], &y[i]);
   }
   if (save_cnt < cnt)
   {
      if (xx)
         free ((char *)xx);
      if (yy)
         free ((char *)yy);
      xx = (long *)malloc (sizeof (long) * (cnt+1));
      yy = (long *)malloc (sizeof (long) * (cnt+1));
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
   if (fill == 0)
      setpen(line ? line : 2);
   else
      setpen (1);
   set_texture (0, fill);
   // draw outline first
   moveto ((int)yy[0], (int)xx[0]);
   for (i = 1;i < ncnt; i++)
      lineto ((int)yy[i], (int)xx[i]);
   if (fill > 0)
   {
      BTTREE bty, bti;
      int   *ip;

      /* close the polygon */
      lineto ((int)yy[0], xx[0]);
      btinit (&bty, sizeof (int), sizeof (int), intcmp);
      btinit (&bti, sizeof (double), sizeof (int), dcmp);
      xx[ncnt] = xx[0];
      yy[ncnt] = yy[0];
      // find the min and max values of x and y to minimize range
      minx = maxx = yy[0];
      miny = maxy = xx[0];
      for (j = 1; j < ncnt; j++)
      {
	 minx = min(yy[j],minx);
	 maxx = max(yy[j],maxx);
	 miny = min(xx[j],miny);
	 maxy = max(xx[j],maxy);
	 // make list of y values
	 btins (&xx[j], &bty);
      }
      for (j = miny; j < maxy; j++)
      {
	 double x, xc, yc;
	 int    n;
	 int    lcnt;

	 yc = j;
	 // if on a y point, offset it slightly
	 if (btloc (&j, &bty))
	    yc += 0.000001;
	 // find all of the intercepts
	 for (n = 0; n < ncnt; n++)
	 {
	    if (max (xx[n], xx[n+1]) >= yc && min (xx[n], xx[n+1]) <= yc)
	    {
	       if (xx[n+1] != xx[n])
		  x = (yc - xx[n])*(yy[n+1]-yy[n])/
		       ((double)(xx[n+1]-xx[n])) + yy[n];
	       else
		  x = (yy[n] + yy[n+1])/2.0;
	       if (x < minx)
		  x = minx - 0.001;
	       if (x <= maxx+0.001)
		  btins (&x, &bti);
	       if ((ip = btloc (&x, &bti)))
		  (*ip)++;
	    }
	 }
	 for (i = minx; i < maxx; i++)
	 {
	    xc = i;
	    lcnt = 0;
	    x = minx - 1;
	    btselect (&x, &xc, &bti);
	    while (btget (&x, (void **) &ip, &bti))
	    {
	       lcnt += *ip;
	       // on the line should be drawn.
	       if (x == xc)
	       {
		  lcnt = 1;
		  break;
	       }
	    }
	    /* if the number of lines to the left is odd, then
	       this is inside of the polygon!
	    */
	    if (lcnt % 2)
	    {
	       writemap (current_color, i, j);
	    }
	 }
	 btfree (&bti);
      }
      btfree (&bty);
      btfree (&bti);
   }
   set_texture (0, 0);
}

void set_adv_dirs(int main, int secondary)
{
   /* main = 0:0, 1:90, 2:180, 3:270; secon = 0 normal, 1 revers */
   //fprintf (stderr, "set_adv_dirs %d %d\n", main, secondary);
   if ( !optport )
   {
      if (main == 1)
	 portrait = 0;
      else
	 portrait = 1;
      //fprintf (stderr, "portrait=%d\n", portrait);
      optport = 1;
      initmap (SIZEX, SIZEY);
   }
   font_main = main;
   font_seco = secondary;
}

void draw_circle(int x, int y, int r, int pen)
{
//   fprintf (stderr, "Circle %d,%d %d ->", x, y, y);
   convert_coordinates (&x, &y);
   r = (magnification * r + 0.5)/5;
   if (r <= 0)
      r = 1;
   if (x + r  < 0 || y + r < 0 || x - r > sizex || y - r > sizey)
      return;
   setpen(pen);
   writecircle (texture_fill ? rgb[1] : current_color, y, x, r, 0);
}

// needs to be done
void circ_arc (int r, int x, int y, int a1, int a2, int pen)
{
   int at;
/* angles are in units of 2^14 per circle */
   convert_coordinates (&x, &y);
   r = (magnification * r + 0.5)/5;
   setpen (pen);
   if (a1 < a2)
   {
      at = a1;
      a1 = a2;
      a2 = at;
   }
   if (portrait)
   {
      a1 += 4096;
      a2 += 4096;
   }
   while (a1 >= 16384 && a2 >= 16384)
   {
      a1 -= 16384;
      a2 -= 16384;
   }
   if (a1 > 16384)
   {
      writearc (texture_fill ? rgb[1] : current_color, y, x, r, 16384, a2, 0);
      a1 -= 16384;
      a2 = 0;
   }
   writearc (texture_fill ? rgb[1] : current_color, y, x, r, a1, a2, 0);
}

void force_portrait (int val)
{
   optport = 1;
   portrait = val;
   //fprintf (stderr, "force_portrait=%d\n", val);
}

/* these are dummy functions only used in xdraw.c */
void force_scale (int val)
{
    val = val;
}
