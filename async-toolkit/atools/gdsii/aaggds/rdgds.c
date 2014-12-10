// "$Id$ AAG

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "c_lib.h"
#include "gdsutil.h"

extern GDS_LOOKUP lookup[];

char topcell[1024];
int  is_topcell;

double fgetreal8(FILE *);
void fgetstring (char *s, int len, FILE *fp);
static unsigned char lastbyte;

void usage (void)
{
   fprintf (stderr, "Usage: rdgds [-t topcell] gds_file > rdg_file\n");
   fprintf (stderr, "    rdgds parses a gds file to make a text file\n");
   fprintf (stderr, "    if topcell specified, only shows the single cell\n");
   exit (1);
}

void lenchk (int len, int shouldbe, char *name)
{
   if (len != shouldbe && shouldbe >= 0)
   {
      fprintf (stderr, "Length %d should be %d in %s\n",
	 len, shouldbe, name);
      return;
   }
}

/* functions */

void null_func (int ndx, int len, FILE *fp)
{
   while (len-- > 0)
      fgetc (fp);
   if (is_topcell)
      printf ("(%s)\n", lookup[ndx].name);
}

void strname_func (int ndx, int len, FILE *fp)
{
   char *s;

   s = calloc ((unsigned) len + 2, 1);
   fgetstring (s, len, fp);
   is_topcell = (!strcmp (s, topcell) || !*topcell);
   if (is_topcell)
      printf ("%s %s\n", lookup[ndx].name, s);
   free (s);
}

void sname_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s ", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
	    fputc (c, stdout);
      fputc ('\n', stdout);
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void bgnstr_func (int ndx, int len, FILE *fp)
{
   int d;

   is_topcell = 0 || !*topcell;
   if (is_topcell)
      printf ("%s", lookup[ndx].name);
   while (len > 0)
   {
      d = gds_fgetint2 (fp);
      if (is_topcell)
	 printf (" %d", d);
      len -= 2;
   }
   if (is_topcell)
      printf ("\n");
}

void endstr_func (int ndx, int len, FILE *fp)
{
   is_topcell = 0 || !*topcell;
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void header_func (int ndx, int len, FILE *fp)
{
   int ver;

   ver = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, ver);
}

void bgnlib_func (int ndx, int len, FILE *fp)
{
   int d;

   lenchk (len % 2, 0, lookup[ndx].name);
   if (is_topcell)
      printf ("%s", lookup[ndx].name);
   while (len > 0)
   {
      d = gds_fgetint2 (fp);
      if (is_topcell)
	 printf (" %d", d);
      len -= 2;
   }
   if (is_topcell)
      printf ("\n");
}

void libname_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s ", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
	    fputc (c, stdout);
      fputc ('\n', stdout);
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void units_func (int ndx, int len, FILE *fp)
{
   double u1, u2;
   unsigned char lb1, lb2;

   u1 = fgetreal8 (fp);
   lb1 = lastbyte;
   u2 = fgetreal8 (fp);
   lb2 = lastbyte;
   if (is_topcell)
      printf ("%s %le %le (%u %u)\n", lookup[ndx].name, u1, u2, lb1, lb2);
}

void endlib_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void endel_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void textnode_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void node_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void texttype_func (int ndx, int len, FILE *fp)
{
   int tt;

   tt = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, tt);
}

void presentation_func (int ndx, int len, FILE *fp)
{
   int p;

   p = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %X\n", lookup[ndx].name, p & 0xffff);
}

void string_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s '", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
         {
            if(isspace(c)) c=' ';
	    fputc (c, stdout);
         }
      printf ("'\n");
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void strans_func (int ndx, int len, FILE *fp)
{
   int s;

   s = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %X\n", lookup[ndx].name, s);
}

void mag_func (int ndx, int len, FILE *fp)
{
   double mag;
   unsigned char lb;

   mag = fgetreal8 (fp);
   lb = lastbyte;
   if (is_topcell)
      printf ("%s %lf (%u)\n", lookup[ndx].name, mag, lb);
}

void angle_func (int ndx, int len, FILE *fp)
{
   double angle;
   unsigned char lb;

   angle = fgetreal8 (fp);
   lb = lastbyte;
   if (is_topcell)
      printf ("%s %lf (%u)\n", lookup[ndx].name, angle, lb);
}

void boundary_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void path_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void sref_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void aref_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void text_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void layer_func (int ndx, int len, FILE *fp)
{
   int layer;

   layer = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, layer);
}

void datatype_func (int ndx, int len, FILE *fp)
{
   int dt;

   dt = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, dt);
}

void width_func (int ndx, int len, FILE *fp)
{
   long w, fgetint4 ();

   w = fgetint4 (fp);
   if (is_topcell)
      printf ("%s %ld\n", lookup[ndx].name, w);
}

void extn_func (int ndx, int len, FILE *fp)
{
   long x, fgetint4 ();

   lenchk (len, 4, lookup[ndx].name);
   x = fgetint4 (fp);
   if (is_topcell)
      printf ("%s %ld\n", lookup[ndx].name, x);
}

void xy_func (int ndx, int len, FILE *fp)
{
   long x, y, fgetint4 ();

   lenchk (len % 8, 0, lookup[ndx].name);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, len / 8);
   while (len > 0)
   {
      x = fgetint4 (fp);
      y = fgetint4 (fp);
      if (is_topcell)
	 printf ("%ld,%ld\n", x, y);
      len -= 8;
   }
}

void colrow_func (int ndx, int len, FILE *fp)
{
   int col, row;

   col = gds_fgetint2 (fp);
   row = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d %d\n", lookup[ndx].name, col, row);
}

void reflibs_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s ", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
	    fputc (c, stdout);
      fputc ('\n', stdout);
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void fonts_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s", lookup[ndx].name);
      while (len > 0)
      {
	 printf (" '");
	 do
	 {
	    if ((c = fgetc (fp)) > 0)
	       fputc (c, stdout);
	 } while ((--len) % 44);
	 printf ("'");
      }
      printf ("\n");
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void pathtype_func (int ndx, int len, FILE *fp)
{
   int pt;

   pt = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, pt);
}

void generations_func (int ndx, int len, FILE *fp)
{
   int g;

   g = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, g);
}

void attrtable_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s ", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
	    fputc (c, stdout);
      fputc ('\n', stdout);
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void elflags_func (int ndx, int len, FILE *fp)
{
   int flags;
   flags = gds_fgetint2 (fp) & 0xffff;
   if (is_topcell)
      printf ("%s %X\n", lookup[ndx].name, flags);
}

void elkey_func (int ndx, int len, FILE *fp)
{
   long key, fgetint4 ();

   key = fgetint4 (fp);
   if (is_topcell)
      printf ("%s %ld\n", lookup[ndx].name, key);
}

void nodetype_func (int ndx, int len, FILE *fp)
{
   int nt;

   nt = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, nt);
}

void propattr_func (int ndx, int len, FILE *fp)
{
   int pa;

   pa = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, pa);
}

void propvalue_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s ", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
	    fputc (c, stdout);
      fputc ('\n', stdout);
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void box_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

void boxtype_func (int ndx, int len, FILE *fp)
{
   int bt;

   bt = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, bt);
}

void plex_func (int ndx, int len, FILE *fp)
{
   long pl, fgetint4 ();

   pl = fgetint4 (fp);
   if (is_topcell)
      printf ("%s %ld\n", lookup[ndx].name, pl);
}

void format_func (int ndx, int len, FILE *fp)
{
   int fm;

   fm = gds_fgetint2 (fp);
   if (is_topcell)
      printf ("%s %d\n", lookup[ndx].name, fm);
}

void mask_func (int ndx, int len, FILE *fp)
{
   int c;

   if (is_topcell)
   {
      printf ("%s ", lookup[ndx].name);
      while (len-- > 0)
	 if ((c = fgetc (fp)) > 0)
	    fputc (c, stdout);
      fputc ('\n', stdout);
   }
   else
      while (len-- > 0)
	 fgetc (fp);
}

void endmasks_func (int ndx, int len, FILE *fp)
{
   if (is_topcell)
      printf ("%s\n", lookup[ndx].name);
}

/* lookup structure */
GDS_LOOKUP lookup[0x40] =
   {
      {"HEADER", 2, 0x0002, header_func},
      {"BGNLIB", 24, 0x0102, bgnlib_func},
      {"LIBNAME", -1, 0x0206, libname_func},
      {"UNITS", 16, 0x0305, units_func},
      {"ENDLIB", 0, 0x0400, endlib_func},
      {"BGNSTR", 24, 0x0502, bgnstr_func},
      {"STRNAME", -1, 0x0606, strname_func},
      {"ENDSTR", 0, 0x0700, endstr_func},
      {"BOUNDARY", 0, 0x0800, boundary_func},
      {"PATH", 0, 0x0900, path_func},
      {"SREF", 0, 0x0a00, sref_func},
      {"AREF", 0, 0x0b00, aref_func},
      {"TEXT", 0, 0x0c00, text_func},
      {"LAYER", 2, 0x0d02, layer_func},
      {"DATATYPE", 2, 0x0e02, datatype_func},
      {"WIDTH", 4, 0x0f03, width_func},
      {"XY", -1, 0x1003, xy_func},
      {"ENDEL", 0, 0x1100, endel_func},
      {"SNAME", -1, 0x1206, sname_func},
      {"COLROW", 4, 0x1302, colrow_func},
      {"TEXTNODE", 0, 0x1400, textnode_func},
      {"NODE", 0, 0x1500, node_func},
      {"TEXTTYPE", 2, 0x1602, texttype_func},
      {"PRESENTATION", 2, 0x1701, presentation_func},
      {"NONE", -1, 0, null_func},
      {"STRING", -1, 0x1906, string_func},
      {"STRANS", 2, 0x1a01, strans_func},
      {"MAG", 8, 0x1b05, mag_func},
      {"ANGLE", 8, 0x1c05, angle_func},
      {"NONE", -1, 0, null_func},
      {"NONE", -1, 0, null_func},
      {"REFLIBS", -1, 0x1f06, reflibs_func},
      {"FONTS", 176, 0x2006, fonts_func},
      {"PATHTYPE", 2, 0x2102, pathtype_func},
      {"GENERATIONS", 2, 0x2202, generations_func},
      {"ATTRTABLE", -1, 0x2306, attrtable_func},
      {"STYPTABLE", -1, 0x2406, null_func},
      {"STRTYPE", 2, 0x2502, null_func},
      {"ELFLAGS", 2, 0x2601, elflags_func},
      {"ELKEY", 4, 0x2703, elkey_func},
      {"LINKTYPE", 2, 0, null_func},
      {"LINKKEYS", 4, 0, null_func},
      {"NODETYPE", 2, 0x2a02, nodetype_func},
      {"PROPATTR", 2, 0x2b02, propattr_func},
      {"PROPVALUE", -1, 0x2c06, propvalue_func},
      {"BOX", 0, 0x2d00, box_func},
      {"BOXTYPE", 2, 0x2e02, boxtype_func},
      {"PLEX", 4, 0x2f03, plex_func},
      {"BGNEXTN", 4, 0x3003, extn_func},
      {"ENDEXTN", 4, 0x3103, extn_func},
      {"TAPENUM", 2, 0x3202, null_func},
      {"TAPECODE", 2, 0x3302, null_func},
      {"STRCLASS", 2, 0x3401, null_func},
      {"RESERVED", 4, 0x3503, null_func},
      {"FORMAT", 2, 0x3602, format_func},
      {"MASK", -1, 0x3706, mask_func},
      {"ENDMASKS", 0, 0x3800, endmasks_func},
   };

void do_code (int code, int len, FILE *fp)
{
   int ndx;

   ndx = (code & 0xff00) >> 8;
   if (code < 0 || ndx > 0x38)
      return;
   if (lookup[ndx].code != code || !lookup[ndx].name)
   {
      fprintf (stdout, "Illegal code %x\n", code);
      exit (0);
   }
   else
   {
      lenchk (len, lookup[ndx].len, lookup[ndx].name);
      lookup[ndx].func (ndx, len, fp);
   }
}

void eoferr (s)
char *s;
{
   fprintf (stdout, "Unexpected end of file in %s\n", s);
   exit (1);
}

int fgetlen (fp)
FILE *fp;
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

int fgetint2 (fp)
FILE *fp;
{
   int a,b,sign;

   a = fgetc (fp);
   b = fgetc (fp);
   if (a < 0 || b < 0)
      eoferr ("fgetint2");
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

long fgetint4 (fp)
FILE *fp;
{
   long a,b,c,d;
   int sign;

   a = fgetc (fp);
   b = fgetc (fp);
   c = fgetc (fp);
   d = fgetc (fp);
   if (d < 0)
      eoferr ("fgetint4");
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

double fgetreal4 ( FILE *fp)
{
   int a, b, c, d, sign, exp;
   double mant;

   a = fgetc (fp);
   b = fgetc (fp);
   c = fgetc (fp);
   d = fgetc (fp);
   if (d < 0)
      eoferr ("fgetreal4");
   sign = (a & 0x80) ? -1 : 1;
   exp = (a & 0x7f) - 64;
   mant = ((d / 256.0 + c ) / 256.0 + b ) / 256.0;
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

double fgetreal8 (FILE *fp)
{
   int a, b, c, d, e, f, g, h, sign, exp;
   double mant;

   a = fgetc (fp);
   b = fgetc (fp);
   c = fgetc (fp);
   d = fgetc (fp);
   e = fgetc (fp);
   f = fgetc (fp);
   g = fgetc (fp);
   h = fgetc (fp);
   lastbyte = h;
   if (d < 0)
      eoferr ("fgetreal8");
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

void fgetstring (char *s, int len, FILE *fp)
{
   if ((unsigned) len != fread (s, 1, len, fp))
      eoferr ("fgetstring");
   s[len] = 0;
}

int main (int argc, char *argv[])
{
   int len;

   ++argv;
   --argc;
   *topcell = 0;
   FILE *fin;
   int   ftype=0;

   while (argc > 0 && **argv == '-')
   {
      switch (argv[0][1])
      {
      case 't':
         if (strlen (argv[0]) == 2 && argc > 0)
         {
            argc--;
            argv++;
            strcpy (topcell, argv[0]);
         }
         else
             strcpy (topcell, &argv[0][2]);
	 break;
      default:
         usage();
         break;
      }
      --argc;
      ++argv;
   }
   if (!*topcell)
      is_topcell = 1;
   fin = stdin;
   if (argc > 0)
      if (strcmp(argv[0]+strlen(argv[0])-3, ".gz"))
      {
          if (!freopen (argv[0], "r", stdin))
          {
             fprintf (stderr, "Cannot open %s\n", argv[0]);
             exit (1);
          }
      }
      else
      {
        char string[3078];
        strcpy (string, "gunzip -c ");
        strcat (string, argv[0]);
        ftype=1;
        if (!(fin = popen (string, "r")))
        {
             fprintf (stderr, "Cannot pipe %s\n", string);
             exit (1);
        }
      }
   while ((len = fgetlen (fin)) >= 0)
   {
      if (len > 0)
	 do_code (gds_fgetint2 (fin), len - 4, fin);
   }
   if (ftype) pclose(fin); else fclose(stdin);
   return 0;
}
