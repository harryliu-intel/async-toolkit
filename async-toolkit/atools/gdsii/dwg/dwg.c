// $Id$ AAG

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <ctype.h>
#include "imag.h"

static FILE *dwg,*plt;
static int kind;
static double xo,yo;
static double *list;
static char    textptr[1024];
static unsigned  maxlist;
static unsigned linenr,listlen;
static int fill;
#define SCALE 300
#define max(a,b) ( (a) > (b) ? (a) : (b))
#define min(a,b) ( (a) < (b) ? (a) : (b))
static  char title[1024];
static char printer[1024];

static  double scale = SCALE;

static double mag = 1.0;

#if defined(isnumber)
#undef isnumber
#endif

static int isnumber (char *s)
{
   /* is this a legal floating point number */
   if (*s != '-' && *s != '+' && *s != '.' && !isdigit (*s))
      return 0;
   if (*s == '+' || *s == '-')
      s++;
   while (isdigit (*s))
      s++;
   if (*s == '.')
   {
      s++;
      while (isdigit (*s))
         s++;
   }
   if (*s == 'e')
   {
      s++;
      if (!*s)
         return 0;
      if (*s == '-' || *s == '+')
         s++;
      while (isdigit (*s))
         s++;
   }
   return *s == 0;
}

static void box (double x1, double y1, double x2, double y2, int f)
{
   draw_box(
   (int)((y1+yo)*scale),
   (int)((x1+xo)*scale),
   (int)((y2+yo)*scale),
   (int)((x2+xo)*scale),
   f);
}

static void rect(double x1, double y1, double x2, double y2, int pen)
{
   setpen(pen);
   box (x1, y1, x2, y2, fill);
}

static void line(double x1, double y1, double x2, double y2, int pen)
{
   draw_line(
   (int)((y1+yo)*scale),
   (int)((x1+xo)*scale),
   (int)((y2+yo)*scale),
   (int)((x2+xo)*scale),
   pen);
}

static void circle(double x, double y, double d, int pen)
{
   draw_circle(
   (int)((y+yo)*scale),
   (int)((x+xo)*scale),
   (int)((d/2)*scale),
   pen);
}

static void dl(double x1, double y1, double x2, double y2)
{
   line( x1, y1, x2, y2, 1);
}

static void cl(double x1, double y1, double x2, double y2)
{
   double x,y;
   if(x1 == x2)
   {
      for(y = max(y1,y2); y > min(y1,y2) + 0.5; y -= 0.8)
      {
         dl(x1, y, x1, y-0.5);
         dl(x1, y-0.6, x1, y-0.7);
      }
      dl(x1, y, x1, y-0.5);
   }
   if(y2 == y1)
   {
      for(x = max(x1,x2); x > min(x1,x2) + 0.5; x -= 0.8)
      {
         dl(x, y1, x-0.5, y1);
         dl(x-0.6, y1, x-0.7, y1);
      }
      dl(x, y1, x-0.5, y1);
   }
}

static void pt(double x, double y, char *s, int f)
{
   text((int)((x+xo)*scale)+300,(int)((y+yo)*scale)+300,s,f);
}

static void setoffset(void)
{
   if (listlen != 2)
   {
      fprintf (stderr, "syntax error at line %d\n", linenr);
      return;
   }
   dwg_setoffset (list[0], list[1]);
}

static void setmag(void)
{
   if (listlen != 1 || list[0] <= 0.0)
   {
      fprintf (stderr, "syntax error at line %d\n", linenr);
      return;
   }
   while (list[0]/mag > 300000.0)
   {
      list[0] = list[0] / 10;
      scale = scale / 10;
   }
   dwg_setmag (mag / list[0]);
}

static void doarrow(void)
{
   double dx,dy,dz,ax,ay,bx,by;
   if(listlen!=4)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   dx = list[2] - list[0];
   dy = list[3] - list[1];
   dz = sqrt(dx * dx + dy * dy);
   ax = 0.05/dz*dx;
   ay = 0.05/dz*dy;
   bx = 0.02/dz*dy;
   by = 0.02/dz*dx;
   line(list[0],list[1],list[2],list[3],1);
      line(list[2],list[3],list[2]-ax+bx,list[3]-ay-by,1);
      line(list[2]-ax+bx,list[3]-ay-by,list[2]-ax-bx,list[3]-ay+by,1);
      line(list[2]-ax-bx,list[3]-ay+by,list[2],list[3],1);
}

static void dofill(void)
{
   fill = (int) list[0];
   // this allows set_texture to give an index
   // for future reference to this color
   fill = set_texture (0, fill);
}

static void docenter(void)
{
   if(listlen!=4)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   cl(list[0],list[1],list[2],list[3]);
}

static void doline(void)
{
   if(listlen!=5)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   line(list[0],list[1],list[2],list[3],(int)(list[4]+0.5));
}

static void dodim(void)
{
   if(listlen!=4)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   line(list[0],list[1],list[2],list[3],1);
}

static void docircle(void)
{
   /* 'c x y diameter pen' */
   if(listlen!=4)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   circle(list[0],list[1],list[2],(int)(list[3]+0.5));
}

static void doarc(void)
{
   /* 'v x y r a1 a2 pen' */
   if(listlen != 6)
   {
      fprintf (stderr, "syntax error at line %d\n", linenr);
      return;
   }
   circ_arc((int)(list[2]*scale), (int)((list[1]+yo)*scale),
      (int)((list[0]+xo)*scale), (int)(list[3]*16384.0/360.0),
      (int)(list[4]*16384.0/360.0), (int)(list[5] + 0.5));
}

static void dorect(void)
{
   if(listlen!=5)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   rect(list[0],list[1],list[2],list[3],(int)(list[4]+0.5));
}

static void dopoly(void)
{
   unsigned j;
   int *x, *y;

   if(!(listlen%2) || listlen < 5)
   {
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   x = (int *) malloc (sizeof (int) * listlen/2);
   y = (int *) malloc (sizeof (int) * listlen/2);
   if (!x || !y)
   {
      fprintf (stderr, "Out of memory at %d %s\n",
         __LINE__, __FILE__);
      exit (1);
   }
   for (j = 0; j < listlen/2; j++)
   {
      x[j] = (int)((list[2*j]+xo)*scale);
      y[j] = (int)((list[2*j+1]+yo)*scale);
   }
   polygon (listlen/2, y, x, fill, (int) (list[listlen-1] + 0.5));
   free ((char *) x);
   free ((char *) y);
}

static void dotext(void)
{
   if(listlen!=3 || !textptr || !*textptr)
   {
      fprintf (stderr, "listlen=%d string='%s'\n", listlen, textptr);
      fprintf (stderr, "syntax error at line %d\n",linenr);
      return;
   }
   pt(list[0],list[1],textptr,(int)(list[2]+0.5));
}

static void dowindowtitle(void)
{
   if (textptr && *textptr)
      dwg_filename (textptr);
}

static int parse(void)
{
   char *s;
   int   c;
   char  string[1024];

   listlen=0;
   kind = fgetc (dwg);
   if (kind == EOF)
      return 0;
   if(isupper(kind))
      kind = tolower(kind);
   s = string;
   while ((c = fgetc (dwg)) != EOF && c != '\n')
   {
      if (c == '\\')
      {
	 if (c == '\n')
	    continue;
	 else
	    c = fgetc (dwg);
      }
      if (c == '\n' || c == EOF)
	 break;
      if (isspace (c) || c == '"')
      {
	 *s = 0;
	 if (strlen (string))
	 {
	    if (listlen >= maxlist)
	    {
	       if (!(list = (double *)
		  realloc ((char *) list, sizeof (double) * (maxlist + 1024))))
	       {
		  fprintf (stderr, "cannot realloc list\n");
		  exit (1);
	       }
	       maxlist += 1024;
	    }
            if (kind == 'f' && ! strncmp (string, "-#", 2) &&
                    strlen (string) == 8)
            {
                long val;
                sscanf (string+2,"%06lx", &val);
                val |= 0xf000000;
                val = -val;
                list[listlen++] = (double)val;
            }
            else if (kind == 'f' && *string == '#' &&
                    strlen (string) == 7)
            {
                long val;
                sscanf (string+1,"%06lx", &val);
                val |= 0xf000000;
                list[listlen++] = (double)val;
            }
            else {
                list[listlen++] = atof(string);
            }
	 }
	 if (c == '"')
	 {
	    s = string;
	    while ((c = fgetc (dwg)) != '"' && c != '\n' && c != EOF)
	    {
	       if (c == '\\')
		  c = fgetc (dwg);
	       if (c != EOF)
		  *s++ = c;
	    }
	    *s = 0;
	    strcpy (textptr, string);
	 }
	 if (c != '\n')
	 {
	    while ((c = fgetc (dwg)) == ' ' || c == '\t')
	       ;
	 }
	 ungetc (c, dwg);
	 s = string;
      }
      else
      {
	 *s++ = c;
      }
   }
   *s = 0;
   if (strlen (string))
   {
      if (listlen >= maxlist)
      {
	 if (!(list = (double *)
	    realloc ((char *) list, sizeof (double) * (maxlist + 1024))))
	 {
	    fprintf (stderr, "cannot realloc list\n");
	    exit (1);
	 }
	 maxlist += 1024;
      }
      if (kind == 'f' && ! strncmp (string, "-#", 2) &&
              strlen (string) == 8)
      {
          long val;
          sscanf (string+2,"%06lx", &val);
          val |= 0xf000000;
          val = -val;
          list[listlen++] = (double)val;
      }
      else if (kind == 'f' && *string == '#' &&
              strlen (string) == 7)
      {
          long val;
          sscanf (string+1,"%06lx", &val);
          val |= 0xf000000;
          list[listlen++] = (double)val;
      }
      else
          list[listlen++] = atof(string);
   }
   return 1;
}

static void do_page (FILE *dwg, char *name)
{
   int atend, start;

   dwg = dwg;
   xo = yo = 3.5;
   fill = 0;
   linenr=0;
   dwg_filename (name);
   atend = 1;
   start = 1;
   while (parse())
   {
      if (start)
      {
         //set_adv_dirs (0, 0);
         start = 0;
      }
      linenr++;
      switch(kind)
      {
         case 'a': doarrow(); atend = 0;; break;
         case 'b': if (!atend) end_page (); atend = 1; break;
         case 'c': docircle(); atend = 0; break;
         case 'd': dodim(); atend = 0; break;
         case 'f': dofill(); break;
         case 'l': doline(); atend = 0; break;
         case 'm': setmag(); atend = 0; break;
         case 'n': docenter(); atend = 0; break;
         case 'o': setoffset(); atend = 0; break;
         case 'p': dopoly(); atend = 0; break;
         case 'r': dorect(); atend = 0; break;
         case 's': set_adv_dirs((int)(list[0]+0.5),(int)(list[1]+0.5)); break;
         case 't': dotext(); atend = 0; break;
         case 'v': doarc(); atend = 0; break;
         case 'w': dowindowtitle(); atend = 0; break;
         case 'x': xo = list[0]; break;
         case 'y': yo = list[0]; break;
      }
   }
   if (!atend)
      end_page();
}

int main (int argc, char *argv[])
{
   char pltfile[64];
   int  plthandle;
   int  keep;
   char cmd[128];
   int i;
   int use_stdin;
   int done_options;
   int printtype;

   *pltfile = 0;
   keep = 0;
   use_stdin = 1;
   fill = 0;
   dwg=stdin;
   printtype = printer_type ();
#ifndef MAC
   if (printtype == DWG_TYPE_IMAG || printtype == DWG_TYPE_POST)
   {
      strcpy (cmd, "/tmp/dwgXXXXXX");
      if ((plthandle = mkstemp (cmd)) != -1) 
      {
          strcpy (pltfile, cmd);
          if((plt=fdopen(plthandle,"w"))==NULL)
          {
             fprintf(stderr,"cannot open %s\n", pltfile);
             exit(1);
          }
      }
   }
   else if (printtype == DWG_TYPE_GIF)
   {
      plt = stdout;
   }
   else
   {
      plt = NULL;
   }
#endif
   done_options = 0;
   maxlist = 1024;
   if (!(list = (double *) malloc (sizeof (double) * maxlist)))
   {
      fprintf (stderr, "Out of memory at %d %s\n",
         __LINE__, __FILE__);
      exit (1);
   }
   for (i = 1; i < argc; i++)
   {
      if (!done_options && argv[i][0] == '-')
      {
         if (!strcmp (argv[i], "--"))
            done_options = 1;
	 else if (!strcmp (argv[i], "-g"))
	 {
	    dwg_docolor (0); // use gray scale vs. color in printing
	 }
         else if (!strcmp (argv[i], "-o") && argc > i + 1)
         {
	    if (plt) // printtype() != 2 here
	    {
	       if (plt != stdout)
	       {
		  fclose (plt);
		  unlink (pltfile);
	       }
	       *pltfile = 0;
	       if (strcmp (argv[i+1], "-"))
	       {
		  strcpy (pltfile, argv[i+1]);
		  if((plt=fopen(pltfile,"w"))==NULL)
		  {
		     fprintf(stderr,"cannot open %s\n", pltfile);
		     exit(1);
		  }
		  keep = 1;
	       }
	       else
	       {
		  plt = stdout;
		  keep = 1;
	       }
	    }
            i++;
         }
         else if ((printtype == DWG_TYPE_X || printtype == DWG_TYPE_GIF) && !strcmp (argv[i], "-p"))
            force_portrait (1);
         else if ((printtype == DWG_TYPE_X || printtype == DWG_TYPE_GIF) && !strcmp (argv[i], "-l"))
            force_portrait (0);
         else if ((printtype == DWG_TYPE_X || printtype == DWG_TYPE_GIF) && !strcmp (argv[i], "-s"))
         {
            if (argc > i + 1 && isdigit (argv[i+1][0]))
            {
               force_scale (atoi (argv[i+1]));
               i++;
            }
            else
            {
               fprintf (stderr, "-s requires number\n");
               exit (1);
            }
         }
         else if (!strcmp (argv[i], "-title"))
         {
            if (argc > i + 1)
            {
               strcpy (title, argv[i+1]);
               i++;
            }
            else
            {
               fprintf (stderr, "-title requires string title\n");
               exit (1);
            }
         }
         else if (!strncmp (argv[i], "-of", 3))
         {
            if (argc > i + 2 && isnumber (argv[i+1]) && isnumber (argv[i+2]))
            {
               dwg_setoffset (atof (argv[i+1]), atof (argv[i+2]));
               i += 2;
            }
            else
            {
               fprintf (stderr, "-offset requires two numbers\n");
               exit (1);
            }
         }
         else if (!strncmp (argv[i], "-m", 2))
         {
            if (argc > i + 1 && isnumber (argv[i+1]) &&
                  (mag = atof (argv[i+1])) > 0.0)
            {
               dwg_setmag (mag);
               i++;
            }
            else
            {
               fprintf (stderr, "-magnification requires number > 0\n");
               exit (1);
            }
         }
	 else if (!strncmp (argv[i], "-P", 2) && strlen (argv[i]) > 2 &&
	    (printtype == DWG_TYPE_IMAG || printtype == DWG_TYPE_POST))
	 {
	    strcpy (printer, argv[i] + 2);
	 }
         else
         {
            fprintf (stderr, "Illegal option %s\n", argv[i]);
            exit (1);
         }
      }
      else
      {
         use_stdin = 0;
         if((dwg=fopen(argv[i],"r"))==NULL)
         {
            fprintf(stderr,"File not found: %s\n",argv[i]);
            continue;
         }
#ifdef MAC
         if (!plt)
         {
            strcpy (pltfile, argv[i]);
            if (strrchr (pltfile, '.') > strrchr (pltfile, '/'))
               strcpy (strrchr (pltfile, '.'), ".post");
            else
               strcat (pltfile, ".post");
            if (plt = fopen (pltfile, "w"))
            {
               set_file (plt);
            }
            else
            {
               fprintf (stderr, "Cannot open %s for writing\n",
                  pltfile);
               exit (1);
            }
         }
#endif
	 set_file(plt);
         do_page (dwg, *title ? title : argv[i]);
      }
   }
   if (use_stdin)
   {
#ifdef MAC
      plt = stdout;
#endif
      set_file (plt);
      do_page (stdin, title);
   }
   end_file();
#ifndef MAC
   if (keep)
      exit (0);
   if ((printtype == DWG_TYPE_IMAG || printtype == DWG_TYPE_POST) && *pltfile)
   {
      strcpy (cmd, "lpr ");
      if (strlen (printer))
      {
	 strcat (cmd, "-P");
	 strcat (cmd, printer);
	 strcat (cmd, " ");
      }
      strcat (cmd, pltfile);
      system(cmd);
   }
   if (!keep && *pltfile)
      unlink (pltfile);
#endif
   return (0);
}
