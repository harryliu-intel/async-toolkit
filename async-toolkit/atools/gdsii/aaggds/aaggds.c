// $Id$ AAG

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include "c_lib.h"
#include "btutil.h"
#include "gdsutil.h"
#include <regex.h>

BTTREE bt_instance, bt_signal;
typedef struct fill_layer FILL_LAYER;
struct fill_layer {
    short layer;
    short datatype;
};

BTTREE bt_fill_map;

//char *re_comp (char *);
char *mktemp (char *);
extern GDS_ITEM *gds_item_ptr;
int error_count;
static int case_insensitive = 0;
extern int dowarning;
extern int dorescale;
FILE *output;
void write_centers (FILE *fp);
char *mktemp (char *);
extern GDS_ITEM *gds_item_ptr;
int error_count;
void write_centers (FILE *fp);
char *re_comp (const char *s);
int re_exec (const char *s);
void write_center_coords (char *topcell, FILE *fp_out);
void write_dwg (char *topcell, FILE *fp_out, GDS_TRANS *trans);
void help (int prefix);
int is_match (char *match, char *compare);
int check_match (char *match, int character);
void do_hier (FILE *fp_out);
void do_print_hier (char *line, BTTREE *tree, int lvl, FILE *fp_out);
void write_origins (FILE *fp);
void write_centers (FILE *fp);

void usage (void)
{
    fprintf (stderr, "Usage: aaggds [-i] [-t <topcell>] [gds_file_list]\n");
    fprintf (stderr, "   -i        : cell names are case insensitive\n");
    fprintf (stderr, "   -s1       : rescale to 1nm resolution\n");
    fprintf (stderr, "   -w        : Issue warnings\n");
    fprintf (stderr, "   -t <name> : specify name of top cell\n");
    fprintf (stderr, "   file list is a space separated list of names.\n");
    fprintf (stderr, "   aaggds is interactive, you will receive an aaggds> prompt\n");
    exit (1);
}

//static char re_comp_string[512];

static regex_t regexlocal;

char *re_comp (const char *s)
{
   regcomp (&regexlocal, s, 0);
   return NULL;
}

int re_exec (const char *s)
{
   return !regexec(&regexlocal, s, 0, NULL, 0);
}

static FILE *fp_out, *fp_scr;
static LONGINT dwg_offset_x, dwg_offset_y;
static double dwg_scale;
static int    errcnt;

#define DMODE_FIRST   0
#define DMODE_GRID    4600
#define DMODE_SPCNG   12
#define DMODE_REPEAT  13

static FILE *input;

extern BTTREE bt_structure;

#define LAYERCOUNT  256
static char   dwg_layers[1024];
static double plot_grid[2] = {0.0,0.0};
static int    isttyi = 1;
static int    isttyo = 1;

static char *mygets (void)
{
    static char *line, *oldline;
    char localline[MAXSTR+1];
    if (isttyi) {
        if ((line = readline ("aaggds> "))) {
            if (*line && ( (! oldline ) || strcmp (line, oldline) ) )
                add_history (line);
            if (oldline) free (oldline);
            oldline = line;
            return line;
        }
    }
    else {
        if (fgets (localline, MAXSTR, stdin)) {
            trimtb (localline);
            trimlb (localline);
            line = malloc (strlen (localline)+1);
            strcpy (line, localline);
            return line;
        }
    }
    return NULL;
}

static int mapcmp (FILL_LAYER *fl1, FILL_LAYER *fl2)
{
    if (fl1 -> layer != fl2 -> layer)
        return fl1->layer - fl2->layer;
    return fl1->datatype - fl2->datatype;
}

static void addfillmap (FILL_LAYER *fl, char *map)
{
    char *ptr;

    if ((ptr = (char *)btloc (fl, &bt_fill_map)) || (ptr = (char *)btins (fl, &bt_fill_map)))
        strcpy (ptr, map);
}

int main (int argc, char *argv[])
{
   GDS_TRANS trans;
   GDS_ITEM **srf;
   char  topcell[MAXSTR];
   char  gds_fn[1024];
   char  dwg_fn[1024];
   char  ofile[1024];
   char *line;
   char cmd[1024];
   char arg[1024];
   double dval[4];
   int   cmd_line;
   int   pcf_layer;
   double   pcf_mag;
   char  *s;
   FILL_LAYER fl;

   *topcell = 0;
   *gds_fn = 0;
   cmd_line = 0;
   ++argv;
   --argc;
   btinit (&bt_fill_map, sizeof (FILL_LAYER), 32, mapcmp);
   if (!ttyname(0))
      isttyi=0;
   if (!ttyname(1))
      isttyo=0;
   while (argc > 0)
   {
      if (argv[0][0] == '-')
      {
         switch (argv[0][1])
         {
         case 't':
            if (argv[0][2])
               strcpy (topcell, argv[0] + 2);
            else if (argc > 1)
            {
               strcpy (topcell, argv[1]);
               ++argv;
               --argc;
            }
            cmd_line |= 2;
            break;
         case 'i':
            case_insensitive = 1;
            break;
         case 'w':
            dowarning = 1;
            break;
         case 's':
            switch(argv[0][2])
            {
            case '1': dorescale=1;
                break;
            default: dorescale=0;
                break;
            }
            break;
         default:
            usage();
            break;
         }
      }
      else if (gds_load (argv[0]))
      {
         cmd_line |= 1;
         fprintf (stderr, "Cannot load %s\n", argv[0]);
         errcnt++;
      }
      --argc;
      ++argv;
   }
   gds_clear_trans (&trans);
   if (! *topcell && (s = gds_topcell()))
   {
      strcpy (topcell, s);
   }
   s = gds_topcell();
   pcf_layer = 11;
   pcf_mag   = 20.0;
   if (cmd_line == 3)
   {
      if (case_insensitive)
          strupper (topcell);
      strcpy (dwg_fn, topcell);
      vsc_add_extension (dwg_fn, ".dwg");
      if (!(fp_out = fopen (dwg_fn, "w")))
      {
         fprintf (stderr, "Cannot open %s\n", dwg_fn);
         errcnt++;
      }
      else
      {
         gds_path2boundary (1);
         if (gds_read_structure (trans, topcell))
            write_dwg (topcell, fp_out, &trans);
         else
         {
            fprintf (stderr, "%s is not in library\n", topcell);
            errcnt++;
	    *topcell = 0;
         }
         fclose (fp_out);
         gds_path2boundary (0);
      }
   }
   else
   {
      int quit;

      quit = 0;
      while (!quit)
      {
         gds_free_bndy ();
         gds_free_text ();
         *cmd = 0;
         *arg = 0;
         if (! (line = mygets ()))
            break;
         sscanf (line, "%s %s", cmd, arg);
         switch (*cmd)
         {
         case 0:
            break;
         case 'g':
            switch (cmd[1])
            {
            case 'd':
               if (!*topcell)
               {
                  fprintf (stderr, "No topcell specified\n");
                  help ('g');
                  break;
               }
               gds_read_structure (trans, topcell);
               break;
            case 'r':
               if (!*arg || atof (arg) <= 0.0)
               {
                  plot_grid[0] = plot_grid[1] = 0.0;
                  fprintf (stderr, "plot grid is off\n");
               }
               else
               {
                  plot_grid[0] = atof (arg);
                  for (s = arg; *s && *s != ',' && *s != 'x'; s++);
                  if (*s)
                  {
                     plot_grid[1] = atof (s+1);
                     fprintf (stderr,
                        "plot grid = %.2lfx%.2lf microns\n",
                           plot_grid[0], plot_grid[1]);
                  }
                  else
                  {
                     plot_grid[1] = plot_grid[0];
                     fprintf (stderr,
                        "plot grid = %.2lf microns\n", plot_grid[0]);
                  }
               }
               break;
            default:
               help ('g');
               break;
            }
            break;
         case 'c':
            gds_free_org ();
            gds_free_bndy ();
            gds_free_text ();
            gds_free_ref ();
            gds_clear ();
            break;
         case 'o': /* output file name */
            if (!*arg)
               help ('o');
            else
            {
               strcpy (ofile, arg);
               if (!(fp_scr = fopen (ofile, "a")))
               {
                  fprintf (stderr, "Cannot open file %s\n", ofile);
                  errcnt++;
               }
               else
                  fprintf (stderr, "File %s opened for output\n", ofile);
            }
            break;
         case 'p':
            switch (cmd[1])
            {
            case 'c':
               if (!*topcell)
               {
                  fprintf (stderr, "No topcell specified\n");
                  errcnt++;
                  help ('t');
                  break;
               }
               if (*arg && !(fp_out = fopen (arg, "w")))
               {
                  fprintf (stderr, "Cannot open %s\n", arg);
                  errcnt++;
                  break;
               }
               if (!*arg)
                  fp_out = stdout;
               if (gds_read_structure (trans, topcell))
                  write_center_coords (topcell, fp_out);
               if (*arg)
                  fclose (fp_out);
               break;
            case 'r':
               if (!*topcell)
               {
                  fprintf (stderr, "No topcell specified\n");
                  errcnt++;
                  help ('t');
                  break;
               }
               if (!(fp_out = popen ("dwg", "w")))
               {
                  fprintf (stderr, "Cannot pipe to dwg\n");
                  errcnt++;
                  break;
               }
               gds_path2boundary (1);
               if (gds_read_structure (trans, topcell))
                  write_dwg (topcell, fp_out, &trans);
               pclose (fp_out);
               gds_path2boundary (0);
               break;
            case 'l':
               if ((pcf_layer = atoi (arg)) > 0 && pcf_layer < 18)
                  fprintf (stderr, "pcf layer %d\n", pcf_layer);
               else
               {
                  pcf_layer = 11;
                  fprintf (stderr, "pcf layer default %d\n", pcf_layer);
               }
               break;
            case 'm':
               if ((pcf_mag = atof (arg)) > 0.0)
               {
                  fprintf (stderr, "pcf mag %.2lf\n", pcf_mag);
               }
               else
               {
                  pcf_mag = 20.0;
                  fprintf (stderr, "pcf mag default %.0lf\n", pcf_mag);
               }
               break;
            default:
               help ('p');
               break;
            }
            break;
         case 'l': /* print list of cells */
            switch (cmd[1])
            {
            case 'h':
               do_hier (fp_scr ? fp_scr : stderr);
               break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               if (case_insensitive)
                   strupper (arg);
               if (*arg)
                  gds_print_hier (arg, atoi (cmd + 1));
               else
                  gds_print_hier (topcell, atoi (cmd + 1));
               break;
            case 's':
               btselect ("", "zzzzzz", &bt_structure);
               if (strlen (arg) == 0)
                  strcpy (arg, ".");
               else if (case_insensitive)
                  strupper (arg);
               {
               int tmp_int = 0;
               if ((s = re_comp (arg)))
               {
                  fprintf (stderr, "%s\n", s);
                  help ('l');
                  break;
               }
               while (btget (cmd, (void * *) &srf, &bt_structure))
               {
                  if (re_exec (cmd) == 1)
                  {
                     if (++tmp_int > 23 && isttyi &&
                           (!fp_scr || !strcmp (ofile, "/dev/tty")))
                     {
                        char tmp[64];
                        fprintf (stderr, "-- more --<CR>\n");
                        fgets (tmp, 63, stdin);
                        if (*tmp == 'q' || *tmp == 'Q')
                           break;
                        tmp_int = 0;
                     }
                     fprintf (fp_scr ? fp_scr : stdout, "%s\n", cmd);
                  }
               }
               }
               break;
            case 'y':
                // list layers
                gds_listlayers (topcell, cmd[2] == 's', arg);
                break;
            default:
               help ('l');
               break;
            }
            break;
         case 'i':
            fprintf (stderr, "Initializing\n");
            gds_init ();
            gds_clear_trans (&trans);
            break;
         case 'x':
            switch (cmd[1])
            {
            case 0:
               if (!*arg)
               {
                  help ('x');
                  break;
               }
               trans.x = atol (arg);
               break;
            case 'd':
               if (!*topcell)
               {
                  fprintf (stderr, "No topcell specified\n");
                  errcnt++;
                  help ('t');
                  break;
               }
               //strcpy (dwg_fn, "/tmp/dwgXXXXXX");
               if (!(fp_out = popen ("xdwg", "w")))
               {
                  fprintf (stderr, "Cannot pipe to xdwg\n");
                  errcnt++;
                  break;
               }
               gds_path2boundary (1);
               if (gds_read_structure (trans, topcell))
                  write_dwg (topcell, fp_out, &trans);
               pclose (fp_out);
               gds_path2boundary (0);
               break;
            }
            break;
         case 'y':
            if (!*arg)
            {
               help ('y');
               break;
            }
            trans.y = atol (arg);
            break;
         case 'm':
            if (!*arg)
            {
               help ('m');
               break;
            }
            trans.mag = atof (arg);
            break;
         case 'a':
            if (!*arg)
            {
               help ('a');
               break;
            }
            trans.angle = atof (arg);
            break;
         case 'd':
            switch (cmd[1])
            {
            case 'x': /* dx  drawing exclude */
            case 'i': /* di  drawing include */
               if (sscanf (arg, "%le,%le,%le,%le",
                     &dval[0], &dval[1], &dval[2], &dval[3]) != 4)
                  help ('d');
               else
               {
                  if (cmd[1] == 'x')
                     gds_exclude_rect ((LONGINT) floor ((dval[0] * 1000 + 0.5)),
                        (LONGINT) (dval[1] * 1000 + 0.5),
                        (LONGINT) (dval[2] * 1000 + 0.5),
                        (LONGINT) (dval[3] * 1000 + 0.5));
                  else
                     gds_include_rect ((LONGINT) floor ((dval[0] * 1000 + 0.5)),
                        (LONGINT) (dval[1] * 1000 + 0.5),
                        (LONGINT) (dval[2] * 1000 + 0.5),
                        (LONGINT) (dval[3] * 1000 + 0.5));
               }
               break;
            case 'l':
               strcpy (dwg_layers, arg);
               if (strlen (dwg_layers))
               {
                  gds_clear_layer_mask ();
                  gds_set_layer (dwg_layers);
               }
               else
                  gds_set_layer_mask ();
               break;
            case 'c':
               if (case_insensitive)
                   strupper (arg);
               gds_set_cell (arg);
               break;
            case 'o':
               dwg_offset_x = dwg_offset_y = 0;
               if (*arg)
               {
                  dwg_offset_x = atof (arg) * 1000;
                  for (s = arg; *s && *s != ','; s++);
                  if (*s)
                     dwg_offset_y = atof (s + 1) * 1000;
                  fprintf (stderr, "Offset %.2lf,%.2lf\n",
                     dwg_offset_x / 1000.0, dwg_offset_y / 1000.0);
               }
               break;
            case 's':
               dwg_scale = atof (arg);
               fprintf (stderr, "Scale set to %.3lf micros/inch\n", dwg_scale);
               break;
            default:
               help ('d');
               break;
            }
            break;
         case 's':
            if (!*arg && cmd[1] != 'f')
            {
               help ('s');
               break;
            }
            switch (cmd[1])
            {
            case 'y':
            case 'h':
               for (s = line + strlen (cmd); isspace (*s); s++);
               system (s);
               break;
            case 's':
               { 
               int tmp_int = -1;
               sscanf (arg, "%x", &tmp_int);
               if (tmp_int & (~0x8003))
               {
                  fprintf (stderr, "Illegal STRANS value\n");
                  help ('s');
               }
               else
                  trans.strans = tmp_int;
	       }
               break;
            case 'f':
               {
                   LONGINT minx,miny,maxx,maxy;

                   gds_size_flat (trans, topcell, &minx, &miny, &maxx, &maxy);
                   fprintf(stderr, "%d,%d %d,%d\n", minx,miny,maxx,maxy);
               }
               break;
            default:
               help('s');
               break;
            }
            break;
         case 'q':
            quit = 1;
            break;
         case 'h':
         case '?':
	    help (*arg);
            break;
         case 't':
            if (!*arg)
            {
               fprintf (stderr, "top cell %s\n",
                  *topcell ? topcell : "NONE");
               break;
            }
            gds_free_bndy ();
            gds_free_text ();
            strcpy (topcell, arg);
            if (case_insensitive)
                strupper (topcell);
            strcpy (dwg_fn, arg);
            if (!gds_find_structure (topcell))
            {
               fprintf (stderr, "%s is not in library\n", topcell);
               errcnt++;
               *topcell = *dwg_fn = 0;
            }
            break;
         case 'f':
            fl.layer = fl.datatype = 0;
            if (!isdigit (cmd[1]) || (!isdigit (arg[0]) && arg[0] != '#'))
            {
               help ('f');
               break;
            }
            if (atoi (cmd + 1) < 1 || atoi (cmd + 1) > 255)
            {
               help ('f');
               break;
            }
            if (! (atoi (arg) >= 0 && atoi (arg) <= 15) &&
                ! (arg[0] == '#' && ( strlen (arg) == 4 || strlen (arg) == 7)))
            {
               fprintf (stderr, "%s\n", arg);
               help ('f');
               break;
            }
            if (atoi (cmd+1) < LAYERCOUNT)
            {
                char na[16];
                int  n;
                strcpy (na, arg);
                if (strlen (arg) == 4)
                {
                    na[2] = arg[1];
                    na[3] = arg[2];
                    na[4] = arg[2];
                    na[5] = arg[3];
                    na[6] = arg[3];
                    na[7] = 0;
                }
                fl.layer = atoi(cmd+1);
                for (n = 1; cmd[n] && cmd[n] != ';'; n++)
                    ;
                if (cmd[n] == ';' && atoi (cmd + n + 1) < LAYERCOUNT)
                {
                    fl.datatype = atoi (cmd + n + 1);
                    addfillmap (&fl, na);
                }
                else
                {
                    for (n = 0; n < 256; n++)
                    {
                        fl.datatype = n;
                        addfillmap (&fl, na);
                    }
                }
            }
            else
                fprintf (stderr, "Layer %s illegal\n", arg);
            break;
         case 'w':
            if (cmd[1] != 'l' && !*topcell)
            {
               fprintf (stderr, "No topcell specified\n");
               errcnt++;
               help ('t');
               help ('w');
               break;
            }
            if (!*arg && *topcell)
               strcpy (arg, topcell);
            strcpy (dwg_fn, arg);
            switch (cmd[1])
            {
	    case 'p': // write postscript
	       vsc_add_extension (dwg_fn, ".ps");
	       sprintf (line, "dwg -o %s", dwg_fn);
	       if (!(fp_out = popen (line, "w")))
	       {
		  fprintf (stderr, "Cannot open pipe %s\n", line);
		  errcnt++;
		  break;
	       }
               gds_path2boundary (1);
               if (gds_read_structure (trans, topcell))
                  write_dwg (topcell, fp_out, &trans);
               pclose (fp_out);
               gds_path2boundary (0);
	       fprintf (stderr, "%s written\n", dwg_fn);
	       break;
            case 'f':
               vsc_add_extension (dwg_fn, ".gds");
               if (! gds_loaded (dwg_fn))
               {
                  if (!(fp_out = fopen (dwg_fn, "w")))
                  {
                     fprintf (stderr, "Cannot open %s\n", dwg_fn);
                     errcnt++;
                     break;
                  }
                  gds_write_flat (trans, topcell, fp_out);
                  fclose (fp_out);
                  fprintf (stderr, "%s written\n", dwg_fn);
               }
               else
                  fprintf (stderr, "%s is open and cannot be written\n", dwg_fn);
               break;
            case 'h':
               vsc_add_extension (dwg_fn, ".gds");
               if (! gds_loaded (dwg_fn))
               {
                  if (!(fp_out = fopen (dwg_fn, "w")))
                  {
                     fprintf (stderr, "Cannot open %s\n", dwg_fn);
                     errcnt++;
                     break;
                  }
                  gds_write_cell (trans, topcell, fp_out);
                  fflush (fp_out);
                  fprintf (stderr, "%s written\n", dwg_fn);
                  fclose (fp_out);
               }
               else
                  fprintf (stderr, "%s is open and cannot be written\n", dwg_fn);
               break;
            case 'l':
               if (!*arg)
               {
                  fprintf (stderr, "Specify library name\n");
                  break;
               }
               strcpy (gds_fn, arg);
               vsc_add_extension (gds_fn, ".gds");
               vsc_add_extension (arg, "");
               if (! gds_loaded (gds_fn))
               {
                  if (!(fp_out = fopen (gds_fn, "w")))
                  {
                     fprintf (stderr, "Cannot open %s\n", gds_fn);
                     errcnt++;
                     break;
                  }
                  if (case_insensitive)
                      strupper (arg);
                  gds_write_lib (arg, fp_out);
                  fprintf (stderr, "%s written\n", gds_fn);
                  fclose (fp_out);
               }
               else
                  fprintf (stderr, "%s is open and cannot be written\n", dwg_fn);
               break;
            case 'c':
               if (!*arg)
               {
                  fprintf (stderr, "Specify center file name\n");
                  break;
               }
               strcpy (gds_fn, arg);
               if (!(fp_out = fopen (gds_fn, "w")))
               {
                  fprintf (stderr, "Cannot open %s\n", gds_fn);
                  errcnt++;
                  break;
               }
               if (case_insensitive)
                   strupper (arg);
               write_centers (fp_out);
               fprintf (stderr, "%s written\n", gds_fn);
               fclose (fp_out);
               break;
            case 'o':
               if (!*arg)
               {
                  fprintf (stderr, "Specify orign file name\n");
                  break;
               }
               strcpy (gds_fn, arg);
               if (!(fp_out = fopen (gds_fn, "w")))
               {
                  fprintf (stderr, "Cannot open %s\n", gds_fn);
                  errcnt++;
                  break;
               }
               if (case_insensitive)
                   strupper (arg);
               gds_clear ();
               if (gds_read_structure (trans, topcell))
                  write_origins (fp_out);
               fprintf (stderr, "%s written\n", gds_fn);
               fclose (fp_out);
               break;
            case 't':
               fprintf (stderr, "Txt not implemented\n");
               errcnt++;
               fprintf (stderr, "  Use rdgds\n");
               break;
            case 'd':
               vsc_add_extension (dwg_fn, ".dwg");
               if (!(fp_out = fopen (dwg_fn, "w")))
               {
                  fprintf (stderr, "Cannot open %s\n", dwg_fn);
                  errcnt++;
                  break;
               }
               gds_path2boundary (1);
               if (gds_read_structure (trans, topcell))
                  write_dwg (topcell, fp_out, &trans);
               fflush (fp_out);
               fprintf (stderr, "%s written\n", dwg_fn);
               fclose (fp_out);
               gds_path2boundary (0);
               break;
            default:
               fprintf (stderr, "Illegal command %s\n", cmd);
               errcnt++;
               help ('w');
               break;
            }
            break;
         case 'r':
            switch (cmd[1])
            {
            case 'm': /* delete a structure */
               if (!*arg)
               {
                  fprintf (stderr, "Specify structure name\n");
                  errcnt++;
                  help ('r');
                  break;
               }
               if (!gds_delete_structure (arg))
               {
                  fprintf (stderr, "  %s not in library to delete\n", arg);
                  errcnt++;
               }
               break;
            case 'p': /* read pcf file */
               if (!*arg)
               {
                  fprintf (stderr, "Specify filename\n");
                  help ('r');
                  break;
               }
               if (!*topcell)
               {
                  fprintf (stderr, "No topcell specified\n");
                  errcnt++;
                  break;
               }
               strcpy (dwg_fn, arg);
               vsc_add_extension (dwg_fn, ".pcf");
               if (!(input = fopen (dwg_fn, "r")))
               {
                  fprintf (stderr, "Cannot open %s for reading\n",
                     dwg_fn);
                  errcnt++;
               }
               break;
            case 'g':
               if (!*arg)
               {
                  fprintf (stderr, "Specify filename\n");
                  help ('r');
                  break;
               }
               strcpy (dwg_fn, arg);
               if (gds_load (arg))
               {
                  fprintf (stderr, "Cannot read %s\n", arg);
                  errcnt++;
               }
               else
                  fprintf (stderr, "%s read\n", arg);
               if (! *topcell && (s = gds_topcell()))
                   strcpy (topcell, s);
               break;
            case 't':
               if (!*arg)
               {
                  fprintf (stderr, "Specify filename\n");
                  help ('r');
                  break;
               }
               fprintf (stderr, "Txt read not implemented\n");
               fprintf (stderr, "  Use wrgds\n");
               errcnt++;
               break;
            default:
               fprintf (stderr, "Illegal command %s\n", cmd);
               errcnt++;
               help ('r');
               break;
            }
            break;
         default:
            fprintf (stderr, "Illegal command %s\n", cmd);
            errcnt++;
            help (0);
            break;
         }
      }
      if (! quit ) printf ("\n");
   }
   return (errcnt != 0);
}

void write_center_coords (char *topcell, FILE *fp_out)
{
   GDS_BNDY *bndy, *bndy_head;
   LONGINT tx, ty, ux, uy;

   bndy_head = gds_bndy_head ();
   if (!bndy_head)
      return;
   for (bndy = bndy_head; bndy; bndy = bndy -> next)
   {
      gds_minmax (bndy, &tx, &ux, &ty, &uy);
      fprintf (fp_out, "%8.1lf %8.1lf\n",
         (tx + ux) / 2000.0, (ty + uy) / 2000.0);
   }
}

void write_dwg (char *topcell, FILE *fp_out, GDS_TRANS *trans)
{
   GDS_BNDY *bndy, *bndy_head;
   GDS_TXT *text, *text_head;
   LONGINT minx, miny, maxx, maxy;
   LONGINT tx, ty, ux, uy;
   LONGINT gridx, gridy;
   LONGINT offset_x, offset_y;
   char  fill[32] = "0";
   double scale;

   bndy_head = gds_bndy_head ();
   text_head = gds_text_head ();
   strcpy (fill, "0");
   if (!bndy_head)
      return;
   gds_minmax (bndy_head, &minx, &maxx, &miny, &maxy);
   for (bndy = bndy_head -> next; bndy; bndy = bndy -> next)
   {
      gds_minmax (bndy, &tx, &ux, &ty, &uy);
      if (tx < minx)
         minx = tx;
      if (ty < miny)
         miny = ty;
      if (ux > maxx)
         maxx = ux;
      if (uy > maxy)
         maxy = uy;
   }
   for (text = text_head; text; text = text -> next)
   {
      if (text -> x < minx)
         minx = text -> x;
      if (text -> y < miny)
         miny = text -> y;
      if (text -> x > maxx)
         maxx = text -> x;
      if (text -> y > maxy)
         maxy = text -> y;
   }
   if (minx >= 0)
      minx = -1;
   if (miny >= 0)
      miny = -1;
   scale = (maxx - minx) / 9.0;
   if (((maxy - miny) / 6.5) > scale)
      scale = (maxy - miny) / 6.5;
   if (scale == 0.0)
      return;
   if (dwg_scale > 0.0)
      scale = 0.0001/dwg_scale;
   else
      scale = 1.0 / scale;
   scale = scale * 10.0;
   if (dwg_offset_x != 0 || dwg_offset_y != 0)
   {
      offset_x = dwg_offset_x * 10;
      offset_y = dwg_offset_y * 10;
   }
   else
   {
      offset_x = minx;
      offset_y = miny;
   }
#if 0
   scale *= 0.956429; /* corrects imagen scaling */
#endif
   fprintf (fp_out, "x 0\ny 0\ns 1 0\n");
   fprintf (fp_out, "m %.0lf\n", 1.0 / scale);
   fprintf (fp_out, "w \"%s\"\n", topcell);
   if ((trans -> strans & 0x8000) || trans -> angle != 0.0)
      fprintf (fp_out, "t %.0lf %.0lf 2 \"Strans 0x%04x Angle %.1lf\"\n",
         -0.52 / scale, 4.0 / scale,
         (unsigned) trans -> strans & 0xffff, trans -> angle);
   if (*dwg_layers)
      fprintf (fp_out, "t %.0lf %.0lf 2 \"Layers %s\"\n",
         -0.64 / scale, 4.0 / scale, dwg_layers);
   if (plot_grid[0] > 0.0 && plot_grid[1] > 0.0)
      fprintf (fp_out, "t %.0lf %.0lf 2 \"Grid %.2lfx%.2lf microns\"\n",
         -0.76 / scale, 4.0 /scale, plot_grid[0], plot_grid[1]);
   fprintf (fp_out, "t %.0lf 0.0 5 \"%s\"\n", -0.6 / scale, topcell);
   fprintf (fp_out, "t %.0lf %.0lf 2 \"scale %.3lf microns per inch\"\n",
         -0.4 / scale, 4.0 / scale, 0.001/scale);
   if (-offset_y * scale > -11.2 && -offset_y * scale <  78. &&
         -offset_x * scale > -11.2 && -offset_x * scale < 111.2)
   {
      /* bow tie at origin */
      fprintf (fp_out, "l %.0lf %.0lf %.0lf %.0lf 1\n",
         0.1 / scale - offset_y/10.0,
         -0.1 / scale - offset_x/10.0,
         -0.1 / scale - offset_y/10.0,
         0.1 / scale - offset_x/10.0);
      fprintf (fp_out, "l %.0lf %.0lf %.0lf %.0lf 1\n",
         -0.1 / scale - offset_y/10.0,
         0.1 / scale - offset_x/10.0,
         0.1 / scale - offset_y/10.0,
         0.1 / scale - offset_x/10.0);
      fprintf (fp_out, "l %.0lf %.0lf %.0lf %.0lf 1\n",
         0.1 / scale - offset_y/10.0,
         0.1 / scale - offset_x/10.0,
         -0.1 / scale - offset_y/10.0,
         -0.1 / scale - offset_x/10.0);
      fprintf (fp_out, "l %.0lf %.0lf %.0lf %.0lf 1\n",
         -0.1 / scale - offset_y/10.0,
         -0.1 / scale - offset_x/10.0,
         0.1 / scale - offset_y/10.0,
         -0.1 / scale - offset_x/10.0);
   }
   for (bndy = bndy_head; bndy; bndy = bndy -> next)
   {
      int i;
      char *fill_ptr = NULL;
      FILL_LAYER fl;

      /* is it a rectangle? */

      if (bndy -> layer < 0 || bndy->layer >= LAYERCOUNT)
         fprintf (stderr, "Illegal Layer %d in BNDY\n", bndy->layer);
      fl.layer = bndy->layer;
      fl.datatype = bndy->datatype;
      fill_ptr = btloc (&fl, &bt_fill_map);
      if (fill_ptr == NULL)
      {
         if (strcmp (fill, "0"))
            fprintf (fp_out, "f 0\n");
         strcpy (fill, "0");
      }
      else if (strcmp (fill, fill_ptr))
      {
         strcpy (fill, fill_ptr);
         fprintf (fp_out, "f %s\n", fill);
      }
      if ((bndy -> kind != GDS_PATH && bndy -> len == 5) &&
         ((bndy -> x[0] == bndy -> x[1] && bndy -> x[2] == bndy -> x[3] &&
            bndy -> y[0] == bndy -> y[3] && bndy -> y[1] == bndy -> y[2]) ||
         (bndy -> y[0] == bndy -> y[1] && bndy -> y[2] == bndy -> y[3] &&
            bndy -> x[0] == bndy -> x[3] && bndy -> x[1] == bndy -> x[2])))
      {
         fprintf (fp_out, "r %.0lf %.0lf %.0lf %.0lf 1\n",
            (double) (bndy -> y[0] - offset_y)/10,
            (double) (bndy -> x[0] - offset_x)/10,
            (double) (bndy -> y[2] - offset_y)/10,
            (double) (bndy -> x[2] - offset_x)/10);
      }
      else if (bndy -> kind != GDS_PATH && bndy -> len < 290)
      {
         fprintf (fp_out, "p");
         for (i = 0; i < bndy -> len; i++)
            fprintf (fp_out, " %.0lf %.0lf",
            (double) (bndy -> y[i] - offset_y)/10,
            (double) (bndy -> x[i] - offset_x)/10);
         fprintf (fp_out, " 1\n");
      }
      else /* PATH or too many points for dwg! */
      {
         for (i = 0; i < bndy -> len - 1; i++)
         {
            if ((bndy -> y[i] - offset_y) * scale/10 > 7.5 &&
                  (bndy -> y[i+1] - offset_y) * scale/10 > 7.5)
               continue;
            if ((bndy -> y[i] - offset_y) * scale/10 < -1.0 &&
                  (bndy -> y[i+1] - offset_y) * scale/10 < -1.0)
               continue;
            if ((bndy -> x[i] - offset_x) * scale/10 > 11.0 &&
                  (bndy -> x[i+1] - offset_x) * scale/10 > 11.0)
               continue;
            if ((bndy -> x[i] - offset_x) * scale/10 < -1.0 &&
                  (bndy -> x[i+1] - offset_x) * scale/10 < -1.0)
               continue;
            fprintf (fp_out, "l %.0lf %.0lf %.0lf %.0lf %d\n",
               (double) (bndy -> y[i] - offset_y)/10,
               (double) (bndy -> x[i] - offset_x)/10,
               (double) (bndy -> y[i+1] - offset_y)/10,
               (double) (bndy -> x[i+1] - offset_x)/10,
               bndy -> kind == GDS_PATH ? 3 : 1);
         }
      }
   }
   for (text = text_head; text; text = text -> next)
   {
      fprintf (fp_out, "t %.0lf %.0lf 2 \"%s\"\n",
         (double) (text -> y - offset_y)/10,
         (double) (text -> x - offset_x)/10,
         text -> string);
   }
   if (plot_grid[0] > 0.0 && plot_grid[1] > 0.0)
   {
      gridx = plot_grid[0] * 1000 + 0.5;
      tx = minx - minx % gridx;
      if (minx < 0)
         tx -= gridx;
      gridy = plot_grid[1] * 1000 + 0.5;
      fprintf (fp_out, "f 1\n");
      for (; tx <= maxx + gridx; tx += gridx)
      {
         ty = miny - miny % gridy;
         if (miny < 0)
            ty -= gridy;
         for (; ty <= maxy + gridy; ty += gridy)
         {
            if ((ty - offset_y) * scale/10 > 7.5 ||
                  (ty - offset_y) * scale/10 < -1.0 ||
                  (tx - offset_x) * scale/10 > 11.0 ||
                  (tx - offset_x) * scale/10 < -1.0)
               continue;
            fprintf (fp_out, "r %.0lf %.0lf %.0lf %.0lf 1\n",
               ((ty - offset_y) - gridy * 0.02)/10,
               ((tx - offset_x) - gridx * 0.02)/10,
               ((ty - offset_y) + gridy * 0.02)/10,
               ((tx - offset_x) + gridx * 0.02)/10);
         }
      }
      fprintf (fp_out, "f 0\n");
   }
   dwg_scale = 0.0;
   dwg_offset_x = dwg_offset_y = 0;
}

void help (int prefix)
{
   static char *help_list[] = {
      "angle degrees ccw\n",
      "c [clear data]\n",
      "dc [drawing cell]\n",
      "di minx,miny,maxx,maxy [include rectangle]\n",
      "dl [drawing layer]\n",
      "do [drawing offset_x,offset_y]\n",
      "ds [drawing scale]\n",
      "dx minx,miny,maxx,maxy [exclude rectangle]\n",
      "f<layer> # [fill] descriptor for layer\n",
      "gd [get data for center and origin writing]\n",
      "gr [set grid plotting]\n",
      "help\n",
      "l# [list to hier level #]\n",
      "lh [list hierarchy]\n",
      "ls expression [list cells]\n",
      "ly <file> [list layers]\n",
      "lys <file> [list layers per structure]\n",
      "magnification ratio\n",
      "output filename\n",
      "pc print centers\n",
      "print\n",
      "pl layer\n",
      "pm magnitude\n",
      "quit\n",
      "rg gdsfile [read gds file]\n",
      "rm structure\n",
      "rp pcffile [read pcf file]\n",
      "rt txtfile [read txt file]\n",
      "sf write bounding box coordinates\n",
      "ss hex [set strans, see GDS for STRANS definition]\n",
      "sy command [executes command (same as sh)]\n",
      "sh command [executes command (same as sy)]\n",
      "top cellname\n",
      "wc center file [write center file]\n",
      "wd dwgfile [write dwg file]\n",
      "wf gdsfile [write flat gds file]\n",
      "wh gdsfile [write hierarchical gds file]\n",
      "wl gdsfile [write gds (lib) file of all structures]\n",
      "wo origin file  [write file of cell origins]\n",
      "wp [write postscript file (uses dwg)]\n",
      "wt txtfile [write txt file]\n",
      "x translation [nano meters]\n",
      "xd [make xwindow dwg (uses xdwg)]\n",
      "y translation [nano meters]\n",
      NULL
      };
   int i;

   for (i = 0; help_list[i]; i++)
      if (!prefix || prefix == help_list[i][0])
         fputs (help_list[i], stderr);
}

#if 0
/* is_match:  match a string to a grep like string */
int is_match (char *match, char *compare)
{
   char *s;

   if (!*match)
      return (1);
   if (*match == '^')
   {
   /* match at beginning */
      match++;
      if (!strncmp (compare, match, strlen (match)))
         return (1);
      if (*match != '[' && *match != *compare)
         return (0);
      while (*match && *compare)
      {
         if (*match == '[')
         {
            while (*match == '[')
            {
               if (!check_match (match, compare[0]))
                  return (0);
               while (*match != ']' && *match)
                  match++;
               if (*match)
                  match++;
               compare++;
            }
         }
         else if (*match != *compare)
            return (0);
         else
         {
            match++;
            compare++;
         }
      }
      if (!*compare && *match)
         return (0);
   }
   else
   {
      for (s = compare; *s && strncmp (s, match, strlen (match)); s++);
      return (*s != 0);
   }
   return (1);
}

int check_match (char *match, int character)
{
   int first, last;

   if (*match == '.')
      return (1);
   if (!*match)
      return (0);
   if (*match == '[')
   {
      ++match;
      while (*match && *match != ']')
      {
         first = *match;
         if (*match == '-')
         {
            if (*match == first)
            {
               if (character == *match)
                  return (1);
            }
            else
            {
               last = *++match;
               while (first <= last)
                  if (first == character)
                     return (1);
            }
            if (*match && *match != ']')
               match++;
            continue;
         }
         else if( *match == character)
            return (1);
         else if (*match && *match != ']')
            match++;
      }
   }
   return (0);
}
#endif

/*************************************************************************
   This function hierarchically searches a tree and adds any
   structure references to a btree (bt_list)
*************************************************************************/
typedef struct top_struct TOP;

struct top_struct {
   BTTREE bt_top;
   int leaf;
   };



void do_hier (FILE *fp_out)
{
   BTTREE bt_top;
   GDS_ITEM *item;
   char line[80];
   TOP *top;

   gds_do_convert (0);
   btinit (&bt_top, 32, sizeof (TOP), strcmp);
   top = NULL;
   gds_item_ptr = gds_item_head ();
   while ((item = gds_get_item ()))
      {
      if (item -> code == GDS_STRNAME && top)
         {
         btins (item -> data.cptr, &top -> bt_top);
         }
      else if (item -> code == GDS_SNAME)
         {
         if ((top = (TOP *) btins (item -> data.cptr, &bt_top)))
            btinit (&top -> bt_top, 32, sizeof (TOP), strcmp);
         }
      }
   btselect ("", "zzzzzzzz", &bt_top);
   while (btget (line, (void * *) &top, &bt_top))
   {
      TOP *ttop, *htop;

      btselect ("", "zzzzzz", &top -> bt_top);
      while (btget (line, (void * *) &ttop, &top -> bt_top))
         if((htop = (TOP *) btloc (line, &bt_top)))
         {
            ttop -> bt_top = htop -> bt_top;
            htop -> leaf = 1;
         }
   }
   btselect ("", "zzzzzz", &bt_top);
   while (btget (line, (void * *) &top, &bt_top))
   {
      if (!top -> leaf)
         do_print_hier (line, &top -> bt_top, 0, fp_out);
   }
   gds_do_convert (1);
}

void do_print_hier (char *line, BTTREE *tree, int lvl, FILE *fp_out)
{
   int i;
   char name[32];
   TOP *top;

   for (i = 0; i < lvl; i++)
      fputs ("  ", fp_out);
   fputs (line, fp_out);
   fputc ('\n', fp_out);
   if (tree)
   {
      btselect ("", "zzzzzz", tree);
      while (btget (name, (void * *) &top, tree))
         do_print_hier (name, &top -> bt_top, lvl + 1, fp_out);
   }
}

void write_origins (FILE *fp)
{
   GDS_ORG *org;

   for (org = gds_org_head (); org; org = org -> next)
   {
      fprintf (fp, "%-16s %8d %8d %.0lf %04X\n",
         org -> structure, org -> trans.x, org -> trans.y, org -> trans.angle,
            ((unsigned) org -> trans.strans) & 0xffff);
   }
}

void write_centers (FILE *fp)
{
   GDS_ORG *org;

   for (org = gds_org_head (); org; org = org -> next)
   {
      fprintf (fp, "%-16s %8d %8d\n",
         org -> structure, org -> cx, org -> cy);
   }
}
