/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aplot.h"
#include "archive.h"
#undef DEBUG

/*** globals and prototypes ***/
LIST *cache_names(char *filename);
int resistive_nodes=1;
char resistive_sep=':';
float NotNum = 1e20;
LIST *activefiles=NULL;
LIST *activecurves=NULL;
int   persist=0;
char *archiver=NULL;

/****************************** SORTING FUNCTIONS ******************************/

int panel_int_cmp(void *p1, void *p2)
  {
  PANEL *panel;
  int i;
  panel=*((PANEL **)p1);
  i=*((int *)p2);
  return panel->number - i;
  }

int curve_cmp(void *p1, void *p2)
  {
  CURVE *curve1 = *((CURVE **)p1), *curve2 = *((CURVE **)p2);
  int c;
  c = strcmp(curve1->file,curve2->file);
  if (c!=0) return c;
  c = strcmp(curve1->name,curve2->name);
  return c;
  }

int curve_name_cmp(void *p1, void *p2)
  {
  CURVE *curve1 = *((CURVE **)p1), *curve2 = *((CURVE **)p2);
  return strcmp(curve1->name,curve2->name);
  }

int curve_node_cmp(void *p1, void *p2)
  {
  CURVE *curve1 = *((CURVE **)p1), *curve2 = *((CURVE **)p2);
  return curve1->node - curve2->node;
  }

int curve_file_cmp(void *p1, void *p2)
  {
  CURVE *curve1 = *((CURVE **)p1), *curve2 = *((CURVE **)p2);
  return strcmp(curve1->file,curve2->file);
  }

int curve_str_cmp(void *p1, void *p2)
  {
  char *pc1=(*((CURVE **)p1))->name;
  char *pc2=*((char **)p2);
  return strcmp(pc1,pc2);
  }

int namesfile_cmp(void *p1,void *p2)
  {
  return strcmp(((NAMESFILE *)p1)->file,((NAMESFILE *)p2)->file);
  }

/****************************** ARCHIVE HANDLING ***************************/

FILE *aplot_fopen(char *path, char *mode)
  {
  return archiver ? archive_fopen(path,mode) : fopen(path,mode);
  }

time_t aplot_mtime(char *path)
  {
  if (archiver)
    {
    return archive_mtime(path);
    }
  else
    {
    struct stat get_stat;
    return stat(path,&get_stat)==0 ? get_stat.st_mtime : (time_t)-1;
    }
  }

/****************************** PARSING ******************************/

char *parse_nodename(LEX *lex)
  {
  char *str,c;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  while (1)
    {
    c=lex_char(lex);
    if ((c==' ') || (c=='\t') || (c=='\n') || (c=='=') || (c==';') || 
        (c=='<') || (c=='>') || (c==EOF)) break;
    lex_eat_char(lex);
    }
  str=leak_strdup(lex_get_token(lex));
  lex_pop_position(lex);
  return str;
  }

void parse_range(LEX *lex, float *min, float *max)
  {
  lex_eatif_sym(lex,"[");
  if (lex_is_real(lex)) *min=lex_eat_real(lex); else *min=NotNum;
  lex_eatif_sym(lex,":");
  if (lex_is_real(lex)) *max=lex_eat_real(lex); else *max=NotNum;
  lex_eatif_sym(lex,"]");
  if ((*min!=NotNum) && (*max!=NotNum) && (*min>*max))
    {double t = *min; *min=*max; *max=t; } // sort min/max
  } 

/****************************** CURVE OPERATIONS ******************************/

CURVE *create_curve(char *file, char *name)
  {
  int k;
  CURVE *curve;
  curve = leak_malloc(sizeof(CURVE));
  curve->t=NULL;
  curve->v=NULL;
  curve->Nsteps=0;
  curve->timestamp=0;
  curve->node=-1;
  curve->name=name;
  curve->is_subnet=0;
  for (k=0; k<strlen(name); k++)
    if (name[k]==resistive_sep) curve->is_subnet=1;
  curve->file=leak_strdup(file);
  curve->alias=NULL;
  curve->subnet=NULL;
  return curve;
  }

void free_curve(CURVE *curve)
  {
  if (curve->file!=NULL) leak_free(curve->file);
  if (curve->name!=NULL) leak_free(curve->name);
  if (curve->t!=NULL) {leak_free(curve->t); curve->t=NULL;}
  if (curve->v!=NULL) {leak_free(curve->v); curve->v=NULL;}
  leak_free(curve);
  }

void add_curve_unique(PANEL *panel, LIST *named, CURVE *curve)
  {
  if (find_element_lazy_sort(named,&curve,&curve_node_cmp)>=0) return;
  list_append_element(panel->curves,&curve);
  list_insert_element_lazy_sort(named,&curve,&curve_node_cmp);
  }

void add_curve(PANEL *panel, char *name)
  {
  LIST *names;
  CURVE *curve,*subnet;
  int j=-1;
  NAMED named;

  /*** add a new curve ***/
  names=cache_names(panel->file);
  if (names!=NULL) j=find_element_sorted(names,&name,&curve_str_cmp);
  if (j<0)
    {
    printf("WARNING: Curve %s not found in file %s.\n",name,panel->file);
    return;
    }
  curve = names->p.curve[j];
  named.curve=curve;
  named.curves=list_create(sizeof(CURVE *));
  add_curve_unique(panel,named.curves,curve);
  
  /*** add more curves for resistively connected nodes ***/
  if (resistive_nodes)
    {
    // add all subnets
    subnet=curve->subnet;
    while (subnet!=curve)
      {
      add_curve_unique(panel,named.curves,subnet);
      subnet=subnet->subnet;
      }
    }

  // add a new named curve set to panel
  list_append_element(panel->named,&named);
  }

void add_glob_curve(PANEL *panel, char *name, int glob_ok)
  {
  int j;
  LIST *names;
  char fullname[STRMAX];
  CURVE *curve;
  safe_sprintf(fullname,"%s%s",panel->prefix,name);
  if (glob_ok && is_glob(name))
    {
    names=cache_names(panel->file);
    if (names==NULL) return;
    for (j=0; j<names->max; j++)
      {
      curve=names->p.curve[j];
      if (glob_matches(fullname,curve->name) && !curve->is_subnet)
        add_curve(panel,curve->name);
      }
    }
  else add_curve(panel,fullname);
  }

void delete_glob_curve(PANEL *panel, char *name)
  {
  int j;
  char fullname[STRMAX];
  CURVE *curve;
  
  // delete matching named curves only
  safe_sprintf(fullname,"%s%s",panel->prefix,name);
  for (j=0; j<panel->named->max; j++)
    {
    curve=panel->named->p.named[j].curve;
    if (glob_matches(fullname,curve->name))
      {
      list_free(panel->named->p.named[j].curves);
      list_remove_element(panel->named,j);
      j--;
      }
    }
  
  // rebuild complete list of curves
  list_realloc(panel->curves,0);
  for (j=0; j<panel->named->max; j++)
    list_append_list(panel->curves,panel->named->p.named[j].curves);
  }

/****************************** NAMES OPERATIONS ******************************/

/*** concatenate subnet rings of curves a and b ***/
void splice_subnets(CURVE *a, CURVE *b)
  {
  CURVE *x,*y;
  // follow subnet pointers from a to a or b
  x = a;
  while(1)
    {
    if (x->subnet==b) return; // already spliced
    if (x->subnet==a) break; // back to a
    x = x->subnet;
    }
  y = b;
  while(1)
    {
    if (y->subnet==b) break; // back to b
    y = y->subnet;
    }
  x->subnet = b;
  y->subnet = a;
  }

/*** read all the names from a names file ***/
LIST *get_names(char *filename)
  {
  FILE *namesfile;
  char fullfilename[STRMAX],prefix[STRMAX];
  CURVE *curve,*alias,*last,*subnet;
  LIST *names;
  LEX *lex;
  int node=0,j,k,len;

  /*** create curves, link aliases ***/
  safe_sprintf(fullfilename,"%s.names",filename);
  namesfile=aplot_fopen(fullfilename,"r");
  if (namesfile==NULL) return NULL;
  bigbuffer(namesfile);
  lex=lex_file(namesfile);
  names=list_create(sizeof(CURVE *));
  while (!lex_is_eof(lex))
    {
    curve = create_curve(filename,parse_nodename(lex));
    curve->node = node;
    list_append_element(names,&curve);
    alias = curve;
    while (lex_eatif_sym(lex,"="))
      {
      last = alias;
      alias = create_curve(filename,parse_nodename(lex));
      alias->node = node;
      alias->alias = last;
      list_append_element(names,&alias);
      }
    curve->alias = alias;
    node++;
    }
  lex_free(lex);
  fclose(namesfile);

  /*** link subnets by prefix: first ***/
  list_sort(names,&curve_name_cmp);
  curve=last=NULL;
  prefix[0]=0;
  len=0;
  for (j=0; j<names->max; j++)
    {
    subnet = names->p.curve[j];
    if ((curve==NULL) || (strncmp(subnet->name,prefix,len)!=0))
      {
      // first curve of a new subnet
      curve = subnet;
      curve->subnet = curve;
      for (k=0; k<strlen(curve->name); k++)
        {
        if (curve->name[k]==resistive_sep) break;
        else prefix[k]=curve->name[k];
        }
      prefix[k]=resistive_sep;
      prefix[k+1]=0;
      len=k+1;
      }
    else
      {
      // link next subnet of curve
      subnet->subnet = last;
      curve->subnet = subnet;
      }
    last = subnet;
    }

  /*** now link subnets across aliases ***/
  for (j=0; j<names->max; j++)
    {
    curve = names->p.curve[j];
    if (curve->alias!=curve) splice_subnets(curve,curve->alias);
    }

#ifdef DEBUG
  /*** debugging ***/
  for (j=0; j<names->max; j++)
    {
    curve = names->p.curve[j];
    fprintf(stderr,"curve=%s\n alias=%s\n subnet=%s\n\n",
            curve->name,curve->alias->name,curve->subnet->name);
    }
#endif

  return names;
  }

/*** free all names associated with a names file ***/
void free_names(LIST *names)
  {
  int i;
  for (i=0; i<names->max; i++) free_curve(names->p.curve[i]);
  list_free(names);
  }

/*** load and/or cache a namesfile ***/
LIST *cache_names(char *filename)
  {
  NAMESFILE namesfile,*pnamesfile=NULL;
  char fullfilename[STRMAX];
  time_t mtime;
  int k;

  safe_sprintf(fullfilename,"%s.names",filename);
  if ((mtime=aplot_mtime(fullfilename))==(time_t)-1) return NULL;
  if (activefiles==NULL) activefiles=list_create(sizeof(NAMESFILE));
  namesfile.file=filename;
  k=find_element(activefiles,&namesfile,&namesfile_cmp);
  if (k>=0)
    {
    pnamesfile=&activefiles->p.namesfile[k];
    if (mtime!=pnamesfile->timestamp)
      {
#ifdef DEBUG
      fprintf(stderr,"Removing out-of-date names for %s\n",filename);
#endif
      free_names(pnamesfile->names);
      if (activecurves!=NULL) list_free(activecurves); // flush curve cache too
      activecurves=NULL;
      leak_free(pnamesfile->file);
      list_remove_element(activefiles,k);
      k=-1;
      }
    }
  if (k<0)
    {
#ifdef DEBUG
    fprintf(stderr,"Loading names for %s\n",filename);
#endif
    namesfile.names=get_names(filename);
    if (namesfile.names==NULL) return NULL;
    namesfile.file=leak_strdup(filename);
    namesfile.timestamp=mtime;
    list_append_element(activefiles,&namesfile);
    pnamesfile=&activefiles->p.namesfile[activefiles->max-1];
    }
  return pnamesfile->names;
  }


/****************************** DATA HANDLING ******************************/

void get_data(LIST *curves)
  {
  int i,j,order,Nnodes,Nsteps,timestamp;
  FILE *tracefile=NULL;
  char filename[STRMAX];
  CURVE *curve,*lastcurve,*lasttime;

  /*** create curve cache ***/
  if (activecurves==NULL) activecurves=list_create(sizeof(CURVE *));

  /*** read all curve data ***/
  for (i=0; i<curves->max; i++)
    {
    curve=curves->p.curve[i];

    /*** read header in order to check Nnodes, Nsteps ***/
    safe_sprintf(filename,"%s.trace",curve->file);
    if (tracefile!=NULL) fclose(tracefile); // close previous tracefile
    tracefile=aplot_fopen(filename,"rb");
    if (tracefile==NULL)
      {
      printf("WARNING: can't read tracefile %s.\n",filename);
      continue;
      }
    else
      {
      bigbuffer(tracefile);
      if (!get_header(tracefile,&timestamp,&Nnodes,&Nsteps,&order))
        {
        printf("WARNING: bad or reordering %s.\n",filename);
        continue;
        }
#ifdef DEBUG
      fprintf(stderr,"Timestamp of %s is %d, Nsteps is %d\n",filename,timestamp,Nsteps);
#endif
      }

    /*** reuse current data ***/
    if ((curve->t!=NULL)&&(curve->v!=NULL)&&
        (curve->Nsteps==Nsteps)&&(curve->timestamp==timestamp))
      {
#ifdef DEBUG
      fprintf(stderr,"Up-to-date for %s %s\n",curve->file,curve->name);
#endif
      continue;
      }

    /*** remove out-of-date curves for this file, find a cached curve by file only ***/
    lasttime=NULL;
    for (j=0; j<activecurves->max; j++)
      {
      if (curve_file_cmp(&curve,&activecurves->p.curve[j])!=0) continue;
      lasttime=activecurves->p.curve[j];
      if ((lasttime->Nsteps!=Nsteps)||(lasttime->timestamp!=timestamp))
        {
#ifdef DEBUG
        fprintf(stderr,"Removing out-of-date %s %s\n",lasttime->file,lasttime->name);
#endif
        list_remove_element(activecurves,j);
        j--;
        lasttime=NULL;
        }
      }

    /*** update curve ***/
    if (curve->t!=NULL) {leak_free(curve->t); curve->t=NULL;}
    if (curve->v!=NULL) {leak_free(curve->v); curve->v=NULL;}
    curve->Nsteps=Nsteps;
    curve->timestamp=timestamp;
    curve->t=(float *) leak_malloc(sizeof(float)*Nsteps);
    curve->v=(float *) leak_malloc(sizeof(float)*Nsteps);

    /*** reuse or read time ***/
    if (lasttime!=NULL)
      {
#ifdef DEBUG
      fprintf(stderr,"Reusing time from %s %s for %s %s\n",
              lasttime->file,lasttime->name,curve->file,curve->name);
#endif
      memcpy(curve->t,lasttime->t,Nsteps*sizeof(float));
      }
    else
      {
#ifdef DEBUG
      fprintf(stderr,"Updating time for %s %s\n",curve->file,curve->name);
#endif
      get_node_values(tracefile,0,Nnodes,Nsteps,order,curve->t);
      }

    /*** find a cached curve by file and name ***/
    j=find_element_sorted(activecurves,&curve,&curve_cmp);
    if (j>=0) lastcurve=activecurves->p.curve[j];
    else      lastcurve=NULL;

    /*** reuse or read voltage ***/
    if (lastcurve!=NULL)
      {
#ifdef DEBUG
      fprintf(stderr,"Reusing data from %s %s for %s %s\n",
              lastcurve->file,lastcurve->name,curve->file,curve->name);
#endif
      memcpy(curve->v,lastcurve->v,Nsteps*sizeof(float));
      }
    else
      {
#ifdef DEBUG
      fprintf(stderr,"Updating data for %s %s\n",curve->file,curve->name);
#endif
      get_node_values(tracefile,curve->node,Nnodes,Nsteps,order,curve->v);
      }

    /*** add curve to activecurves ***/
    if ((lasttime==NULL)||(lastcurve==NULL))
      list_insert_element_sorted(activecurves,&curve,&curve_cmp);
    }

  /*** close last tracefile ***/
  if (tracefile!=NULL) fclose(tracefile);
  }

/****************************** PANEL OPERATIONS ******************************/

PANEL *create_panel(int number)
  {
  PANEL *panel;
  panel=leak_malloc(sizeof(PANEL));
  panel->tmin=panel->tmax=panel->vmin=panel->vmax=NotNum;
  panel->number=number;
  panel->file=leak_strdup("");
  panel->prefix=leak_strdup("");
  panel->curves=list_create(sizeof(CURVE *));
  panel->named=list_create(sizeof(NAMED));
  panel->gnuplot=NULL;
  if (number>=0)
    {
    if (persist)
        panel->gnuplot=popen("gnuplot -persist","w");
    else
        panel->gnuplot=popen("gnuplot","w");
    if (panel->gnuplot==NULL) {fprintf(stderr,"ERROR: Can't open gnuplot back end.\n"); exit(1);}
    fprintf(panel->gnuplot,"set data style lines\n"
            "set title \"Panel %d\"\n",number);
    }
  return panel;
  }

PANEL *clone_panel(int number, PANEL *lastpanel)
  {
  PANEL *panel;
  panel = create_panel(number);
  leak_free(panel->file);
  leak_free(panel->prefix);
  panel->file=leak_strdup(lastpanel->file);
  panel->prefix=leak_strdup(lastpanel->prefix);
  return panel;
  }

PANEL *measure_panel(char *name, PANEL *panel)
  {
  PANEL *measure;
  measure = clone_panel(-1,panel);
  measure->tmin = panel->tmin;
  measure->tmax = panel->tmax;
  measure->vmin = panel->vmin;
  measure->vmax = panel->vmax;
  add_glob_curve(measure,name,0);
  get_data(measure->curves);
  return measure;
  }

void close_panel(PANEL *panel)
  {
  int j;
  list_free(panel->curves);
  for (j=0; j<panel->named->max; j++)
    list_free(panel->named->p.named[j].curves);
  list_free(panel->named);
  leak_free(panel->file);
  leak_free(panel->prefix);
  if (panel->gnuplot!=NULL)
    {
    fprintf(panel->gnuplot,"quit\n");
    fflush(panel->gnuplot);
    pclose(panel->gnuplot);
    }
  leak_free(panel);
  }

void clear_panel(PANEL *panel)
  {
  int j;
  list_realloc(panel->curves,0);
  for (j=0; j<panel->named->max; j++)
    list_free(panel->named->p.named[j].curves);
  list_realloc(panel->named,0);
  }

void write_panel(PANEL *panel)
  {
  int i,j,k;
  FILE *fifo=NULL;
  char **fifoname;
  CURVE *curve;

  /*** empty panel ***/
  if (panel->named->max==0) return;

  /*** get the data ***/
  get_data(panel->curves);

  /*** create temporary fifos for named curves ***/
  fifoname=(char **) leak_malloc(panel->named->max*sizeof(char *));
  for (i=0; i<panel->named->max; i++)
    {
      char *s;
      fifoname[i]=malloc(strlen(panel->named->p.named[i].curve->name)+
        strlen(panel->file)+8);
      if ((s=strrchr(panel->file, '/')))
        strcpy (fifoname[i], s+1);
      else
        strcpy(fifoname[i],panel->file);
      strcat(fifoname[i], ":");
      strcat(fifoname[i], panel->named->p.named[i].curve->name);
      strcat(fifoname[i], ".pwl");
      while ((s=strchr(fifoname[i], '|')))
        strcpy (s, s+1);
    }

  /*** plot all curve data ***/
  for (i=0; i<panel->named->max; i++)
    {
    fifo=fopen(fifoname[i],"w");
    if (fifo==NULL) {fprintf(stderr,"ERROR: can't open %s.\n",fifoname[i]); exit(1);}
    for (j=0; j<panel->named->p.named[i].curves->max; j++)
      {
      curve=panel->named->p.named[i].curves->p.curve[j];
      if ((curve->t==NULL)||(curve->v==NULL)) fprintf(fifo,"0 0\n");
      else for (k=0; k<curve->Nsteps; k++)
        fprintf(fifo,"%0.6f %0.6f\n",curve->t[k]*1e9,curve->v[k]);
      fprintf(fifo,"\n");
      }
    fclose(fifo);
    }

  /*** free stuff ***/
  for (i=0; i<panel->named->max; i++)
    {
    free(fifoname[i]); /* allocated by tempnam */
    }
  leak_free(fifoname);
  }

void draw_panel(PANEL *panel)
  {
  int i,j,k;
  FILE *fifo=NULL;
  char **fifoname;
  CURVE *curve;

  /*** empty panel ***/
  if (panel->named->max==0) return;

  /*** get the data ***/
  get_data(panel->curves);

  /*** create temporary fifos for named curves ***/
  fifoname=(char **) leak_malloc(panel->named->max*sizeof(char *));
  for (i=0; i<panel->named->max; i++)
    {
    fifoname[i]=tempnam(NULL,"aplot.");
    mkfifo(fifoname[i],S_IRWXU);
    }

  /*** send commands to gnuplot ***/
  fprintf(panel->gnuplot,"plot [");
  if (panel->tmin!=NotNum) fprintf(panel->gnuplot,"%g",panel->tmin);
  fprintf(panel->gnuplot,":");
  if (panel->tmax!=NotNum) fprintf(panel->gnuplot,"%g",panel->tmax);
  fprintf(panel->gnuplot,"][");
  if (panel->vmin!=NotNum) fprintf(panel->gnuplot,"%g",panel->vmin);
  fprintf(panel->gnuplot,":");
  if (panel->vmax!=NotNum) fprintf(panel->gnuplot,"%g",panel->vmax);
  fprintf(panel->gnuplot,"] ");
  for (i=0; i<panel->named->max; i++)
    fprintf(panel->gnuplot,"%s\"%s\" title \"%s\"",(i>0 ? "," : ""),
          fifoname[i],panel->named->p.named[i].curve->name);
  
  fprintf(panel->gnuplot,"\n");
  fflush(panel->gnuplot);

  /*** plot all curve data ***/
  for (i=0; i<panel->named->max; i++)
    {
    fifo=fopen(fifoname[i],"w");
    if (fifo==NULL) {fprintf(stderr,"ERROR: can't open %s.\n",fifoname[i]); exit(1);}
    for (j=0; j<panel->named->p.named[i].curves->max; j++)
      {
      curve=panel->named->p.named[i].curves->p.curve[j];
      if ((curve->t==NULL)||(curve->v==NULL)) fprintf(fifo,"0 0\n");
      else for (k=0; k<curve->Nsteps; k++)
        fprintf(fifo,"%0.6f %0.6f\n",curve->t[k]*1e9,curve->v[k]);
      fprintf(fifo,"\n");
      }
    fclose(fifo);
    }

  /*** free stuff ***/
  for (i=0; i<panel->named->max; i++)
    {
    unlink(fifoname[i]);
    free(fifoname[i]); /* allocated by tempnam */
    }
  leak_free(fifoname);
  }

/****************************** MEASUREMENTS ******************************/

/*** get nominal, min, and max voltages over a set of curves ***/
void get_v_fancy(PANEL *panel, int step, float *nom_v, float *min_v, float *max_v,
           int *min_j, int *max_j)
  {
  int j;
  float v,minv,maxv;
  minv=maxv=panel->curves->p.curve[0]->v[step];
  *min_j=*max_j=0;
  for (j=1; j<panel->curves->max; j++)
    {
    v=panel->curves->p.curve[j]->v[step];
    if (v<minv) { minv=v; *min_j=j; }
    if (v>maxv) { maxv=v; *max_j=j; }
    }
  if (nom_v!=NULL) *nom_v=(minv+maxv)/2;
  if (min_v!=NULL) *min_v=minv;
  if (max_v!=NULL) *max_v=maxv;
  }

void get_v(PANEL *panel, int step, float *nom_v, float *min_v, float *max_v)
  {
  int min_j, max_j;
  get_v_fancy(panel,step,nom_v,min_v,max_v,&min_j,&max_j);
  }

void trigger(LIST *panels, int dir, float value,
             int occurance1, int occurance2, int min_nom_max)
  {
  int i,step,count=0,got1=0,got2=0,done;
  float t,v,ot,ov,trig1=0,trig2=0,mid_t;
  float nomv,minv,maxv;
  PANEL *panel;
  CURVE *curve;

  /*** scan for trigger points ***/
  for (step=1, done=0; !done; step++)
    {
    for (i=0; i<panels->max; i++)
      {
      // get next panel and curve
      panel = panels->p.panel[i];
      if (panel->curves->max==0) {done=1; break;}
      curve = panel->curves->p.curve[0];
      if (step>=curve->Nsteps) {done=1; break;}

      // get last time and voltage
      ot=curve->t[step-1]*1e9;
      if ((ot<panel->tmin) && (panel->tmin!=NotNum)) continue;
      get_v(panel,step-1,&nomv,&minv,&maxv);
      if      (min_nom_max<0) ov = minv;
      else if (min_nom_max>0) ov = maxv;
      else                    ov = nomv;

      // get current time and voltage
      t=curve->t[step]*1e9;
      if ((t>panel->tmax) && (panel->tmax!=NotNum)) continue;
      get_v(panel,step,&nomv,&minv,&maxv);
      if      (min_nom_max<0) v = minv;
      else if (min_nom_max>0) v = maxv;
      else                    v = nomv;

      // look for trigger
      if (((dir>=0)&&(v>=value)&&(ov<value)) || 
          ((dir<=0)&&(v<=value)&&(ov>value)))
        {
        count++;
        mid_t=(value-ov)*(t-ot)/(v-ov)+ot;
        if (count==occurance1) {trig1=mid_t; got1=1;}
        if (count==occurance2) {trig2=mid_t; got2=1;}
        }
      }
    }
  if ((occurance1<0)&&(occurance2<0)) printf("%d\n",count);
  else if (got1&&got2)
    printf("%g\n",(occurance2-occurance1)/(trig2-trig1));
  else if (got1&&(occurance2<0)) printf("%g\n",trig1);
  else printf("WARNING: no trigger\n");
  }

void print(PANEL *panel)
  {
  CURVE *curve;
  float t,v;
  int i,j;
  for (i=0; i<panel->curves->max; i++)
    {
    curve = panel->curves->p.curve[i];
    if (i>0) printf("\n");
    for (j=0; j<curve->Nsteps; j++)
      {
      t=curve->t[j]*1e9;
      v=curve->v[j];
      if ((t<panel->tmin)&&(panel->tmin!=NotNum)) continue;
      if ((t>panel->tmax)&&(panel->tmax!=NotNum)) continue;
      printf("%0.6f %0.6f\n",t,v);
      }
    }
  }

void minmaxv(PANEL *panel, int domin)
  {
  CURVE *curve;
  float t,minv,maxv,min_v=0,min_t=0,max_v=0,max_t=0;
  int j,minj,maxj,min_j=-1,max_j=-1;

  /*** get first curve ***/
  if (panel->curves->max==0) return;
  curve=panel->curves->p.curve[0];

  /*** min/max up all curves over all timesteps ***/
  for (j=0; j<curve->Nsteps; j++)
    {
    t=curve->t[j]*1e9;
    if ((t<panel->tmin)&&(panel->tmin!=NotNum)) continue;
    if ((t>panel->tmax)&&(panel->tmax!=NotNum)) continue;
    get_v_fancy(panel,j,NULL,&minv,&maxv,&minj,&maxj);
    if ((min_j<0) || (minv<min_v)) {min_j = minj; min_v = minv; min_t = t;}
    if ((max_j<0) || (maxv>max_v)) {max_j = maxj; max_v = maxv; max_t = t;}
    }
  if (domin)
    {
    if (min_j>=0) printf("%g %g %s\n",min_t,min_v,panel->curves->p.curve[min_j]->name);
    else printf("WARNING: no trigger\n");
    }
  else
    {
    if (max_j>=0) printf("%g %g %s\n",max_t,max_v,panel->curves->p.curve[max_j]->name);
    else printf("WARNING: no trigger\n");
    }
  }

void integrate(PANEL *panel, int dorms)
  {
  CURVE *curve;
  float avg=0,rms=0,v0,v1,t0,t1,tmin,tmax;
  int j;

  /*** get first curve ***/
  if (panel->curves->max==0) return;
  curve=panel->curves->p.curve[0];
  if (curve->Nsteps<2) { printf("WARNING: not enough samples\n"); return; }

  /*** get tmin/tmax range ***/
  if (panel->tmin!=NotNum) tmin=panel->tmin;
  else tmin=curve->t[0]*1e9; // first sample
  if (panel->tmax!=NotNum) tmax=panel->tmax;
  else tmax=curve->t[curve->Nsteps-1]*1e9; // last sample

  /*** integrate over time range ***/
  for (j=0; j<curve->Nsteps-1; j++)
    {
    t0=curve->t[j+0]*1e9;
    t1=curve->t[j+1]*1e9;
    if (t1<tmin) continue;
    if (t0>tmax) continue;
    get_v(panel,j+0,&v0,NULL,NULL);
    get_v(panel,j+1,&v1,NULL,NULL);

    // clip to tmin/tmax range by linear interpolation
    if ((t0<tmin) && (t1>=tmin))
      {
      v0 = v0 + (v1-v0) * (tmin-t0) / (t1-t0);
      t0 = panel->tmin;
      }
    if ((t0<=tmax) && (t1>tmax))
      {
      v1 = v0 + (v1-v0) * (tmax-t0) / (t1-t0);
      t1 = tmax;
      }

    // accumulate average or squared value assuming linear interpolation
    if (tmin>=tmax) avg = 0.5 * (v0+v1); // degenerate range
    else
      {
      avg += 0.5 * (v0+v1) * (t1-t0);
      rms += (v0*v1 + (v1-v0)*(v1-v0)/3) * (t1-t0);
      }
    }

  /*** report results ***/
  if (tmin>=tmax) printf("0 %g\n",avg); // degenerate range
  else if (dorms)
    {
    rms = sqrt(rms/(tmax - tmin)); // root mean
    printf("%g %g\n",tmax-tmin,rms);
    }
  else
    {
    avg = avg/(tmax - tmin); // mean
    printf("%g %g\n",tmax-tmin,avg);
    }
  }

void skew(PANEL *panel, float value)
  {
  int i,o_valid,min_trig_valid,max_trig_valid,skew_valid=0;
  CURVE *curve;
  float t,ot,min_v,max_v,o_min_v,o_max_v,min_trig_t,max_trig_t;
  float skew,worst_skew,worst_t,mid_t;

  /*** get first curve ***/
  if (panel->curves->max==0) return;
  curve=panel->curves->p.curve[0];

  /*** scan for trigger points ***/
  o_valid=min_trig_valid=max_trig_valid=0;
  o_min_v=o_max_v=min_trig_t=max_trig_t=ot=0;
  worst_skew=worst_t=0;
  for (i=0; i<curve->Nsteps; i++)
    {
    // get time, min_v, max_v
    t=curve->t[i]*1e9;
    if ((t<panel->tmin)&&(panel->tmin!=NotNum)) continue;
    if ((t>panel->tmax)&&(panel->tmax!=NotNum)) continue;
    get_v(panel,i,NULL,&min_v,&max_v);

    if (o_valid)
      {
      // minv falling sets trigger
      if ((min_v<=value)&&(o_min_v>value)) 
        {
        mid_t=(value-o_min_v)*(t-ot)/(min_v-o_min_v)+ot;
        min_trig_t=mid_t;
        min_trig_valid=1;
        }

      // maxv rising sets trigger
      if ((max_v>=value)&&(o_max_v<value))
        {
        mid_t=(value-o_max_v)*(t-ot)/(max_v-o_max_v)+ot;
        max_trig_t=mid_t;
        max_trig_valid=1;
        }

      // minv rising checks skew
      if (max_trig_valid&&(min_v>=value)&&(o_min_v<value))
        {
        mid_t=(value-o_min_v)*(t-ot)/(min_v-o_min_v)+ot;
        skew=mid_t-max_trig_t;
        if (!skew_valid || (skew>worst_skew)) 
          {skew_valid=1; worst_skew=skew; worst_t=mid_t;}
        max_trig_valid=0;
        }

      // maxv falling checks skew
      if (min_trig_valid&&(max_v<=value)&&(o_max_v>value))
        {
        mid_t=(value-o_max_v)*(t-ot)/(max_v-o_max_v)+ot;
        skew=mid_t-min_trig_t;
        if (!skew_valid || (skew>worst_skew))
          {skew_valid=1; worst_skew=skew; worst_t=mid_t;}
        min_trig_valid=0;
        }
      }

    // store old values
    ot=t;
    o_max_v=max_v;
    o_min_v=min_v;
    o_valid=1;
    }

  // results
  if (skew_valid) printf("%g %g\n",worst_t,worst_skew);
  else printf("WARNING: no trigger\n");
  }

void minmaxdelay(LIST *trig_panels, LIST *targ_panels,
                 int domin,
                 int trig_dir, int targ_dir,
                 float trig_thresh, float targ_thresh)
  {
  int step,done,i,trig_valid=0,min_valid=0,max_valid=0;
  float mid_t,ot,t,trig_t=0;
  float nomv,minv,maxv,onomv,ominv,omaxv,v,ov;
  float delay,min_t=0,max_t=0,min_delay=1e10,max_delay=-1e10;
  PANEL *panel;
  CURVE *curve;

  /*** scan for trigger points ***/
  for (step=1, done=0; !done; step++)
    {
    // check for any triggers
    for (i=0; i<trig_panels->max; i++)
      {
      // get trigger panel and curve
      panel = trig_panels->p.panel[i];
      if (panel->curves->max==0) {done=1; break;}
      curve = panel->curves->p.curve[0];
      if (step>=curve->Nsteps) {done=1; break;}

      // get last time and voltage
      ot=curve->t[step-1]*1e9;
      if ((ot<panel->tmin) && (panel->tmin!=NotNum)) continue;
      get_v(panel,step-1,&onomv,&ominv,&omaxv);

      // get current time and voltage
      t=curve->t[step]*1e9;
      if ((t>panel->tmax) && (panel->tmax!=NotNum)) continue;
      get_v(panel,step,&nomv,&minv,&maxv);

      // choose which resistive curve to use for trigger
      if (domin) // minimum delays -- trigger on leading edge
        {
        if      (trig_dir<0) {v = minv; ov = ominv;}
        else if (trig_dir>0) {v = maxv; ov = omaxv;}
        else                 {v = nomv; ov = onomv;}
        }
      else // maximum delays -- trigger on trailing edge
        {
        if      (trig_dir<0) {v = maxv; ov = omaxv;}
        else if (trig_dir>0) {v = minv; ov = ominv;}
        else                 {v = nomv; ov = onomv;}
        }

      // set trigger
      if ((trig_dir>=0)&&(v>=trig_thresh)&&(ov<trig_thresh))
        {
        mid_t=(trig_thresh-ov)*(t-ot)/(v-ov)+ot;
        trig_t=mid_t;
        trig_valid=1;
        }
      else if ((trig_dir<=0)&&(v<=trig_thresh)&&(ov>trig_thresh))
        {
        mid_t=(trig_thresh-ov)*(t-ot)/(v-ov)+ot;
        trig_t=mid_t;
        trig_valid=1;
        }
      }

    // check for any targets
    for (i=0; i<targ_panels->max; i++)
      {
      // get target panel and curve
      panel = targ_panels->p.panel[i];
      if (panel->curves->max==0) {done=1; break;}
      curve = panel->curves->p.curve[0];
      if (step>=curve->Nsteps) {done=1; break;}

      // get last time and voltage
      ot=curve->t[step-1]*1e9;
      if ((ot<panel->tmin) && (panel->tmin!=NotNum)) continue;
      get_v(panel,step-1,&onomv,&ominv,&omaxv);

      // get current time and voltage
      t=curve->t[step]*1e9;
      if ((t>panel->tmax) && (panel->tmax!=NotNum)) continue;
      get_v(panel,step,&nomv,&minv,&maxv);

      // choose which resistive curve to use for target
      if (domin) // minimum delays -- target on leading edge
        {
        if      (targ_dir<0) {v = minv; ov = ominv;}
        else if (targ_dir>0) {v = maxv; ov = omaxv;}
        else                 {v = nomv; ov = onomv;}
        }
      else // maximum delays -- target on trailing edge
        {
        if      (targ_dir<0) {v = maxv; ov = omaxv;}
        else if (targ_dir>0) {v = minv; ov = ominv;}
        else                 {v = nomv; ov = onomv;}
        }

      // fire trigger on target, measure delay
      if (trig_valid&&
          (targ_dir>=0)&&(v>=targ_thresh)&&(ov<targ_thresh))
        {
        mid_t=(targ_thresh-ov)*(t-ot)/(v-ov)+ot;
        delay=mid_t-trig_t;
        if (delay>0) // make sure target follows trigger
          {
          if ((!min_valid)||(delay<min_delay))
            {min_delay=delay; min_t=mid_t; min_valid=1;}
          if ((!max_valid)||(delay>max_delay))
            {max_delay=delay; max_t=mid_t; max_valid=1;}
          trig_valid=0;
          }
        }
      else if (trig_valid&&
               (targ_dir<=0)&&(v<=targ_thresh)&&(ov>targ_thresh))
        {
        mid_t=(targ_thresh-ov)*(t-ot)/(v-ov)+ot;
        delay=mid_t-trig_t;
        if (delay>0) // make sure target follows trigger
          {
          if ((!min_valid)||(delay<min_delay))
            {min_delay=delay; min_t=mid_t; min_valid=1;}
          if ((!max_valid)||(delay>max_delay))
            {max_delay=delay; max_t=mid_t; max_valid=1;}
          trig_valid=0;
          }
        }
      }
    }

  // results
  if (domin)
    {
    if (min_valid) printf("%g %g\n",min_t,min_delay);
    else printf("WARNING: no trigger\n");
    }
  else
    {
    if (max_valid) printf("%g %g\n",max_t,max_delay);
    else printf("WARNING: no trigger\n");
    }               
  }

/**************************** MAIN COMMAND LINE INTERFACE ****************************/

void free_caches()
  {
  int i;
  if (activefiles!=NULL) 
    {
    for (i=0; i<activefiles->max; i++) 
      {
      NAMESFILE n;
      n=activefiles->p.namesfile[i];
      free_names(n.names);
      leak_free(n.file);
      }
    list_free(activefiles);
    }
  if (activecurves!=NULL) list_free(activecurves);
  activefiles=NULL;
  activecurves=NULL;
  }

void append_measure_panel(LIST *panels, PANEL *panel, char *name)
  {
  PANEL *measure;
  measure = clone_panel(-1,panel);
  measure->tmin = panel->tmin;
  measure->tmax = panel->tmax;
  measure->vmin = panel->vmin;
  measure->vmax = panel->vmax;
  add_curve(measure,name);
  get_data(measure->curves);
  list_append_element(panels,&measure);
  }

/*** create several measure panels from a list of globbed nodes ***/
LIST *create_measure_panels(PANEL *panel, LEX *lex)
  {
  int j;
  LIST *names;
  LIST *panels;
  CURVE *curve;
  char *name;
  char fullname[STRMAX];
  panels=list_create(sizeof(PANEL *));
  while (1)
    {
    name=parse_nodename(lex);
    if (name[0]==0) {leak_free(name); break;}
    safe_sprintf(fullname,"%s%s",panel->prefix,name);
    if (is_glob(name))
      {
      names=cache_names(panel->file);
      if (names==NULL) return NULL;
      for (j=0; j<names->max; j++)
        {
        curve=names->p.curve[j];
        if (glob_matches(fullname,curve->name) && !curve->is_subnet)
          append_measure_panel(panels,panel,curve->name);
        }
      }
    else append_measure_panel(panels,panel,fullname);
    leak_free(name);
    }
  return panels;
  }

/*** free a list of measure panels ***/
void free_measure_panels(LIST *panels)
  {
  int i;
  for (i=0; i<panels->max; i++)
    close_panel(panels->p.panel[i]);
  list_free(panels);
  }

/*** parse a trigger direction ***/
int parse_direction(LEX *lex)
  {
  if      (lex_eatif_sym(lex,">")) return +1;
  else if (lex_eatif_sym(lex,"<")) return -1;
  else if (lex_eatif_sym(lex,"=")) return  0;
  return 2;
  }

/*********************** command usage and help ****************************/

char *command[][3]=
  { {"help","[[command | all]]",
     "List all commands.  If a command is specified, give detailed help on\n"
     "that command.  \"Help all\" gives detailed help on all commands."},
    {"file","\"filename\"",
     "Changes aspice source data file.  Omit the .trace/.names extension.\n"
     "Can also specify the file as a command line argument."},
    {"with","[prefix]",
     "Adds an instance prefix to subsequent node names.  Include the last\n"
     "\".\".  If no prefix is specified, clears the prefix."},
    {"append","moreprefix",
     "Adds a suffix to the default prefix."},
    {"up","",
     "Removes the trailing \".\" delimited field from the prefix."},
    {"resistive","[on|off]",
     "Enables or disables the plotting of multiple resistive subnets.  These\n"
     "voltages must have been saved by aspice, which is done by default\n"
     "for alint but requires the -traceR option for aspice."},
    {"add","node1 [node2 [...]]",
     "Add nodes to the current panel.  Supports globbing."},
    {"delete","node1 [node2 [...]]",
     "Delete nodes from the current panel.  Supports globbing."},
    {"trace","node1 [node2 [...]]",
     "Clear the panel then add the specified nodes.  Supports globbing."},
    {"update","",
     "Refresh data from the trace file in case it has changed."},
    {"list","[node]",
     "List nodes matching a name or glob pattern, or all nodes names if node\n"
     "argument is omitted."},
    {"clear","",
     "Delete all nodes from current panel."},
    {"close","",
     "Close the current panel, unless it is the last panel."},
    {"free","",
     "Free cached data.  Used by scripts like lve_raw to limit memory usage."},
    {"range","[tmin:tmax [vmin:vmax]]",
     "Set the time and voltage range of the panel.  Restricts measurements\n"
     "to the specified time interval."},
    {"print","node",
     "Print time/voltage pairs for a specified node.  Globbing unsupported."},
    {"minv","node",
     "Measure minimum voltage of node over current time range.  Globbing\n"
     "unsupported, but does cover resistive envelope."},
    {"maxv","node",
     "Measure maxmimum voltage of node over current time range.  Globbing\n"
     "unsupported, but does cover resistive envelope."},
    {"avg","node",
     "Measure average voltage of node over current time range.  Globbing\n"
     "unsupported, but does cover resistive envelope."},
    {"rms","node",
     "Measure root-mean-squared voltage of node over current time range.\n"
     "Globbing unsupported, but does cover resistive envelope."},
    {"skew","node thresh",
     "Measure maximum width in time of the resistive envelope at the\n"
     "specified voltage threshold.  Globbing unsupported."},
    {"trigger","node ... [>|<|=] thresh [occurance1 [occurance2]]",
     "Reports the time at which the specified node(s) rise, fall, or cross\n"
     "the given voltage threshold.  Supports globs and uses the average voltage\n"
     "for resistive envelopes.  If neither occurance index is given, reports the\n"
     "number of events in the current time range.  If occurance1 is given,\n"
     "reports the time of that event occurance.  If occurance2 is also given,\n"
     "reports a frequency in GHz computed as n/dt, where n is the number of\n"
     "events in the time range, and dt is the time difference between the first\n"
     "and last event.  This is the normal way to measure a frequency.  For\n"
     "example \"trigger R.e < 0.45 4 8\"."},
    {"trigger_minv","node ... [>|<|=] thresh [occurance1 [occurance2]]",
     "Same as trigger but uses the minimum voltage for resistive envelopes."},
    {"trigger_maxv","node ... [>|<|=] thresh [occurance1 [occurance2]]",
     "Same as trigger but uses the maximum voltage for resistive envelopes."},
    {"mindelay","node1 ... [>|<|=] thresh1 node2 ... [>|<|=] thresh2",
     "Measures the minimum delay between any rise, fall, or crossing event\n"
     "on node1 at voltage thresh1 and the next rise, fall, or crossing\n"
     "event on node2 at voltage thresh2.  Supports globs and resistive\n"
     "envelopes. If multiple trigger and target events exist, reports the\n"
     "minimum interval over all of them.  Rising/falling trigger/target\n"
     "events use the leading edge of the resistive envelope, but crossing\n"
     "events use the average voltage."},
    {"maxdelay","node1 ... [>|<|=] thresh1 node2 ... [>|<|=] thresh2",
     "Measures the maximum delay between any rise, fall, or crossing event\n"
     "on node1 at voltage thresh1 and the next rise, fall, or crossing\n"
     "event on node2 at voltage thresh2.  Supports globs and resistive\n"
     "envelopes.  If multiple trigger and target events exist, reports the\n"
     "maximum interval over all of them.  Rising/falling trigger/target\n"
     "events use the trailing edge of the resistive envelope, but crossing\n"
     "events use the average voltage."},
    {"aliases","node",
     "Reports all aliases of a node connected by wires.  Globbing unsupported."},
    {"subnets","node",
     "Reports all resistive subnets of a node.  Globbing unsupported."},
    {"echo","[comment]",
     "Echoes the rest of the line to stdout.  Used by scripts."},
    {"write","node1 [node2 [...]]",
      "writes pwl file for each node: file:node.pwl"},
    {"quit","",
     "Exits aplot.  Ctrl-D also works."},
    {"png","\"file.png\" [\"title\"] [x y]",
     "Write a png file of the current panel, with optional title and\n"
     "dimensions."},
    {"\\gnuplot","...",
     "Passes the rest of the line directly to gnuplot.  This can be used for\n"
     "detailed control of titles and axes, and to make plots to non-png file\n"
     "formats like postscript."},
    {NULL,NULL,NULL} // NULL terminate the command list
  };

/********************* Top level command line parser *******************/

int main(int argc, char *argv[])
  {
  int i,j,k;
  PANEL *panel,*measure;
  LIST *panels;
  char prompt[STRMAX],templine[STRMAX],*line,*oldline=NULL,
    *newprefix,*addprefix,*name;
  LEX *lex;
  int gnu=1;
  gnu=isatty(0);
  setlinebuf(stdin);
  setlinebuf(stdout);
  setlinebuf(stderr);
  int running=1;
  char *history_file=NULL;

  for (i=0; i<argc; i++) {
    int shift=0;
    if (!strcmp (argv[i], "-help")) {
        fprintf(stderr, "Usage: aplot [-persist] [-archiver cmd] [file]\n");
        exit (1);
    }
    if (!strcmp (argv[i], "-persist")) {
      shift=1;
      persist=1;
    } else if (!strcmp (argv[i], "-archiver")) {
      if (i+1<argc) {
        shift=2;
        archiver=argv[i+1];
      } else {
        fprintf(stderr, "-archiver requires an argument\n");
      }
    }
    if (shift>0) {
      for(j=i; j<argc-shift; j++) argv[j]=argv[j+shift];
      argc-=shift;
    }
  }
  if (archiver!=NULL) archive_init(archiver);
  panel=create_panel(0);
  if (argc==2) {leak_free(panel->file); panel->file=leak_strdup(argv[1]);}
  panels=list_create(sizeof(PANEL *));
  list_append_element(panels,&panel);
  if (gnu)
    {
    char *home = getenv("HOME");
    if (home)
      {
      history_file=malloc (strlen (home)+strlen ("/.aplot_history")+2);
      strcpy (history_file, home);
      strcat (history_file, "/.aplot_history");
      }
    else
      {
      history_file=malloc (strlen (".aplot_history") + 2);
      strcpy (history_file, ".aplot_history");
      }
    read_history(history_file);
    }
  while(running)
    {
    /*** get the line with readline ***/
    safe_sprintf(prompt,"%d:%s:%s> ",panel->number,panel->file,panel->prefix);
    if (gnu)
      {
      line=readline(prompt);
      if (line==NULL) break;
      if ((line[0]!=0)&&((oldline==NULL)||(strcmp(line,oldline)!=0))&&strncmp(line, "quit", 4))
        {
        add_history(line);
        write_history(history_file);
        }
      if (oldline!=NULL) free(oldline);
      oldline=line;
      }
    else
      {
      line=fgets(templine,sizeof(templine),stdin);
      if (line==NULL) break;
      }

    /*** parse the line ***/
    lex=lex_string(line);
    while(!lex_is_eof(lex))
      {
      /*** get panel prefix if any ***/
      if (lex_is_integer(lex))
        {
        j=lex_eat_integer(lex);
        k=find_element(panels,&j,&panel_int_cmp);
        if (k<0)
          {
          panel=clone_panel(j,panel);
          list_append_element(panels,&panel);
	  }
        else panel=panels->p.panel[k];
        if (lex_eatif_sym(lex,":"));
        else {lex_warning(lex,":"); lex_eat_until(lex,";\n");}
        }

      /*** parse the rest of the line ***/
      if (lex_eatif_keyword(lex,"with"))
        {
        leak_free(panel->prefix);
        panel->prefix=parse_nodename(lex);
        }
      else if (lex_eatif_keyword(lex,"append"))
        {
        addprefix=parse_nodename(lex);
        newprefix=leak_malloc(strlen(panel->prefix)+strlen(addprefix)+1);
        strcpy(newprefix,panel->prefix);
        strcat(newprefix,addprefix);
        leak_free(addprefix);
        leak_free(panel->prefix);
        panel->prefix=newprefix;
        }
      else if (lex_eatif_keyword(lex,"up"))
	{
        newprefix=leak_strdup(panel->prefix);
        i=strlen(newprefix)-2;
        while ((i>=0)&&(newprefix[i]!='.')) i--;
        if (i<0) newprefix[0]=0;
        else newprefix[i+1]=0;
        leak_free(panel->prefix);
        panel->prefix=newprefix;
	}
      else if (lex_eatif_keyword(lex,"range"))
	{
        parse_range(lex,&panel->tmin,&panel->tmax);
        parse_range(lex,&panel->vmin,&panel->vmax);
	}
      else if (lex_eatif_keyword(lex,"close"))
	{
        if (panels->max>1)
          {
          for (i=0; i<panels->max; i++) if (panels->p.panel[i]->number==panel->number) break;
          list_remove_element(panels,i);
          close_panel(panel);
          panel=panels->p.panel[0];
	  }
        else printf("WARNING: can't close last panel.\n");
	}
      else if (lex_eatif_keyword(lex,"clear"))
	{
        clear_panel(panel);
	}
      else if (lex_eatif_keyword(lex,"file"))
	{
        if (lex_is_quote(lex))
	  {
          leak_free(panel->file);
          panel->file=leak_strdup(lex_eat_quote(lex));
	  }
        else lex_warning(lex,"quoted filename\n");
	}
      else if (lex_eatif_keyword(lex,"add"))
	{
        while (1)
	  {
          name=parse_nodename(lex);
          if (name[0]==0) {leak_free(name); break;}
          add_glob_curve(panel,name,1);
          leak_free(name);
          lex_eatif_sym(lex,",");
          if (lex_is_sym(lex,";")||lex_is_sym(lex,"\n")) break;
	  }
        draw_panel(panel);
	}
      else if (lex_eatif_keyword(lex,"delete"))
	{
        while (1)
	  {
          name=parse_nodename(lex);
          if (name[0]==0) {leak_free(name); break;}
          delete_glob_curve(panel,name);
          leak_free(name);
          lex_eatif_sym(lex,",");
          if (lex_is_sym(lex,";")||lex_is_sym(lex,"\n")) break;
	  }
        draw_panel(panel);
	}
      else if (lex_eatif_keyword(lex,"trace"))
        {
        clear_panel(panel);
        while (1)
	  {
          name=parse_nodename(lex);
          if (name[0]==0) {leak_free(name); break;}
          add_glob_curve(panel,name,1);
          leak_free(name);
          lex_eatif_sym(lex,",");
          if (lex_is_sym(lex,";")||lex_is_sym(lex,"\n")) break;
	  }
        draw_panel(panel);
        }
      else if (lex_eatif_keyword(lex,"write"))
        {
        clear_panel(panel);
        while (1)
	  {
          name=parse_nodename(lex);
          if (name[0]==0) {leak_free(name); break;}
          add_glob_curve(panel,name,1);
          leak_free(name);
          lex_eatif_sym(lex,",");
          if (lex_is_sym(lex,";")||lex_is_sym(lex,"\n")) break;
	  }
        write_panel(panel);
        }
      else if (lex_eatif_keyword(lex,"update"))
	{
        draw_panel(panel);
	}
      else if (lex_eatif_keyword(lex,"png"))
        {
        if (lex_is_quote(lex))
          {
          int x=640,y=240;
          char *filename;
          FILE *file;
          filename = lex_eat_quote(lex);
          file = fopen(filename,"w");
          if (!file)
            {
            printf("WARNING: can't write %s.\n",filename);
            lex_eat_until(lex,";\n");
            break;
            }
          fclose(file);
          fprintf(panel->gnuplot,"set out \"%s\"\n",filename);
          if (lex_is_quote(lex))
            fprintf(panel->gnuplot,"set title \"%s\"\n",lex_eat_quote(lex));
          if (lex_is_integer(lex)) x=lex_eat_integer(lex);
          if (lex_is_integer(lex)) y=lex_eat_integer(lex);
          fprintf(panel->gnuplot,"set terminal png size %d,%d\n",x,y);
          draw_panel(panel);
          fprintf(panel->gnuplot,"set terminal x11\n");
          fprintf(panel->gnuplot,"set out\n");
          fprintf(panel->gnuplot,"set title \"Panel %d\"\n",panel->number);
          }
        else printf("WARNING: expected quoted filename.\n");
        }
      else if (lex_eatif_keyword(lex,"resistive"))
	{
	if      (lex_eatif_keyword(lex,"on"))  resistive_nodes=1;
	else if (lex_eatif_keyword(lex,"off")) resistive_nodes=0;
	}
      else if (lex_eatif_keyword(lex,"print"))
        {
        name=parse_nodename(lex);
        if (name[0]!=0)
          {
          measure=measure_panel(name,panel);
          print(measure);
          close_panel(measure);
          }
        }
      else if (lex_eatif_keyword(lex,"skew"))
        {
        float value=1;
        name=parse_nodename(lex);
        if ((name[0]!=0)&&(lex_is_real(lex)))
          {
          value=lex_eat_real(lex);
          measure=measure_panel(name,panel);
          skew(measure,value);
          close_panel(measure);
          }
        else {printf("WARNING: bad syntax.\n"); lex_eat_until(lex,";\n");}
        leak_free(name);
        }
      else if (lex_is_keyword(lex,"minv")||lex_is_keyword(lex,"maxv"))
        {
        int domin=0;
        if      (lex_eatif_keyword(lex,"minv")) domin=1;
        else if (lex_eatif_keyword(lex,"maxv")) domin=0;
        name=parse_nodename(lex);
        if (name[0]!=0)
          {
          measure=measure_panel(name,panel);
          minmaxv(measure,domin);
          close_panel(measure);
          }
        else {printf("WARNING: bad syntax.\n"); lex_eat_until(lex,";\n");}
        leak_free(name);
        }
      else if (lex_is_keyword(lex,"avg")||lex_is_keyword(lex,"rms"))
        {
        int rms=0;
        if      (lex_eatif_keyword(lex,"avg")) rms=0;
        else if (lex_eatif_keyword(lex,"rms")) rms=1;
        name=parse_nodename(lex);
        if (name[0]!=0)
          {
          measure=measure_panel(name,panel);
          integrate(measure,rms);
          close_panel(measure);
          }
        else {printf("WARNING: bad syntax.\n"); lex_eat_until(lex,";\n");}
        leak_free(name);
        }
      else if (lex_is_keyword(lex,"mindelay")||lex_is_keyword(lex,"maxdelay"))
        {
        int domin=0,error=0;
        int trig_dir=0,targ_dir=0;
        float trig_thresh=0,targ_thresh=0;
        LIST *trig_panels,*targ_panels;
        if      (lex_eatif_keyword(lex,"mindelay")) domin=1;
        else if (lex_eatif_keyword(lex,"maxdelay")) domin=0;

        // get trigger arguments
        trig_panels=create_measure_panels(panel,lex);
        trig_dir=parse_direction(lex);
        if (trig_dir==2) error=1;
        if (lex_is_real(lex)) trig_thresh=lex_eat_real(lex);
        else error=1;

        // get target arguments
        targ_panels=create_measure_panels(panel,lex);
        targ_dir=parse_direction(lex);
        if (targ_dir==2) error=1;
        if (lex_is_real(lex)) targ_thresh=lex_eat_real(lex);
        else error=1;

        // do measurement
        if (!error && (trig_panels->max>0) && (targ_panels->max>0))
          minmaxdelay(trig_panels,targ_panels,domin,
                      trig_dir,targ_dir,trig_thresh,targ_thresh);
        else {printf("WARNING: bad syntax.\n"); lex_eat_until(lex,";\n");}
        free_measure_panels(trig_panels);
        free_measure_panels(targ_panels);
        }
      else if (lex_is_keyword(lex,"trigger")||
               lex_is_keyword(lex,"trigger_minv")||
               lex_is_keyword(lex,"trigger_maxv"))
        {
        LIST *panels;
        int dir,occurance1=-1,occurance2=-1,error=0,min_nom_max=0;
        float value=0;
        if      (lex_eatif_keyword(lex,"trigger_minv")) min_nom_max=-1;
        else if (lex_eatif_keyword(lex,"trigger_maxv")) min_nom_max= 1;
        else if (lex_eatif_keyword(lex,"trigger"))      min_nom_max= 0;

        // create measure panels
        panels=create_measure_panels(panel,lex);

        // get direction
        dir=parse_direction(lex);
        if (dir==2) error=1;

        // get threshold
        if (lex_is_real(lex)) value=lex_eat_real(lex);
        else error=1;

        // get occurance counts
        if (lex_is_integer(lex)) occurance1=lex_eat_integer(lex);
        if (lex_is_integer(lex)) occurance2=lex_eat_integer(lex);

        // do trigger measurement
        if (!error && (panels->max>0))
          trigger(panels,dir,value,occurance1,occurance2,min_nom_max);
        else {printf("WARNING: bad syntax.\n"); lex_eat_until(lex,";\n");}
        free_measure_panels(panels);
	}
      else if (lex_eatif_keyword(lex,"free"))
        free_caches();
      else if (lex_eatif_keyword(lex,"subnets"))
        {
        CURVE *a,*b;
        LIST *names;
        name=parse_nodename(lex);
        if (name[0]!=0)
          {
          j=-1;
          names=cache_names(panel->file);
          if (names!=NULL) j=find_element_sorted(names,&name,&curve_str_cmp);
          if (j>=0)
            {
            a = names->p.curve[j];
            b = a;
            do 
              {
              printf("%s\n",b->name);
              b=b->subnet;
              } while (b!=a);
            }
          }
        leak_free(name);
        }
      else if (lex_eatif_keyword(lex,"aliases"))
        {
        CURVE *a,*b;
        LIST *names;
        name=parse_nodename(lex);
        if (name[0]!=0)
          {
          j=-1;
          names=cache_names(panel->file);
          if (names!=NULL) j=find_element_sorted(names,&name,&curve_str_cmp);
          if (j>=0)
            {
            a = names->p.curve[j];
            b = a;
            do 
              {
              printf("%s\n",b->name);
              b=b->alias;
              } while (b!=a);
            }
          }
        leak_free(name);
        }
      else if (lex_eatif_keyword(lex,"help"))
        {
        if (lex_is_id(lex))
          {
          // expanded help for a specific command
          char *str = lex_eat_id(lex);
          for (j=0; command[j][0]!=NULL; j++)
            if (strcmp(str,command[j][0])==0 || strcmp(str,"all")==0 ||
                (strcmp(str,"gnuplot")==0 && strcmp(command[j][0],"\\gnuplot")==0))
              printf("%s %s\n\n%s\n\n",
                     command[j][0],command[j][1],command[j][2]);
          }
        else
          {
          // print usage for all commands
          printf("Commands:\n\n");
          for (j=0; command[j][0]!=NULL; j++)
            printf("  %s %s\n",command[j][0],command[j][1]);
          printf("\n");
          printf("Can preceed commands with panel#: to create or switch panels.\n"
                 "Can put multiple commands on a line separated by semicolons.\n"
                 "Prompt specifies default panel#:file:prefix.\n"
                 "Many commands allow node globs with a single * and multiple ?.\n"
                 "\n");
          }
	}
      else if (lex_eatif_keyword(lex,"echo"))
        printf("%s\n",lex_eat_until(lex,"\n"));
      else if (lex_eatif_sym(lex,"\\"))
	{
        fprintf(panel->gnuplot,"%s\n",lex_eat_until(lex,";\n"));
        fflush(panel->gnuplot);
        }
      else if (lex_eatif_sym(lex, "list"))
        {
        int j;
        char fullname[STRMAX];
        LIST *names;
        CURVE *curve;
        name=parse_nodename(lex);
        if (name[0] == 0)
            name = "*";
        safe_sprintf(fullname,"%s%s",panel->prefix,name);
        if (is_glob(name))
          {
          names=cache_names(panel->file);
          if (names!=NULL)
            {
            for (j=0; j<names->max; j++)
              {
              curve=names->p.curve[j];
              if (glob_matches(fullname,curve->name))
                printf("%s\n", curve->name);
              }
            }
          }
        else printf("%s\n", fullname);
        }
      else if (lex_eatif_sym(lex, "quit"))
        {
        running=0;
        break;
        }

      /*** handle rest of command ***/
      if      (lex_is_eof(lex)) break;
      else if (lex_eatif_sym(lex,";"));
      else {lex_warning(lex,"valid command"); lex_eat_until(lex,";\n");}
      }

    /*** more readline stuff ***/
    lex_free(lex);
    }
    if (gnu)
      {
      history_truncate_file(history_file, 1024);
      }

  /*** free, close, and exit ***/
  for (i=0; i<panels->max; i++) close_panel(panels->p.panel[i]);
  list_free(panels);
  free_caches();
  free_temp_list();
  leak_check();
  return 0;
  }
