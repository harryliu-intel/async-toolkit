#include "auto.h"

/*** convert ()[b] to [a,b] style ***/
char *convert_magic_name(char *old)
  {
  char new[STRMAX];
  int i,j,len;
  len=strlen(old);
  for (i=j=0; i<len; i++)
    {
    /*** sparse array processing ***/
    if (old[i]=='(') new[j++]='[';
    else if ((strncmp(old+i,"][",2)==0)||
	     (strncmp(old+i,"](",2)==0)||
	     (strncmp(old+i,")(",2)==0)||
	     (strncmp(old+i,")[",2)==0)) {new[j++]=','; i++;}
    else if (old[i]==')') new[j++]=']';
    /*** nothing changed ***/
    else new[j++]=old[i];
    }
  new[j]=0;
  return leak_strdup(new);
  }

/*** compare two labels by their root alias ***/
int label_alias_cmp(void *p1, void *p2)
  {
  LABEL *pl1=(LABEL *)p1,*pl2=(LABEL *)p2;
  return strcmp(root_al(pl1->alias)->name,root_al(pl2->alias)->name);
  }

/*** parse a single rlabel, apply the transform, add to list ***/
void parse_label(LEX *lex, LIST *aliases, LIST *labels,
                 char *prefix, TRANSFORM transform, RECT *pr)
  {
  LABEL lab;
  char last;
  char *name,fullname[STRMAX];
  lab.paint=lex_eat_paint(lex);
  lab.x0=lex_eat_integer(lex);
  lab.y0=lex_eat_integer(lex);
  lab.x1=lex_eat_integer(lex);
  lab.y1=lex_eat_integer(lex);
  lab.position=lex_eat_integer(lex);
  name=parse_nodename(lex);
  last=name[strlen(name)-1];
  if ((last=='#')||(last=='!')) return;
  strcpy(fullname,prefix);
  strcat(fullname,name);
  name=convert_magic_name(fullname);
  lab.alias=find_alias(aliases,name);
  leak_free(name);
  apply_transform(&lab,transform);
  pr->x0=min(pr->x0,lab.x0);
  pr->x1=max(pr->x1,lab.x1);
  pr->y0=min(pr->y0,lab.y0);
  pr->y1=max(pr->y1,lab.y1);
  if (lab.alias==NULL) return; /* ignore internal connections of cells */
  /*** only add an alias once per cell ***/ 
  if (find_element_lazy_sort(labels,&lab,&label_alias_cmp)<0)
    list_insert_element_lazy_sort(labels,&lab,&label_alias_cmp);
  }

/*** parse .mag lines associated with using a subcell ***/
USE parse_use(LEX *lex, int for_auto)
  {
  USE use;
  lex_do_whitespace(lex); use.type=leak_strdup(lex_eat_until(lex," \n\t"));
  lex_do_whitespace(lex); use.name=leak_strdup(lex_eat_until(lex," \n\t"));
  use.x0=use.x1=use.dx=use.y0=use.y1=use.dy=0;
  if (for_auto) lex_eat_sym(lex,"@magic");
  if (lex_eatif_keyword(lex,"array"))
    {
    use.x0=lex_eat_integer(lex);
    use.x1=lex_eat_integer(lex);
    use.dx=lex_eat_integer(lex);
    use.y0=lex_eat_integer(lex);
    use.y1=lex_eat_integer(lex);
    use.dy=lex_eat_integer(lex);
    if (for_auto) lex_eatif_sym(lex,"@magic");
    }
  lex_eat_keyword(lex,"timestamp");
  use.timestamp=lex_eat_integer(lex);
  if (for_auto) lex_eatif_sym(lex,"@magic");
  lex_eat_keyword(lex,"transform");
  use.transform.M[0][0]=lex_eat_integer(lex);
  use.transform.M[0][1]=lex_eat_integer(lex);
  use.transform.O[0]=lex_eat_integer(lex);
  use.transform.M[1][0]=lex_eat_integer(lex);
  use.transform.M[1][1]=lex_eat_integer(lex);
  use.transform.O[1]=lex_eat_integer(lex);
  if (for_auto) lex_eatif_sym(lex,"@magic");
  lex_eat_keyword(lex,"box");
  use.box.x0=lex_eat_integer(lex);
  use.box.y0=lex_eat_integer(lex);
  use.box.x1=lex_eat_integer(lex);
  use.box.y1=lex_eat_integer(lex);
  use.box.pr=NULL;
  use.box.paint=-1;
  use.box.name=NULL;
  use.box.block=-1;
  return use;
  }

/*** print use ***/
void print_use(FILE *fout, USE use, int for_auto)
  {
  if (for_auto) fprintf(fout,"@magic ");
  fprintf(fout,"use %s %s\n",use.type,use.name);
  if ((use.x0!=use.x1)||(use.y0!=use.y1))
    {
    if (for_auto) fprintf(fout,"@magic ");
    fprintf(fout,"array %d %d %d %d %d %d\n",use.x0,use.x1,use.dx,use.y0,use.y1,use.dy);
    }
  if (for_auto) fprintf(fout,"@magic ");
  fprintf(fout,"timestamp %d\n",use.timestamp);
  if (for_auto) fprintf(fout,"@magic ");
  fprintf(fout,"transform %d %d %d %d %d %d\n",
	  use.transform.M[0][0],use.transform.M[0][1],use.transform.O[0],
	  use.transform.M[1][0],use.transform.M[1][1],use.transform.O[1]);
  if (for_auto) fprintf(fout,"@magic ");
  fprintf(fout,"box %g %g %g %g\n",use.box.x0,use.box.y0,use.box.x1,use.box.y1);
  }

/*** parse all labels in a magfile, adding to flattened list ***/
/*** also keep track of all subcell instances ***/
void parse_mag(char *filename, char *prefix, TRANSFORM transform,
	       LIST *labels, LIST *aliases, LIST *instances, int recurse)
  {
  char *fullname;
  FILE *file;
  LEX *lex;
  char newprefix[STRMAX];
  TRANSFORM newtransform;
  INSTANCE instance;
  USE use;
  LIST *local_labels;
  LABEL *pl;
  RECT bbox;
  int i,ix,iy,dx,dy,sx,sy;

  /*** open .mag file ***/
  fullname=find_filename(filename,".mag",SEARCH_PATH);
  if (fullname==NULL)
    {
    ERROR=1; fprintf(stderr,"ERROR: parse_mag can't find %s.\n",filename);
    return;
    }
  file=fopen(fullname,"rt");
  if (file==NULL)
    {
    leak_free(fullname);
    ERROR=1; fprintf(stderr,"ERROR: parse_mag can't read %s.\n",filename);
    return;
    }
  lex=lex_file_with_name(file,fullname);
  leak_free(fullname);

  /*** parse .mag file ***/
  local_labels=list_create(sizeof(LABEL));
  bbox.x0=bbox.y0= 1e10;
  bbox.x1=bbox.y1=-1e10;
  while (!lex_is_eof(lex))
    {
    if (lex_eatif_keyword(lex,"rlabel")&&(labels!=NULL))
      parse_label(lex,aliases,local_labels,prefix,transform,&bbox);
    else if (lex_eatif_keyword(lex,"use"))
      {
      use=parse_use(lex,0);
      sx= (use.x0<=use.x1) ? 1 : -1;
      sy= (use.y0<=use.y1) ? 1 : -1;
      for (ix=use.x0, dx=0; sx*ix<=sx*use.x1; ix+=sx, dx+=use.dx)
        for (iy=use.y0, dy=0; sy*iy<=sy*use.y1; iy+=sy, dy+=use.dy)
	  {
	  if ((use.x0!=use.x1)&&(use.y0!=use.y1))
            safe_sprintf(newprefix,"%s%s[%d,%d].",prefix,use.name,iy,ix);
          else if (use.x0!=use.x1)
            safe_sprintf(newprefix,"%s%s[%d].",prefix,use.name,ix);
          else if (use.y0!=use.y1)
            safe_sprintf(newprefix,"%s%s[%d].",prefix,use.name,iy);
          else
            safe_sprintf(newprefix,"%s%s.",prefix,use.name);
          newtransform=compose_transforms(offset_transform(dx,dy),use.transform);
          newtransform=compose_transforms(newtransform,transform);

	  /*** add instance to list ***/
	  instance.type=leak_strdup(use.type);
	  instance.name=convert_magic_name(newprefix);
	  instance.pcircuit=NULL;
	  list_append_element(instances,&instance);

	  /*** recurse ***/
          if (recurse) parse_mag(use.type,newprefix,newtransform,labels,aliases,instances,1);
	  }
      leak_free(use.type);
      leak_free(use.name);
      }
    else lex_eat_until(lex,"\n");
    }

  /*** use same bbox for all local labels ***/
  for (i=0; i<local_labels->max; i++)
    {
    pl=&local_labels->p.label[i];
    pl->x0=bbox.x0;
    pl->x1=bbox.x1;
    pl->y0=bbox.y0;
    pl->y1=bbox.y1;
    }

  /*** transfer unique local labels to labels ***/
  if (labels!=NULL) list_append_list(labels,local_labels);
  list_free(local_labels);

  /*** free and return ***/
  lex_free(lex);
  fclose(file);
  }

/*** print one label ***/
void print_label(LABEL *pl)
  {
  printf("rlabel %s %d %d %d %d %d %s\n",
	 Paints->p.paint[pl->paint].name,pl->x0,pl->y0,pl->x1,pl->y1,
	 pl->position,pl->alias->name);
  }

/*** dump out list of labels ***/
void debug_labels(LIST *labels)
  {
  int i;
  LABEL *pl;
  if (labels==NULL) {printf("NO LABELS\n"); return;}
  printf("magic\ntech scmos\ntimestamp 0\n<< labels >>\n");
  for (i=0; i<labels->max; i++)
    {
    pl=&labels->p.label[i];
    print_label(pl);
    }
  printf("<< end >>\n");
  }

/*** sort labels by root alias ***/
int label_cmp(void *p1, void *p2)
  {
  ALIAS *pa1,*pa2;
  pa1=root_al(((LABEL *)p1)->alias);
  pa2=root_al(((LABEL *)p2)->alias);
  return alias_cmp(&pa1,&pa2);
  }

/*** compute spanning tree length for one node from label [a,b] inclusive ***/
double spanning_tree(LIST *labels, int a, int b)
  {
  int i;
  double length=0;
  LABEL *pl;
  PORT port;
  LIST *ports,*wires;

  /*** convert labels to ports ***/
  ports=list_create(sizeof(PORT));
  for (i=a; i<=b; i++)
    {
    pl=&labels->p.label[i];
    port.n=port.block=i;
    port.paint=pl->paint;
    port.x0=pl->x0;
    port.y0=pl->y0;
    port.x1=pl->y1;
    port.y1=pl->y1;
    list_append_element(ports,&port);
    }

  /*** build spanning tree and add up cost ***/
  wires=wire_ports(ports,NULL);
  for (i=0; i<wires->max; i++) length+=wires->p.wire[i].distance;
  list_free(wires);
  list_free(ports);
  return length;
  }

/*** compute spanning tree capacitance of connected labels ***/
void spanning_trees(LIST *labels, double cap, double res)
  {
  int a,b;
  LABEL *pl;
  ALIAS *root;
  double length;
  if (labels==NULL) return;
  if (labels->max==0) return;
  list_sort(labels,&label_cmp);
  a=0;
  while(a<labels->max) /* find spans of labels with same root alias */
    {
    pl=&labels->p.label[a];
    b=a;
    while ((b+1<labels->max) && (label_cmp(pl,&labels->p.label[b+1])==0)) b++;
    length=spanning_tree(labels,a,b); /* from [a,b] inclusive */
    root=root_al(pl->alias);
    root->cap+=length*cap;
    root->res+=length*res;
    a=b+1;
    }
  }

/*** compare aliases by capacitance ***/
int alias_cap_cmp(void *p1, void *p2)
  {
  ALIAS *pa1,*pa2;
  pa1=*((ALIAS **)p1);
  pa2=*((ALIAS **)p2);
  if (pa1->cap<pa2->cap) return -1;
  if (pa1->cap>pa2->cap) return 1;
  return 0;
  }

/*** print capacitances on root aliases ***/
void print_caps(int format)
  {
  int i;
  ALIAS *pa;
  LIST *aliases=Size.aliases;
  list_sort(aliases,&alias_cap_cmp);
  for (i=0; i<aliases->max; i++)
    {
    pa=aliases->p.palias[i];
    if ((pa->root!=NULL)||(pa->cap==0)) continue;
    if (format==1) printf("cap (GND,%s) (%g);\n",pa->name,pa->cap);
    else           printf("%s %g\n",pa->name,pa->cap*1e15);
    }
  list_sort(aliases,&alias_cmp);
  }

/*** find all dependent magic files, add to a list ***/
/*** NOTE: could be done with parse_mag, but this runs a lot faster ***/
void magic_depend(char *filename, LIST *cells)
  {
  char *fullname;
  FILE *file;
  LEX *lex;
  USE use;

  /*** open .mag file ***/
  fullname=find_filename(filename,".mag",SEARCH_PATH);
  if (fullname==NULL)
    {
    ERROR=1; fprintf(stderr,"ERROR: can't find %s.\n",filename);
    return;
    }
  file=fopen(fullname,"rt");
  if (file==NULL)
    {
    leak_free(fullname);
    ERROR=1; fprintf(stderr,"ERROR: magic_depend can't read %s.\n",filename);
    return;
    }
  lex=lex_file_with_name(file,fullname);
  leak_free(fullname);

  /*** recursively parse subcells ***/
  while (!lex_is_eof(lex))
    {
    if (lex_eatif_keyword(lex,"use"))
      {
      use=parse_use(lex,0);
      if (find_element_lazy_sort(cells,&use.type,&str_cmp)<0)
      {
      list_insert_element_lazy_sort(cells,&use.type,&str_cmp);
      magic_depend(use.type,cells);
        }
      else leak_free(use.type);
      leak_free(use.name);
      }
    else lex_eat_until(lex,"\n");
    }

  /*** free and return ***/
  lex_free(lex);
  fclose(file);
  }

/*** handle naming oddities of cadence types ***/
char *convert_cadence_type(char *old)
  {
  char new[STRMAX];
  int i,j,len;
  len=strlen(old);
  for (i=j=0; i<len; i++)
    {
    if (old[i]=='-') new[j++]='/';
    else if (strncmp(old+i,"##28",4)==0) {new[j++]='('; i+=3;}
    else if (strncmp(old+i,"##29",4)==0) {new[j++]=')'; i+=3;}
    else if (strncmp(old+i,"##2c",4)==0) {new[j++]=','; i+=3;}
    else new[j++]=old[i];
    }
  new[j]=0;
  return leak_strdup(new);
  }

/*** parse all cadence instances ***/
void parse_cadence(char *filename, LIST *aliases, LIST *instances)
  {
  char *fullname;
  FILE *file;
  LEX *lex;
  INSTANCE instance;

  /*** open .cadence file ***/
  fullname=find_filename(filename,".instances",SEARCH_PATH);
  if (fullname==NULL)
    {
    ERROR=1; fprintf(stderr,"ERROR: parse_cadence can't find %s.\n",filename);
    return;
    }
  file=fopen(fullname,"rt");
  if (file==NULL)
    {
    leak_free(fullname);
    ERROR=1; fprintf(stderr,"ERROR: parse_cadence can't read %s.\n",filename);
    return;
    }
  lex=lex_file_with_name(file,fullname);
  leak_free(fullname);

  /*** finish sorting aliases ***/
  list_finish_lazy_sort(aliases,&alias_cmp);

  /*** parse .cadence file ***/
  while (!lex_is_eof(lex))
    {
    /*** add the instance to the list ***/
    instance.pcircuit=NULL;
    instance.type=convert_cadence_type(lex_eat_quote(lex));
    instance.name=convert_magic_name(lex_eat_quote(lex));
    lex_eat_sym(lex,"(");
    lex_eat_sym(lex,"(");
    instance.x0=lex_eat_real(lex);
    instance.y0=lex_eat_real(lex);
    lex_eat_sym(lex,")");
    lex_eat_sym(lex,"(");
    instance.x1=lex_eat_real(lex);
    instance.y1=lex_eat_real(lex);
    lex_eat_sym(lex,")");
    lex_eat_sym(lex,")");
    list_append_element(instances,&instance);
    }

  /*** free and return ***/
  lex_free(lex);
  fclose(file);
  }

