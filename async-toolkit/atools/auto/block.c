#include "auto.h"

/*** find a paints number ***/
int find_paint(char *str)
  {
  int i;
  PAINT paint;

  i=find_element(Paints,str,&paint_str_cmp);
  if (i<0)
    {
    paint.name=str;
    paint.maxspacing=0;
    i=Paints->max;
    assert(i<MAXPAINTS);
    list_append_element(Paints,&paint);
    }
  return i;
  }

/*** parse a paint name ***/
int lex_eat_paint(LEX *lex)
  {
  return find_paint(get_name(lex_eat_id(lex)));
  }

/*** parse a nodename ***/
char *parse_nodename(LEX *lex)
  {
  char *str,c;
  lex_do_whitespace(lex);
  if (lex_is_quote(lex)) return lex_eat_quote(lex);
  lex_push_position(lex);
  do
    {
    while(1)
      {
      c=lex_char(lex);
      if (isalnum(c)||c=='_') lex_eat_char(lex);
      else break;
      }
    while (lex_do_sym(lex,"["))
      {
      do lex_eat_integer(lex); while (lex_do_sym(lex,","));
      lex_eat_sym(lex,"]");
      }
    } while (lex_do_sym(lex,"."));
  if (lex_do_sym(lex,"#"));
  else if (lex_do_sym(lex,"$"));
  else if (lex_do_sym(lex,"!"));
  str=lex_get_token(lex);
  lex_pop_position(lex);
  if (str[0]==0) lex_error(lex,"legal node name");
  return str;
  }

/*** parse extra stuff for cadence ***/
char *parse_extra(LEX *lex)
  {
  char *str,c;
  while(1) /*** NOTE: eat digits between name and extra to eliminate !0 stuff ***/
    {
    c=lex_char(lex);
    if (isdigit(c)) lex_eat_char(lex);
    else break;
    }
  lex_push_position(lex);
  while(1)
    {
    c=lex_char(lex);
    if (isalnum(c)||(c=='_')||(c=='|')) lex_eat_char(lex);
    else break;
    }
  str=lex_get_token(lex);
  lex_pop_position(lex);
  if (str[0]==0) return NULL;
  return str;
  }

/*** check if two rectangles touch ***/
/*** assumes paints which touch have >=0 Spacing ***/
int connected(RECT *pr1, RECT *pr2)
  {
  return /*(Spacing[pr1->paint][pr2->paint][0]>=0)&&*/
         overlap(pr1->x0,pr1->x1,pr2->x0,pr2->x1)&&
         overlap(pr1->y0,pr1->y1,pr2->y0,pr2->y1);
  }

/*** clear all blocks and nodes ***/
void clear_layout(LIST *blocks, LIST *nodes, LIST *subcells)
  {
  BLOCK *pb;
  NODE *pn;
  int i;

  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    list_free(pb->rects);
    list_free(pb->bboxs);
    list_free(pb->sboxs);
    }

  for (i=0; i<nodes->max; i++)
    {
    pn=&nodes->p.node[i];
    list_free(pn->ports);;
    list_free(pn->wires);
    }

  list_realloc(blocks,0);
  list_realloc(nodes,0);
  list_realloc(subcells,0);
  }

/*** translate a name ***/
char *translate_name(char *old, LIST *renames)
  {
  int i;
  if (old[strlen(old)-1]=='!') return old;
  i=find_element_lazy_sort(renames,old,&rename_str_cmp);
  if (i<0) return NULL;
  else return renames->p.rename[i].new;
  }

/*** fills in lots of initial values ***/
BLOCK create_block()
  {
  BLOCK block;
  block.fixed=0;
  block.column=0;
  block.rulenum=10000;
  block.type=0;
  block.dx=block.dy=0;
  block.x=block.y=0;
  block.rects=list_create(sizeof(RECT));
  block.bboxs=list_create(sizeof(RECT));
  block.sboxs=list_create(sizeof(RECT));
  block.bbox.x0=block.bbox.y0= 1e10;
  block.bbox.x1=block.bbox.y1=-1e10;
  block.bbox.paint=0;
  block.bbox.name=NULL;
  block.bbox.block=0;
  block.bbox.pr=NULL;
  block.filename=NULL;
  return block;
  }

/*** compute block bboxs, sboxs ***/
void finish_block(BLOCK *pb, int num)
  {
  int j,k;
  double x,y,dx,dy;
  RECT rect;
  
  /*** compute bbox, bboxs ***/
  for (j=0; j<pb->rects->max; j++)
    {
    rect=pb->rects->p.rect[j];
    rect.name=NULL;
    rect.block=num;
    rect.pr=NULL;
    rectangle_union(&pb->bbox,&rect);
    for (k=0; k<pb->bboxs->max; k++)
      if (pb->bboxs->p.rect[k].paint==rect.paint) break;
    if (k<pb->bboxs->max) rectangle_union(&pb->bboxs->p.rect[k],&rect);
    else list_append_element(pb->bboxs,&rect);
    }
  
  /*** create symmetrized sboxs from bboxs ***/
  x=0.5*(pb->bbox.x0 + pb->bbox.x1);
  y=0.5*(pb->bbox.y0 + pb->bbox.y1);
  for (j=0; j<pb->bboxs->max; j++)
    {
    rect=pb->bboxs->p.rect[j];
    rect.name=NULL;
    rect.block=num;
    rect.pr=NULL;
    dx=max(fabs(rect.x0-x),fabs(rect.x1-x));
    dy=max(fabs(rect.y0-y),fabs(rect.y1-y));
    rect.x0=x-dx;
    rect.x1=x+dx;
    rect.y0=y-dy;
    rect.y1=y+dy;
    list_append_element(pb->sboxs,&rect);
    }
  }

/*** parse a magfile, identify blocks and nodes ***/
int read_layout(char *filename, LIST *blocks, LIST *nodes,
                LIST *renames, LIST *stretches, LIST *subcells, int rulenum)
  {
  LIST *rects,*stretchlabels;
  SLAB slab;
  RECT rect,*pr,*pr2,*pr3;
  BLOCK block,*pb;
  NODE *pn;
  PORT *pp;
  FILE *file;
  LEX *lex;
  int pai,paint=-2,i,j,k,l,old_blocks;
  char *str,*pc;
  USE use;

  /*** read rectangles and ports ***/
  pc=find_filename(filename,"",SEARCH_PATH);
  if (pc!=NULL) file=fopen(pc,"rt");
  else file=NULL;
  if (file==NULL)
    {
    ERROR=1; fprintf(stderr,"ERROR: read_layout can't read %s.\n",filename);
    leak_free(pc);
    return 0;
    }
  lex=lex_file_with_name(file,pc);
  leak_free(pc);
  rects=list_create(sizeof(RECT));
  stretchlabels=list_create(sizeof(SLAB));
  while (!lex_is_eof(lex))
    {
    if (lex_is_sym(lex,"<<"))
      {
      lex_eat_sym(lex,"<<");
      if (lex_is_keyword(lex,"labels")) {lex_eat_id(lex); paint=-1;}
      else if (lex_is_keyword(lex,"checkpaint")||
	       lex_is_keyword(lex,"error_p")||
	       lex_is_keyword(lex,"error_s")||
	       lex_is_keyword(lex,"error_ps"))
	{lex_eat_id(lex); paint=-2;}
      else paint=lex_eat_paint(lex);
      lex_eat_sym(lex,">>");
      }
    else if (lex_eatif_keyword(lex,"rect")&&(paint>=0))
      {
      rect.x0=lex_eat_real(lex);
      rect.y0=lex_eat_real(lex);
      rect.x1=lex_eat_real(lex);
      rect.y1=lex_eat_real(lex);
      rect.name=NULL;
      rect.block=-1;
      rect.pr=NULL;
      rect.paint=paint;
      list_append_element(rects,&rect);
      }
    else if (lex_eatif_keyword(lex,"rlabel")&&(paint==-1))
      {
      pai=lex_eat_paint(lex);
      slab.x0=lex_eat_real(lex);
      slab.y0=lex_eat_real(lex);
      slab.x1=lex_eat_real(lex);
      slab.y1=lex_eat_real(lex);
      slab.dir=lex_eat_integer(lex);
      if (lex_eatif_sym(lex,"^")) /* stretch label */
	{
	str=parse_nodename(lex);
        slab.n=find_element_lazy_sort(stretches,str,&stretch_str_cmp);
        if (slab.n>=0) list_append_element(stretchlabels,&slab);
        else {ERROR=1; fprintf(stderr,"ERROR: unassigned stretch label %s in %s.\n",str,filename);}
	}
      else /* regular label */
	{
        char *name;
        name=get_name(parse_nodename(lex));
	str=translate_name(name,renames);
        if (str==NULL) str=name;
	add_port_extra(str,parse_extra(lex),pai,slab.x0,slab.y0,slab.x1,slab.y1,-1,nodes);
	}
      }
    else if (lex_eatif_keyword(lex,"use")) /* subcell instantiation */
      {
      use=parse_use(lex,0);
      list_append_element(subcells,&use);
      }
    else while (lex_eat_char(lex)!='\n');
    }
  lex_free(lex);
  fclose(file);

  /*** identify connected rectangles ***/
  for (i=0; i<rects->max; i++) rects->p.rect[i].pr=&rects->p.rect[i];
  for (i=0; i<rects->max; i++)
    {
    pr=&rects->p.rect[i];
    for (j=i+1; j<rects->max; j++)
      {
      pr2=&rects->p.rect[j];
      if (connected(pr,pr2))
        {
        /*** check for previous connection ***/
        pr3=pr2;
        do
          {
          if (pr3==pr) break;
          pr3=pr3->pr;
	  } while (pr3!=pr2);
 
        /*** create new connection ***/
        if (pr3!=pr)
	  {
          pr3=pr->pr;
          pr->pr=pr2->pr;
          pr2->pr=pr3;
	  }
        }
      }
    }

  /*** create blocks, attach rects ***/
  old_blocks=blocks->max;
  for (i=0; i<rects->max; i++) if (rects->p.rect[i].block<0)
    {
    pr=&rects->p.rect[i];

    /*** add connected rectangles to block ***/
    block=create_block();
    block.filename=leak_strdup(filename);
    block.rulenum=rulenum;
    pr2=pr;
    do
      {
      pr2->block=blocks->max;
      list_append_element(block.rects,pr2);
      pr2=pr2->pr;
      } while (pr2!=pr);

    /*** add to list ***/
    list_append_element(blocks,&block);
    }
  list_free(rects);

  /*** attach new ports to new blocks ***/
  for (i=0; i<nodes->max; i++)
    {
    pn=&nodes->p.node[i];
    for (j=0; j<pn->ports->max; j++)
      {
      pp=&pn->ports->p.port[j];
      for (k=old_blocks; (pp->block<0)&&(k<blocks->max); k++)
	{
        pb=&blocks->p.block[k];
        for (l=0; (pp->block<0)&&(l<pb->rects->max); l++)
          {
          pr=&pb->rects->p.rect[l];
          if ((pp->paint==pr->paint)&&
              overlap(pp->x0,pp->x1,pr->x0,pr->x1)&&
              overlap(pp->y0,pp->y1,pr->y0,pr->y1))
            {
            pp->block=pr->block;
            pr->name=pn->name; /*** can propagate by connectivity in extract.c ***/
            stretch_rect(&pp->x0,&pp->y0,&pp->x1,&pp->y1,stretches,stretchlabels);
	    }
	  }
	}
      }
    }

  /*** stretch rectangles ***/
  for (i=old_blocks; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    for (j=0; j<pb->rects->max; j++)
      {
      pr=&pb->rects->p.rect[j];
      stretch_rect(&pr->x0,&pr->y0,&pr->x1,&pr->y1,stretches,stretchlabels);
      }
    }

  /*** finish ***/
  list_free(stretchlabels);
  for (i=old_blocks; i<blocks->max; i++) finish_block(&blocks->p.block[i],i);
  return 1;
  }
