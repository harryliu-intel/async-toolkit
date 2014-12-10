#include "auto.h"
#include "exportskill.h"

/*** global variables ***/
double area_cost=1,cap_cost=1,overlap_cost=1,
       global_cap_cost_ratio=1,internal_cap_cost_ratio=1,
       xpitch=0,ypitch=0,Spacing[MAXPAINTS][MAXPAINTS][2],
       width_tolerance=1,epsilon=0,subcell_spacing=12;
int detail_level=0,GRADIENT=0,distance_measure=2,ERROR=0,
    Connect[MAXPAINTS][MAXPAINTS],Abut[MAXPAINTS][MAXPAINTS];
LIST *Paints;
LIST *Parms;
LIST *GlobalNames;
char *SEARCH_PATH=".:$AUTO_PATH";

/*** set a parm name/value pair ***/
void set_parm(char *name, double value)
  {
  int i;
  PARM *parm;
  parm=leak_malloc(sizeof(PARM));
  parm->name=leak_strdup(name);
  parm->value=value;
  i=find_element_lazy_sort(Parms,&parm,&parm_cmp);
  if (i<0) list_insert_element_lazy_sort(Parms,&parm,&parm_cmp);
  else {Parms->p.pparm[i]->value=value; leak_free(parm->name); leak_free(parm);}
  }

/*** return a pointer to parm value ***/
double *get_parm(char *name)
  {
  int i;
  i=find_element_lazy_sort(Parms,name,&parm_str_cmp);
  if (i<0) return NULL;
  else return &Parms->p.pparm[i]->value;
  }

/*** user interface ***/
int main (int argc, char *argv[])
  {
  char *filename,prsname[STRMAX],magname[STRMAX];
  FILE *fin[FILEMAX],*fout;
  LEX *lex[FILEMAX];
  LIST *blocks,*nodes,*subcells;
  LIST *renames,*stretches;
  RENAME rename;
  STRETCH stretch;
  RECT *pr,r[3];
  BLOCK *pb;
  PORT *pp;
  NODE *pn;
  CIRCUIT circuit;
  PARAMETERS parameters;
  double total,area,cap,overlap,
         x=0,y=0,t,*p,dx,dy,min_gradient,powerup,
         xmin,xmax,ymin,ymax;
  int i,j,maxiter,maxruns,tos;

  for (i=0; i<MAXPAINTS; i++)
    for (j=0; j<MAXPAINTS; j++)
      {
      Spacing[i][j][0]=Spacing[i][j][1]=-1;
      Connect[i][j]=(i==j);
      Abut[i][j]=0;
      }

  if (argc>1) sscanf(argv[1],"%d",&i);
  else i=0;
  srand48(i);

  setlinebuf(stdout);
  setlinebuf(stderr);
  setenv_from_file(find_filename(".castrc","","~"));
  default_sizing_options();
  Paints=list_create(sizeof(PAINT));
  Parms=list_create(sizeof(PARM *));
  blocks=list_create(sizeof(BLOCK));
  nodes=list_create(sizeof(NODE));
  subcells=list_create(sizeof(USE));
  renames=list_create(sizeof(RENAME));
  stretches=list_create(sizeof(STRETCH));
  GlobalNames=list_create(sizeof(char *));
  create_names();
  circuit=create_circuit();
  tos=0;
  fin[tos]=stdin;
  lex[tos]=lex_file(fin[tos]);
  while (tos>=0)
    {
    /*** handle included files ***/
    if (lex_is_eof(lex[tos]))
      {
      lex_free(lex[tos]);
      if (fin[tos]!=stdin) fclose(fin[tos]);
      tos--;
      }
    else if (lex_eatif_keyword(lex[tos],"read"))
      {
      char *pc;
      filename=lex_eat_quote_arg(lex[tos],argc,argv);
      pc=find_filename(filename,"",SEARCH_PATH);
      if (pc!=NULL) fin[tos+1]=fopen(pc,"rt");
      else fin[tos]=NULL;
      if (fin[tos]==NULL) {ERROR=1; fprintf(stderr,"ERROR: auto can't read %s.\n",filename);}
      else
        {
        tos++;
        lex[tos]=lex_file_with_name(fin[tos],pc);
	}
      leak_free(pc);
      }

    /*** get, set parms ***/
    else if (lex_eatif_keyword(lex[tos],"set"))
      {
      char *name;
      name=leak_strdup(lex_eat_id(lex[tos]));
      x=lex_eat_real(lex[tos]);
      set_parm(name,x);
      leak_free(name);
      }
    else if (lex_eatif_keyword(lex[tos],"default"))
      {
      char *name;
      name=leak_strdup(lex_eat_id(lex[tos]));
      x=lex_eat_real(lex[tos]);
      if (get_parm(name)==NULL) set_parm(name,x);
      leak_free(name);
      }
    else if (lex_eatif_keyword(lex[tos],"get"))
      {
      char *name;
      name=lex_eat_id(lex[tos]);
      p=get_parm(name);
      printf("%s = %g\n",name,p!=NULL?*p:0);
      }

    /*** load, print, and match circuit ***/
    else if (lex_eatif_keyword(lex[tos],"circuit"))
      {
      circuit=read_circuit("",lex_eat_quote_arg(lex[tos],argc,argv),0);
      list_realloc(subcells,0);
      list_copy(subcells,circuit.subcells);
      }
    else if (lex_eatif_keyword(lex[tos],"debugcircuit"))
      print_circuit(stderr,&circuit,1,1,1,1,1,1);
    else if (lex_eatif_keyword(lex[tos],"printcircuit"))
      {
      filename=lex_eat_quote_arg(lex[tos],argc,argv);
      fout=fopen(filename,"wt");
      print_circuit(fout,&circuit,1,0,1,1,1,1);
      fclose(fout);
      }
    else if (lex_eatif_keyword(lex[tos],"paths"))
      find_paths(&circuit);
    else if (lex_eatif_keyword(lex[tos],"matchcircuit"))
      {
      CIRCUIT sub;
      int rulenum;
      filename=lex_eat_quote_arg(lex[tos],argc,argv);
      safe_sprintf(prsname,"%s.prs",filename);
      safe_sprintf(magname,"%s.mag",filename);
      sub=read_circuit(filename,prsname,1);
      while (1)
	{
        list_realloc(renames,0);
        list_realloc(stretches,0);
        rulenum=10000;
        if (!match_circuit(&circuit,&sub,renames,stretches,0,0,&rulenum)) break;
	read_layout(magname,blocks,nodes,renames,stretches,subcells,rulenum);
	}
      }
    else if (lex_eatif_keyword(lex[tos],"stacks"))
      {
      stack_gen(&circuit,blocks,nodes,0);
      }
    else if (lex_eatif_keyword(lex[tos],"fullauto_stacks"))
      {
      stack_gen(&circuit,blocks,nodes,1);
      }
    else if (lex_eatif_keyword(lex[tos],"espresso"))
      {
      espresso(&circuit);
      }
    else if (lex_eatif_keyword(lex[tos],"fullauto"))
      {
      fullauto(blocks,nodes,&circuit);
      }
    else if (lex_eatif_keyword(lex[tos],"dump_verilog"))
      {
      propagate_nodes(blocks,nodes);
      dump_blocks(blocks,nodes,&circuit,0);
      }
    else if (lex_eatif_keyword(lex[tos],"dump_spice"))
      {
      propagate_nodes(blocks,nodes);
      dump_blocks(blocks,nodes,&circuit,1);
      }

    /*** define paints, spacing ***/
    else if (lex_is_keyword(lex[tos],"spacing")||lex_is_keyword(lex[tos],"connect")||
	     lex_is_keyword(lex[tos],"abut"))
      {
      LIST *A,*B;
      int a,b,type=0;

      /*** choose between spacing and connect ***/
      if      (lex_eatif_keyword(lex[tos],"spacing")) type=0;
      else if (lex_eatif_keyword(lex[tos],"connect")) type=1;
      else if (lex_eatif_keyword(lex[tos],"abut"))    type=2;

      /*** parse techfile style spacing rules ***/
      A=list_create(sizeof(int));
      B=list_create(sizeof(int));
      do {a=lex_eat_paint(lex[tos]); list_append_element(A,&a);} while (lex_eatif_sym(lex[tos],","));
      do {b=lex_eat_paint(lex[tos]); list_append_element(B,&b);} while (lex_eatif_sym(lex[tos],","));
      if (type==0)
	{
	x=lex_eat_real_arg(lex[tos],argc,argv);
	if      (lex_is_real(lex[tos])) y=lex_eat_real(lex[tos]);
	else if (lex_eatif_sym(lex[tos],"touching_illegal")) y= x;
	else if (lex_eatif_sym(lex[tos],"touching_ok"))      y=-1;
	else                                                 y=-1;
	if (lex_is_quote(lex[tos])) lex_eat_quote(lex[tos]); /* ignore optional Magic error message */
        }

      /*** store all pairs in spacing table ***/
      for (i=0; i<A->max; i++) for (j=0; j<B->max; j++)
        {
	a=A->p.i[i]; b=B->p.i[j];
	if (type==0)
	  {
	  Spacing[a][b][0]=Spacing[b][a][0]=max(Spacing[a][b][0],x);
	  Spacing[a][b][1]=Spacing[b][a][1]=max(Spacing[a][b][1],y);
          Paints->p.paint[a].maxspacing=max(Paints->p.paint[a].maxspacing,max(x,y));
	  Paints->p.paint[b].maxspacing=max(Paints->p.paint[b].maxspacing,max(x,y));
	  }
	else if (type==1) Connect[a][b]=Connect[b][a]=1;
	else if (type==2) Abut[a][b]=Abut[b][a]=1;
        }
      list_free(A);
      list_free(B);
      }
    else if (lex_eatif_keyword(lex[tos],"clearpaint"))
      {
      list_realloc(Paints,0);
      for (i=0; i<MAXPAINTS; i++)
        for (j=0; j<MAXPAINTS; j++)
          Spacing[i][j][0]=Spacing[i][j][1]=-1;
      }
    else if (lex_eatif_keyword(lex[tos],"debugpaint"))
      {
      printf("Paints:\n");
      for (i=0; i<Paints->max; i++)
        printf("  %s maxspacing=%g\n",
          Paints->p.paint[i].name,
          Paints->p.paint[i].maxspacing);
      printf("Spacing:\n");
      for (i=0; i<Paints->max; i++)
        for (j=0; j<Paints->max; j++)
          if (Spacing[i][j][0]>=0)
            printf("  %s %s %g %g\n",
              Paints->p.paint[i].name,
              Paints->p.paint[j].name,
              Spacing[i][j][0],Spacing[i][j][1]);
      }

    /*** set parameters ***/
    else if (lex_eatif_keyword(lex[tos],"seed"))
      srand48(lex_eat_integer_arg(lex[tos],argc,argv));
    else if (lex_eatif_keyword(lex[tos],"detail_level"))
      detail_level=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"distance_measure"))
      distance_measure=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"xpitch"))
      {ypitch=0; xpitch=lex_eat_real_arg(lex[tos],argc,argv);}
    else if (lex_eatif_keyword(lex[tos],"ypitch"))
      {xpitch=0; ypitch=lex_eat_real_arg(lex[tos],argc,argv);}
    else if (lex_eatif_keyword(lex[tos],"width_tolerance"))
      width_tolerance=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"epsilon"))
      epsilon=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"subcell_spacing"))
      subcell_spacing=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"cap_cost"))
      cap_cost=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"global_cap_cost_ratio"))
      global_cap_cost_ratio=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"internal_cap_cost_ratio"))
      internal_cap_cost_ratio=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"area_cost"))
      area_cost=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"overlap_cost"))
      overlap_cost=lex_eat_real_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"transistor_length"))
      transistor_length=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"transistor_spacing"))
      transistor_spacing=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"transistor_contact_spacing"))
      transistor_contact_spacing=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"contact_width"))
      contact_width=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"diffusion_overhang"))
      diffusion_overhang=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"poly_overhang"))
      poly_overhang=lex_eat_integer_arg(lex[tos],argc,argv);
    else if (lex_eatif_keyword(lex[tos],"parameters"))
      {
      printf("epsilon=%g lambda\n",epsilon);
      printf("subcell_spacing=%g lambda\n",subcell_spacing);
      printf("detail_level=%d\n",detail_level);
      printf("distance_measure=%d\n",distance_measure);
      printf("xpitch=%g ypitch=%g\n",xpitch,ypitch);
      printf("width_tolerance=%g\n",width_tolerance);
      printf("area_cost=%g\n",area_cost);
      printf("cap_cost=%g\n",cap_cost);
      printf("global_cap_cost_ratio=%g\n",global_cap_cost_ratio);
      printf("internal_cap_cost_ratio=%g\n",internal_cap_cost_ratio);
      printf("overlap_cost=%g\n",overlap_cost);
      }

    /*** create and destory circuits ***/
    else if (lex_eatif_keyword(lex[tos],"clear"))
      clear_layout(blocks,nodes,subcells);
    else if (lex_eatif_keyword(lex[tos],"global"))
      {
      char *pc;
      pn=add_node(parse_nodename(lex[tos]),nodes);
      pn->global=1;
      pc=leak_strdup(pn->name);
      list_insert_element_sorted(GlobalNames,&pc,&str_cmp);
      }
    else if (lex_eatif_keyword(lex[tos],"rename"))
      {
      rename.old=get_name(parse_nodename(lex[tos]));
      rename.new=get_name(parse_nodename(lex[tos]));
      list_insert_element_lazy_sort(renames,&rename,&rename_cmp);
      }
    else if (lex_eatif_keyword(lex[tos],"clearrenames"))
      list_realloc(renames,0);
    else if (lex_eatif_keyword(lex[tos],"debugrenames"))
      {
      printf("Renames:\n");
      for (i=0; i<renames->max; i++)
	printf("  %s -> %s\n",renames->p.rename[i].old,renames->p.rename[i].new);
      }
    else if (lex_eatif_keyword(lex[tos],"stretch"))
      {
      stretch=parse_stretch(lex[tos]);
      i=find_element_lazy_sort(stretches,&stretch,&stretch_cmp);
      if (i<0) list_insert_element_lazy_sort(stretches,&stretch,&stretch_cmp);
      else stretches->p.stretch[i]=stretch;
      }
    else if (lex_eatif_keyword(lex[tos],"clearstretches"))
      list_realloc(stretches,0);
    else if (lex_eatif_keyword(lex[tos],"debugstretches"))
      {
      printf("Stretches:\n");
      for (i=0; i<stretches->max; i++)
	print_stretch(&stretches->p.stretch[i]);
      }
    else if (lex_eatif_keyword(lex[tos],"load"))
      read_layout(lex_eat_quote_arg(lex[tos],argc,argv),
		  blocks,nodes,renames,stretches,subcells,-1);
    else if (lex_eatif_keyword(lex[tos],"debugblocks"))
      {
      printf("Blocks:\n");
      for (i=0; i<blocks->max; i++)
        {
        pb=&blocks->p.block[i];
        printf("  %d x=%g y=%g\n",i,pb->x,pb->y);
        for (j=0; j<pb->rects->max; j++)
          {
          pr=&pb->rects->p.rect[j];
          printf("    %s x0=%g y0=%g x1=%g y1=%g\n",
            Paints->p.paint[pr->paint].name,
            pr->x0,pr->y0,pr->x1,pr->y1);
	  }
	}
      }
    else if (lex_eatif_keyword(lex[tos],"debugnodes"))
      {
      printf("Nodes:\n");
      for (i=0; i<nodes->max; i++)
        {
        pn=&nodes->p.node[i];
        printf("  %s connects to:\n",pn->name);
        for (j=0; j<pn->ports->max; j++)
          {
          pp=&pn->ports->p.port[j];
          printf("    block %d at x0=%g y0=%g x1=%g y1=%g \n",
            pp->block,pp->x0,pp->y0,pp->x1,pp->y1);
	  }
	}
      }

    /*** parse magic and give feedback ***/
    else if (lex_eatif_keyword(lex[tos],"magic_instances"))
      {
      LIST *aliases,*instances;
      aliases=list_create(sizeof(ALIAS *));
      instances=list_create(sizeof(INSTANCE));
      parse_mag(lex_eat_quote_arg(lex[tos],argc,argv),"",identity_transform(),NULL,aliases,instances,1);
      for (i=0; i<instances->max; i++)
	printf("%s %s\n",instances->p.instance[i].type, instances->p.instance[i].name);
      list_free(aliases);
      list_free(instances);
      /* mem leak */
      }
    else if (lex_eatif_keyword(lex[tos],"magic_subcells"))
      {
      LIST *aliases,*instances;
      INSTANCE *pinst;
      aliases=list_create(sizeof(ALIAS *));
      instances=list_create(sizeof(INSTANCE));
      parse_mag(lex_eat_quote_arg(lex[tos],argc,argv),"",identity_transform(),NULL,aliases,instances,0);
      for (i=0; i<instances->max; i++)
	{
	pinst=&instances->p.instance[i];
	printf("%s %s\n",pinst->type,pinst->name);
        }
      list_free(aliases);
      list_free(instances);
      /* mem leak */
      }
    else if (lex_eatif_keyword(lex[tos],"magic_cellcounts"))
      {
      LIST *aliases,*instances,*cells;
      char *pc,*pcn;
      aliases=list_create(sizeof(ALIAS *));
      instances=list_create(sizeof(INSTANCE));
      parse_mag(lex_eat_quote_arg(lex[tos],argc,argv),"",identity_transform(),NULL,aliases,instances,1);
      cells=list_create(sizeof(char *));
      for (i=0; i<instances->max; i++)
	list_append_element(cells,&instances->p.instance[i].type);
      list_sort(cells,&str_cmp);
      j=1;
      for (i=0; i<cells->max; i++, j++)
	{
        pc=cells->p.pc[i];
	if (i+1<cells->max) pcn=cells->p.pc[i+1];
	else pcn="";
	if (strcmp(pc,pcn)!=0) {printf("%6d %s\n",j,pc); j=0;}
        }
      list_free(aliases);
      list_free(instances);
      /* mem leak */
      }
    else if (lex_eatif_keyword(lex[tos],"magic_depend"))
      {
      LIST *cells;
      cells=list_create(sizeof(char *));
      magic_depend(lex_eat_quote_arg(lex[tos],argc,argv),cells);
      for (i=0; i<cells->max; i++) printf("%s\n",cells->p.pc[i]);
      list_free(cells);
      /* mem leak */
      }

    /*** some layout altering commands ***/
    else if (lex_eatif_keyword(lex[tos],"center"))
      {
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
	if (pb->fixed) continue;
        pb->x=-0.5*(pb->bbox.x0+pb->bbox.x1);
        pb->y=-0.5*(pb->bbox.y0+pb->bbox.y1);
	}
      }
    else if (lex_eatif_keyword(lex[tos],"align"))
      {
      int llx,lly;

      /*** find lower left corner ***/
      llx=lly=2000000000;
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
	for (j=0; j<pb->rects->max; j++)
	  {
	  pr=&pb->rects->p.rect[j];
	  llx=min(llx,round(pb->x+pr->x0));
	  llx=min(llx,round(pb->x+pr->x1));
	  lly=min(lly,round(pb->y+pr->y0));
	  lly=min(lly,round(pb->y+pr->y1));
	  }
        }

      /*** displace all blocks to align lowerleft bbox of all paint at 0,0  ***/
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
	pb->x-=llx;
	pb->y-=lly;
        }
      }

    else if (lex_eatif_keyword(lex[tos],"jiggle"))
      {
      t=lex_eat_real_arg(lex[tos],argc,argv);
      for (i=0; i<blocks->max; i++)
        {
        pb=&blocks->p.block[i];
	if (pb->fixed) continue;
        pb->x+=(drand48()*2-1)*t;
        pb->y+=(drand48()*2-1)*t;
	}
      }
    else if (lex_eatif_keyword(lex[tos],"place"))
      {
      LIST *sorted_blocks;
      double maxspacing=0,h,w;
      for (i=0; i<Paints->max; i++)
	maxspacing=max(maxspacing,Paints->p.paint[i].maxspacing);
      sorted_blocks=list_create(sizeof(BLOCK *));
      for (i=0; i<blocks->max; i++)
	{
	pb=&blocks->p.block[i];
	list_append_element(sorted_blocks,&pb);
        }
      list_sort(sorted_blocks,&pblock_width_cmp);
      x=y=dx=0;
      for (i=0; i<sorted_blocks->max; i++)
	{
        pb=sorted_blocks->p.pblock[i];
	h=pb->bbox.y1-pb->bbox.y0;
	w=pb->bbox.x1-pb->bbox.x0;
	if ((circuit.ypitch>0)&&(y+h+maxspacing>circuit.ypitch))
	  {
	  x+=dx+maxspacing;
	  y=dx=0;
          }
	pb->x= x - pb->bbox.x0;
	pb->y= y - pb->bbox.y0;
	y+=h+maxspacing;
	dx=max(dx,w);
        }
      list_free(sorted_blocks);
      }
    else if (lex_eatif_keyword(lex[tos],"expand"))
      {
      x=lex_eat_real_arg(lex[tos],argc,argv);
      y=lex_eat_real_arg(lex[tos],argc,argv);
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
	if (pb->fixed) continue;
        dx=(x-1)*0.5*(pb->bbox.x0+pb->bbox.x1);
        dy=(y-1)*0.5*(pb->bbox.y0+pb->bbox.y1);
        pb->x+=dx;
        pb->y+=dy;
	}
      }
    else if (lex_eatif_keyword(lex[tos],"bloat_paint")) /*** bloats rectangles of specified paint ***/
      {
      int paint,bloat;
      paint=lex_eat_paint(lex[tos]);
      bloat=lex_eat_integer(lex[tos]);
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
        for (j=0; j<pb->rects->max; j++)
	  {
	  pr=&pb->rects->p.rect[j];
          if (pr->paint==paint)
	    {
            pr->x0-=bloat;
	    pr->x1+=bloat;
	    pr->y0-=bloat;
	    pr->y1+=bloat;
	    }
	  }
        finish_block(pb,i);
	}
      }
    else if (lex_eatif_keyword(lex[tos],"extra_paint")) /*** creates rect with a different paint ***/
      {
      int from,to,rmax;
      RECT rect;
      from=lex_eat_paint(lex[tos]);
      to=lex_eat_paint(lex[tos]);
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
	rmax=pb->rects->max;
        for (j=0; j<rmax; j++)
	  {
	  pr=&pb->rects->p.rect[j];
          if (pr->paint==from) {rect=*pr; rect.paint=to; list_append_element(pb->rects,&rect);}
	  }
        finish_block(pb,i);
	}
      }
    else if (lex_eatif_keyword(lex[tos],"convert_paint")) /*** converts paint ***/
      {
      int from,to;
      from=lex_eat_paint(lex[tos]);
      to=lex_eat_paint(lex[tos]);
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
        for (j=0; j<pb->rects->max; j++)
	  {
	  pr=&pb->rects->p.rect[j];
          if (pr->paint==from) pr->paint=to;
	  }
        finish_block(pb,i);
	}
      for (i=0; i<nodes->max; i++)
	{
	pn=&nodes->p.node[i];
	for (j=0; j<pn->ports->max; j++)
	  {
          pp=&pn->ports->p.port[j];
	  if (pp->paint==from) pp->paint=to;
	  }
        }
      }
    else if (lex_eatif_keyword(lex[tos],"label_layer")) /*** relabels specified layers ***/
      {
      int paint;
      char *label;
      paint=lex_eat_paint(lex[tos]);
      label=lex_eat_quote(lex[tos]);

      /*** strip old labels ***/
      for (i=0; i<nodes->max; i++)
	{
	pn=&nodes->p.node[i];
	for (j=0; j<pn->ports->max; j++)
	  {
          pp=&pn->ports->p.port[j];
	  if (pp->paint==paint) list_remove_element(pn->ports,j--);
	  }
        }

      /*** add new labels ***/
      for (i=0; i<blocks->max; i++)
	{
        pb=&blocks->p.block[i];
        for (j=0; j<pb->rects->max; j++)
	  {
	  pr=&pb->rects->p.rect[j];
          if (pr->paint==paint)
	    add_port(label,paint,pr->x0,pr->y0,pr->x1,pr->y1,pr->block,nodes);
	  }
	}
      }

    /*** routing ***/
    else if (lex_eatif_keyword(lex[tos],"route"))
      for (i=0; i<nodes->max; i++)
	{
        pn=&nodes->p.node[i];
        list_free(pn->wires);
        pn->wires=wire_ports(pn->ports,blocks);
	}

    /*** extraction ***/
    else if (lex_eatif_keyword(lex[tos],"extract"))
      {
      propagate_nodes(blocks,nodes);
      }

    /*** output commands ***/
    else if (lex_eatif_keyword(lex[tos],"layout"))
      {
      propagate_nodes(blocks,nodes);
      write_layout(lex_eat_quote_arg(lex[tos],argc,argv),blocks,nodes,subcells,-1);
      }

    /*** writes out skill file ***/
    else if (lex_eatif_keyword(lex[tos],"outputskill"))
      {
      propagate_nodes(blocks,nodes);
      export_skill_layout(lex_eat_quote_arg(lex[tos],argc,argv),blocks,nodes,subcells,-1);
      }

    /*** energy and minimization commands ***/
    else if (lex_eatif_keyword(lex[tos],"energy"))
      {
      total=compute_energy(blocks,nodes,
        &overlap,&area,&cap,&xmin,&ymin,&xmax,&ymax);
      dx=xmax-xmin;
      dy=ymax-ymin;
      printf("Variables = %d\n",sizeof_state(blocks));
      printf("Width = %g\n",dx);
      printf("Height = %g\n",dy);
      printf("Area = %g\n",(xpitch>0?xpitch:dx)*(ypitch>0?ypitch:dy));
      printf("Area Energy = %g * %g = %g\n",
        area/area_cost,area_cost,area);
      printf("Capacitance Energy = %g * %g = %g\n",
        cap/cap_cost,cap_cost,cap);
      printf("Overlap Energy = %g * %g = %g\n",
        overlap/overlap_cost,overlap_cost,overlap);
      printf("Total Energy = %g\n",total);
      }
    else if (lex_eatif_keyword(lex[tos],"minimize"))
      {
      x=lex_eat_real_arg(lex[tos],argc,argv);
      maxiter=(int)(x*sizeof_state(blocks));
      maxiter=max(4,maxiter);
      min_gradient=lex_eat_real_arg(lex[tos],argc,argv);
      maxruns=lex_eat_integer_arg(lex[tos],argc,argv);
      powerup=lex_eat_real_arg(lex[tos],argc,argv);
      if (blocks->max<=1) continue;
      parameters.blocks=blocks;
      parameters.nodes=nodes;
      parameters.N=sizeof_state(blocks);
      p=(double *) leak_malloc(sizeof(double)*parameters.N);
      pack_state(blocks,p);
      for (i=0; i<maxruns; i++,overlap_cost*=powerup)
	{
        printf("minimizing with overlap_cost=%g\n",overlap_cost);
        minimize(stdout,&parameters,
          (ENERGY *) &energy,(DOWN *) &down,
          parameters.N,p,maxiter,min_gradient,0);
	}
      unpack_state(blocks,p);
      leak_free(p);
      }

    /*** debug overlap energy ***/
    else if (lex_eatif_keyword(lex[tos],"testoverlap"))
      {
      r[0].x0=-1; r[0].x1=1; r[0].block=-1;
      r[0].y0=-1; r[0].y1=1; r[1].block=-1;
      for (x=-2; x<=2; x+=0.2)
	{
        for (y=-2; y<=2; y+=0.2)
          {
          r[1].x0=x-0.1;
          r[1].y0=y-0.1;
          r[1].x1=x+0.1;
          r[1].y1=y+0.1;
          printf("%g\n",overlap_energy(blocks,&r[0],&r[1],1));
	  }
        printf("\n");
	}
      }

    /*** comments, echos ***/
    else if (lex_is_sym(lex[tos],"//")||lex_is_sym(lex[tos],"#")) while (lex_eat_char(lex[tos])!='\n');
    else if (lex_eatif_keyword(lex[tos],"echo"))
      {
      lex_push_position(lex[tos]);
      while (lex_eat_char(lex[tos])!='\n');
      fprintf(stderr,"%s",lex_get_token(lex[tos]));
      lex_pop_position(lex[tos]);
      }

    /*** sizing options parsed in size.c ***/
    else if (parse_sizing_options(lex[tos],argc,argv));

    /*** syntax error ***/
    else
      {
      lex_push_position(lex[tos]);
      while (lex_eat_char(lex[tos])!='\n');
      ERROR=1; fprintf(stderr,"SYNTAX ERROR: %s",lex_get_token(lex[tos]));
      lex_pop_position(lex[tos]);
      }
    }

  /*** free and check for memory leaks ***/
  list_free(Paints);
  list_free(blocks);
  list_free(nodes);
  list_free(subcells);
  list_free(renames);
  list_free(stretches);
  free_names();
  free_temp_list();
  leak_check();
  return ERROR;
  }
