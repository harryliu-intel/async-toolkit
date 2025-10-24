#include "auto.h"

/*** globals for stack layout ***/
int transistor_length=3,transistor_spacing=5,transistor_contact_spacing=1,
    contact_width=8,diffusion_overhang=6,poly_overhang=4;

char *VDD="Vdd!",*GND="GND!";

extern void real_add_port(char *name, char* labelstr, int paint, int x0, int y0, int x1, int y1, int block, LIST *nodes) ;


/*** get a unique internal node name ***/
int unique_num=0;
char *unique_name()
  {
  char name[STRMAX];
  safe_sprintf(name,"%d#",unique_num++);
  return get_name(name);
  }

/*** check if its internal (ends in # or %) ***/
int is_internal(char *name)
  {
  int l;
  l=strlen(name);
  if (name[l-1]=='#') return 1;
  if (name[l-1]=='%') return 1;
  return 0;
  }

/*** debugging  info ***/
void print_gate(GATE *pg)
  {
  printf("%s s=%8s g=%8s d=%8s\n",pg->dir?"P":"N",pg->s,pg->g,pg->d);
  }

/**************************** TURN GATES INTO STACKS ***************************/

/*** flip a stack upside down ***/
void flip_stack(STACK *ps)
  {
  int i;
  char *t;
  GATE *pg;
  LIST *new;

  t=ps->d;
  ps->d=ps->s;
  ps->s=t;
  new=list_create(sizeof(GATE));
  for (i=ps->gates->max-1; i>=0; i--)
    {
    pg=&ps->gates->p.gate[i];
    t=pg->s;
    pg->s=pg->d;
    pg->d=t;
    list_append_element(new,pg);
    }
  list_move(ps->gates,new);
  }

/*** check if node is a power supply ***/
int is_power(char *s)
  {
  if (s==NULL) return 0;
  return (strcmp(s,VDD)==0)||(strcmp(s,GND)==0);
  }

/*** rotate nth element to first position ***/
void rotate_stack(STACK *stack, int n)
  {
  int i;
  LIST *new;

  new=list_create(sizeof(GATE));
  for (i=n; i<stack->gates->max; i++)
    list_append_element(new,&stack->gates->p.gate[i]);
  for (i=0; i<n; i++)
    list_append_element(new,&stack->gates->p.gate[i]);
  list_move(stack->gates,new);
  stack->s=stack->gates->p.gate[0].s;
  stack->d=stack->gates->p.gate[stack->gates->max-1].d;
  }

/*** attempt to splice looped stack ps1 into stack ps2 ***/
int try_splice(STACK *ps1, STACK *ps2)
  {
  int i,j;
  char *pc1,*pc2;
  for (i=0; i<ps1->gates->max; i++)
    {
    pc1=ps1->gates->p.gate[i].s;
    if (pc1==NULL) continue;
    for (j=0; j<ps2->gates->max+1; j++)
      {
      pc2=(j<ps2->gates->max) ? ps2->gates->p.gate[j].s : ps2->gates->p.gate[j-1].d;
      if (pc2==NULL) continue;
      if ((pc1==pc2)&&(!is_power(pc1)))
	{
        rotate_stack(ps1,i);
        list_insert_list(ps2->gates,ps1->gates,j);
	return 1;
	}
      }
    }
  return 0;
  }

/*** is it ok to share these contacts ***/
int can_share_contacts(char *a, char *b)
  {
  return (a==b);
  }

/*** count the spokes on a node, +1 for named ***/
int count_spokes(char *name, LIST *gates)
  {
  int i,count=0;
  GATE *pg;
  for (i=0; i<gates->max; i++)
    {
    pg=&gates->p.gate[i];
    if (pg->s==name) count++;
    if (pg->d==name) count++;
    }
  if (!is_internal(name)) count+=2;
  return count;
  }

/*** make stacks from gates ***/
LIST *stack_gates(LIST *gates)
  {
  LIST *stacks;
  int i,j;
  GATE gate;
  STACK stack,*ps1,*ps2;

  /*** create a stack for each gate ***/
  stacks=list_create(sizeof(STACK));
  for (i=0; i<gates->max; i++)
    {
    /*** create a new stack ***/
    gate=gates->p.gate[i];
    stack.s=gate.s;
    stack.d=gate.d;
    stack.dir=gate.dir;
    stack.minwidth=stack.maxwidth=get(gate.width);
    stack.gates=list_create(sizeof(GATE));
    list_append_element(stack.gates,&gate);
    list_append_element(stacks,&stack);
    }

  /*** concatenate stacks into series stacks ***/
  for (i=0; i<stacks->max; i++)
    {
    j=i+1;
    while(j<stacks->max)
      {
      ps1=&stacks->p.stack[i];
      ps2=&stacks->p.stack[j];
      if      (ps1->dir!=ps2->dir) j++;
      else if (max(ps1->maxwidth,ps2->maxwidth)>
               min(ps1->minwidth,ps2->minwidth)*width_tolerance) j++;
      else
	{
        if      (can_share_contacts(ps1->d,ps2->d)) flip_stack(ps2);
	else if (can_share_contacts(ps1->s,ps2->s)) flip_stack(ps1);
	else if (can_share_contacts(ps1->s,ps2->d)) {flip_stack(ps1); flip_stack(ps2);}
        if      (can_share_contacts(ps1->d,ps2->s)&&(count_spokes(ps1->d,gates)==2))
	  {
	  ps1->gates->p.gate[ps1->gates->max-1].d=NULL;
	  ps2->gates->p.gate[0].s=NULL;
          ps1->minwidth=min(ps1->minwidth,ps2->minwidth);
          ps1->maxwidth=max(ps1->maxwidth,ps2->maxwidth);
	  ps1->d=ps2->d;
	  list_append_list(ps1->gates,ps2->gates);
	  list_remove_element(stacks,j);
          j=i+1;
	  }
        else j++;
	}
      }
    }

  return stacks;
  }

/*** merge stacks together ***/
void merge_stacks(LIST *stacks)
  {
  int i,j,H;
  STACK *ps1,*ps2;
  GATE *pg;
  double *pH;

  /*** get maxstackedgates ***/
  pH=get_parm("maxstackedgates");
  if (pH!=NULL) H=(int) *pH;
  else H=10000;

  /*** next, concatenate stacks into bigger stacks ***/
  for (i=0; i<stacks->max; i++)
    {
    j=i+1;
    while(j<stacks->max)
      {
      ps1=&stacks->p.stack[i];
      ps2=&stacks->p.stack[j];
      if      (ps1->dir!=ps2->dir) j++;
      else if (max(ps1->maxwidth,ps2->maxwidth)>
               min(ps1->minwidth,ps2->minwidth)*width_tolerance) j++;
      else
	{
        if      (can_share_contacts(ps1->d,ps2->d)) flip_stack(ps2);
	else if (can_share_contacts(ps1->s,ps2->s)) flip_stack(ps1);
	else if (can_share_contacts(ps1->s,ps2->d)) {flip_stack(ps1); flip_stack(ps2);}
        if      (can_share_contacts(ps1->d,ps2->s) && (ps1->gates->max + ps2->gates->max <= H))
	  {
          ps1->minwidth=min(ps1->minwidth,ps2->minwidth);
          ps1->maxwidth=max(ps1->maxwidth,ps2->maxwidth);
	  ps1->d=ps2->d;
	  list_append_list(ps1->gates,ps2->gates);
	  list_remove_element(stacks,j);
          j=i+1;
	  }
        else j++;
	}
      }
    }

  /*** splice looped stacks into other stacks ***/
  for (i=0; i<stacks->max; i++) for (j=0; j<stacks->max; j++)
    {
    ps1=&stacks->p.stack[i];
    ps2=&stacks->p.stack[j];
    if (ps1->s!=ps1->d) break;
    if (i==j) continue;
    if (ps1->dir!=ps2->dir) continue;
    if (ps1->gates->max + ps2->gates->max > H) continue;
    if (max(ps1->maxwidth,ps2->maxwidth)>min(ps1->minwidth,ps2->maxwidth)*width_tolerance) continue;
    if (try_splice(ps1,ps2)) {list_remove_element(stacks,i); i=j=0;}
    }

  /*** put power at bottom of stack ***/
  for (i=0; i<stacks->max; i++)
    {
    ps1=&stacks->p.stack[i];
    if (is_power(ps1->d)&&!is_power(ps1->s)) flip_stack(ps1);
    }

  /*** try to get power supplies at ends of a looped stack ***/
  for (i=0; i<stacks->max; i++)
    {
    ps1=&stacks->p.stack[i];
    if (ps1->s!=ps1->d) continue;
    if (is_power(ps1->s)) continue;
    for (j=0; j<ps1->gates->max; j++)
      {
      pg=&ps1->gates->p.gate[j];
      if (is_power(pg->s)) {rotate_stack(ps1,j); break;}
      }
    }
  }

/****************************** LAYOUT STACKS ******************************/

/*** state structure for fullauto stacks ***/
typedef struct _fullauto_state
  {
  int side;     /* which side to use */
  int pitch[2]; /* current available m2 pitch on each side */
  int offset[3]; /* offset contact left or right */
  char *last_contact[3][2]; /* last contact in each position and offset */
  int last_contact_y[3][2]; /* top of each contact for each position and offset */
  } FULLAUTO_STATE;

/*** make the next contact ***/
void make_contact(double w, double *y, char *name, int paint, int metal1, BLOCK *pb,
                  LIST *blocks, LIST *nodes, int fullauto, FULLAUTO_STATE *state)
  {
  RECT rect;
  int h,offset,match;
  if (name!=NULL) /* labeled contact */
    {
    /*** pick contact location ***/
    if (fullauto)
      {
      if (state->offset[1]==0) {rect.x0=-w/2; rect.x1=-2;}
      else                     {rect.x0=2; rect.x1=w/2;}
      }
    else
      {
      if (name[strlen(name)-1]=='!') {rect.x0=-w/2; rect.x1=w/2;}
      else {rect.x0=-w/2+contact_width; rect.x1=+w/2-contact_width;}
      rect.x0=min(-contact_width/2,rect.x0);
      rect.x1=max(+contact_width/2,rect.x1);
      }
    rect.y0=*y+transistor_contact_spacing;
    rect.y1=rect.y0 + contact_width;
    rect.name=name;
    rect.block=blocks->max;
    rect.paint=paint;  list_append_element(pb->rects,&rect);
    rect.paint=metal1; list_append_element(pb->rects,&rect);
    add_port(name,metal1,rect.x0,rect.y0,rect.x1,rect.y1,blocks->max,nodes);

    /*** more stuff for fullauto ***/
    if (fullauto)
      {
      if      ((state->last_contact[1][0]!=NULL)&&(state->last_contact[1][0]==name)&&
	       (rect.y0-state->last_contact_y[1][0]<=96)) match=offset=0;
      else if ((state->last_contact[1][1]!=NULL)&&(state->last_contact[1][1]==name)&&
	       (rect.y0-state->last_contact_y[1][1]<=96)) match=offset=1;
      else {match=-1; offset=state->offset[1];}
      if (match>=0)
	{
        /*** vertical m1 shortcut ***/
        rect.y0=state->last_contact_y[1][offset];
        if (offset) {rect.x0=2;   rect.x1=10;}
        else        {rect.x0=-10; rect.x1=-2;}
        rect.paint=metal1; list_append_element(pb->rects,&rect);
        }
      else
        {
        /*** find picth ***/
        h=(state->pitch[state->side])*12;
        while (h<rect.y0-6) {h+=12; state->pitch[state->side]++;} /* skip metal2 slots */
        rect.y0=h; rect.y1=h+6;

        /*** m2contact, m3contact for Vdd/GND ***/
        if (strcmp(name,GND)==0)
	  {
          rect.x0=max(rect.x0,-21);
          rect.x1=min(rect.x1,+21);
          rect.paint=find_paint("m2contact"); list_append_element(pb->rects,&rect);
	  rect.x0=min(rect.x0,-21);
          rect.paint=find_paint("metal2"); list_append_element(pb->rects,&rect);
          rect.x0=-21; rect.x1=-3;
	  rect.paint=find_paint("m3contact"); list_append_element(pb->rects,&rect);
          }
        else if (strcmp(name,VDD)==0)
          {
          rect.x0=max(rect.x0,-21);
          rect.x1=min(rect.x1,+21);
          rect.paint=find_paint("m2contact"); list_append_element(pb->rects,&rect);
	  rect.x1=max(rect.x1,21);
	  rect.paint=find_paint("metal2"); list_append_element(pb->rects,&rect);
	  rect.x0=3; rect.x1=21;
	  rect.paint=find_paint("m3contact"); list_append_element(pb->rects,&rect);
          }
	else /*** m2contact, metal2 and label ***/
	  {
	  rect.name=name;
          rect.x0=max(-21,rect.x0);
	  rect.x1=min(+21,rect.x1);
          rect.paint=find_paint("m2contact"); list_append_element(pb->rects,&rect);
          rect.x0=min(0,rect.x0);
	  rect.x1=max(0,rect.x1);
	  rect.paint=find_paint("metal2"); list_append_element(pb->rects,&rect);
	  add_port(name,rect.paint,rect.x0,rect.y0,rect.x1,rect.y1,blocks->max,nodes);
	  }
        }

      /*** flip offset, consume pitch ***/
      state->last_contact[1][offset]=name;
      state->last_contact_y[1][offset]=rect.y1;
      state->offset[1]=!offset;
      state->pitch[state->side]++;
      }

    /*** move y, flip side ***/
    *y+=2*transistor_contact_spacing + contact_width;
    state->side=!state->side;
    }
  else *y+=transistor_spacing;
  }

/*** layout the gate ***/
void make_gate(double w, double *y, char *name, int poly, int fet, int diff, BLOCK *pb,
               LIST *blocks, LIST *nodes, int fullauto, FULLAUTO_STATE *state)
  {
  RECT rect;
  int h,offset,match;
  char* NetNameBuffer;
  const char* NetNameFormatStr = "pin|w|%d|poly";


  /*** paint the poly endcap ***/
  rect.paint=poly;
  rect.name=name;
  rect.block=blocks->max;
  rect.y0=*y;
  rect.y1=*y+transistor_length;
  rect.x0=-w/2-poly_overhang;
  rect.x1=-w/2;
  list_append_element(pb->rects,&rect);
  rect.x0=w/2;
  rect.x1=w/2+poly_overhang;
  list_append_element(pb->rects,&rect);

  /*** paint the gate ***/
  rect.paint=fet;
  rect.x0=-w/2;
  rect.x1= w/2;
  list_append_element(pb->rects,&rect);

  /*** label the poly endcap ***/
  NetNameBuffer = ( char* ) leak_malloc( sizeof( char ) * ( strlen( name ) + strlen( NetNameFormatStr ) + 1 ) );
  safe_sprintf( NetNameBuffer, NetNameFormatStr, ( ( int ) *y ) );
  add_port_extra(name, NetNameBuffer,poly,rect.x0-poly_overhang,rect.y0,rect.x0,rect.y1,blocks->max,nodes);
  add_port_extra(name, NetNameBuffer,poly,rect.x1,rect.y0,rect.x1+poly_overhang,rect.y1,blocks->max,nodes);
  leak_free( NetNameBuffer ) ;

  /*** paint the diffusion overhang ***/
  rect.paint=diff;
  rect.name=NULL;
  rect.block=blocks->max;
  rect.x0=-w/2; rect.x1=w/2;
  rect.y0=*y-diffusion_overhang; 
  rect.y1=*y;
  list_append_element(pb->rects,&rect);
  rect.y0=*y+transistor_length;
  rect.y1=*y+transistor_length+diffusion_overhang;
  list_append_element(pb->rects,&rect);

  /*** more stuff for fullauto ***/
  if (fullauto)
    {
    if      ((state->last_contact[2*state->side][0]!=NULL)&&(state->last_contact[2*state->side][0]==name)&&
	     (rect.y0 - state->last_contact_y[2*state->side][0]<=96)) match=offset=0;
    else if ((state->last_contact[2*state->side][1]!=NULL)&&(state->last_contact[2*state->side][1]==name)&&
	     (rect.y0 - state->last_contact_y[2*state->side][1]<=96)) match=offset=1;
    else {match=-1; offset=state->offset[2*state->side];}

    /*** extend poly to polycontact ***/
    if (state->side==0) {rect.x1=-w/2; rect.x0 = rect.x1 - 9 - 12*offset;}
    else                {rect.x0= w/2; rect.x1 = rect.x0 + 9 + 12*offset;}
    rect.y0=*y; rect.y1=*y+transistor_length;
    rect.paint=poly;
    list_append_element(pb->rects,&rect);

    /*** add polycontacts on proper side ***/
    if (state->side==0) {rect.x1=-w/2 - 9 - 12*offset; rect.x0=rect.x1-8;}
    else                {rect.x0= w/2 + 9 + 12*offset; rect.x1=rect.x0+8;}
    rect.y0=*y-2; rect.y1=rect.y0+8;
    rect.paint=find_paint("polycontact");
    list_append_element(pb->rects,&rect);
    rect.paint=find_paint("metal1");
    list_append_element(pb->rects,&rect);

    /*** do shortcut metal1 or create metal2 port ***/
    if (match>=0)
      {
      /*** vertical m1 shortcut ***/
      rect.y0=state->last_contact_y[2*state->side][offset];
      rect.paint=find_paint("metal1"); list_append_element(pb->rects,&rect);
      }
    else
      {
      /*** m2contact ***/
      h=(state->pitch[state->side])*12;
      while (h<rect.y0-6) {h+=12; state->pitch[state->side]++;} /* skip metal2 slots */
      rect.y0=h; rect.y1=h+6;
      rect.paint=find_paint("m2contact");
      list_append_element(pb->rects,&rect);

      /*** metal2 and label ***/
      if (state->side==0)
	{
        rect.x0=min(rect.x0,-27);
        rect.x1=max(-27,max(rect.x1,rect.x0+8));
        if (rect.x1-rect.x0<12) rect.x0=rect.x1-12;
        }
      else
        {
        rect.x1=max(rect.x1, 27);
        rect.x0=min(27,min(rect.x0,rect.x1-8));
        if (rect.x1-rect.x0<12) rect.x1=rect.x0+12;
        }
      rect.paint=find_paint("metal2");
      rect.name=name;
      list_append_element(pb->rects,&rect);
      add_port(name,rect.paint,rect.x0,rect.y0,rect.x1,rect.y1,blocks->max,nodes);
      }
 
    /*** flip offset, consume pitch ***/
    state->last_contact[2*state->side][offset]=name;
    state->last_contact_y[2*state->side][offset]=rect.y1;
    state->offset[2*state->side]=!offset;
    state->pitch[state->side]++;
    }

  /*** move y, flip side ***/
  *y+=transistor_length;
  state->side=!state->side;
  }

/*** paint power strip and round up to m2 pitch for fullauto stack ***/
void paint_power(STACK *stack, BLOCK *pb, int y, int metal3)
  {
  RECT rect;
  rect.paint=metal3;
  y = ((y - 1 + 6 + 11)/12)*12; /* height rounded up */
  rect.y0 = -3;  rect.y1 = y - 3;
  rect.x0 =  3;  rect.x1 = 21;
  list_append_element(pb->rects,&rect);
  rect.x0 = -21; rect.x1 = -3;
  list_append_element(pb->rects,&rect);
  }

/*** create primative layout for stacks ***/
void layout_stack(STACK *stack, LIST *blocks, LIST *nodes, int fullauto, int rulenum)
  {
  int i,j;
  int ndiff,pdiff,nfet,pfet,ndc,pdc,poly,metal1,metal2,metal3;
  double y,w;
  GATE *pg=NULL;
  BLOCK block;
  FULLAUTO_STATE state;
  RECT rect;

  /*** find the paint numbers ***/
  ndiff=find_paint("ndiffusion");
  pdiff=find_paint("pdiffusion");
  nfet=find_paint("ntransistor");
  pfet=find_paint("ptransistor");
  ndc=find_paint("ndcontact");
  pdc=find_paint("pdcontact");
  poly=find_paint("polysilicon");
  metal1=find_paint("metal1");
  metal2=find_paint("metal2");
  metal3=find_paint("metal3");

  /*** start the block ***/
  block=create_block();
  block.rulenum=rulenum;
  block.type=stack->dir ? 1 : -1;
  state.side=state.pitch[0]=state.pitch[1]=0;
  for (i=0; i<3; i++)
    {
    state.offset[i]=0;
    for (j=0; j<2; j++)
      {
      state.last_contact[i][j]=NULL;
      state.last_contact_y[i][j]=0;
      }
    }

  /*** generate rectangles and ports ***/
  y=0;
  w=rint(stack->maxwidth/2)*2; /* even width */
  if (fullauto) w=max(w,20); /* bigger minimum width */
  else          w=max(w,contact_width);
  for (i=0; i<stack->gates->max; i++)
    {
    pg=&stack->gates->p.gate[i];
    make_contact(w,&y,pg->s,stack->dir?pdc:ndc,metal1,&block,blocks,nodes,fullauto,&state);
    make_gate(w,&y,pg->g,poly,stack->dir?pfet:nfet,stack->dir?pdiff:ndiff,&block,blocks,nodes,fullauto,&state);
    }
  make_contact(w,&y,pg->d,stack->dir?pdc:ndc,metal1,&block,blocks,nodes,fullauto,&state);

  /*** paint select ***/
  rect.paint=stack->dir ? find_paint("pselect") : find_paint("nselect");
  rect.x0=(-w/2-poly_overhang)-3;
  rect.x1=(+w/2+poly_overhang)+3;
  rect.y0=-2;
  rect.y1=y+2;
  rect.name=NULL;
  rect.block=blocks->max;
  list_append_element(block.rects,&rect);

  /*** add metal3 power supply for fullauto stacks ***/
  if (fullauto) paint_power(stack,&block,y,metal3);

  /*** finish up and append block to list ***/
  finish_block(&block,blocks->max);
  list_append_element(blocks,&block);
  }

/****************************** MAIN ROUTINE ******************************/

/*** actually verify that two conjunctive rules are exclusive ***/
/*** base this test on the exclhi, excllo properties of the gates ***/
int verify_actual_exclusion(RULE rule1, RULE rule2, LIST *properties)
  {
  PROPERTY prop;
  int i,j,k,e1,e2;

  for (i=0; i<properties->max; i++)
    {
    prop=properties->p.prop[i];
    if (prop.sense==rule1.dir) continue;
    for (j=0; j<rule1.guard->max; j++)
      {
      e1=find_element(prop.items,&rule1.guard->p.pc[j],&str_cmp);
      if (e1<0) continue;
      for (k=0; k<rule2.guard->max; k++)
	{
        e2=find_element(prop.items,&rule2.guard->p.pc[k],&str_cmp);
	if ((e2>=0)&&(e2!=e1)) return 1;
	}
      }
    }
  return 0;
  }

/*** check that rules were declared exclusive ***/
int verify_declared_exclusion(RULE rule1, RULE rule2, LIST *properties)
  {
  PROPERTY prop;
  int i,e1,e2;

  for (i=0; i<properties->max; i++)
    {
    prop=properties->p.prop[i];
    if (prop.sense!=rule1.dir) continue;
    e1=find_element(prop.items,&rule1.target,&str_cmp);
    e2=find_element(prop.items,&rule2.target,&str_cmp);
    if ((e1>=0)&&(e2>=0)) return 1;
    }
  return 0;
  }

/*** check if a new rule can share gates with given set of rules ***/
int can_share_rules(LIST *shared, RULE rule2, LIST *properties)
  {
  int i;
  RULE rule1;

  for (i=0; i<shared->max; i++)
    {
    rule1=shared->p.rule[i];
    if (rule1.dir!=rule2.dir) return 0;
    if (rule1.target==rule2.target) continue;
    if (rule1.unshared||rule2.unshared) return 0;
#if 0
    if (!verify_declared_exclusion(rule1,rule2,properties)) return 0;
#endif
    if (!verify_actual_exclusion(rule1,rule2,properties)) return 0;
    }
  return 1;
  }

/*** should this gate be a foot for given target/dir? ***/
int is_foot(LIST *feet, char *gate, char *target, int dir)
  {
  int i;
  RULE *pr;
  if (strcmp(gate,"_SReset")==0) return 1;
  for (i=0; i<feet->max; i++)
    {
    pr=&feet->p.rule[i];
    if (pr->guard->max!=1) continue;
    if (pr->target!=target) continue;
    if (pr->dir==dir) continue;
    if (pr->guard->p.pc[0]!=gate) continue;
    return 1;
    }
  return 0;
  }

/*** grab a set of rules which can share gates ***/
LIST *shared_rules(LIST *rules, LIST *properties, LIST *feet, int lessfolding)
  {
  LIST *shared;
  RULE rule1,rule2,rule,*pr;
  int i,j,N,strict;
  VAR *width;
  char *gate;
  double maxwidth=0,*x,min_folded=20,stackwidth=150;

  /*** make list of shared rules ***/
  list_sort(rules,&rule_decreasing_length_cmp);
  shared=list_create(sizeof(RULE));
  rule1=rules->p.rule[0];
  strict=rule1.strict;
  if (rule1.dir) {x=get_parm("min_pfolded"); if (x!=NULL) min_folded=*x;}
  else           {x=get_parm("min_nfolded"); if (x!=NULL) min_folded=*x;}
  if (rule1.dir) {x=get_parm("pstackwidth"); if (x!=NULL) stackwidth=*x;}
  else           {x=get_parm("nstackwidth"); if (x!=NULL) stackwidth=*x;}
  maxwidth=get(rule1.width);
  list_remove_element(rules,0);
  list_append_element(shared,&rule1);
  for (i=0; i<rules->max; i++)
    {
    rule2=rules->p.rule[i];
    if (can_share_rules(shared,rule2,properties))
      {
      list_remove_element(rules,i);
      if (rule2.strict) strict=1;
      maxwidth=max(maxwidth,get(rule2.width));
      list_append_element(shared,&rule2);
      i--;
      }
    }

  /*** if strict or multiple targets, return ***/
  if (strict) return shared;
  for (i=1; i<shared->max; i++)
    if (shared->p.rule[i].target!=shared->p.rule[0].target) return shared;

  /*** otherwise, bisect and symmetrize the shared rules ***/
  N=shared->max;
  for (i=0; i<N; i++)
    {
    pr=&shared->p.rule[i];
    if (get(pr->width)<min_folded) continue; /*** too small to fold ***/
    if (lessfolding&&(get(pr->width)<stackwidth)) continue; /*** still to small to symmetrize ***/
    width=new_var("FOLDW",0);
    fmla_var(width,pr->width);
    fmla_constant(width,0.5);
    fmla_func(width,func_mul);
    pr->width=width;
    rule=*pr;
    rule.reverse=1;
    rule.guard=list_create(sizeof(char *));
    for (j=pr->guard->max-1; j>=0; j--)
      {
      gate=pr->guard->p.pc[j];
      if (is_foot(feet,gate,rule.target,rule.dir))
	list_insert_element(rule.guard,&gate,0); /* not backwards */
      else list_append_element(rule.guard,&gate); /* backwards */
      }
    list_append_element(shared,&rule);
    }
  return shared;
  }

/*** debugging ***/
void print_stacks(LIST *stacks)
  {
  int i,j;
  STACK *ps;
  GATE *pg;
  for (i=0; i<stacks->max; i++)
    {
    ps=&stacks->p.stack[i];
    printf("Stack %d: %s to %s\n",i,ps->s,ps->d);
    for (j=0; j<ps->gates->max; j++)
      {
      pg=&ps->gates->p.gate[j];
      print_gate(pg);
      }
    }
  }

/*** reverse gate order (but leave _SReset alone) ***/
void reverse_gates(STACK *ps)
  {
  LIST *gates;
  GATE *pg;
  int i;
  gates=list_create(sizeof(char *));
  for (i=0; i<ps->gates->max; i++)
    {
    pg=&ps->gates->p.gate[i];
    if (strcmp(pg->g,"_SReset")!=0) list_append_element(gates,&pg->g);
    }
  for (i=0; i<ps->gates->max; i++)
    {
    pg=&ps->gates->p.gate[i];
    if (strcmp(pg->g,"_SReset")!=0)
      {
      pg->g=gates->p.pc[gates->max-1];
      list_remove_element(gates,gates->max-1);
      }
    }
  list_free(gates);
  }

/*** fold/flip/stretch stacks ***/
void fold_stacks(LIST *gates, LIST *stacks, double stackwidth, double min_stackwidth, double min_folded, 
		 int strict)
  {
  int i,j,N,folds;
  STACK stack,*ps;
  N=stacks->max; /* remember number of original stacks */
  for (i=0; i<N; i++)
    {
    ps=&stacks->p.stack[i];
    folds=ceil(ps->maxwidth/stackwidth);
    if ((folds>1)||(!strict)) folds=2*((folds+1)/2); /* even folds only */
    if (ps->maxwidth<min_folded) folds=1; /* too small to fold */
    ps->maxwidth=ps->maxwidth/folds;
    ps->minwidth=ps->minwidth/folds;
    ps->maxwidth=max(ps->maxwidth,min_stackwidth);
    ps->minwidth=max(ps->minwidth,min_stackwidth);
    for (j=1; j<folds; j++)
      {
      ps=&stacks->p.stack[i];
      stack=*ps;
      stack.gates=list_dup(ps->gates);
      if ((j%2==1)&&(!strict)) reverse_gates(&stack);
      list_append_element(stacks,&stack);
      }
    }
  }

/*** main stack generation routine ***/
void stack_gen(CIRCUIT *circuit, LIST *blocks, LIST *nodes, int fullauto)
  {
  int i,rulenum,dir,strict,multiple;
  LIST *gates,*stacks,*shared;
  double *x,nstackwidth=1000,pstackwidth=1000,
    min_nstackwidth=8,min_pstackwidth=8,min_nfolded=16,min_pfolded=16;

  /*** get stackwidths ***/
  x=get_parm("nstackwidth"); if (x!=NULL) nstackwidth=*x;
  x=get_parm("pstackwidth"); if (x!=NULL) pstackwidth=*x;
  x=get_parm("min_nstackwidth"); if (x!=NULL) min_nstackwidth=*x;
  x=get_parm("min_pstackwidth"); if (x!=NULL) min_pstackwidth=*x;
  x=get_parm("min_nfolded"); if (x!=NULL) min_nfolded=*x;
  x=get_parm("min_pfolded"); if (x!=NULL) min_pfolded=*x;

  /*** generate gates from rules ***/
  list_sort(circuit->rules,&rule_decreasing_length_cmp);
  while(circuit->rules->max>0)
    {
    /*** create list of rules which may be shared ***/
    shared=shared_rules(circuit->rules,circuit->properties,circuit->feet,circuit->lessfolding);
    dir=shared->p.rule[0].dir;
    rulenum=10000;
    strict=0;
    multiple=0;
    for (i=0; i<shared->max; i++)
      {
      rulenum=min(rulenum,shared->p.rule[i].num);
      if (shared->p.rule[i].strict) strict=1;
      if (shared->p.rule[i].target!=shared->p.rule[0].target) multiple=1;
      }

    /*** find minimal gate network for shared rules ***/
    gates=minimal_gate_network(shared,circuit->properties);
    list_free(shared);

    /*** combine gates into series stacks ***/
    stacks=stack_gates(gates);

    /*** fold/flip/expand stacks ***/
    fold_stacks(gates,stacks,
		dir ? pstackwidth : nstackwidth,
		dir ? min_pstackwidth : min_nstackwidth,
		dir ? min_pfolded : min_nfolded, 
		strict || (!multiple) || circuit->lessfolding);

    /*** merge stacks into bigger stacks ***/
    merge_stacks(stacks);

    /*** layout stacks ***/
    for (i=0; i<stacks->max; i++) layout_stack(&stacks->p.stack[i],blocks,nodes,fullauto,rulenum);
    list_free(gates);
    list_free(stacks);
    }
  }
