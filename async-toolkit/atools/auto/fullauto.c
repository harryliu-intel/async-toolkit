#include "auto.h"
#include "exportskill.h"
#define MAX_SLOTS 200
#define MAX_COLUMNS 20

int PlaceOnly=0;

/******************** structures, creation, free, and compare ********************/

typedef struct _fnode
  {
  char *name;
  int minthirdcol,maxthirdcol;
  unsigned strut:1,top:1,bottom:1,vertical:1;
  int ymin,ymax;
  int yminL,ymaxL,yminR,ymaxR;
  } FNODE;

typedef struct _row
  {
  FNODE *L,*M,*R; /* what node occupies each segment of row */
  unsigned Lint:1,Lext:1,Rint:1,Rext:1,Lmid:1,Rmid:1,Lport:1,Rport:1; /* extend horizontally */
  int Lslot,Rslot; /* slots to extend to boundary from */
  } ROW;

typedef struct _column
  {
  LIST *blocks; /* layout blocks in this column */
  int num; /* column number */
  int Lport,Rport; /* a port column */
  double x; /* x offset of this column */
  int H; /* height in rows */
  int Y; /* height in lambda */
  int **L,**R; /* grid of used m1/m3 */
  int minslot,maxslot; /* min and max slot used for vertical wiring */
  ROW *row; /* array of H rows */
  } COLUMN;

/*** create a column ***/
COLUMN create_column(int num)
  {
  COLUMN col;
  col.blocks=list_create(sizeof(BLOCK *));
  col.num=num;
  col.x=0;
  col.Y=0;
  col.H=0;
  col.L=NULL;
  col.R=NULL;
  col.minslot=0;
  col.maxslot=0;
  col.Lport=col.Rport=0;
  col.row=NULL; /* allocate later */
  return col;
  }

ROW create_row()
  {
  ROW row;
  row.L=NULL;
  row.M=NULL;
  row.R=NULL;
  row.Lint=row.Lext=0;
  row.Rint=row.Rext=0;
  row.Lmid=row.Rmid=0;
  row.Lport=row.Rport=0;
  row.Lslot=0;
  row.Rslot=0;
  return row;
  }

/*** allocate L,R,row fields in a column once H is known ***/
void finish_column(COLUMN *pcol)
  {
  int i,j;
  pcol->L=leak_malloc(pcol->H*sizeof(int *));
  pcol->R=leak_malloc(pcol->H*sizeof(int *));
  pcol->row=(ROW *) leak_malloc(sizeof(ROW)*pcol->H);
  for (i=0; i<pcol->H; i++)
    {
    pcol->row[i]=create_row(); 
    pcol->L[i]=leak_malloc(MAX_SLOTS*sizeof(int));
    pcol->R[i]=leak_malloc(MAX_SLOTS*sizeof(int));
    for (j=0; j<MAX_SLOTS; j++) pcol->L[i][j]=pcol->R[i][j]=0;
    }
  }

void free_column(COLUMN *pcol)
  {
  list_free(pcol->blocks);
  if (pcol->row!=NULL) leak_free(pcol->row);
  }

FNODE *USED=NULL; /*** used but unlabeled segment of row ***/

FNODE *create_fnode(char *name, int global)
  {
  FNODE *pfn;
  pfn=leak_malloc(sizeof(FNODE));
  pfn->name=name;
  pfn->minthirdcol=1000;
  pfn->maxthirdcol=-1000;
  pfn->strut=0;
  pfn->top=global;
  pfn->bottom=global;
  pfn->ymin=pfn->ymax=0;
  pfn->yminL=pfn->ymaxL=0;
  pfn->yminR=pfn->ymaxR=0;
  return pfn;
  }

int pfn_str_cmp(void *p1, void *p2)
  {
  int c;
  FNODE *pfn=*((FNODE **)p1);
  char *str=(char *)p2;
  c=(str[strlen(str)-1]=='#') - (pfn->name[strlen(pfn->name)-1]=='#');
  if (c!=0) return c;
  return strcmp(pfn->name,str);
  }

int pfn_cmp(void *p1, void *p2)
  {
  FNODE *pfn=*((FNODE **)p2);
  return pfn_str_cmp(p1,pfn->name);
  }

int pfn_y_cmp(void *p1, void *p2)
  {
  FNODE *pfn1=*((FNODE **)p1),*pfn2=*((FNODE **)p2);
  return (pfn1->ymax - pfn1->ymin) - (pfn2->ymax - pfn2->ymin);
  }

int pfn_column_span_cmp(void *p1, void *p2)
  {
  FNODE *pfn1=*((FNODE **)p1),*pfn2=*((FNODE **)p2);
  return (pfn1->maxthirdcol - pfn1->minthirdcol) - (pfn2->maxthirdcol - pfn2->minthirdcol);
  }

/************************************ WIRE UP A COLUMN *****************************************/

/*** return left side of slot ***/
int slot_x0(int slot)
  {
  if (slot>0) return  27 + 12*((slot-1)/2);
  else        return -33 + 12*((slot+1)/2); 
  }

/*** return right side of slot ***/
int slot_x1(int slot)
  {
  return slot_x0(slot) + 6;
  }

/*** return 1 or 3 for which metal is used in this slot ***/
int slot_metal_num(int slot)
  {
  int m=1;
  if (slot<0)
    {
    slot=-slot-1;
    if (slot%2==0) m=3;
    }
  else
    {
    slot=slot-1;
    if (slot%2==0) m=3;
    }
  return m;
  }

/*** return vertical metal and contact to use ***/
void slot_metal(int slot, int *metal, int *contact, int *top)
  {
  if (slot_metal_num(slot)==3)
    {
    *metal=find_paint("metal3");
    *contact=find_paint("m3contact");
    *top=find_paint("metal3");
    }
  else
    {
    *metal=find_paint("metal1");
    *contact=find_paint("m2contact");
    *top=find_paint("metal2");
    }
  }

/*** allocate best slot for the vertical wire, update state, draw it ***/
void draw_vertical(COLUMN *pcol, FNODE *pfn, int dx, int dy1, int dy2,
		   BLOCK *pb, int blocknum, LIST *nodes)
  {
  int i,j,bestL=0,bestR=0;
  int slot,force_m3=0;
  RECT rect,rect2;
  int Hmetal,Vmetal,contact,top;
  ROW *prow;

  /*** look for top/bottom/strut properties ***/
  if (pfn->strut||pfn->top) {force_m3=1; dy2=pcol->H-1;}
  if (pfn->strut||pfn->bottom) {force_m3=1; dy1=0;}
  if (dy2<=dy1) return;

  /*** search for closest available slot to middle ***/
  if (dx<=0) for (j=1; j<MAX_SLOTS; j++)
    {
    if (force_m3&&(slot_metal_num(-j)!=3)) continue;
    for (i=dy1; i<=dy2; i++) if (pcol->L[i][j]) break;
    if (i>dy2) {bestL=j; break;}
    }
  if (dx>=0) for (j=1; j<MAX_SLOTS; j++)
    {
    if (force_m3&&(slot_metal_num(j)!=3)) continue;
    for (i=dy1; i<=dy2; i++) if (pcol->R[i][j]) break;
    if (i>dy2) {bestR=j; break;}
    }

  /*** error check ***/
  if ((bestL==0)&&(bestR==0))
    {
    fprintf(stderr,"ERROR: used up all %d vertical slots!\n",MAX_SLOTS);
    exit(1);
    }

  /*** pick the slot ***/
  if ((dx<0)||((dx==0)&&(bestL<bestR)))
    {
    for (i=dy1; i<=dy2; i++) pcol->L[i][bestL]=1;
    slot=-bestL;
    }
  else
    {
    for (i=dy1; i<=dy2; i++) pcol->R[i][bestR]=1;
    slot=+bestR;
    }

  /*** feedback ***/
  printf("  connecting %s from pitch %d to %d with slot %d\n",pfn->name,dy1,dy2,slot);

  /*** pick layers, start rect ***/
  Hmetal=find_paint("metal2");
  slot_metal(slot,&Vmetal,&contact,&top);
  rect.name=NULL;
  rect.block=0;
  rect.pr=NULL;

  /*** the vertical metal ***/
  rect.x0=slot_x0(slot);
  rect.x1=slot_x1(slot);
  rect.y0=pfn->bottom ? 0 : 12*dy1 + 3;
  rect.y1=pfn->top ? 12*pcol->H : 12*dy2 + 3 + 6;
  pfn->top=pfn->bottom=pfn->strut=0; /* only do this once */
  rect.paint=Vmetal;
  if (dy2>dy1) list_append_element(pb->rects,&rect);

  /*** contacts on appropriate rows ***/
  rect.x0-=1; rect.x1+=1;
  for (i=dy1; i<=dy2; i++)
    {
    prow=&pcol->row[i];
    if ((slot<0)&&(prow->L!=pfn)&&(prow->M!=pfn)) continue;
    if ((slot>0)&&(prow->R!=pfn)&&(prow->M!=pfn)) continue;
    if ((slot<0)&&(prow->L!=pfn)&&(prow->L!=NULL)) continue;
    if ((slot>0)&&(prow->R!=pfn)&&(prow->R!=NULL)) continue;

    /*** update Lslot,Rslot for ports ***/
    if (slot<0) prow->Lslot=slot;
    if (slot>0) prow->Rslot=slot;

    /*** contact ***/
    rect.y0=12*i + 3;
    rect.y1=rect.y0 + 6;
    if (dy2>dy1)
      {
      rect.paint=contact; list_append_element(pb->rects,&rect);
      rect.paint=top;     list_append_element(pb->rects,&rect);
      }

    /*** horizontal metal ***/
    rect2=rect;
    if (slot<0)
      {
      if      (prow->Lmid)  rect2.x1=0;
      else if (prow->Lport) {rect2.x1=-24; rect2.x0=min(rect.x0,rect2.x1-11);}
      else if (prow->Lint)  rect2.x1=-27;
      }
    else
      {
      if      (prow->Rmid)  rect2.x0=0;
      else if (prow->Rport) {rect2.x0=24; rect2.x1=max(rect2.x1,rect2.x0+11);}
      else if (prow->Rint)  rect2.x0=27;
      }
    rect2.paint=Hmetal; list_append_element(pb->rects,&rect2);
    }

  /*** update minslot, maxslot of column ***/
  pcol->minslot=min(pcol->minslot,slot);
  pcol->maxslot=max(pcol->maxslot,slot);
  }

/*** extend horizontal metal2 for left/right ports ***/
void draw_port_extensions(COLUMN *pcol, BLOCK *pb, int x0, int x1)
  {
  int i;
  ROW *prow;
  RECT rect;
  rect.name=NULL;
  rect.block=0;
  rect.pr=NULL;
  rect.paint=find_paint("metal2");
  for (i=0; i<pcol->H; i++)
    {
    prow=&pcol->row[i];
    rect.y0=12*i + 3;
    rect.y1=rect.y0 + 6;
    if (prow->Lext)
      {
      rect.x0=x0;
      rect.x1=slot_x1(prow->Lslot);
      if      (prow->Lmid)  rect.x1=0;
      else if (prow->Lport) rect.x1=-24;
      else if (prow->Lint)  rect.x1=-27;
      list_append_element(pb->rects,&rect);
      }
    if (prow->Rext)
      {
      rect.x0=slot_x0(prow->Rslot);
      rect.x1=x1;
      if      (prow->Rmid)  rect.x0=0;
      else if (prow->Rport) rect.x0=+24;
      else if (prow->Rint)  rect.x0=+27;
      list_append_element(pb->rects,&rect);
      }
    }
  }

/*** paint metal3,metal4,m4contact Vdd/GND grid ***/
void draw_power(BLOCK *pb, COLUMN *pcol, int x0, int x1)
  {
  int i,N;
  RECT rect;
  if ((pcol->minslot==0)&&(pcol->maxslot==0)) return;

  /*** vertical power strips ***/
  rect.name=NULL;
  rect.block=0;
  rect.pr=NULL;
  if ((!pcol->Rport)&&(!pcol->Lport))
    {
    rect.paint=find_paint("metal3");
    rect.y0=0; rect.y1=12*pcol->H;
    rect.x0=-21; rect.x1=-3;
    list_append_element(pb->rects,&rect);
    rect.x0=3; rect.x1=21;
    list_append_element(pb->rects,&rect);
    }

  /*** metal4 and m4contacts ***/
  N=pcol->H/10;
  for (i=0; i<N; i++)
    {
    rect.paint=find_paint("metal4");
    rect.x0=x0;      rect.x1=x1;
    rect.y0=3+120*i; rect.y1=rect.y0+18;
    if (pcol->Rport) rect.x1=-24;
    if (pcol->Lport) rect.x0=+24;
    list_append_element(pb->rects,&rect);
    if (pcol->Rport||pcol->Lport) continue;
    rect.paint=find_paint("m4contact");
    if (i%2==0) {rect.x0=-21; rect.x1=-3;}
    else        {rect.x0=3;   rect.x1=21;}
    list_append_element(pb->rects,&rect);
    }
  }

/*** wire up an FNODE within a COLUMN ***/
void wire_fnode(LIST *nodes, COLUMN *pcol, BLOCK *pb, int blocknum, FNODE *pfn)
  {
  int i,Lmin,Lmax,Mmin,Mmax,Rmin,Rmax,doL,doM,doR;
  ROW *prow;

  /*** return if pfn isn't used in this column ***/
  if (pfn->minthirdcol>3*pcol->num+2) return;
  if (pfn->maxthirdcol<3*pcol->num+0) return;

  /*** scan for vertical range of this node ***/
  Lmin=Mmin=Rmin=+1000;
  Lmax=Mmax=Rmax=-1000;
  for (i=0; i<pcol->H; i++)
    {
    prow=&pcol->row[i];
    if (prow->L==pfn) {Lmin=min(Lmin,i); Lmax=max(Lmax,i);}
    if ((prow->M==pfn)&&(prow->L==NULL)&&(prow->R==NULL)) {Mmin=min(Mmin,i); Mmax=max(Mmax,i);}
    if (prow->R==pfn) {Rmin=min(Rmin,i); Rmax=max(Rmax,i);}
    }
  doL=(Lmax>=Lmin);
  doM=(Mmax>=Mmin);
  doR=(Rmax>=Rmin);
  if (!doL&&!doM&&!doR) return; /*** nothing to route ***/ 
  else if (doL&&!doR) /*** left side only ***/
    {
    Lmin=min(Lmin,Mmin); Lmax=max(Lmax,Mmax);
    draw_vertical(pcol,pfn,-1,Lmin,Lmax,pb,blocknum,nodes);
    }
  else if (doR&&!doL) /*** left side only ***/
    {
    Rmin=min(Rmin,Mmin); Rmax=max(Rmax,Mmax);
    draw_vertical(pcol,pfn,+1,Rmin,Rmax,pb,blocknum,nodes);
    }
  else if (!doR&&doM&&!doL) /*** middle ***/
    draw_vertical(pcol,pfn,0,Mmin,Mmax,pb,blocknum,nodes);
  else /*** must route on both sides ***/
    {
    /*** attach M connections to left or right side ***/
    Lmin=min(Lmin,Mmin); Lmax=max(Lmax,Mmax);
    Rmin=min(Rmin,Mmin); Rmax=max(Rmax,Mmax);
    draw_vertical(pcol,pfn,-1,Lmin,Lmax,pb,blocknum,nodes);
    draw_vertical(pcol,pfn,+1,Rmin,Rmax,pb,blocknum,nodes);
    }
  }

/*** channel route a column with 2 halves ***/
void wire_column(LIST *blocks, LIST *nodes, LIST *fnodes, COLUMN *pcol, int *x)
  {
  int i;
  int x0,x1;
  BLOCK block;

  /*** feedback ***/
  printf("wiring column %d:\n",pcol->num);

  /*** start a block ***/
  block=create_block();
  block.column=pcol->num;

  /*** wire up fnodes in order, internal then external ***/
  list_sort(fnodes,&pfn_column_span_cmp);
  for (i=0; i<fnodes->max; i++) wire_fnode(nodes,pcol,&block,blocks->max,fnodes->p.pfn[i]);
  
  /*** compute column width, offset next columns ***/
  x0=slot_x0(pcol->minslot) - 3;
  x1=slot_x1(pcol->maxslot) + 3;
  *x += -x0;
  pcol->x = *x;
  *x +=  x1;

  /*** extend ports to column boundary ***/
  draw_port_extensions(pcol,&block,x0,x1);

  /*** paint Vdd/GND strips ***/
  draw_power(&block,pcol,x0,x1);

  /*** finish up and append block to list ***/
  finish_block(&block,blocks->max);
  list_append_element(blocks,&block);
  }

/*********************************** FIND PORTS **************************************************/

/*** insert spacer blocks until column height filled in ***/
int pad_column(COLUMN *pcol, int ypitch)
  {
  int i;
  BLOCK *spacer=NULL,*pb;
  int last=0;

  /*** insert spacers between dissimilar blocks ***/
  for (i=0; i<pcol->blocks->max; i++)
    {
    pb=pcol->blocks->p.pblock[i];
    if ((last!=pb->type)||(PlaceOnly&&(pb->type!=0)))
      {
      list_insert_element(pcol->blocks,&spacer,i++);
      pcol->H++;
      pcol->Y+=12;
      }
    last=pb->type;
    }
  if (last!=0) {list_append_element(pcol->blocks,&spacer); pcol->H++; pcol->Y+=12;}
  if ((ypitch>0)&&(pcol->H>ypitch)) return 0; /* overflowed */

  /*** insert spacer blocks at top and bottom to pad to ypitch ***/
  while((ypitch>0)&&(pcol->H<ypitch))
    {
    if (pcol->blocks->max%2==0) list_append_element(pcol->blocks,&spacer);
    else list_insert_element(pcol->blocks,&spacer,0);
    pcol->H++; pcol->Y+=12;
    }
  return 1;
  }

/*** arranges blocks vertically into a column ***/
void stack_blocks(COLUMN *pcol)
  {
  int i;
  double h,y0,y=0,left=0,right=0;
  BLOCK *pb;
  for (i=0; i<pcol->blocks->max; i++)
    {
    pb=pcol->blocks->p.pblock[i];
    if (pb==NULL) {y+=12; continue;} /* spacer */
    h=pb->bbox.y1 - pb->bbox.y0;
#if 0
    if (((int)h)%12!=0) fprintf(stderr,"WARNING: cell %d height not multiple of 12\n",pb->rulenum);
#endif
    y0=pb->y + pb->bbox.y0;
    pb->y += (y - y0);
    y+=h;
    left  = min(left,  pb->bbox.x0);
    right = max(right, pb->bbox.x1);
    pb->column=pcol->num;
    }

  /*** convert to wire pitches ***/
  pcol->Y=y;
  pcol->H=(y+11)/12;
  pcol->minslot=pcol->maxslot=0;
  while (slot_x1(pcol->minslot-1)>left-6)  pcol->minslot--;
  while (slot_x0(pcol->maxslot+1)<right+6) pcol->maxslot++;
  if ((-pcol->minslot>=MAX_SLOTS)||(pcol->maxslot>=MAX_SLOTS))
    {
    fprintf(stderr,"ERROR: needed %d vertical slots, only have %d.\n",
	    max(-pcol->minslot,pcol->maxslot),MAX_SLOTS);
    exit(1);
    }
  finish_column(pcol);
  }

/*** search for a label within a rectangle on a specified paint ***/
NODE *find_label_in_rect(LIST *blocks, LIST *nodes, RECT *pr, int paint, int colnum)
  {
  int i,j;
  NODE *pn;
  PORT *pp;
  BLOCK *pb;
  double x0,y0,x1,y1;
  for (i=0; i<nodes->max; i++)
    {
    pn=&nodes->p.node[i];
    for (j=0; j<pn->ports->max; j++)
      {
      pp=&pn->ports->p.port[j];
      pb=&blocks->p.block[pp->block];
      if ((pp->paint!=paint)||(pb->column!=colnum)) continue;
      x0=pb->x + pp->x0;
      y0=pb->y + pp->y0;
      x1=pb->x + pp->x1;
      y1=pb->y + pp->y1;
      if ((pr->x0<=x0)&&(pr->x1>=x1)&&(pr->y0<=y0)&&(pr->y1>=y1)) return pn;
      }
    }
  return NULL;
  }

/*** search for a rectangle overlapping with specified rectangle and paint ***/
int test_overlap(LIST *blocks, RECT *pr, int paint, int colnum)
  {
  int i,j;
  BLOCK *pb;
  RECT rect;
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    if (pb->column!=colnum) continue;
    for (j=0; j<pb->rects->max; j++)
      {
      rect=pb->rects->p.rect[j];
      if (rect.paint!=paint) continue;
      rectangle_move(&rect,pb->x,pb->y);
      if (overlap(rect.x0,rect.x1,pr->x0,pr->x1)&&overlap(rect.y0,rect.y1,pr->y0,pr->y1)) return 1;
      }
    }
  return 0;
  }

/*** find an FNODE by name, creating it if necessary ***/
FNODE *add_fnode(LIST *fnodes, char *name, int global, int y)
  {
  int i;
  FNODE *pfn;
  i=find_element_lazy_sort(fnodes,name,&pfn_str_cmp);
  if (i<0)
    {
    pfn=create_fnode(name,global);
    list_insert_element_lazy_sort(fnodes,&pfn,&pfn_cmp);
    }
  else pfn=fnodes->p.pfn[i];
  return pfn;
  }

/*** extract the fnodes from a stack of blocks ***/
void find_rows(LIST *blocks, LIST *nodes, LIST *fnodes, COLUMN *pcol)
  {
  int i,j,pitch;
  int testL,testM,testR;
  NODE *nameL,*nameM,*nameR;
  RECT rect;
  int width,spacing;
  int metal2,m2contact,metal,contact,top;
  ROW *prow;

  /*** parameters ***/
  width=6;
  spacing=6;
  pitch=width+spacing;
  metal2=find_paint("metal2");
  m2contact=find_paint("m2contact");

  /*** try all pitchs on metal2, see whats there ***/
  for (i=0; i<pcol->H; i++)
    {
    /*** set y's ***/
    rect.y0=pitch*i + width/2;
    rect.y1=rect.y0 + spacing;

    /*** look for metal2/m2contact/label on left side ***/
    rect.x0=-10000; rect.x1=-24;
    testL=test_overlap(blocks,&rect,metal2,pcol->num)||test_overlap(blocks,&rect,m2contact,pcol->num);
    nameL=find_label_in_rect(blocks,nodes,&rect,metal2,pcol->num);

    /*** look for metal2/m2contact/label across middle ***/
    rect.x0=-24; rect.x1=24;
    testM=test_overlap(blocks,&rect,metal2,pcol->num)||test_overlap(blocks,&rect,m2contact,pcol->num);
    nameM=find_label_in_rect(blocks,nodes,&rect,metal2,pcol->num);

    /*** look for metal2/m2contact/label on right side ***/
    rect.x0=24; rect.x1=10000;
    testR=test_overlap(blocks,&rect,metal2,pcol->num)||test_overlap(blocks,&rect,m2contact,pcol->num);
    nameR=find_label_in_rect(blocks,nodes,&rect,metal2,pcol->num);

    /*** create the row, fill in segments ***/
    prow=&pcol->row[i];
    *prow=create_row();
    if (nameL!=NULL) prow->L=add_fnode(fnodes,nameL->name,nameL->global,i);
    else if (testL)  prow->L=USED;
    if (nameM!=NULL) prow->M=add_fnode(fnodes,nameM->name,nameM->global,i);
    else if (testM)  prow->M=USED;
    if (nameR!=NULL) prow->R=add_fnode(fnodes,nameR->name,nameR->global,i);
    else if (testR)  prow->R=USED;

    /*** scan m1/m3 slots to fill in L/R arrays ***/
    rect.y0-=spacing-1;
    rect.y1+=spacing-1;
    for (j=1; j<=-pcol->minslot; j++)
      {
      /*** L side ***/
      rect.x0=slot_x0(-j)-(spacing-1);
      rect.x1=slot_x1(-j)+(spacing-1);
      slot_metal(-j,&metal,&contact,&top);
      pcol->L[i][j]=test_overlap(blocks,&rect,metal,pcol->num)||
	            test_overlap(blocks,&rect,contact,pcol->num);
      }
    for (j=1; j<=pcol->maxslot; j++)
      {
      /*** R side ***/
      rect.x0=slot_x0(j)-spacing/2;
      rect.x1=slot_x1(j)+spacing/2;
      slot_metal(j,&metal,&contact,&top);
      pcol->R[i][j]=test_overlap(blocks,&rect,metal,pcol->num)||
	            test_overlap(blocks,&rect,contact,pcol->num);
      if (pcol->R[i][j]) pcol->maxslot=max(pcol->maxslot,j);
      }
    }
  }

/*** copy L1/R1 entries to L2/R2 if fnode extends beyond column ***/
void extend_segments(COLUMN *pcol)
  {
  int i;
  ROW *prow;
  for (i=0; i<pcol->H; i++)
    {
    prow=&pcol->row[i];
    if ((prow->M!=NULL)&&(prow->M!=USED))
      {
      /*** merge L or R into M if they are the same ***/
      if (prow->L==prow->M) prow->L=NULL;
      if (prow->R==prow->M) prow->R=NULL;

      /*** make sure M can get out both sides ***/
      if      ((prow->L!=NULL)&&(prow->M!=prow->L)) {prow->R=prow->M; prow->M=NULL; prow->Rmid=1;}
      else if ((prow->R!=NULL)&&(prow->M!=prow->R)) {prow->L=prow->M; prow->M=NULL; prow->Lmid=1;}
      }
    if ((prow->M!=NULL)&&(prow->M!=USED)) prow->Lmid=prow->Rmid=1;
    if ((prow->L!=NULL)&&(prow->L!=USED)) prow->Lint=1;
    if ((prow->R!=NULL)&&(prow->R!=USED)) prow->Rint=1;
    }
  }

/*** debugging feedback ***/
void print_fnodes(LIST *fnodes)
  {
  int i;
  FNODE *pfn;
  printf("FNODES:\n");
  for (i=0; i<fnodes->max; i++)
    {
    pfn=fnodes->p.pfn[i];
    printf("  %s colmin=%d:%d colmax=%d:%d\n",pfn->name,
	   pfn->minthirdcol/3,pfn->minthirdcol%3,pfn->maxthirdcol/3,pfn->maxthirdcol%3);
    }
  }

/*** more debugging feedback ***/
void print_column(COLUMN *pcol)
  {
  int i;
  ROW *prow;
  printf("COLUMN %d: H=%d, minslot=%d, maxslot=%d\n",pcol->num,pcol->H,pcol->minslot,pcol->maxslot);
  for (i=0; i<pcol->H; i++)
    {
    prow=&pcol->row[i];
    printf("  row %d: L=%s M=%s R=%s Lmid=%d Lint=%d Lext=%d Rmid=%d Rint=%d Rext=%d\n",i,
	   prow->L?prow->L->name:NULL,prow->M?prow->M->name:NULL,prow->R?prow->R->name:NULL,
	   prow->Lmid,prow->Lint,prow->Lext,prow->Rmid,prow->Rint,prow->Rext);
    }
  }

/*************************************** PORT DIRECTIVES **********************************/

void vertical_directives(LIST *fnodes, LIST *directives)
  {
  int i;
  FNODE *pfn;
  DIRECTIVE *pd;

  /*** transfer strut/top/bottom/vertical directives to FNODE'S ***/
  for (i=0; i<directives->max; i++)
    {
    pd=&directives->p.directive[i];
    pfn=add_fnode(fnodes,pd->name,0,pd->pos);
    if      (pd->type==0) pfn->strut=1;
    else if (pd->type==1) pfn->top=1;
    else if (pd->type==2) pfn->bottom=1;
    else if (pd->type==3) pfn->top=pfn->bottom=1;
    else if (pd->type==6) fprintf(stderr,"WARNING: ignoring unpositioned pin %s\n",pd->name);
    }
  }

void horizontal_directives(LIST *fnodes, LIST *directives, COLUMN *pcolL, COLUMN *pcolR, int H)
  {
  int i;
  FNODE *pfn;
  DIRECTIVE *pd;
  ROW *prow;

  /*** allocate rows for port columns ***/
  pcolL->H=pcolR->H=H;
  finish_column(pcolL);
  finish_column(pcolR);

  /*** transfer strut/top/bottom/vertical directives to FNODE'S ***/
  for (i=0; i<directives->max; i++)
    {
    pd=&directives->p.directive[i];
    pfn=add_fnode(fnodes,pd->name,0,pd->pos);
    if (pd->type==4)
      {
      prow=&pcolL->row[pd->pos];
      prow->R=pfn;
      prow->Rport=1;
      printf("creating left port for %s at %d\n",pfn->name,pd->pos);
      }
    else if (pd->type==5)
      {
      prow=&pcolR->row[pd->pos];
      prow->L=pfn;
      prow->Lport=1;
      printf("creating right port for %s at %d\n",pfn->name,pd->pos);
      }
    }
  }

/****************************** MULTICOLUMN SUPPORT ***************************************/

/*** separate blocks into one or more columns ***/
LIST *separate_into_columns(LIST *blocks, LIST *directives, int ypitch, int N)
  {
  int i,j,progress,y,YL,YR,NYL,NYR;
  LIST *columns;
  COLUMN col,*pcol,*pcolL,*pcolR;
  BLOCK *pb;

  /*** create N columns, put blocks into column 0 ***/
  printf("Creating %d columns...\n",N);
  columns=list_create(sizeof(COLUMN));
  for (i=0; i<N+2; i++)
    {
    col=create_column(i);
    if (i==0)   col.Lport=1;
    if (i==N+1) col.Rport=1;
    list_append_element(columns,&col);
    }
  pcol=&columns->p.column[1];
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    list_append_element(pcol->blocks,&pb);
    }
  list_sort(pcol->blocks,&pblock_rulenum_cmp);
  stack_blocks(pcol);

  /*** transfer blocks between neigboring columns until they are roughly even ***/
  do
    {
    progress=0;
    for (i=1; i<N; i++)
      {
      pcolL=&columns->p.column[i];
      pcolR=&columns->p.column[i+1];
      YL=0; YR=0; NYL=0; NYR=0;
      for (j=1; j<=N; j++)
        { 
        if (j<=i) {YL+=columns->p.column[j].Y; NYL++;}
        else      {YR+=columns->p.column[j].Y; NYR++;}
        }

      /*** try moving a block left ***/
      if (pcolR->blocks->max>0)
	{
        j = (i%2==1) ? pcolR->blocks->max-1 : 0;
        pb=pcolR->blocks->p.pblock[j];
        y=abs(pb->bbox.y1-pb->bbox.y0);
        if (NYR*(YL + y) < NYL*YR)
	  {
	  list_remove_element(pcolR->blocks,j);
	  if (i%2==1) list_append_element(pcolL->blocks,&pb);
          else        list_insert_element(pcolL->blocks,&pb,0);
          pcolL->Y+=y; pcolL->H=(pcolL->Y+11)/12;
          pcolR->Y-=y; pcolR->H=(pcolR->Y+11)/12;
          progress=1;
          continue;
	  }
	}

      /*** try moving a block right ***/
      if (pcolL->blocks->max>0)
	{
        j = (i%2==1) ? pcolL->blocks->max-1 : 0;
        pb=pcolL->blocks->p.pblock[j];
        y=abs(pb->bbox.y1-pb->bbox.y0);
        if (NYL*(YR + y) < NYR*YL)
	  {
	  list_remove_element(pcolL->blocks,j);
	  if (i%2==1) list_append_element(pcolR->blocks,&pb);
          else        list_insert_element(pcolR->blocks,&pb,0);
          pcolR->Y+=y; pcolR->H=(pcolR->Y+11)/12;
          pcolL->Y-=y; pcolL->H=(pcolL->Y+11)/12;
          progress=1;
          continue;
	  }
        }
      }
    } while (progress);

  /*** debugging output ***/
  for (i=0; i<columns->max; i++)
    {
    pcol=&columns->p.column[i];
    printf("Column %d height=%d (%d)\n",i,pcol->Y,pcol->H);
    }

  /*** check height of columns against ypitch ***/
  for (i=0; i<columns->max; i++)
    {
    pcol=&columns->p.column[i];
    if ((ypitch>0)&&(pcol->H>ypitch)) {printf("Exceeds bitpitch of %d\n",ypitch); return NULL;}
    }

  /*** pad and stack columns ***/
  for (i=1; i<=N; i++)
    {
    pcol=&columns->p.column[i];
    if (!pad_column(pcol,ypitch)) return NULL;
    stack_blocks(pcol);
    list_free(pcol->blocks);
    pcol->blocks=NULL;
    }

  return columns;
  }

/*** update min/max thirdcol of fnode ***/
void expand_fnode_range(FNODE *fnode, int thirdcol)
  {
  if (fnode==NULL) return;
  fnode->minthirdcol=min(fnode->minthirdcol,thirdcol);
  fnode->maxthirdcol=max(fnode->maxthirdcol,thirdcol);
  }

/*** compute the bandwidth right side or over specifed coulmn ***/
void compute_fnode_ranges(LIST *columns)
  {
  int i,j;
  COLUMN *pcol;
  ROW *prow;
  for (i=0; i<columns->max; i++)
    {
    pcol=&columns->p.column[i];
    for (j=0; j<pcol->H; j++)
      {
      prow=&pcol->row[j];
      expand_fnode_range(prow->L,3*i+0);
      expand_fnode_range(prow->M,3*i+1);
      expand_fnode_range(prow->R,3*i+2);
      }
    }
  }

/*** pick preferred crossovers ***/
int add_preferred_crossover(COLUMN *pcol, FNODE *pfn)
  {
  int i,best=-1,bestV=0;
  ROW *prow;

  /*** find existing crossover or extend from L slot ***/
  for (i=0; i<pcol->H; i++)
    {
    prow=&pcol->row[i];
    if      ((bestV<2)&&(prow->M==pfn)&&(prow->L==NULL)&&(prow->R==NULL)) {best=i; bestV=2;}
    else if ((bestV<1)&&(prow->L==pfn)&&(prow->M==NULL)&&(prow->R==NULL)) {best=i; bestV=1;}
    }

  /*** do it ***/
  if (bestV==2)
    {
    printf("  using existing crossover of column %d for %s at slot %d\n",pcol->num,pfn->name,best);
    return 1;
    }
  else if (bestV==1)
    {
    prow=&pcol->row[best];
    printf("  using extension crossover of column %d for %s at slot %d\n",pcol->num,pfn->name,best);
    prow->L=NULL;
    prow->M=pfn; prow->Lmid=prow->Rmid=1;
    return 1;
    }
  else return 0; /*** unsuccessful ***/
  }

/*** find closest available crossover slot ***/
int closest_crossover(COLUMN *pcol, FNODE *pfn)
  {
  int y,i,j,H2;
  ROW *prow;
  y=(pfn->ymin+pfn->ymax)/2;
  H2=2*pcol->H;
  for (i=0; i<H2; i++)
    {
    if (i%2==0) j=y+i/2;
    else        j=y-i/2-1;
    if ((j<0)||(j>=pcol->H)) continue;
    prow=&pcol->row[j];
    if ((prow->L==NULL)&&(prow->M==NULL)&&(prow->R==NULL)) return j;
    }
  return -1;
  }

/*** add a crossover of the column ***/
void add_crossovers(COLUMN *pcol, LIST *cross)
  {
  int i;
  ROW *prow;
  FNODE *pfn;
  while (cross->max>0)
    {
    pfn=cross->p.pfn[0];
    i=closest_crossover(pcol,pfn);
    if (i<0) break;
    prow=&pcol->row[i];
    printf("  connecting %s over column %d on slot %d\n",pfn->name,pcol->num,i);
    prow->M=pfn; prow->Lmid=prow->Rmid=1;
    list_remove_element(cross,0);
    }
  if (cross->max>0) {printf("FAILED TO CROSSOVER COLUMN %d: ",pcol->num); print_fnodes(cross);}
  }

/*** can you put something in L slot of row? ***/
int Lavail(ROW *prow)
  {
  if ((prow->M!=NULL)&&(prow->M!=USED)) return 0;
  return prow->L==NULL;
  }

/*** can you put something in R slot of row? ***/
int Ravail(ROW *prow)
  {
  if ((prow->M!=NULL)&&(prow->M!=USED)) return 0;
  return prow->R==NULL;
  }

/*** add connection between columns where it could be extended from left or right ***/
int add_preferred_between(COLUMN *pcolL, COLUMN *pcolR, FNODE *pfn)
  {
  int H,i,best=-1,bestV=0,Leq,Lav,Rav;
  ROW *prowL,*prowR;

  /*** find existing crossover or extend from L slot ***/
  H=min(pcolL->H,pcolR->H);
  for (i=0; i<H; i++)
    {
    prowL=&pcolL->row[i];
    prowR=&pcolR->row[i];
    Leq=(prowL->R==pfn)||((prowL->M==pfn)&&(prowL->R==NULL));
    Lav=Ravail(prowL);
    Rav=Lavail(prowR);
    if      ((bestV<6)&&Leq&&(prowR->M==pfn)&&(prowR->L==NULL))                    {best=i; bestV=6;}
    else if ((bestV<5)&&Leq&&(prowR->L==pfn))                                      {best=i; bestV=5;}
    else if ((bestV<4)&&Lav&&(prowR->M==pfn)&&(prowR->L==NULL))                    {best=i; bestV=4;}
    else if ((bestV<3)&&Lav&&(prowR->L==pfn))                                      {best=i; bestV=3;}
    else if ((bestV<2)&&Leq&&(prowR->L==NULL)&&(prowR->M==NULL)&&(prowR->R==NULL)) {best=i; bestV=2;}
    else if ((bestV<1)&&Leq&&Rav)                                                  {best=i; bestV=1;}
    }

  /*** do it ***/
  if (best>=0)
    {
    prowL=&pcolL->row[best];
    prowR=&pcolR->row[best];
    printf("  using preferred crossing between column %d and %d for %s at slot %d\n",
	   pcolL->num,pcolR->num,pfn->name,best);
    if ((prowL->M==NULL)||(prowL->M==USED)) prowL->R=pfn;
    if ((prowR->M==NULL)||(prowR->M==USED)) prowR->L=pfn;
    prowL->Rext=prowR->Lext=1;
    return 1;
    }
  else return 0; /*** unsuccessful ***/
  }

/*** find closest available between slot ***/
int closest_between(COLUMN *pcolL, COLUMN *pcolR, FNODE *pfn)
  {
  int y,i,j,H;
  ROW *prowL,*prowR;
  y=(pfn->ymin+pfn->ymax)/2;
  H=min(pcolL->H,pcolR->H);
  for (i=0; i<2*H; i++)
    {
    if (i%2==0) j=y+i/2;
    else        j=y-i/2-1;
    if ((j<0)||(j>=H)) continue;
    prowL=&pcolL->row[j];
    prowR=&pcolR->row[j];
    if (Lavail(prowR)&&Ravail(prowL)) return j;
    }
  return -1;
  }

/*** add connections between two columns ***/
void add_betweens(COLUMN *pcolL, COLUMN *pcolR, LIST *cross)
  {
  int i;
  ROW *prowL,*prowR;
  FNODE *pfn;
  while (cross->max>0)
    {
    pfn=cross->p.pfn[0];
    i=closest_between(pcolL,pcolR,pfn);
    if (i<0) break;
    prowL=&pcolL->row[i];
    prowR=&pcolR->row[i];
    printf("  connecting %s between column %d and %d on slot %d\n",pfn->name,pcolL->num,pcolR->num,i);
    prowL->R=prowR->L=pfn;
    prowL->Rext=prowR->Lext=1;
    list_remove_element(cross,0);
    }
  if (cross->max>0)
    {
    printf("FAILED TO CROSS BETWEEN COLUMNS %d and %d: ",pcolL->num,pcolR->num);
    print_fnodes(cross);
    }
  }

/*** construct list of crossing fnodes ***/
LIST *crossing_fnodes(COLUMN *pcol, LIST *fnodes, int between)
  {
  int i,x;
  FNODE *pfn;
  LIST *cross;
  cross=list_create(sizeof(FNODE *));
  for (i=0; i<fnodes->max; i++)
    {
    pfn=fnodes->p.pfn[i];
    x= ((!between)&&(pfn->minthirdcol<3*pcol->num+1)&&(pfn->maxthirdcol>3*pcol->num+1)) ||
      (between&&(pfn->minthirdcol<=3*pcol->num+2)&&(pfn->maxthirdcol>3*pcol->num+2));
    if (x!=0) list_append_element(cross,&pfn);
    }
  return cross;
  }

/*** pick the greedy order of routing fnodes ***/
void order_crossing_fnodes(LIST *cross, COLUMN *pcolL, COLUMN *pcolR)
  {
  int i,t,H;
  FNODE *pfn;
  ROW *prowL,*prowR;

  /*** clear ymin/ymax fields ***/
  for (i=0; i<cross->max; i++)
    {
    pfn=cross->p.pfn[i];
    pfn->yminL=pfn->yminR=+1000;
    pfn->ymaxL=pfn->ymaxR=-1000;
    }

  /*** figure out ymin/ymax L/R ranges ***/
  H=min(pcolL->H,pcolR->H);
  for (i=0; i<H; i++)
    {
    prowL=&pcolL->row[i];
    prowR=&pcolR->row[i];
    pfn=prowL->M;
    if ((pfn!=NULL)&&(pfn!=USED))
      {
      pfn->yminL=min(pfn->yminL,i);
      pfn->ymaxL=max(pfn->ymaxL,i);
      }
    pfn=prowL->R;
    if ((pfn!=NULL)&&(pfn!=USED))
      {
      pfn->yminL=min(pfn->yminL,i);
      pfn->ymaxL=max(pfn->ymaxL,i);
      }
    pfn=prowR->M;
    if ((pfn!=NULL)&&(pfn!=USED))
      {
      pfn->yminR=min(pfn->yminR,i);
      pfn->ymaxR=max(pfn->ymaxR,i);
      }
    pfn=prowR->L;
    if ((pfn!=NULL)&&(pfn!=USED))
      {
      pfn->yminR=min(pfn->yminR,i);
      pfn->ymaxR=max(pfn->ymaxR,i);
      }
    }

  /*** compute ymin/ymax fields ***/
  for (i=0; i<cross->max; i++)
    {
    pfn=cross->p.pfn[i];
    if      (pfn->ymaxL<pfn->yminL) {pfn->ymin=pfn->yminR; pfn->ymax=pfn->ymaxR;}
    else if (pfn->ymaxR<pfn->yminR) {pfn->ymin=pfn->yminL; pfn->ymax=pfn->ymaxL;}
    else
      {
      pfn->ymax=min(pfn->ymaxL,pfn->ymaxR);
      pfn->ymin=max(pfn->yminL,pfn->yminR);
      if (pfn->ymin>pfn->ymax) {t=pfn->ymin; pfn->ymin=pfn->ymax; pfn->ymax=t;}
      }
    }

  /*** sort ***/
  list_sort(cross,&pfn_y_cmp);

#if 0
  /*** debugging ***/
  for (i=0; i<cross->max; i++)
    {
    pfn=cross->p.pfn[i];
    printf("@ pcolL=%d pcolR=%d %s L=[%d,%d] R=[%d,%d] OVERLAP=[%d,%d]\n",pcolL->num,pcolR->num,pfn->name,
	   pfn->yminL,pfn->ymaxL,pfn->yminR,pfn->ymaxR,pfn->ymin,pfn->ymax);
    }
#endif
  }

/*** assign available slots between and over columns ***/
int assign_horizontal_wires(LIST *columns, LIST *fnodes)
  {
  int i,j,ok=1;
  COLUMN *pcolL,*pcolR;
  LIST *cross;

  for (i=0; i<columns->max; i++)
    {
    /*** find and assign wires over the column ***/
    pcolL=&columns->p.column[i];
    cross=crossing_fnodes(pcolL,fnodes,0);
    for (j=0; j<cross->max; j++)
      if (add_preferred_crossover(pcolL,cross->p.pfn[j])) list_remove_element(cross,j--);
    order_crossing_fnodes(cross,pcolL,pcolL);
    add_crossovers(pcolL,cross);
    if (cross->max!=0) ok=0;
    list_free(cross);

    /*** find and assign wires between columns ***/
    if (i+1>=columns->max) continue;
    pcolR=&columns->p.column[i+1];
    cross=crossing_fnodes(pcolL,fnodes,1);
    for (j=0; j<cross->max; j++)
      if (add_preferred_between(pcolL,pcolR,cross->p.pfn[j])) list_remove_element(cross,j--);
    order_crossing_fnodes(cross,pcolL,pcolR);
    add_betweens(pcolL,pcolR,cross);
    if (cross->max!=0) ok=0;
    list_free(cross);
    }

  return ok; /* success */
  }

/*** explicit wells painted over blocks ***/
void paint_wells(LIST *blocks, LIST *nodes)
  {
  RECT rect;
  int i,pwell,nwell,metal3;
  BLOCK *pb;
  pwell=find_paint("pwell");
  nwell=find_paint("nwell");
  metal3=find_paint("metal3");
  for (i=0; i<blocks->max; i++)
    { 
    pb=&blocks->p.block[i];
    if (pb->type<=0) /* pwell */
      {
      rect=pb->bbox; if (pb->type==0) rect.x1=0;
      rect.x0=min(-18,rect.x0); /* make sure wells are wide enough */
      rect.paint=pwell;
      list_append_element(pb->rects,&rect);
      }
    if (pb->type>=0) /* nwell */
      {
      rect=pb->bbox; if (pb->type==0) rect.x0=0;
      rect.x1=max(+18,rect.x1); /* make sure wells are wide enough */
      rect.paint=nwell;
      list_append_element(pb->rects,&rect);
      }

    finish_block(pb,i);
    }
  }

/*** top level fullauto ***/
void fullauto(LIST *blocks, LIST *nodes, CIRCUIT *circuit)
  {
  int i,x=0,ypitch,N=1;
  LIST *columns,*fnodes;
  BLOCK *pb;

  /*** add explicit well paint and metal3 power strips to top of blocks ***/
  paint_wells(blocks,nodes);

  /*** repeat until it wires it up in the right ypitch ***/
  USED=create_fnode("",0);
  ypitch=(((int)circuit->ypitch)+11)/12;
  while(1)
    {
    /*** separate into multiple columns ***/
    fnodes=list_create(sizeof(FNODE *));
    columns=separate_into_columns(blocks,circuit->directives,ypitch,N);
    if (columns==NULL)
      {
      N++;
      if (N<MAX_COLUMNS) continue;
      else
	{
	fprintf(stderr,"ERROR: CELL %s exceeded %d columns!\n",circuit->filename,MAX_COLUMNS);
	break;
	}
      }

    /*** figure out fnodes and rows ***/
    for (i=0; i<columns->max; i++) find_rows(blocks,nodes,fnodes,&columns->p.column[i]);
    list_finish_lazy_sort(fnodes,&pfn_cmp);
    vertical_directives(fnodes,circuit->directives);
    horizontal_directives(fnodes,circuit->directives,
			  &columns->p.column[0],&columns->p.column[columns->max-1],ypitch);
    for (i=0; i<columns->max; i++) extend_segments(&columns->p.column[i]);
    compute_fnode_ranges(columns);

    /*** assign horizontal wires to available slots ***/
    if (!assign_horizontal_wires(columns,fnodes))
      {
      N++;
      if (N<MAX_COLUMNS) continue;
      else
	{
	fprintf(stderr,"ERROR: CELL %s exceeded %d columns!\n",circuit->filename,MAX_COLUMNS);
	break;
        }
      }
    print_fnodes(fnodes);
    for (i=0; i<columns->max; i++) print_column(&columns->p.column[i]);

    /*** generate wires for each column ***/
    for (i=0; i<columns->max; i++) wire_column(blocks,nodes,fnodes,&columns->p.column[i],&x);

    /*** offset blocks belong to columns ***/
    for (i=0; i<blocks->max; i++)
      {
      pb=&blocks->p.block[i];
      pb->x=columns->p.column[pb->column].x;    
      }

    break;
    }
  }

/*** table of allowed characters in calma ***/
static char calmaMapTable[] =
{
      0,    0,    0,    0,    0,    0,    0,    0,	/* NUL - BEL */
      0,    0,    0,    0,    0,    0,    0,    0,	/* BS  - SI  */
      0,    0,    0,    0,    0,    0,    0,    0,	/* DLE - ETB */
      0,    0,    0,    0,    0,    0,    0,    0,	/* CAN - US  */
      0,    0,    0,    0,  '$',    0,    0,    0,	/* SP  - '   */
      0,    0,    0,    0,    0,    0,    0,    0,	/* (   - /   */
    '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',	/* 0   - 7   */
    '8',  '9',    0,    0,    0,    0,    0,    0,	/* 8   - ?   */
      0,  'A',  'B',  'C',  'D',  'E',  'F',  'G',	/* @   - G   */
    'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',	/* H   - O   */
    'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',	/* P   - W   */
    'X',  'Y',  'Z',    0,    0,    0,    0,  '_',	/* X   - _   */
      0,  'a',  'b',  'c',  'd',  'e',  'f',  'g',	/* `   - g   */
    'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',	/* h   - o   */
    'p',  'q',  'r',  's',  't',  'u',  'v',  'w',	/* p   - w   */
    'x',  'y',  'z',    0,    0,    0,    0,    0,	/* x   - DEL */
};

/*** convert labels to calma same way magic does ***/
char *calma_name(NODE *pn)
  {
  unsigned char *new,*p;
  new=leak_malloc(strlen(pn->name)+2);
  strcpy(new,pn->name);
  if (pn->global) {new[strlen(new)+1]=0; new[strlen(new)]='!';}
  p=new;
  while (*p!=0)
    {
    if (*p > 127 || calmaMapTable[*p] == 0) *p = 'X';
    else *p = calmaMapTable[*p];
    p++;
    }
  return new;
  }

/*** find list of labels used in a block ***/
LIST *pinlist(LIST *nodes, int blocknum)
  {
  int i,k;
  NODE *pn;
  PORT *pp;
  LIST *pins;
  char *new;
  int metal2;
  metal2=find_paint("metal2");
  pins=list_create(sizeof(char *));
  for (i=0; i<nodes->max; i++)
    {
    pn=&nodes->p.node[i];
    for (k=0; k<pn->ports->max; k++)
      {
      pp=&pn->ports->p.port[k];
      if (pp->paint!=metal2) continue;
      if ((blocknum>=0)&&(pp->block!=blocknum)) continue;
      new=calma_name(pn);
      list_append_element(pins,&new);
      break;
      }
    }
  return pins;
  }

/*** print comma separated pinlist ***/
void print_pinlist(LIST *pins, int language)
  {
  int j;
  char *sep,*lastsep;
  if      (language==0) {sep=", "; lastsep="";}
  else if (language==1) {sep=" ";  lastsep="";}
  else assert(0);
  for (j=0; j<pins->max; j++)
    {
    printf("%s",pins->p.pc[j]);
    printf("%s",j<pins->max-1 ? sep : lastsep);
    }
  }

/*** dump pin declarations ***/
void dump_pins( export_skill_layout_state* pExportState, LIST *directives)
{
  DIRECTIVE *pdir;
  int i;

  assert( pExportState != NULL ) ;
  assert( directives != NULL ) ;

  for (i=0; i<directives->max; i++) {
    pdir=&directives->p.directive[i];

    switch( pdir->type ) {
    case 0:
      /* strut */
      export_skill_output_strut_pin( pExportState, pdir->name ) ;
      /* fprintf(fout,"strut %s %d\n",pdir->name,pdir->pos); */
      break;

    case 1:
      /* top */
      export_skill_output_top_pin( pExportState, pdir->name ) ;
      /*  fprintf(fout,"top %s %d\n",pdir->name,pdir->pos); */
      break;
      
    case 2:
     /* bottom */
      export_skill_output_bottom_pin( pExportState, pdir->name ) ; 
      /*  fprintf(fout,"bottom %s %d\n",pdir->name,pdir->pos); */
      break;
      
    case 3:
      /* vertical */
      export_skill_output_vertical_pin( pExportState, pdir->name ) ;
      /*  fprintf(fout,"vertical %s %d\n",pdir->name,pdir->pos); */
      break;
      
    case 4:
      /* left */
      export_skill_output_left_pin( pExportState, pdir->name, pdir->pos ) ;
      /*  fprintf(fout,"left %s %d\n",pdir->name,pdir->pos); */
      break;
      
    case 5:
      /* right */
      export_skill_output_right_pin( pExportState, pdir->name, pdir->pos ) ;
      /*  fprintf(fout,"right %s %d\n",pdir->name,pdir->pos); */
      break;

    case 6:
      /* pin */
      export_skill_output_pin( pExportState, pdir->name, pdir->pos ) ;
      /*  fprintf(fout,"right %s %d\n",pdir->name,pdir->pos); */
      break;
    case 7:
      /* pinfixme */
      export_skill_output_pin( pExportState, pdir->name, pdir->pos ) ;
      /*  fprintf(fout,"right %s %d\n",pdir->name,pdir->pos); */
      break;
    case 8:
      /* inplace */
      export_skill_output_inplace_pin( pExportState, pdir->name );
      break;
      

    default:
      assert(0);
      break;
    }
  }
}

/*** just output the blocks and connections for other tools ***/
/*** language=0 for Verilog, language=1 for SPICE ***/
void dump_blocks(LIST *blocks, LIST *nodes, CIRCUIT *circuit, int language)
  {
  int i;
  LIST *pins;
  BLOCK *pb;
  RECT bbox,comment;
  char* cellname;
  char* pChar;

  export_skill_layout_state exportstate;

  

  /*** add explicit well paint to top of blocks ***/
  paint_wells(blocks,nodes);

  /*** compute bboxs of blocks ***/
  bbox.x0=bbox.y0=+1e10;
  bbox.y1=bbox.y1=-1e10;
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    rectangle_union(&bbox,&pb->bbox);
    }
  bbox.x0=-12*((((int)(-bbox.x0))+11)/12);
  bbox.x1=+12*((((int)(+bbox.x1))+11)/12);
  fprintf(stderr,"DUMP_BLOCKS: nwidth=%g pwidth=%g\n",-bbox.x0,bbox.x1);

  /*** add comment paint with bbox x0 and x1 to all blocks ***/
  for (i=0; i<blocks->max; i++)
    {
    pb=&blocks->p.block[i];
    comment=bbox;
    comment.y0=pb->bbox.y0;
    comment.y1=pb->bbox.y1;
    comment.paint=find_paint("comment");
    list_append_element(pb->rects,&comment);
    }

  cellname = leak_malloc( ( strlen( circuit->filename ) + 32 ) * sizeof( char ) ) ;

   
  if ( cellname != NULL ) {

    strcpy( cellname, circuit->filename ) ;

    pChar = cellname;
    pChar += strlen( cellname );

    while ( ( pChar > cellname ) && ( *pChar != '.' ) && ( *pChar != '/' ) ) {
      --pChar ;
    }

    if ( ( pChar > cellname ) && ( *pChar == '.' ) ) {
      *pChar = '\0';
      while ( ( pChar > cellname ) && ( *pChar != '/' ) ) {
	--pChar ;
      }
      if ( ( pChar > cellname ) && ( *pChar == '/' ) ) {
	++pChar ;
      }
    }

    strcat( pChar, ".il" ) ;

    if ( export_skill_init_state( &exportstate,
				  pChar,
				  blocks,
				  nodes,
				  circuit->subcells,
				  (int) circuit->ypitch ) ) {

      /*** write convert blocks ***/
      for (i=0; i<blocks->max; i++) {
	export_skill_block_layout( &exportstate, i );
      }

      export_skill_end_blocks( &exportstate ) ;

      dump_pins( &exportstate ,circuit->directives);

/*        export_skill_output_ypitch( &exportstate, ( (int) circuit->ypitch ) ) ; */ 
      
      export_skill_deinit_state( &exportstate ) ;
    }
    
    leak_free( cellname ) ;
  }

  /*** declare blocks as modules ***/
  for (i=0; i<blocks->max; i++)
    {
    pins=pinlist(nodes,i);
    pb=&blocks->p.block[i];
    if (language==0)
      {
      if (pb->filename!=NULL) printf("// %s\n",pb->filename);
      else printf("// %s stack\n",(pb->type==1? "PMOS" : pb->type==-1? "NMOS": "UNKNOWN"));
      printf("module BLOCK%d (",i); print_pinlist(pins,0); printf(");\n");
      printf("  inout "); print_pinlist(pins,0); printf(";\n");
      printf("endmodule\n\n");
      }
    else if (language==1)
      {
      if (pb->filename!=NULL) printf("* %s\n",pb->filename);
      else printf("* %s stack\n",(pb->type==1? "PMOS" : pb->type==-1? "NMOS": "UNKNOWN"));
      printf(".SUBCKT BLOCK%d ",i); print_pinlist(pins,1); printf("\n");
      printf(".ENDS\n\n");
      }
    list_free(pins);
    }

  /*** top level module instantiates blocks ***/
  if      (language==0) printf("module TOP;\n");
  else if (language==1) printf(".SUBCKT TOP\n");
  if (language==0)
    {
    pins=pinlist(nodes,-1);
    printf("  wire ");
    print_pinlist(pins,0);
    printf(";\n");
    }
  for (i=0; i<blocks->max; i++)
    {
    pins=pinlist(nodes,i);
    if (language==0)
      {
      printf("  BLOCK%d y%d(",i,i);
      print_pinlist(pins,0);
      printf(");\n");
      }
    else if (language==1)
      {
      printf("  X%d ",i);
      print_pinlist(pins,1);
      printf(" BLOCK%d\n",i);
      }
    list_free(pins);
    }
  if      (language==0) printf("endmodule\n");
  else if (language==1) printf(".ENDS\n");
  }
