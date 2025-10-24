#include "auto.h"

/*** create or find a node ***/
NODE *add_node(char *name, LIST *nodes)
  {
  NODE node,*pn;
  char name2[STRMAX];
  int i;

  /*** handle globals ***/
  if (name[strlen(name)-1]=='!')
    {
    strcpy(name2,name);
    name2[strlen(name2)-1]=0;
    node.name=get_name(name2);
    node.global=1;
    node.internal=0;
    }
  else if (name[strlen(name)-1]=='#')
    {
    node.name=get_name(name);
    node.global=0;
    node.internal=1;
    }
  else
    {
    node.name=get_name(name);
    node.global=0;
    node.internal=0;
    }

  /*** search for node ***/
  i=find_element_lazy_sort(nodes,&node,&nodecmp);
  if (i<0)
    {
    node.wires=list_create(sizeof(WIRE));
    node.ports=list_create(sizeof(PORT));
    list_insert_element_lazy_sort(nodes,&node,&nodecmp);
    i=find_element_lazy_sort(nodes,&node,&nodecmp);
    }
  pn=&nodes->p.node[i];
  pn->global=pn->global||node.global;
  return pn;
  }

/*** add a rectangle port ***/
void add_port_extra(char *name, char *extra, int paint, int x0, int y0, int x1, int y1, int block, LIST *nodes)
  {
  PORT port;
  NODE *pn;

  port.n=0;
  port.paint=paint;
  port.x0=x0;
  port.y0=y0;
  port.x1=x1;
  port.y1=y1;
  port.block=block;
  if (extra!=NULL) port.extra=leak_strdup(extra);
  else             port.extra=NULL;
  pn=add_node(name,nodes);
  list_append_element(pn->ports,&port);
  }

/*** add a port without extra field ***/
void add_port(char *name, int paint, int x0, int y0, int x1, int y1, int block, LIST *nodes)
  {
  add_port_extra(name,NULL,paint,x0,y0,x1,y1,block,nodes);
  }

/*** create a list of wires which are a spanning tree of specified ports ***/
LIST *wire_ports(LIST *ports, LIST *blocks)
  {
  int i,j,k,l;
  WIRE wire;
  PORT *pp[2];
  LIST *wires,*used_wires;

  /*** compute distance of wires, sort wires ***/
  wires=list_create(sizeof(WIRE));
  for (i=0; i<ports->max; i++) for (j=i+1; j<ports->max; j++)
    {
    wire.port[0]=i;
    wire.port[1]=j;
    wire.distance=port_distance(&ports->p.port[i],&ports->p.port[j],blocks,1);
    list_append_element(wires,&wire);
    }
  list_sort(wires,&wire_cmp);

  /*** initialize port indices to block indicies ***/
  for (i=0; i<ports->max; i++) ports->p.port[i].n=ports->p.port[i].block;

  /*** connect up best wires in order, if different n ***/
  used_wires=list_create(sizeof(WIRE));
  for (i=0; i<wires->max; i++)
    {
    wire=wires->p.wire[i];
    pp[0]=&ports->p.port[wire.port[0]];
    pp[1]=&ports->p.port[wire.port[1]];
    if (pp[0]->n!=pp[1]->n)
      {
      list_append_element(used_wires,&wire);
      l=pp[0]->n;
      for (k=0; k<ports->max; k++)
        if (ports->p.port[k].n==l) ports->p.port[k].n=pp[1]->n;
      }
    }

  /*** free wires, return used_wires ***/
  list_free(wires);
  return used_wires;
  }

