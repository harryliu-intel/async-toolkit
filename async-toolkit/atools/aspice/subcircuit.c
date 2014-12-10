/*****************************************************
 * Copyright 2007 Fucrum Microsystems                *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** mark nodes relevant to MARK_VICTIM ***/
int mark_commit(CIRCUIT *circuit) {
  int i;
  NODE *pn,*pnGND,*pnVdd;

  /*** initialize and remove all but VICTIM/BARRIER markings ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->C=0;
    pn->relevant=0;
    pn->map=NULL;
    if ((pn->mark!=MARK_VICTIM && pn->mark!=MARK_BARRIER) || pn->seed!=pn)
      pn->mark=MARK_UNKNOWN;
    }

  /*** special handling of GND, Vdd nodes ***/
  pnGND = find_node(circuit,"","$GND");
  pnVdd = find_node(circuit,"","$Vdd");
  if (pnGND==NULL)  fprintf(dsim_err,"ERROR: Can't find $GND node.\n");
  if (pnVdd==NULL)  fprintf(dsim_err,"ERROR: Can't find $Vdd node.\n");
  if (pnGND==pnVdd) fprintf(dsim_err,"ERROR: $Vdd shorted to $GND by aliases.\n");
  if ((pnGND==NULL)||(pnVdd==NULL)||(pnGND==pnVdd)) return 1;
  pnGND = pnGND->seed;
  pnVdd = pnVdd->seed;
  if (pnGND==pnVdd)
    {
    fprintf(dsim_err,"ERROR: $Vdd shorted to $GND by resistors.\n");
    return 1;
    }
  pnGND->mark=MARK_GLOBAL;
  pnVdd->mark=MARK_GLOBAL;

  /*** mark VICTIM/SHARED/INTERNAL nodes relevant to victim ***/
  mark_transistor_networks(circuit);

  /*** mark GATE ***/
  mark_gates(circuit);
  return 0;
}

/*** report marked subcircuit ***/
int marked_subcircuit(CIRCUIT *circuit, char *filename)
  {
  CIRCUIT subcircuit;
  LIST *marked_nodes;
  int i,j,type;
  NODE *pn,*pnS,*pnD,*pnG;
  DEVICE *pdev;
  FILE *out;
  char name[STRMAX];

  /*** apply any pending markings ***/
  mark_commit(circuit);

  /*** identify load nodes ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnS=pdev->sub.trans->nodes[0]->seed;
    pnD=pdev->sub.trans->nodes[1]->seed;
    pnG=pdev->sub.trans->nodes[2]->seed;
    if (((pnG->mark==MARK_VICTIM)||
         (pnG->mark==MARK_INVERSE)||
         (pnG->mark==MARK_SHARED_INVERSE)||
         (pnG->mark==MARK_SHARED))&&
        (pnS->mark==MARK_UNKNOWN)) pnS->mark=MARK_LOAD;
    if (((pnG->mark==MARK_VICTIM)||
         (pnG->mark==MARK_INVERSE)||
         (pnG->mark==MARK_SHARED_INVERSE)||
         (pnG->mark==MARK_SHARED))&&
        (pnD->mark==MARK_UNKNOWN)) pnD->mark=MARK_LOAD;
    }

  /*** identify coupling nodes ***/
  mark_capacitors(circuit);

  /*** mark resistive subnets too ***/
  set_subnets(circuit);

  /*** big trick -- swap original circuit for relevant subcircuit ***/
  subcircuit = create_subcircuit(circuit);
  circuit = &subcircuit;

  /*** print marked nodes ***/
  out=NULL;
  if (filename[0]!=0)
    {
    sprintf(name,"%s.in",filename);
    out=fopen(name,"w");
    fprintf(dsim_out,"Marked nodes written to %s.\n",name);
    }
  else fprintf(dsim_out,"Marked nodes:\n");
  marked_nodes=list_create(sizeof (NODE *));
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    list_append_element(marked_nodes,&pn);
    }
  list_sort(marked_nodes,&node_cmp);
  for (j=0; j<MARK_MAX; j++)
    {
    for (i=0; i<marked_nodes->max; i++)
      {
      pn=marked_nodes->p.pn[i];
      if (pn->mark!=j) continue;
      if (pn->seed!=pn) continue;
      if (out!=NULL)
        {
        fprintf(out,"/* %s %s */\n",mark_name[pn->mark],get_node_name(pn));
        if (pn->mark==MARK_LOAD || pn->mark==MARK_COUPLING)
          fprintf(out,"force %s : u\n",get_node_name(pn));
        }
      else
        {
        fprintf(dsim_out,"  %s %s\n",mark_name[pn->mark],get_node_name(pn));
        } 
      }
    }
  list_free(marked_nodes);
  if (filename[0]!=0) fclose(out);

  /*** print relevant devices and aliases of victims ***/
  out=dsim_out;
  if (filename[0]!=0)
    {
    sprintf(name,"%s.aspice",filename);
    out=fopen(name,"w");
    fprintf(dsim_out,"Relevant Devices written to %s.\n",name);
    }
  else fprintf(dsim_out,"Relevant Devices:\n");
  for (i=0; i<circuit->Nnodes; i++)
    {
    PARSENODE *pname[2];
    pn=&circuit->nodes[i];
    if (pn->seed->mark==MARK_COUPLING ||
        pn->seed->mark==MARK_GLOBAL ||
        pn->seed->mark==MARK_LOAD)
      continue;
    pname[1]=pname[0]=pn->name;
    fprintf(out,"  wire(\"%s\"",pname[1]->name);
    pname[1]=pname[1]->palias;
    while(pname[1]!=pname[0])
      {
      fprintf(out,",\"%s\"",pname[1]->name);
      pname[1]=pname[1]->palias;
      }
    fprintf(out,")\n");
    }
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    fprintf(out,"  ");
    print_device(out,pdev);
    }
  if (filename[0]!=0) fclose(out);

  /*** free and return ***/
  free_subcircuit(circuit);
  return 0;
  }

/*** clear all markings ***/
void mark_clear(CIRCUIT *circuit) {
  int i;
  NODE *pn;
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->mark=MARK_UNKNOWN;
    pn->relevant=0;
    }
}

/*** recursively change MARK_GATE to MARK_VICTIM **/
void mark_fanin(CIRCUIT *circuit, int max_levels) {
  int i,progress,levels=0;
  NODE *pn;
  do 
    {
    progress=0;
    levels++;
    mark_commit(circuit);
    for (i=0; i<circuit->Nnodes; i++)
      {
      pn=&circuit->nodes[i];
      if (pn->seed!=pn) continue;
      if (pn->mark==MARK_GATE)
        {
        pn->mark=MARK_VICTIM;
        progress++;
        }
      }
    if (interrupt_sim)
      {
      fprintf(dsim_err,"WARNING: mark_fanin interrupted.\n");
      break;
      }
    if (progress)
      fprintf(dsim_out,"Marked %d nodes at level %d.\n",progress,levels);
    if (max_levels>=0 && levels>=max_levels) break;
    } while (progress);
}
