/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** global aspice options ***/
OPTIONS options;

/*** create blank circuit data structure ***/
CIRCUIT create_circuit()
  {
  CIRCUIT circuit;
  circuit.Nnodes=0; circuit.nodes=NULL;
  circuit.Ndevices=0; circuit.devices=NULL;
  circuit.Nblocks=0; circuit.blocks=NULL;
  circuit.Ndnodes=0; circuit.dnodes=NULL;
  circuit.Ndrules=0; circuit.drules=NULL;
  circuit.Ngmas=0; circuit.gmas=NULL;
  circuit.Ngroups=0; circuit.groups=NULL;
  circuit.Nexclusives=0; circuit.exclusives=NULL;
  circuit.time=0;
  circuit.ftrace=NULL;
  circuit.run=NULL;
  circuit.names=NULL;
  circuit.trace_started=0;
  circuit.do_restore=0;
  circuit.do_reorder=1;
  return circuit;
  }

/*** create an analog node ***/
void create_node(NODE *pn, DIGITAL_NODE *dnode,
                 int watch, int fixed, int top_port, PARSENODE *name)
  {
  pn->digital_node=dnode;
  if (dnode!=NULL)
    {
    dnode->level |= LEVEL_LOWER; // analog is lower cosim level
    dnode->has_analog_fanin = 1; // make sure digital node knows
    }
  pn->V=pn->X=pn->B=pn->CG=pn->C=0;
  pn->smoothV=pn->smoothX=0;
  pn->watch=watch;
  pn->floatingC=0;
  pn->pending=0;
  pn->unstab=0;
  pn->top_port=top_port;
  pn->portG=0;
  pn->portN=0;
  pn->portP=0;
  pn->fixed=fixed;
  pn->vsource=0;
  pn->mark=0;
  pn->force=pn->old_force=pn->custom_force=FORCE_NONE;
  pn->forceV=0;
  pn->aggressor=pn->custom_aggressor=0;
  pn->exposed=0;
  pn->initial_exposed=0;
  pn->relevant=1;
  pn->canonical=0;
  pn->bad=0;
  pn->oldbad=0;
  pn->good=0;
  pn->oldgood=0;
  pn->pblock=NULL;
  pn->jblock=-1;
  pn->A=NULL;
  pn->name=name;
  pn->seed=NULL;
  pn->map=NULL;
  pn->leaky=0;
  pn->nocc=0;
  }

/*** returns debugging name for an analog node ***/
char *get_node_name(NODE *pn)
  {
  return pn->name->name;
  }

/*** free an analog node ***/
void free_node(NODE *pn)
  {
  if (pn->A!=NULL) leak_free(pn->A);
  }

/*** free a block ***/
void free_block(BLOCK *pblock)
  {
  leak_free(pblock->nodes);
  }

/*** free an exclusive set ***/
void free_exclusive(EXCLUSIVE *pex)
  {
  leak_free(pex->nodes);
  }

/*** frees the circuit data structures ***/
void free_circuit(CIRCUIT *circuit)
  {
  int j;

  /*** free nodes ***/
  for (j=0; j<circuit->Nnodes; j++) free_node(&circuit->nodes[j]);
  leak_free(circuit->nodes);

  /*** free devices ***/
  for (j=0; j<circuit->Ndevices; j++) free_device(&circuit->devices[j]);
  leak_free(circuit->devices);

  /*** free blocks ***/
  for (j=0; j<circuit->Nblocks; j++) free_block(&circuit->blocks[j]);
  if (circuit->blocks!=NULL) leak_free(circuit->blocks);
  
  /*** free digital nodes ***/
  for (j=0; j<circuit->Ndnodes; j++) free_digital_node(&circuit->dnodes[j]);
  leak_free(circuit->dnodes);

  /*** free digital rules ***/
  for (j=0; j<circuit->Ndrules; j++) free_digital_rule(&circuit->drules[j]);
  leak_free(circuit->drules);

  /*** free digital gmas ***/
  for (j=0; j<circuit->Ngmas; j++) free_digital_gma(&circuit->gmas[j]);
  leak_free(circuit->gmas);

  /*** free digital groups ***/
  for (j=0; j<circuit->Ngroups; j++) free_digital_group(&circuit->groups[j]);
  leak_free(circuit->groups);

  /*** free exclusive invariants ***/
  for (j=0; j<circuit->Nexclusives; j++) free_exclusive(&circuit->exclusives[j]);
  leak_free(circuit->exclusives);

  /*** free names ***/
  for (j=0; j<circuit->names->max; j++)
    {
    PARSENODE *pnode;
    pnode=circuit->names->p.parsenode[j];
    leak_free(pnode->name);
    leak_free(pnode);
    }
  list_free(circuit->names);

  /*** close ftrace and free run name ***/
  if (circuit->ftrace!=NULL) fclose(circuit->ftrace);

  /*** reinitialize circuit ***/
  *circuit = create_circuit();
  }

/*** set default options ***/
OPTIONS default_options()
  {
  OPTIONS options;

  options.timestep=5e-12;
  options.timeratio=0;
  options.poststep=100e-12;
  options.timemax=50e-9;
  options.maxerr=1e-6;
  options.partition_mode=1;
  options.coupling_cutoff=0.25;
  options.vmin=-1e10;
  options.vmax= 1e10;
  options.checkpoint_interval=36000; /* every 10 hours run time */

  options.falseV=0;
  options.trueV=3.3;
  options.lo_thresholdV=1.1;
  options.hi_thresholdV=2.2;
  options.prs_tau=0;
  options.digital_time_unit=1e-12;
  options.d2a_shape=0;
  options.d2a_saturation=0.25;
  options.a2d_ewma_tau=10e-12;
  options.settle_time=0.25e-9;
  options.scale_after_delay=1;
  options.scale_after_ps_delay=1;

  options.VTN=0.5;
  options.VTP=0.5;
  options.leakage_enabled=0;
  options.small_leak_voltage=0;
  options.large_leak_voltage=0;
  options.max_bump_fanin_aggressors=32;
  options.max_delay_fanin_aggressors=32;
  options.stable_dVdt=1e-3/1e-9; // consider 1mV/ns to be stable
  options.bump_event_interval=0.25e-9;
  options.delay_event_interval=0.25e-9;
  options.scenario_timemax=10e-9;
  options.thresh_scenario_timemax=1e-9;
  options.relevant_cap_ratio=0.05;
  options.alint_voltage_margin=0.1;
  options.delay_slow_inverse=0;

  return options;
  }
