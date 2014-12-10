/*****************************************************
 * Copyright 2005 Fucrum Microsystems                *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

char *mark_name[MARK_MAX]={"unknown","victim","global","gate","shared",
                           "N-internal","P-internal",
                           "shared N-internal","shared P-internal",
                           "inverse","shared-inverse","load","coupling","barrier",
                           "shared-gate","prime"};
NODE *Victim=NULL,*pnGND=NULL,*pnVdd=NULL,*pnEnvGND=NULL,*pnEnvVdd=NULL;
double trueV,falseV,midV,prs_pct;
int doCouplingCap = 1;
int CountScenarios = 0;
int verbose = 0;
int debug_alint = 0;

/*** function prototypes ***/
void mark_exposed(CIRCUIT *circuit, int initial, int final, int expose_globals);

/*** check if a node is MARK_GATE or MARK_SHARED_GATE **/
int isMarkGate(NODE *pn)
  {
  return (pn->mark==MARK_GATE) || (pn->mark==MARK_SHARED_GATE);
  }

/*** check if a node belongs to the fanin of the victim **/
int isMarkDUT(NODE *pn)
  {
  return (pn->mark==MARK_VICTIM ||pn->mark==MARK_SHARED || pn->mark==MARK_SHARED_GATE ||
          pn->mark==MARK_NINTERNAL || pn->mark==MARK_PINTERNAL ||
          pn->mark==MARK_SHARED_NINTERNAL || pn->mark==MARK_SHARED_PINTERNAL);
  }

/*** flip direction a node is forced ***/
int flip_force(int force)
  {
  if      (force==FORCE_UP) return FORCE_DN;
  else if (force==FORCE_DN) return FORCE_UP;
  else                      return force;
  }

/*** compare two NODE *'s by worst C in descending order ***/
int coupling_cmp(void *p1, void *p2)
  {
  NODE *pn1 = *((NODE **) p1);
  NODE *pn2 = *((NODE **) p2);
  if (pn1->C>pn2->C) return -1;
  if (pn2->C>pn1->C) return  1;
  return 0;
  }

/*** compare fanin groups by size ***/
int fanin_group_cmp(void *p1, void *p2)
  {
  EXCLUSIVE *pex1 = *((EXCLUSIVE **) p1), *pex2 = *((EXCLUSIVE **) p2);
  return pex2->Nnodes - pex1->Nnodes;
  }

/*** compare FANIN's to sort a scenario ***/
int fanin_cmp(void *p1, void *p2)
  {
  int c;
  FANIN *pf1 = ((FANIN *) p1), *pf2 = ((FANIN *) p2);
  c = node_cmp(&pf1->pn,&pf2->pn);
  if (c!=0) return c;
  c = pf2->aggressor-pf1->aggressor;
  if (c!=0) return c;
  c = pf2->initial-pf1->initial;
  return c;
  }

/*** Compare two scenarios ***/
int scenario_cmp(void *p1, void *p2)
  {
  LIST *pl1 = *((LIST **) p1), *pl2 = *((LIST **) p2);
  return list_compare(pl1,pl2,&fanin_cmp);
  }

/*** check if an individual node contributes to an exclusive count, with exclcc ***/
int cc_exclusion_active(NODE *pn, int type, int delay, int after)
  {
  int active,old_force;
  if (after) /* final state of scenario, assumes nothing is still switching */
    {
    old_force=pn->force;
    if (pn->aggressor || pn->mark==MARK_COUPLING || (pn->mark==MARK_VICTIM && delay))
      pn->force=flip_force(pn->force);
    active = (type==EXCLHI && pn->force==FORCE_UP) ||
             (type==EXCLLO && pn->force==FORCE_DN);
    pn->force=old_force;
    }
  else /* initial state of scenario with fanin and coupling flipping */
    {
    active = pn->aggressor ||
      ((type==EXCLHI || pn->mark==MARK_COUPLING) && (pn->force==FORCE_UP)) ||
      ((type==EXCLLO || pn->mark==MARK_COUPLING) && (pn->force==FORCE_DN));
    }
  return active;
  }

/*** check if an individual node contributes to an exclusive count, without exclcc ***/
int exclusion_active(NODE *pn, int type, int delay, int after)
  {
  if ((type!=EXCLLO) && (type!=EXCLHI)) return 0; // ignore exclcc
  return cc_exclusion_active(pn,type,delay,after);
  }

/*** check if exclusion properties satisfied before, during, after event ***/
int exclusion_satisfied(CIRCUIT *circuit, int delay)
  {
  int i,j,count,after_count,shared;
  EXCLUSIVE *pex;
  NODE *pn;
  for (i=0; i<circuit->Nexclusives; i++)
    {
    pex=&circuit->exclusives[i];

    // check exclusion violations, detect SHARED nodes
    shared=count=after_count=0;
    for (j=0; j<pex->Nnodes; j++)
      {
      pn=pex->nodes[j]->seed;
      count += cc_exclusion_active(pn,pex->type,delay,0);
      after_count += cc_exclusion_active(pn,pex->type,delay,1);
      if (count>1 || after_count>1) return 0;
      if (pn->mark==MARK_SHARED) shared=1;
      }
    if (!shared || pex->type==EXCLCC) continue;

    // mark SHARED nodes pulled toward exclusion activation
    for (j=0; j<circuit->Nnodes; j++) circuit->nodes[j].exposed=0;
    if      (pex->type==EXCLHI) { pnVdd->exposed=1; mark_exposed(circuit,1,1,0); }
    else if (pex->type==EXCLLO) { pnGND->exposed=1; mark_exposed(circuit,1,1,0); }

    // check exclusion violations with SHARED nodes
    count=after_count=0;
    for (j=0; j<pex->Nnodes; j++)
      {
      pn=pex->nodes[j]->seed;
      if (pn->mark==MARK_SHARED)
        {
        count += pn->exposed;
        after_count += pn->exposed;
        }
      else
        {
        count += exclusion_active(pn,pex->type,delay,0);
        after_count += exclusion_active(pn,pex->type,delay,1);
        }
      if (count>1 || after_count>1) return 0;
      }
    }
  return 1;
  }

/*** configure subnets of circuit to do whatever their seed's do ***/
void set_subnets(CIRCUIT *circuit)
  {
  int i;
  NODE *pn,*seed;
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    seed=pn->seed;
    if (!pn->fixed) pn->V=seed->V;
    pn->smoothV=pn->V;
    pn->X=pn->smoothX=0;
    pn->mark=seed->mark;
    pn->force=seed->force;
    pn->aggressor=seed->aggressor;
    pn->exposed=seed->exposed;
    }
  }

/*** tests if a gate is on during initial or final state or either ***/
int gate_on(int type, NODE *pnG, int initial, int final)
  {
  int force;
  if ((pnG->force==FORCE_NONE) && !isMarkGate(pnG)) return 0;
  force=pnG->force;
  if (initial&&(((type==NTYPE)&&(force!=FORCE_DN))||
		((type==PTYPE)&&(force!=FORCE_UP)))) return 1;
  if (pnG->aggressor) force=flip_force(force);
  if (final&&(((type==NTYPE)&&(force!=FORCE_DN))||
	      ((type==PTYPE)&&(force!=FORCE_UP)))) return 1;
  return 0;
  }

/*** return leakage voltage away from rail ***/
double leakV(NODE *pn)
  {
  if (options.leakage_enabled &&  pn->leaky) return options.large_leak_voltage;
  if (options.leakage_enabled && !pn->leaky) return options.small_leak_voltage;
  return 0;
  }

/***
 * Propagate voltage from exposed nodes through transistors.  Dir is
 * used as a tie-breaker to decide wheather low or high voltages
 * dominate in the case of interference.  Dir=2 keeps whatever voltage
 * a node is set to first, and is used for "analog off", not alint.
 ***/
int propagate_voltage(NODE *pnS, NODE *pnD, NODE *pnG, int type, int dir)
  {
  double V=pnD->V;
  if (!pnS->exposed) return 0; // S must already be exposed to a power supply
  if (pnD->fixed || pnD->force!=FORCE_NONE) return 0; // D is fixed or forced

  // set drain voltage via transistor using simple current model
  if (type==NTYPE)
    {
    if      (pnG->V-pnS->V>options.VTN) V=pnS->V; // Vgs above threshold
    else if (pnS->V>pnD->V) V=max(V,pnG->V-options.VTN); // NMOS pulls up
    }
  else if (type==PTYPE)
    {
    if      (pnS->V-pnG->V>options.VTP) V=pnS->V; // Vsg above threshold
    else if (pnS->V<pnD->V) V=min(V,pnG->V+options.VTP); // PMOS pulls dn
    }

  // handle interference, chose voltage based on dir
  if (pnD->exposed)
    {
    // Yuck -- must have numerical slop on voltage comparisons!
    if      ((dir==0) && (V<pnD->V-trueV*1e-6)) { pnD->V=V; return 1; }
    else if ((dir==1) && (V>pnD->V+trueV*1e-6)) { pnD->V=V; return 1; }
    return 0;
    }

  // expose D, set voltage, report progress
  pnD->exposed=1;
  pnD->V=V;
  return 1;
  }

/*** propagate exposed nodes through on transistors, mark all exposed ***/
void propagate_voltages(CIRCUIT *circuit, int dir, int d)
  {
  int i,progress,type;
  NODE *pnG,*pnS,*pnD,*pnSP,*pnDP,*pn;
  DEVICE *pdev;

  /*** set voltages for forced, mark forced nodes as exposed ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->exposed=1;
    if (pn->fixed) continue; // don't force GND/Vdd
    if      (pn->force == FORCE_UP)
      pn->V=trueV -(isMarkGate(pn)&&(dir==1)?leakV(pn->seed):0); // input leakage
    else if (pn->force == FORCE_DN)
      pn->V=falseV+(isMarkGate(pn)&&(dir==0)?leakV(pn->seed):0); // input leakage
    else if (pn->force == FORCE_MID)
      pn->V=midV;
    else pn->exposed=0;
    }

  /*** propagate through on gates ***/
  do
    {
    progress=0;
    for (i=0; i<circuit->Ndevices; i++)
      {
      pdev=&circuit->devices[i];
      if (!isTransistor(pdev)) continue;
      type=((BSIM4model *)pdev->sub.trans->model)->type;
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      pnG=pdev->sub.trans->nodes[2]->seed;
      if (!gate_on(type,pnG,1,0)) continue;
      progress += propagate_voltage(pnS,pnD,pnG,type,d);
      progress += propagate_voltage(pnD,pnS,pnG,type,d);
      }
    } while (progress);

  /*** propagate through S/D resistors to prime nodes ***/
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnS=pdev->sub.trans->nodes[0]->seed;
    pnD=pdev->sub.trans->nodes[1]->seed;
    pnSP=pdev->sub.trans->nodes[4]->seed;
    pnDP=pdev->sub.trans->nodes[5]->seed;
    pnSP->exposed=pnS->exposed; pnSP->V=pnS->V;
    pnDP->exposed=pnD->exposed; pnDP->V=pnD->V;
    }
  }

/*** mark exposed (propagate only to GLOBAL, SHARED, or INTERNAL) ***/
void mark_exposed(CIRCUIT *circuit, int initial, int final, int expose_globals)
  {
  int i,progress,type;
  NODE *pnG,*pnS,*pnD,*pnSP,*pnDP;
  DEVICE *pdev;
  do
    {
    progress=0;
    for (i=0; i<circuit->Ndevices; i++)
      {
      pdev=&circuit->devices[i];
      if (!isTransistor(pdev)) continue;
      type=((BSIM4model *)pdev->sub.trans->model)->type;
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      pnG=pdev->sub.trans->nodes[2]->seed;
      pnSP=pdev->sub.trans->nodes[4]->seed;
      pnDP=pdev->sub.trans->nodes[5]->seed;
      if (!isMarkGate(pnG)&&(pnG->mark!=MARK_GLOBAL)) continue;
      if (!gate_on(type,pnG,initial,final)) continue;
      if ((pnS->exposed)&&(!pnD->exposed)&&
          (((pnD->mark==MARK_GLOBAL)&&expose_globals)||
           (pnD->mark==MARK_NINTERNAL)||
           (pnD->mark==MARK_PINTERNAL)||
           (pnD->mark==MARK_SHARED_NINTERNAL)||
           (pnD->mark==MARK_SHARED_PINTERNAL)||
           (pnD->mark==MARK_SHARED)))
        {pnD->exposed=pnDP->exposed=1; progress++;}
      else if ((pnD->exposed)&&(!pnS->exposed)&&
               (((pnS->mark==MARK_GLOBAL)&&expose_globals)||
                (pnS->mark==MARK_NINTERNAL)||
                (pnS->mark==MARK_PINTERNAL)||
                (pnS->mark==MARK_SHARED_NINTERNAL)||
                (pnS->mark==MARK_SHARED_PINTERNAL)||
                (pnS->mark==MARK_SHARED)))
        {pnS->exposed=pnSP->exposed=1; progress++;}
      }
    } while (progress);
  set_subnets(circuit);
  }

/***
 * Count total width of transistors between nodes exposed to the
 * victim and nodes exposed to the opposite power supply.  Convert
 * width to an integer in nanometers to allow precise comparisons.
 ***/
int total_leak_width(CIRCUIT *circuit, int dir)
  {
  int i,type;
  NODE *pnS,*pnG,*pnD;
  DEVICE *pdev;
  int width=0;

  /*** mark nodes exposed to opposite power supply ***/
  for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
  if (dir==0) pnGND->exposed=1;
  if (dir==1) pnVdd->exposed=1;
  mark_exposed(circuit,1,0,0);
  for (i=0; i<circuit->Nnodes; i++)
    circuit->nodes[i].initial_exposed=circuit->nodes[i].exposed;

  /*** mark nodes exposed to Victim ***/
  for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
  Victim->exposed=1;
  mark_exposed(circuit,1,0,0);

  /*** add up transistor width between exposed and initial_exposed ***/
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnS=pdev->sub.trans->nodes[0]->seed;
    pnD=pdev->sub.trans->nodes[1]->seed;
    pnG=pdev->sub.trans->nodes[2]->seed;
    if (!isMarkGate(pnG)&&(pnG->mark!=MARK_GLOBAL)) continue;
    if ((pnS->exposed && !pnS->initial_exposed &&
         pnD->initial_exposed && !pnD->exposed) ||
        (pnD->exposed && !pnD->initial_exposed &&
         pnS->initial_exposed && !pnS->exposed))
      width += (int) (pdev->sub.trans->W*1e9 + 0.5); // convert to integer nm
    }
  return width;
  }

/*** make sure victim is driven appropriately during the scenario ***/
int check_driven_appropriately(CIRCUIT *circuit, int dir, int delay)
  {
  int i;

  if (!delay) // must not be driven to opposite rail before/during/after event
    {
    /*** mark initial/final states ***/
    for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
    Victim->exposed=1;
    mark_exposed(circuit,1,1,1);

    /*** check for driven cases ***/
    if      ((dir==0)&&(pnGND->exposed)) return 0;
    else if ((dir==1)&&(pnVdd->exposed)) return 0;
    return 1;
    }
  else // must not be driven opposite before, must be driven opposite after event
    {
    /*** mark initial ***/
    for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
    Victim->exposed=1;
    mark_exposed(circuit,1,0,1);

    /*** check for driven wrong way ***/
    if (pnGND->exposed&&pnVdd->exposed) return 0; // interference
    if ((dir==0)&&(pnGND->exposed)) return 0;
    if ((dir==1)&&(pnVdd->exposed)) return 0;

    /*** mark final ***/
    for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
    Victim->exposed=1;
    mark_exposed(circuit,0,1,1);

    /*** check for driven right way ***/
    if (pnGND->exposed&&pnVdd->exposed) return 0; // interference
    if ((dir==0)&&(pnGND->exposed)) return 1;
    if ((dir==1)&&(pnVdd->exposed)) return 1;
    return 0;
    }
  }

/*** recompute coupling cap of non-exposed nodes to exposed nodes ***/
double update_coupling_cap(CIRCUIT *circuit)
  {
  int i;
  DEVICE *pdev;
  NODE *pnS,*pnD;
  double victim_cap=0;
  for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].C=0;
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (!isCapacitor(pdev)) continue;
    pnS=pdev->sub.cap->nodes[0]->seed;
    pnD=pdev->sub.cap->nodes[1]->seed;
    if (pnS->exposed && !pnD->exposed) pnD->C+=pdev->sub.cap->C;
    if (pnD->exposed && !pnS->exposed) pnS->C+=pdev->sub.cap->C;
    if ((pnS==Victim) != (pnD==Victim)) victim_cap+=pdev->sub.cap->C;
    }
  return victim_cap;
  }

/*** mark fanin gates as relevant if they are close to exposed nodes ***/
void mark_relevant_gates(CIRCUIT *circuit, int markShared)
  {
  int i,agS,agD;
  NODE *pnG,*pnS,*pnD;
  DEVICE *pdev;
  double victim_cap,minC;

  /*** mark nodes exposed to Victim or Shared nodes in initial or final state ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pnG = &circuit->nodes[i];
    pnG->relevant = 0;
    pnG->exposed = (pnG->mark==MARK_VICTIM ||
                    (markShared && (pnG->mark==MARK_SHARED ||
                                    pnG->mark==MARK_SHARED_GATE)));
    }
  mark_exposed(circuit,1,1,0);
  victim_cap = update_coupling_cap(circuit);

  /*** mark gates as relevant if they are close to an exposed node ***/
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (isTransistor(pdev))
      {
      // a gate of a transistor is relevant if source or drain is exposed
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      pnG=pdev->sub.trans->nodes[2]->seed;
      if (pnS->exposed||pnD->exposed) pnG->relevant=1;
      }
    else if (isCapacitor(pdev))
      {
      // a gate aggressor is relevant if it has sizable capacitor to an exposed node
      pnS=pdev->sub.cap->nodes[0]->seed;
      pnD=pdev->sub.cap->nodes[1]->seed;
      agS = isMarkGate(pnS) && (pnS->aggressor || (pnS->force==FORCE_NONE));
      agD = isMarkGate(pnD) && (pnD->aggressor || (pnD->force==FORCE_NONE));
      minC = options.relevant_cap_ratio*victim_cap; // significant percent of victim cap
      agS = agS && (pnS->C > minC);
      agD = agD && (pnD->C > minC);
      if (agS && pnD->exposed) pnS->relevant=1;
      if (agD && pnS->exposed) pnD->relevant=1;
      }
    }
  }

/*** mark internal nodes as either good or bad for this scenario ***/
void mark_good_bad_nodes(CIRCUIT *circuit, int dir)
  {
  int i;
  NODE *pn;

  /*** mark nodes exposed to good voltages in initial state ***/
  for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
  Victim->exposed=1;
  if (dir==0) pnVdd->exposed=1;
  if (dir==1) pnGND->exposed=1;
  mark_exposed(circuit,1,0,0);
  for (i=0; i<circuit->Nnodes; i++)
    circuit->nodes[i].initial_exposed=circuit->nodes[i].exposed;

  /*** mark nodes exposed to VICTIM in final state ***/
  for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
  Victim->exposed=1;
  mark_exposed(circuit,0,1,0);
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->bad=pn->good=0;
    if (pn->seed!=pn) continue;
    pn->bad= !pn->initial_exposed && pn->exposed; // exposed bad-cap
    pn->good= pn->initial_exposed && pn->exposed; // exposed good-cap
    }
  }

/*** compare bad against oldbad and good against oldgood ***/
void compare_good_bad_nodes(CIRCUIT *circuit, int *worse, int *better)
  {
  int j;
  NODE *pn;
  *worse=*better=0;
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->seed!=pn) continue;
    if ( pn->oldbad  && !pn->bad)  *better=1; // less bad-cap  is better
    if (!pn->oldgood &&  pn->good) *better=1; // more good-cap is better
    if (!pn->oldbad  &&  pn->bad)  *worse=1;  // more bad-cap  is worse
    if ( pn->oldgood && !pn->good) *worse=1;  // less good-cap is worse
    }
  }

/*** is this node being forced to its canonical value? ***/
int is_force_canonical(NODE *pn)
  {
  if ((pn->force==FORCE_DN)&&(pn->canonical==0)) return 1;
  if ((pn->force==FORCE_UP)&&(pn->canonical==1)) return 1;
  return 0;
  }

/*** check if a node satisifies current custom scenario ***/
int node_satisfies_custom_scenario(NODE *pn)
  {
  return pn->custom_force==FORCE_NONE ||
    (pn->force==pn->custom_force && pn->aggressor==pn->custom_aggressor);
  }

/*** check if all nodes in a fanin group satisfy current custom scenario ***/
int fanin_group_satisfies_custom_scenario(EXCLUSIVE *group)
  {
  int i;
  for (i=0; i<group->Nnodes; i++)
    if (!node_satisfies_custom_scenario(group->nodes[i])) return 0;
  return 1;
  }

/*** try to find a monotonically worse scenario ***/
int find_worse_scenario(CIRCUIT *circuit, LIST *fanin, int dir, int delay, int slow)
  {
  int i,worse,better,on;
  NODE *pn;

  /*** skip this scenario if it contains any irrelevant aggressors or active rails ***/
  mark_relevant_gates(circuit,0);
  for (i=0; i<fanin->max; i++)
    {
    pn=fanin->p.pn[i];
    if (!delay && slow) continue; // leakage
    if (pn->relevant) continue;   // gate is relevant
    if (pn->custom_force!=FORCE_NONE) continue; // custom alint scenario
    if (pn->aggressor) return 1;  // irrelevant aggressor
    if (!is_force_canonical(pn))  // irrelevant non-canonical
      {
      pn->force=flip_force(pn->force);
      if (!exclusion_satisfied(circuit,delay)||
          !check_driven_appropriately(circuit,dir,delay)||
          !node_satisfies_custom_scenario(pn))
        {pn->force=flip_force(pn->force); continue;}
      pn->force=flip_force(pn->force);
      return 1;
      }
    }

  /*** filter delay scenarios appropriately ***/
  if (delay)
    {
    for (i=0; i<fanin->max; i++)
      {
      pn=fanin->p.pn[i];
      on = ((dir==0) && (pn->force==FORCE_UP)) ||
           ((dir==1) && (pn->force==FORCE_DN));
      if (pn->aggressor)
        {
        // skip scenario if any aggressor turns gates off
        if (on) return 1;
        }
      else if (pn->relevant && slow==on)
        {
        // try flipping relevant non-aggressors to turn more gates on/off
        pn->force=flip_force(pn->force);
        if (!exclusion_satisfied(circuit,delay)||
            !check_driven_appropriately(circuit,dir,delay)||
            !node_satisfies_custom_scenario(pn))
          {pn->force=flip_force(pn->force); continue;}
        pn->force=flip_force(pn->force);
        return 1;
        }
      }
    return 0;
    }

  /*** filter leakage scenarios appropriately ***/
  if (slow)
    {
    // get total width of leaking transistors, discard if none
    int lw, leak_width;
    leak_width = total_leak_width(circuit,dir);
    if (leak_width==0) return 1;

    // consider alternate scenarios
    for (i=0; i<fanin->max; i++)
      {
      pn=fanin->p.pn[i];

      // no aggressors allowed for leakage
      if (pn->aggressor) return 1;

      // try flipping the state of this non-aggressor fanin to increase leak_width
      pn->force=flip_force(pn->force);
      if (!exclusion_satisfied(circuit,delay)||
          !check_driven_appropriately(circuit,dir,delay)||
          !node_satisfies_custom_scenario(pn))
        {pn->force=flip_force(pn->force); continue;}
      lw = total_leak_width(circuit,dir);
      pn->force=flip_force(pn->force);

      // alternate scenario has more leakage width, so skip this one
      if (lw>leak_width) return 1;

      // alternate scenario has less leakage width, so continue
      if (lw<leak_width) continue;

      // tied alternate scenario's gate is irrelevant and more canonical
      if (!pn->relevant && !is_force_canonical(pn)) return 1;

      // tied alternate scenario's gate is relevant but turns more gates on
      if (pn->relevant && (((dir==0)&&(pn->force==FORCE_DN))||
                           ((dir==1)&&(pn->force==FORCE_UP)))) return 1;
      }
    return 0;
    }

  /*** mark oldbad and oldgood nodes ***/
  mark_good_bad_nodes(circuit,dir);
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->oldbad=pn->bad;
    pn->oldgood=pn->good;
    }

  /*** filter bump scenarios appropriately ***/
  for (i=0; i<fanin->max; i++)
    {
    pn=fanin->p.pn[i];
    if (!pn->aggressor)
      {
      // try flipping the state of this non-aggressor fanin
      pn->force=flip_force(pn->force);
      if (!exclusion_satisfied(circuit,delay)||
          !check_driven_appropriately(circuit,dir,delay)||
          !node_satisfies_custom_scenario(pn))
        {pn->force=flip_force(pn->force); continue;}
      mark_good_bad_nodes(circuit,dir);
      pn->force=flip_force(pn->force);
      compare_good_bad_nodes(circuit,&worse,&better);

      // alternate bump scenario might be better so can't discard this one
      if (better) continue;

      // alternate bump scenario is monotonically worse
      if (worse) return 1;

      // alternate scenario is similar but turns more gates on
      if (pn->relevant && (((dir==0)&&(pn->force==FORCE_DN))||
                           ((dir==1)&&(pn->force==FORCE_UP)))) return 1;
      }
    else if (((dir==0)&&(pn->force==FORCE_DN)) ||
             ((dir==1)&&(pn->force==FORCE_UP)))
      {
      // try making this "good" aggressor a non-aggressor
      pn->aggressor=0;
      if (!exclusion_satisfied(circuit,delay)||
          !check_driven_appropriately(circuit,dir,delay)||
          !node_satisfies_custom_scenario(pn))
        {pn->aggressor=1; continue;}
      mark_good_bad_nodes(circuit,dir);
      pn->aggressor=1;
      compare_good_bad_nodes(circuit,&worse,&better);

      // alternate scenario is not better and lacks this "good" aggressor
      if (!better) return 1;
      }
    }
  return 0;
  }

/**
 * find initial voltages without simulation
 * dir 0 is down, dir 1 is up.
 * slow 1 assumes slow voltages/aggressors (worst delay)
 * slow 0 assumes fast voltages/aggressors (worst noise)
 **/
void setup_initial(CIRCUIT *circuit, LIST *coupling, int dir, int delay, int slow)
  {
  int i,j,k,on,count,after_count,relevant;
  NODE *pn,*pnS;
  EXCLUSIVE *pex;

  /*** set force or initial voltage ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (pn->mark==MARK_UNKNOWN)
      pn->force = FORCE_MID;
    else if (pn->mark==MARK_LOAD)
      pn->force = FORCE_MID;
    else if ((pn->mark==MARK_COUPLING) && doCouplingCap && !pn->seed->nocc)
      pn->force = (dir==slow) ? FORCE_UP : FORCE_DN;
    else if ((pn->mark==MARK_COUPLING) && (!doCouplingCap || pn->seed->nocc))
      pn->force = FORCE_MID;
    else if (pn->mark==MARK_SHARED)
      pn->V = midV; // let float for now
    else if ((pn->mark==MARK_NINTERNAL)||(pn->mark==MARK_SHARED_NINTERNAL))
      pn->V = (dir==slow) ? falseV : trueV-options.VTN;
    else if ((pn->mark==MARK_PINTERNAL)||(pn->mark==MARK_SHARED_PINTERNAL))
      pn->V = (dir==slow) ? falseV+options.VTP : trueV;
    else if (pn->mark==MARK_VICTIM)
      assert(pn->force == dir ? FORCE_DN : FORCE_UP); // set earlier by enumerate_scenarios
    else if (pn->mark==MARK_INVERSE)
      pn->force = (dir==0) ? FORCE_DN : FORCE_UP;
    else if (pn->mark==MARK_SHARED_INVERSE)
      pn->V = midV; // set by propagate_voltages
    }

  /*** compute coupling capacitance to the final exposed seed nodes ***/
  for (i=0; i<circuit->Nnodes; i++) circuit->nodes[i].exposed=0;
  Victim->exposed=1;
  mark_exposed(circuit,0,1,0);
  update_coupling_cap(circuit);

  /*** propagate voltages from initial exposed nodes ***/
  propagate_voltages(circuit,dir,dir!=slow);

  /*** force SHARED nodes UP or DN ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (pn->seed!=pn) continue;
    if (pn->mark!=MARK_SHARED) continue;
    if      ((pn->exposed)&&(pn->V<midV)) pn->force = FORCE_DN; // driven
    else if ((pn->exposed)&&(pn->V>midV)) pn->force = FORCE_UP; // driven
    else pn->force = (dir==slow) ? FORCE_UP : FORCE_DN; // floating
    }

  /*** sort COUPLING/SHARED nodes by decreasing C ***/
  list_sort(coupling,&coupling_cmp);

  /*** disable COUPLING or flip_force SHARED if excllo or exclhi violated ***/
  for (i=coupling->max-1; i>=0; i--) // process smallest C to largest
    {
    pn=coupling->p.pn[i];
    for (j=0; j<circuit->Nexclusives; j++)
      {
      pex=&circuit->exclusives[j];
      count=after_count=relevant=0;
      for (k=0; k<pex->Nnodes; k++)
        {
        pnS=pex->nodes[k]->seed;
        on=exclusion_active(pnS,pex->type,delay,0);
        if (on && (pnS==pn)) relevant=1; // pn contributes to violation
        count+=on;
        on=exclusion_active(pnS,pex->type,delay,1);
        if (on && (pnS==pn)) relevant=1; // pn contributes to violation
        after_count+=on;
        }
      if (!relevant) continue; // only check exclusions involving active pn
      if (count<=1 && after_count<=1)  continue; // exclusion satisfied
      if (pn->mark==MARK_COUPLING) pn->force=FORCE_MID; // disable cap-coupling
      else if (!pn->exposed) pn->force=flip_force(pn->force); // flip shared
      }
    }

  /*** disable more COUPLING nodes if exclcc violated ***/
  for (i=coupling->max-1; i>=0; i--) // process smallest C to largest
    {
    pn=coupling->p.pn[i];
    if (pn->mark!=MARK_COUPLING) continue;
    for (j=0; j<circuit->Nexclusives; j++)
      {
      pex=&circuit->exclusives[j];
      count=after_count=relevant=0;
      for (k=0; k<pex->Nnodes; k++)
        {
        pnS=pex->nodes[k]->seed;
        on=cc_exclusion_active(pnS,pex->type,delay,0);
        if (on && (pnS==pn)) relevant=1; // pn contributes to violation
        count+=on;
        on=cc_exclusion_active(pnS,pex->type,delay,1);
        if (on && (pnS==pn)) relevant=1; // pn contributes to violation
        after_count+=on;
        }
      if (!relevant) continue; // only check exclusions involving active pn
      if (count<=1 && after_count<=1)  continue; // exclusion satisfied
      pn->force=FORCE_MID;     // disable cap-coupling
      }
    }

  /*** verify that exclusion is actually satisfied ***/
  for (i=0; i<circuit->Nexclusives; i++)
    {
    pex=&circuit->exclusives[i];
    count=after_count=0;
    for (j=0; j<pex->Nnodes; j++)
      {
      pnS=pex->nodes[j]->seed;
      count += cc_exclusion_active(pnS,pex->type,delay,0);
      after_count += cc_exclusion_active(pnS,pex->type,delay,1);
     }
    if (count<=1 && after_count<=1) continue;
    fprintf(dsim_out,"WARNING: exclusion violated by:\n");
    for (j=0; j<pex->Nnodes; j++)
      {
      pnS=pex->nodes[j]->seed;
      if (cc_exclusion_active(pnS,pex->type,delay,0) || cc_exclusion_active(pnS,pex->type,delay,1))
        fprintf(dsim_out,"  %s %s:%s\n",
                mark_name[pnS->mark],
                get_node_name(pnS),
                (pnS->aggressor || (delay&&(pnS->mark==MARK_VICTIM)) ||
                 ((pnS->mark==MARK_COUPLING)&&(pnS->force!=FORCE_MID))) ?
                (pnS->force==FORCE_DN ? "+":(pnS->force==FORCE_UP ? "-" : "")) :
                (pnS->force==FORCE_DN ? "0":(pnS->force==FORCE_UP ? "1" : "U")));
      }
    }

  /*** cap-coupling debugging ***/
  if (debug_alint) for (i=0; i<coupling->max; i++)
    {
    pn=coupling->p.pn[i];
    fprintf(dsim_out,"    %s%s\t%g\n",
            get_node_name(pn),
            pn->force==FORCE_DN ? "+":(pn->force==FORCE_UP ? "-" : ""),
            pn->C*1e15);
    }

  /*** propagate voltages again to set SHARED_INVERSE nodes ***/
  propagate_voltages(circuit,dir,dir!=slow);

  /*** force SHARED_INVERSE nodes UP or DN ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (pn->seed!=pn) continue;
    if (pn->mark!=MARK_SHARED_INVERSE) continue;
    if      ((pn->exposed)&&(pn->V<midV)) pn->force = FORCE_DN; // driven
    else if ((pn->exposed)&&(pn->V>midV)) pn->force = FORCE_UP; // driven
    else assert(0); // MARK_SHARED_INVERSE node shouldn't be floating!
    }

  /*** final propagate voltages to set all internal nodes ***/
  propagate_voltages(circuit,dir,dir!=slow);
  }

/**
 * setup the event
 * let victim/shared/inverse/internals float
 * flip coupling aggressors and fanins
 * handle special voltage offsets for threshold and leakage analysis
 **/
void setup_event(CIRCUIT *circuit, int dir, int delay, int slow)
  {
  int i,slow_inverse;
  NODE *pn;

  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    slow_inverse = (options.delay_slow_inverse!=0) && delay && slow && pn->top_port;
    pn->old_force=pn->force;
    if (pn->seed!=pn) continue;
    if      (pn->mark==MARK_COUPLING) pn->force = flip_force(pn->force);
    else if (pn->mark==MARK_VICTIM) pn->force = FORCE_NONE;
    else if (pn->mark==MARK_SHARED) pn->force = FORCE_NONE;
    else if (pn->mark==MARK_INVERSE && !slow_inverse) pn->force = FORCE_NONE;
    else if (pn->mark==MARK_SHARED_INVERSE) pn->force = FORCE_NONE;
    else if (isMarkGate(pn) && pn->aggressor)
      {
      pn->force = flip_force(pn->force);
      // for noise threshold scenarios, allow partial flipping
      if      (pn->force==FORCE_UP)
        { pn->force=FORCE_V; pn->forceV=falseV + (trueV-falseV)*prs_pct/100; }
      else if (pn->force==FORCE_DN)
        { pn->force=FORCE_V; pn->forceV=trueV  - (trueV-falseV)*prs_pct/100; }
      }
    else if (isMarkGate(pn) && options.leakage_enabled)
      {
      // for leakage scenarios, off gates leak away from rails
      if      (pn->force==FORCE_UP && dir==1)
        { pn->force=FORCE_V; pn->forceV=trueV  - leakV(pn); }
      else if (pn->force==FORCE_DN && dir==0)
        { pn->force=FORCE_V; pn->forceV=falseV + leakV(pn); }
      }
    }
  set_subnets(circuit);
  }

/*** flip aggressors back, undo setup_event ***/
void finish_event(CIRCUIT *circuit, int dir)
  {
  int i;
  NODE *pn;
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (isMarkGate(pn))             pn->force=pn->old_force; // unflip aggressors
    else if (pn->mark!=MARK_GLOBAL) pn->force=FORCE_NONE;    // unforce most nodes
    }
  Victim->force = dir ? FORCE_DN : FORCE_UP;
  }

/*** print out the current scenario ***/
void print_scenario(CIRCUIT *circuit, LIST *fanin, char *sim)
  {
  int i;
  NODE *pn;
  fprintf(dsim_out,"  %s %6g",sim,1e9*circuit->time);
  for (i=0; i<fanin->max; i++)
    {
    pn=fanin->p.pn[i];
    fprintf(dsim_out," %s",get_node_name(pn));
    if (pn->aggressor)
      {
      if      (pn->force==FORCE_DN) fprintf(dsim_out,":+");
      else if (pn->force==FORCE_UP) fprintf(dsim_out,":-");
      }
    else
      {
      if      (pn->force==FORCE_DN) fprintf(dsim_out,":0");
      else if (pn->force==FORCE_UP) fprintf(dsim_out,":1");
      }
    }
  fprintf(dsim_out,"\n");
  }

/** Return a scenario as sorted list of FANIN **/
LIST *scenario_to_list(LIST *fanin)
  {
  int i;
  NODE *pn;
  LIST *list;
  FANIN f;
  list = list_create(sizeof(FANIN));
  for (i=0; i<fanin->max; i++)
    {
    pn=fanin->p.pn[i];
    f.pn=pn;
    f.aggressor=pn->aggressor;
    f.initial=pn->force==FORCE_UP;
    list_append_element(list,&f);
    }
  return list;
  }

/*** get minimum smoothV across a resistive subnet ***/
double minV(CIRCUIT *circuit, NODE *pn) {
  int i;
  double V = pn->smoothV;
  for (i=0; i<circuit->Nnodes; i++)
    if (circuit->nodes[i].seed == pn->seed)
      if (circuit->nodes[i].smoothV < V)
        V = circuit->nodes[i].smoothV;
  return V;
}

/*** get maximum smoothV across a resistive subnet ***/
double maxV(CIRCUIT *circuit, NODE *pn) {
  int i;
  double V = pn->smoothV;
  for (i=0; i<circuit->Nnodes; i++)
    if (circuit->nodes[i].seed == pn->seed)
      if (circuit->nodes[i].smoothV > V)
        V = circuit->nodes[i].smoothV;
  return V;
}

/*** get minimum smoothX across a resistive subnet ***/
double minX(CIRCUIT *circuit, NODE *pn) {
  int i;
  double X = pn->smoothX;
  for (i=0; i<circuit->Nnodes; i++)
    if (circuit->nodes[i].seed == pn->seed)
      if (circuit->nodes[i].smoothX < X)
        X = circuit->nodes[i].smoothX;
  return X;
}

/*** get maximum smoothX across a resistive subnet ***/
double maxX(CIRCUIT *circuit, NODE *pn) {
  int i;
  double X = pn->smoothX;
  for (i=0; i<circuit->Nnodes; i++)
    if (circuit->nodes[i].seed == pn->seed)
      if (circuit->nodes[i].smoothX > X)
        X = circuit->nodes[i].smoothX;
  return X;
}

/*** simulate a single charge sharing scenario ***/
void simulate_scenario(CIRCUIT *circuit, LIST *coupling, int dir, int delay, int slow)
  {
  double interval,marginX,marginV,min_time,start_time,timemax;
  int i,lo,hi,bump;
  bump = (delay==0) && (slow==0);

  /*** settle initial state ***/
  setup_initial(circuit,coupling,dir,delay,slow);

  /*** setup the event (flip aggressors) ***/
  setup_event(circuit,dir,delay,slow);

  /*** simulate event ***/
  timemax = delay==2 ? options.thresh_scenario_timemax : options.scenario_timemax;
  if (!CountScenarios)
    {
    interval = delay ? options.delay_event_interval : options.bump_event_interval;
    marginX = options.stable_dVdt; // dVdt stability test
    marginV = options.alint_voltage_margin * (trueV-falseV);
    min_time = circuit->time
      // wait for aggressors to switch 95%
      + options.prs_tau * (options.d2a_shape!=0 ? 0.95 : log(20))
      // and for smoothing to decay 50%
      + options.a2d_ewma_tau * log(2);
    options.timemax = start_time = circuit->time;
    for (i=0; options.timemax-start_time<timemax; i++)
      {
      // transient simulation
      options.timemax = start_time + i * interval; // simulate next interval
      simulate_circuit_inner(circuit);

      // check for termination of simulation
      lo = maxV(circuit,Victim) < falseV + marginV; // near falseV
      hi = minV(circuit,Victim) > trueV  - marginV; // near trueV
      if (circuit->time<min_time) continue;  // simulate at least this long

      // all scenarios terminate if signal approaches opposite power rail
      if ((dir==1) && hi) break;
      if ((dir==0) && lo) break;

      // bump/leakage/thresh
      if (delay!=1)
        {
        // terminate if voltage stabilizes or reverses direction
        // bumps must also return to near the starting rail
        if ((dir==1) && ( maxX(circuit,Victim) < marginX) && (lo | ~bump)) break;
        if ((dir==0) && (-minX(circuit,Victim) < marginX) && (hi | ~bump)) break;
        // or be outside of true/false range entirely (i.e. safe coupling)
        if ((minV(circuit,Victim)>trueV) || (maxV(circuit,Victim)<falseV)) break;
        }
      }
    circuit->time = options.timemax; // make time range accurate
    if (options.timemax-start_time>=timemax && delay!=2)
      fprintf(dsim_out,
              "WARNING: simulation hasn't finished after %g seconds.\n",
              options.scenario_timemax);
    }

  /*** restore search state ***/
  finish_event(circuit,dir);
  }

/*** count the ones in an integer ***/
int count_ones(int x)
  {
  int i,n=0;
  for (i=0; i<8*sizeof(int); i++) n+=(x>>i)&1;
  return n;
  }

/*** do the scenario after fanin force and aggressor has been set ***/
int do_scenario(CIRCUIT *circuit, int dir, int delay, int slow, int CustomForce,
                LIST *fanin, LIST *coupling, LIST *visited, char *sim)
  {
  // initial value of Victim, needed early for exclusion checking
  assert(Victim->force == dir ? FORCE_DN : FORCE_UP);
  if (exclusion_satisfied(circuit,delay))
    {
    if (CustomForce || check_driven_appropriately(circuit,dir,delay))
      {
      if (!find_worse_scenario(circuit,fanin,dir,delay,slow))
        {
        int ok=1;
        if (visited!=NULL)
          {
          LIST *s = scenario_to_list(fanin);
          ok = find_element_lazy_sort(visited,&s,&scenario_cmp)<0;
          if (ok) list_insert_element_lazy_sort(visited,&s,&scenario_cmp);
          else list_free(s);
          }
        if (ok)
          {
          print_scenario(circuit,fanin,sim);
          simulate_scenario(circuit,coupling,dir,delay,slow);
          return 1;
          }
        }
      else if (debug_alint)
        {
        fprintf(dsim_out,"FOUND_WORSE: ");
        print_scenario(circuit,fanin,sim);
        }
      }
    else if (debug_alint)
      {
      fprintf(dsim_out,"NOT_DRIVEN_RIGHT: ");
      print_scenario(circuit,fanin,sim);
      }
    }
  else if (debug_alint)
    {
    fprintf(dsim_out,"EXCLUSION_VIOLATED: ");
    print_scenario(circuit,fanin,sim);
    }
  
  return 0;
  }

/*** recursively enumerate scenarios, one group at a time ***/
int recurse_groups(CIRCUIT *circuit, int dir, int delay, int slow,
                   int max_fanin_aggressors, int CustomForce,
                   LIST *fanin, LIST *fanin_groups,
                   LIST *coupling, LIST *visited, char *sim,
                   int first_grp)
  {
  int leak,i,scenarios=0,all_custom;
  int group_relevant=0;
  int neutral_lw=0,max_lw=0,lw=0;
  EXCLUSIVE *group;
  NODE *pn;
  leak = !delay && slow;

  /*** debugging ***/
  if (debug_alint)
    {
    fprintf(dsim_out,"RECURSING %d: ",first_grp);
    print_scenario(circuit,fanin,sim);
    }

  /*** terminate recursion ***/
  if (max_fanin_aggressors<0) return 0;  // too many aggressors
  else if (first_grp>=fanin_groups->max) // no more groups remain
    return do_scenario(circuit,dir,delay,slow,CustomForce,fanin,coupling,visited,sim);

  /*** get current group ***/
  group=fanin_groups->p.pex[first_grp];

  /*** identify relevant gates given scenario so far ***/
  mark_relevant_gates(circuit,1);
  for (i=0; i<group->Nnodes; i++)
    group_relevant = group_relevant || group->nodes[i]->relevant;

#if 0
  /*** if all nodes in this group are custom_force, recurse precisely as specified ***/
  all_custom=1;
  for (i=0; i<group->Nnodes; i++)
    {
    pn = group->nodes[i];
    if (pn->custom_force==FORCE_NONE) all_custom=0;
    }
  if (all_custom)
    {
    for (i=0; i<group->Nnodes; i++)
      {
      pn = group->nodes[i];
      pn->force = pn->custom_force;
      pn->aggressor = pn->custom_aggressor;
      }
    scenarios+=recurse_groups(circuit,dir,delay,slow,max_fanin_aggressors,
                              CustomForce,fanin,fanin_groups,coupling,visited,
                              sim,first_grp+1);
    for (i=0; i<group->Nnodes; i++)
      {
      pn = group->nodes[i];
      pn->force = FORCE_NONE;
      pn->aggressor = 0;
      }
    mark_relevant_gates(circuit,1);
    return scenarios;
    }
#endif

  /*** set all rails of this group to neutral, non-aggressor ***/
  for (i=0; i<group->Nnodes; i++)
    {
    group->nodes[i]->force = (group->type==1 ? FORCE_DN : FORCE_UP);
    group->nodes[i]->aggressor = 0;
    }

  /*** maximum total_leak_width of irrelevant groups to prune leakage scenarios ***/
  if (leak && !group_relevant)
    {
    if (fanin_group_satisfies_custom_scenario(group))
      max_lw=neutral_lw=total_leak_width(circuit,dir);
    for (i=0; i<group->Nnodes; i++)
      {
      pn=group->nodes[i];
      pn->force=flip_force(pn->force);
      if (fanin_group_satisfies_custom_scenario(group))
        {
        lw=total_leak_width(circuit,dir);
        if (lw>max_lw) max_lw=lw;
        }
      pn->force=flip_force(pn->force);
      }
    }

  /*** recurse with neutral group and no aggressors ***/
  if ((!leak || group_relevant || neutral_lw==max_lw) &&
      fanin_group_satisfies_custom_scenario(group))
    scenarios+=recurse_groups(circuit,dir,delay,slow,max_fanin_aggressors,
                              CustomForce,fanin,fanin_groups,coupling,visited,
                              sim,first_grp+1);

  /*** try all options for each rail of this group ***/
  for (i=0; i<group->Nnodes; i++)
    {
    pn = group->nodes[i];

    /*** prune irrelevant gates from bump/delay scenario enumeration ***/
    if (!leak && !pn->relevant && pn->custom_force==FORCE_NONE) continue;

    /*** non-neutral, non-aggressor ***/
    pn->force=flip_force(pn->force);
    if (leak && !group_relevant) lw=total_leak_width(circuit,dir);
    if ((!leak || group_relevant || (lw==max_lw && lw>neutral_lw)) &&
        fanin_group_satisfies_custom_scenario(group))
      scenarios+=recurse_groups(circuit,dir,delay,slow,max_fanin_aggressors,
                                CustomForce,fanin,fanin_groups,coupling,visited,
                                sim,first_grp+1);
    pn->force=flip_force(pn->force);

    /*** no aggressors allowed for leakage ***/
    if (leak) continue;

    /*** neutral, aggressor ***/
    pn->aggressor=1;
    if (fanin_group_satisfies_custom_scenario(group))
      scenarios+=recurse_groups(circuit,dir,delay,slow,
                                max_fanin_aggressors-(pn->custom_force==FORCE_NONE),
                                CustomForce,fanin,fanin_groups,coupling,visited,
                                sim,first_grp+1);
    pn->aggressor=0;

    /*** non-neutral, aggressor ***/
    pn->force=flip_force(pn->force); pn->aggressor=1;
    if (fanin_group_satisfies_custom_scenario(group))
      scenarios+=recurse_groups(circuit,dir,delay,slow,
                                max_fanin_aggressors-(pn->custom_force==FORCE_NONE),
                                CustomForce,fanin,fanin_groups,coupling,visited,
                                sim,first_grp+1);
    pn->force=flip_force(pn->force); pn->aggressor=0;
    }

  /*** restore all fanins to FORCE_NONE, restore relevant markings ***/
  for (i=0; i<group->Nnodes; i++) group->nodes[i]->force = FORCE_NONE;
  mark_relevant_gates(circuit,1);

  /*** return ***/
  return scenarios;
  }

/*** is a custom alint scenario valid for this simulation type and direction ***/
int valid_custom_scenario(SCENARIO scenario, int dir, int delay, int leak)
  {
  // check custom scenario victim direction
  if ((scenario.dir==0 && dir!=0) ||
      (scenario.dir==1 && dir!=1)) return 0;

  // check custom scenario simulation type
  if ((scenario.sim==SIM_DELAY && (leak || !delay)) ||
      (scenario.sim==SIM_BUMP  && (leak ||  delay)) ||
      (scenario.sim==SIM_LEAK  && !leak)) return 0;

  return 1;
  }

/*** Enumerate all or custom scenarios ***/
int enumerate_scenarios(CIRCUIT *circuit,
                        int dir, int delay, int slow, int leak,
                        int max_fanin_aggressors,
                        LIST *fanin, LIST *fanin_groups,
                        LIST *coupling, char *sim,
                        LIST *CustomScenarios, int CustomForce)
  {
  int i,j;
  NODE *pn;
  FANIN nf;
  SCENARIO scenario;
  LIST *visited=NULL;
  int scenarios=0;

  /** Force Victim for earlier exclusion pruning **/
  Victim->force = dir ? FORCE_DN : FORCE_UP;

  /** Enumerate all scenarios if no custom scenarios speficied **/
  if (CustomScenarios->max==0)
    return recurse_groups(circuit,dir,delay,slow,max_fanin_aggressors,0,
                          fanin,fanin_groups,coupling,NULL,sim,0);

  /** Handle each custom scenario **/
  visited = list_create(sizeof(LIST *));
  for (i=0; i<CustomScenarios->max; i++)
    {
    scenario=CustomScenarios->p.scenario[i];
    if (!valid_custom_scenario(scenario,dir,delay,leak)) continue;

    // all fanins default to non-aggressor, non-forced
    for (j=0; j<fanin->max; j++)
      {
      pn=fanin->p.pn[j];
      pn->aggressor=pn->custom_aggressor=0;
      pn->force=pn->custom_force=FORCE_NONE;
      }

    // set custom_force and custom_aggressor fields of fanin
    for (j=0; j<scenario.fanin->max; j++)
      {
      nf = scenario.fanin->p.fanin[j];
      pn = nf.pn->map;
      if ((pn!=NULL) && isMarkGate(pn))
        {
        if (nf.aggressor) pn->custom_aggressor=1;
        if      (nf.initial==0) pn->custom_force=FORCE_DN;
        else if (nf.initial==1) pn->custom_force=FORCE_UP;
        }
      else fprintf(dsim_err,"ERROR: Scenario for %s specified non-fanin %s\n",
                   get_node_name(Victim),get_node_name(nf.pn));
      }

    // recursively enumerate consistent with given scenario
    scenarios+=recurse_groups(circuit,dir,delay,slow,max_fanin_aggressors,CustomForce,
                              fanin,fanin_groups,coupling,visited,sim,0);

    // clear custom_force and custom_aggressor fields of fanin
    for (j=0; j<fanin->max; j++)
      {
      pn=fanin->p.pn[j];
      assert(pn->aggressor==0);
      assert(pn->force==FORCE_NONE);
      pn->custom_aggressor=0;
      pn->custom_force=FORCE_NONE;
      }
    }

  /** free visited and return **/
  for (i=0; i<visited->max; i++) list_free(visited->p.list[i]);
  list_free(visited);
  return scenarios;
  }

/*** propagate marking from source to drain ***/
/*** uses portG || portN && portP to distinguish shared nodes ***/
int mark_source_drain(NODE *pnS, NODE *pnD, int type)
  {
  int external = pnD->seed->portG || (pnD->seed->portN && pnD->seed->portP);
  if ((pnS->mark==MARK_VICTIM)||
      (pnS->mark==MARK_NINTERNAL)||
      (pnS->mark==MARK_PINTERNAL))
    {
    if ((pnD->mark==MARK_UNKNOWN)||
        (pnD->mark==MARK_SHARED_NINTERNAL)||
        (pnD->mark==MARK_SHARED_PINTERNAL))
      {
      if (external) pnD->mark= MARK_SHARED;
      else pnD->mark= (type==PTYPE) ? MARK_PINTERNAL : MARK_NINTERNAL;
      return 1;
      }
    }
  else if ((pnS->mark==MARK_SHARED)||
	   (pnS->mark==MARK_SHARED_NINTERNAL)||
	   (pnS->mark==MARK_SHARED_PINTERNAL))
    {
    if (pnD->mark==MARK_UNKNOWN)
      {
      if (external) pnD->mark= MARK_SHARED;
      else pnD->mark= (type==PTYPE) ? MARK_SHARED_PINTERNAL : MARK_SHARED_NINTERNAL;
      return 1;
      }
    }
  return 0;
  }

/*** mark gate of a transistor if it gates the network of the victim ***/
void mark_gate(NODE *pnG, NODE *pn)
  {
  if ((pn->mark==MARK_VICTIM)||
      (pn->mark==MARK_NINTERNAL)||
      (pn->mark==MARK_PINTERNAL)||
      (pn->mark==MARK_SHARED)||
      (pn->mark==MARK_SHARED_NINTERNAL)||
      (pn->mark==MARK_SHARED_PINTERNAL)||
      (pn->mark==MARK_SHARED_GATE))
    {
    if      (pnG->mark==MARK_UNKNOWN) pnG->mark=MARK_GATE;
    else if (pnG->mark==MARK_SHARED)  pnG->mark=MARK_SHARED_GATE;
    }
  }

/*** mark capacitive coupling nodes ***/
void mark_capacitor(NODE *pnS, NODE *pnD, double C)
  {
  if ((pnS->mark==MARK_VICTIM)||
      (pnS->mark==MARK_NINTERNAL)||
      (pnS->mark==MARK_PINTERNAL))
    {
    if (pnD->mark==MARK_UNKNOWN) pnD->mark = MARK_COUPLING;
    if ((pnD->mark!=MARK_VICTIM) &&
        (pnD->mark!=MARK_NINTERNAL) &&
        (pnD->mark!=MARK_PINTERNAL))
      pnD->C+=C; // needed to sort coupling to victim
    }
  else if ((pnS->mark==MARK_SHARED)||
           (pnS->mark==MARK_SHARED_NINTERNAL)||
           (pnS->mark==MARK_SHARED_PINTERNAL)||
           (pnS->mark==MARK_INVERSE)||
           (pnS->mark==MARK_SHARED_INVERSE))
    {
    if (pnD->mark==MARK_UNKNOWN) pnD->mark = MARK_COUPLING;
    }
  }

/*** check if a node is always forced ***/
int always_forced(NODE *pn)
  {
  return (pn->mark==MARK_LOAD) ||
    (pn->mark==MARK_COUPLING) ||
    isMarkGate(pn) ||
    (pn->mark==MARK_GLOBAL) ||
    (pn->mark==MARK_BARRIER);
  }

/*** check if all ports are relevant nodes ***/
int device_relevant(DEVICE *pdev)
  {
  int i,Nnodes,relevant=0;
  NODE **nodes;
  Nnodes=device_Nnodes(pdev);
  nodes=device_nodes(pdev);
  for (i=0; i<Nnodes; i++)
    {
    // if any port is UNKNOWN, device is irrelevant
    if (nodes[i]->mark==MARK_UNKNOWN) return 0;

    // internal transistor ports can't make the device relevant
    if (nodes[i]->mark==MARK_PRIME) continue;

    // voltage/current sources are relevant if their nodes aren't UNKNOWN
    if (isSource(pdev)) relevant=1;

    // if any port will ever be FORCE_NONE, device is relevant
    if (!always_forced(nodes[i])) relevant=1;
    }

  // mark relevant port nodes
  if (relevant) for (i=0; i<Nnodes; i++)
    {
    nodes[i]->relevant=1;
    nodes[i]->seed->relevant=1; // need seed node, because we collapse subnets
    }
  return relevant;
  }

/*** check if device is named and some ports are Victim/Internal ***/
int device_named(DEVICE *pdev)
  {
  int i,Nnodes;
  NODE **nodes;
  Nnodes=device_Nnodes(pdev);
  nodes=device_nodes(pdev);
  if (pdev->name==NULL) return 0; // unnamed anyways
  if (isSource(pdev)) return 1;
  for (i=0; i<Nnodes; i++)
    if ((nodes[i]->mark==MARK_VICTIM) ||
        (nodes[i]->mark==MARK_NINTERNAL) ||
        (nodes[i]->mark==MARK_PINTERNAL)) return 1;
  return 0;
  }

/*** check if any ports are unique relevant nodes ***/
int exclusive_relevant(EXCLUSIVE *pex)
  {
  int i,j=0,last=-1;
  for (i=0; i<pex->Nnodes; i++)
    if (pex->nodes[i]->relevant)
      {
      j+=((last>=0) && (pex->nodes[i]!=pex->nodes[last]));
      last=i;
      }
  return j>0;
  }

/*** create a reduced subcircuit ***/
CIRCUIT create_subcircuit(CIRCUIT *circuit)
  {
  CIRCUIT subcircuit;
  int i,j,s,dut;
  NODE **nodes1,**nodes2,*pn1,*pn2;
  DEVICE *pdev1,*pdev2;
  EXCLUSIVE *pex1,*pex2;
  int Nnodes,watch;

  /*** allocate circuit ***/
  subcircuit=create_circuit();
  subcircuit.run=NULL;
  subcircuit.names=circuit->names;

  /*** count relevant devices, mark relevant nodes ***/
  for (i=0; i<circuit->Ndevices; i++)
    if (device_relevant(&circuit->devices[i])) subcircuit.Ndevices++;

  /*** create relevant nodes ***/
  for (i=0; i<circuit->Nnodes; i++)
    if (circuit->nodes[i].relevant) subcircuit.Nnodes++;
  subcircuit.nodes=(NODE *) leak_malloc(subcircuit.Nnodes*sizeof(NODE));
  subcircuit.Nnodes=0;
  for (s=1; s>=0; s--) // first do seeds nodes, then non-seed nodes
    for (i=0; i<circuit->Nnodes; i++)
      if (circuit->nodes[i].relevant)
        {
        pn1=&circuit->nodes[i];
        if ((pn1->seed==pn1) != s) continue; // only consider [non]seed nodes
        if ((s==0) && always_forced(pn1))
          {
          pn1->map=pn1->seed->map; // collapse non-seed always-forced nodes to seed
          continue;
          }
        pn2=&subcircuit.nodes[subcircuit.Nnodes++];
        watch = traceAllNodes ||
          ((pn1->watch || root_alias(pn1->name)->active_used || (s==1)) &&
           ((pn1->mark==MARK_VICTIM) || (pn1->mark==MARK_INVERSE) ||
            (pn1->mark==MARK_GATE) || (pn1->mark==MARK_SHARED_GATE) ||
            (pn1->mark==MARK_SHARED)));
        create_node(pn2,NULL,watch,pn1->fixed,pn1->top_port,pn1->name);
        pn2->V=pn1->V;
        pn2->C=pn1->C;
        pn2->CG=pn1->CG;
        pn2->portG=pn1->portG;
        pn2->portN=pn1->portN;
        pn2->portP=pn1->portP;
        pn2->leaky=pn1->leaky;
        pn2->nocc=pn1->nocc;
        pn2->floatingC=pn1->floatingC;
        pn2->mark=pn1->mark;
        pn2->force=pn1->force;
        if (isMarkGate(pn2) ||
            (pn2->mark==MARK_COUPLING) ||
            (pn2->mark==MARK_LOAD))
          pn2->force=FORCE_MID; // hint to partitioning
        pn2->relevant=1;
        pn2->canonical=pn1->canonical;
        pn1->map=pn2; // use to translate nodes
        }

  /*** create relevant devices ***/
  subcircuit.devices=(DEVICE *) leak_malloc(subcircuit.Ndevices*sizeof(DEVICE));
  subcircuit.Ndevices=0;
  for (i=0; i<circuit->Ndevices; i++)
    if (device_relevant(&circuit->devices[i]))
      {
      pdev1=&circuit->devices[i];
      pdev2=&subcircuit.devices[subcircuit.Ndevices++];
      pdev2->device_func=pdev1->device_func;
      pdev2->name=NULL;
      Nnodes=device_Nnodes(pdev1);
      nodes1=device_nodes(pdev1);
      dut=1;
      if (device_named(pdev1)) pdev2->name=leak_strdup(pdev1->name); // keep name
      if (isTransistor(pdev1))
        {
        pdev2->sub.trans=(TRANSISTOR *) leak_malloc(sizeof(TRANSISTOR));
        *pdev2->sub.trans=*pdev1->sub.trans; // clone most fields
        for (j=0; j<Nnodes*Nnodes; j++) pdev2->sub.trans->Aentry[j]=NULL;
        dut = isMarkDUT(pdev1->sub.trans->nodes[0]->seed) ||
              isMarkDUT(pdev1->sub.trans->nodes[1]->seed);
        }
      else if (isDiode(pdev1))
        {
        pdev2->sub.diode=(DIODE *) leak_malloc(sizeof(DIODE));
        *pdev2->sub.diode=*pdev1->sub.diode; // clone most fields
        for (j=0; j<Nnodes*Nnodes; j++) pdev2->sub.diode->Aentry[j]=NULL;
        dut = isMarkDUT(pdev1->sub.diode->nodes[0]->seed) ||
              isMarkDUT(pdev1->sub.diode->nodes[1]->seed);
        }
      else if (isCapacitor(pdev1))
        {
        pdev2->sub.cap=(CAPACITOR *) leak_malloc(sizeof(CAPACITOR));
        *pdev2->sub.cap=*pdev1->sub.cap; // clone most fields
        for (j=0; j<Nnodes*Nnodes; j++) pdev2->sub.cap->Aentry[j]=NULL;
        dut = isMarkDUT(pdev1->sub.cap->nodes[0]->seed) ||
              isMarkDUT(pdev1->sub.cap->nodes[1]->seed);
        }
      else if (isResistor(pdev1))
        {
        pdev2->sub.res=(RESISTOR *) leak_malloc(sizeof(RESISTOR));
        *pdev2->sub.res=*pdev1->sub.res; // clone most fields
        for (j=0; j<Nnodes*Nnodes; j++) pdev2->sub.res->Aentry[j]=NULL;
        dut = isMarkDUT(pdev1->sub.res->nodes[0]->seed) ||
              isMarkDUT(pdev1->sub.res->nodes[1]->seed);
        }
      else if (isSource(pdev1))
        {
        int Nops = pdev1->sub.source->Nops;
        pdev2->sub.source=(SOURCE *) leak_malloc(sizeof(SOURCE));
        *pdev2->sub.source=*pdev1->sub.source; // clone most fields
        pdev2->sub.source->nodes=(NODE **) leak_malloc(Nnodes*sizeof(NODE *));
        pdev2->sub.source->ops=(OP *) leak_malloc(Nops*sizeof(OP));
        pdev2->sub.source->Aentry=(REAL **) leak_malloc(Nnodes*Nnodes*sizeof(REAL *));
        for (j=0; j<Nops; j++) pdev2->sub.source->ops[j]=pdev1->sub.source->ops[j];
        for (j=0; j<Nnodes*Nnodes; j++) pdev2->sub.source->Aentry[j]=NULL;
        }
      nodes2=device_nodes(pdev2);
      for (j=0; j<Nnodes; j++)
        {
        if      (!dut && nodes1[j]==pnGND) nodes2[j]=pnEnvGND->map;
        else if (!dut && nodes1[j]==pnVdd) nodes2[j]=pnEnvVdd->map;
        else                               nodes2[j]=nodes1[j]->map;
        }
      }

  /*** create relevant exclusives ***/
  for (i=0; i<circuit->Nexclusives; i++)
    if (exclusive_relevant(&circuit->exclusives[i])) subcircuit.Nexclusives++;
  subcircuit.exclusives=(EXCLUSIVE *)
    leak_malloc(subcircuit.Nexclusives*sizeof(EXCLUSIVE));
  subcircuit.Nexclusives=0;
  for (i=0; i<circuit->Nexclusives; i++)
    if (exclusive_relevant(&circuit->exclusives[i]))
      {
      pex1=&circuit->exclusives[i];
      pex2=&subcircuit.exclusives[subcircuit.Nexclusives++];
      *pex2=*pex1; // clone most fields
      pex2->nodes=(NODE **) leak_malloc(pex1->Nnodes*sizeof(NODE *));
      pex2->Nnodes=0;
      for (j=0; j<pex1->Nnodes; j++)
        if (pex1->nodes[j]->relevant)
          pn1=pex2->nodes[pex2->Nnodes++]=pex1->nodes[j]->map; // translate
      }

  /*** now partition it ***/
  prepare_circuit(&subcircuit);
  partition_circuit(&subcircuit);
  summarize_circuit(&subcircuit,dsim_out);
  return subcircuit;
  }

/*** free the reduced subcircuit ***/
void free_subcircuit(CIRCUIT *circuit)
  {
  int i;
  for (i=0; i<circuit->Nnodes; i++)      free_node(&circuit->nodes[i]);
  for (i=0; i<circuit->Ndevices; i++)    free_device(&circuit->devices[i]);
  for (i=0; i<circuit->Nexclusives; i++) free_exclusive(&circuit->exclusives[i]);
  for (i=0; i<circuit->Nblocks; i++)     free_block(&circuit->blocks[i]);
  leak_free(circuit->nodes);
  leak_free(circuit->devices);
  leak_free(circuit->exclusives);
  leak_free(circuit->blocks);
  }

/** recursively propagate markings through transistor S/D connections ***/
void mark_transistor_networks(CIRCUIT *circuit)
  {
  int i,progress,type;
  NODE *pnS,*pnD,*pnG;
  DEVICE *pdev;
  do
    {
    progress=0;
    for (i=0; i<circuit->Ndevices; i++)
      {
      pdev=&circuit->devices[i];
      if (!isTransistor(pdev)) continue;
      type=((BSIM4model *)pdev->sub.trans->model)->type;
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      pnG=pdev->sub.trans->nodes[2]->seed;
      progress += mark_source_drain(pnS,pnD,type);
      progress += mark_source_drain(pnD,pnS,type);
      }
    } while (progress);
  }

/*** mark nodes that gate the S/D marked network ***/
void mark_gates(CIRCUIT *circuit)
  {
  int i,type;
  NODE *pnS,*pnD,*pnG;
  DEVICE *pdev;
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnS=pdev->sub.trans->nodes[0]->seed;
    pnD=pdev->sub.trans->nodes[1]->seed;
    pnG=pdev->sub.trans->nodes[2]->seed;
    mark_gate(pnG,pnS);
    mark_gate(pnG,pnD);
    }
  }

/*** mark nodes that couple to subcircuit of interest ***/
void mark_capacitors(CIRCUIT *circuit)
  {
  int i;
  NODE *pnS,*pnD;
  DEVICE *pdev;
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (!isCapacitor(pdev)) continue;
    pnS=pdev->sub.cap->nodes[0]->seed;
    pnD=pdev->sub.cap->nodes[1]->seed;
    mark_capacitor(pnS,pnD,pdev->sub.cap->C);
    mark_capacitor(pnD,pnS,pdev->sub.cap->C);
    }
  }

/*** set alint global variables **/
void set_true_false_mid()
  {
  trueV = options.trueV;
  falseV = options.falseV;
  midV = 0.5*(trueV+falseV);
  }

/*** print marked nodes, sorted by C ***/
void print_marked_nodes(CIRCUIT *circuit)
  {
  int i,j;
  LIST *marked_nodes;
  NODE *pn;
  fprintf(dsim_out,"Marked nodes:\n");
  marked_nodes=list_create(sizeof (NODE *));
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    list_append_element(marked_nodes,&pn);
    }
  list_sort(marked_nodes,&coupling_cmp);
  for (j=0; j<MARK_MAX; j++)
    {
    for (i=0; i<marked_nodes->max; i++)
      {
      pn=marked_nodes->p.pn[i];
      if (pn->mark!=j) continue;
      if (pn->seed!=pn) continue;
      if (!verbose && pn->mark==MARK_PRIME) continue;
      fprintf(dsim_out,"  %s %s",mark_name[pn->mark],get_node_name(pn));
      if (pn->C>0) fprintf(dsim_out," %gfF",1e15*pn->C);
      fprintf(dsim_out,"\n");
      }
    }
  list_free(marked_nodes);
  }

/*** print fanin in canonical order ***/
void print_fanin(LIST *fanin)
  {
  NODE *pn;
  int i;
  fprintf(dsim_out,"Fanin:\n");
  for (i=0; i<fanin->max; i++)
    {
    pn=fanin->p.pn[i];
    fprintf(dsim_out,"  %s\n",get_node_name(pn));
    }
  }

/*** print fanin_groups in canonical order ***/
void print_fanin_groups(LIST *fanin_groups)
  {
  int i,j,dir;
  NODE *pn;
  EXCLUSIVE *group;
  fprintf(dsim_out,"Fanin groups:\n");
  for (i=0; i<fanin_groups->max; i++)
    {
    group=fanin_groups->p.pex[i];
    dir=group->type;
    fprintf(dsim_out,"  %s: ", (dir==1 ? "exclhi" : "excllo"));
    for (j=0; j<group->Nnodes; j++)
      {
      pn=group->nodes[j];
      fprintf(dsim_out,"%s ",get_node_name(pn));
      }
    fprintf(dsim_out,"\n");
    }
  }

/** print exclusive sets **/
void print_exclusive_sets(CIRCUIT *circuit)
  {
  int i;
  NODE *pn;
  EXCLUSIVE *pex;
  fprintf(dsim_out,"Relevant Exclusive Sets:\n");
  for (i=0; i<circuit->Nexclusives; i++)
    {
    pex=&circuit->exclusives[i];
    if      (pex->type==EXCLLO) fprintf(dsim_out,"  %s (",TYPE_EXCLLO);
    else if (pex->type==EXCLHI) fprintf(dsim_out,"  %s (",TYPE_EXCLHI);
    else if (pex->type==EXCLCC) fprintf(dsim_out,"  %s (",TYPE_EXCLCC);
    print_nodes(dsim_out,pex->Nnodes,pex->nodes);
    fprintf(dsim_out,")\n");
    }
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (pn->nocc) fprintf(dsim_out,"  %s (\"%s\")\n",TYPE_NOCC,get_node_name(pn));
    }
  }

/** print devices **/
void print_devices(CIRCUIT *circuit)
  {
  int i;
  DEVICE *pdev;
  fprintf(dsim_out,"Relevant Devices:\n");
  for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    fprintf(dsim_out,"  ");
    print_device(dsim_out,pdev);
    }
  }

/*** outer loop of charge sharing analysis ***/
int alint(CIRCUIT *circuit, char *AlintVictim,
          LIST *CustomScenarios, int CustomForce)
  {
  LIST *fanin,*fanin_groups,*coupling;
  LIST *CC[2],*Tau[2];
  int i,j,h,s,err=0,type,scenarios,dir,delay,slow,cc,cap,
    old_doCouplingCap,tau,max_fanin_aggressors,is_inverter=0;
  NODE *pn,*pnS,*pnD,*pnSP,*pnDP,*pnG,*pnB;
  DEVICE *pdev;
  EXCLUSIVE *pex,*group;
  CIRCUIT subcircuit;
  char name[STRMAX],sim[STRMAX];
  double start_time,old_prs_tau,old_prs_pct,old_VTN,old_VTP,loadCap;
  double ng,pg,na,pa,np,pp,nw,pw,cw,cg;

  /*** report some values ***/
  fprintf(dsim_out,"Options:\n");
  fprintf(dsim_out,"  true = %g\n",options.trueV);
  fprintf(dsim_out,"  false = %g\n",options.falseV);
  fprintf(dsim_out,"  VTN = %g\n",options.VTN);
  fprintf(dsim_out,"  VTP = %g\n",options.VTP);
  fprintf(dsim_out,"  max_bump_fanin_aggressors = %g\n",
          options.max_bump_fanin_aggressors);
  fprintf(dsim_out,"  max_delay_fanin_aggressors = %g\n",
          options.max_delay_fanin_aggressors);
  fprintf(dsim_out,"  bump_event_interval = %g\n",
          options.bump_event_interval);
  fprintf(dsim_out,"  delay_event_interval = %g\n",
          options.delay_event_interval);
  fprintf(dsim_out,"  d2a_shape = %g\n",options.d2a_shape);
  fprintf(dsim_out,"  d2a_saturation = %g\n",options.d2a_saturation);

  /*** settings ***/
  start_time=user_time();
  warnall=0;
  set_true_false_mid();
  CC[0] = bumpCC;
  CC[1] = delayCC;
  Tau[0] = bumpTau;
  Tau[1] = delayTau;

  /*** initialize charge sharing fields of nodes ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    // NOTE: mark_barrier blocks alint's mark_transistor_networks from
    // recursing via the S/D of transistors, but is cleared after that
    // step.
    if (pn->mark!=MARK_BARRIER) pn->mark=MARK_UNKNOWN;
    pn->force=FORCE_NONE;
    pn->C=0;
    pn->relevant=0;
    pn->map=NULL;
    }

  /*** special handling of Victim, GND, Vdd nodes ***/
  Victim = find_node(circuit,"",AlintVictim);
  pnGND  = find_node(circuit,"","$GND");
  pnVdd  = find_node(circuit,"","$Vdd");
  pnEnvGND = find_node(circuit,"","$EnvGND");
  pnEnvVdd = find_node(circuit,"","$EnvVdd");
  if (pnEnvGND==NULL) pnEnvGND=pnGND;
  if (pnEnvVdd==NULL) pnEnvVdd=pnVdd;
  if (Victim==NULL) fprintf(dsim_err,"ERROR: Can't find Victim node.\n");
  if (pnGND==NULL)  fprintf(dsim_err,"ERROR: Can't find $GND node.\n");
  if (pnVdd==NULL)  fprintf(dsim_err,"ERROR: Can't find $Vdd node.\n");
  if (pnGND==pnVdd) fprintf(dsim_err,"ERROR: $Vdd shorted to $GND by aliases.\n");
  if (Victim==pnVdd) fprintf(dsim_err,"ERROR: Victim shorted to $Vdd.\n");
  if (Victim==pnGND) fprintf(dsim_err,"ERROR: Victim shorted to $GND.\n");
  if ((Victim==NULL)||(pnGND==NULL)||(pnVdd==NULL)||
      (pnGND==pnVdd)||(Victim==pnGND)||(Victim==pnVdd)) return 1;

  /*** translate to resistive seed nodes ***/
  Victim = Victim->seed;
  pnGND = pnGND->seed;
  pnVdd = pnVdd->seed;
  pnEnvGND = pnEnvGND->seed;
  pnEnvVdd = pnEnvVdd->seed;
  if (pnGND==pnVdd)
    {
    fprintf(dsim_err,"ERROR: $Vdd shorted to $GND by resistors.\n");
    return 1;
    }

  /*** mark special nodes ***/
  Victim->mark=MARK_VICTIM;
  pnGND->mark=MARK_GLOBAL;
  pnGND->fixed=1;
  pnGND->V=falseV;
  pnGND->force=FORCE_DN;
  pnGND->relevant=1;
  pnVdd->mark=MARK_GLOBAL;
  pnVdd->fixed=1;
  pnVdd->V=trueV;
  pnVdd->force=FORCE_UP;
  pnVdd->relevant=1;
  pnEnvGND->mark=MARK_GLOBAL;
  pnEnvGND->fixed=1;
  pnEnvGND->V=falseV;
  pnEnvGND->force=FORCE_DN;
  pnEnvGND->relevant=1;
  pnEnvVdd->mark=MARK_GLOBAL;
  pnEnvVdd->fixed=1;
  pnEnvVdd->V=trueV;
  pnEnvVdd->force=FORCE_UP;
  pnEnvVdd->relevant=1;

  /*** mark internal/shared nodes via S/D of transistors ***/
  mark_transistor_networks(circuit);

  /*** clear MARK_BARRIER ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (pn->mark==MARK_BARRIER) pn->mark=MARK_UNKNOWN;
    }

  /*** mark gates of relevant transistors ***/
  mark_gates(circuit);

  /*** identify inverters (make pn->map point to input) ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnS=pdev->sub.trans->nodes[0]->seed;
    pnD=pdev->sub.trans->nodes[1]->seed;
    pnG=pdev->sub.trans->nodes[2]->seed;
    pnS->map=pnG; // guess that this forms an inverter
    pnD->map=pnG; // guess that this forms an inverter
    }

  /*** eliminate nodes which are clearly not inverter outputs ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnS=pdev->sub.trans->nodes[0]->seed;
    pnD=pdev->sub.trans->nodes[1]->seed;
    pnG=pdev->sub.trans->nodes[2]->seed;
    if ((type==NTYPE)&&((pnS->map!=pnG)||(pnD!=pnGND))) pnS->map=NULL;
    if ((type==NTYPE)&&((pnD->map!=pnG)||(pnS!=pnGND))) pnD->map=NULL;
    if ((type==PTYPE)&&((pnS->map!=pnG)||(pnD!=pnVdd))) pnS->map=NULL;
    if ((type==PTYPE)&&((pnD->map!=pnG)||(pnS!=pnVdd))) pnD->map=NULL;
    }

  /*** debugging ***/
  if (debug_alint) for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->seed!=pn) continue;
    printf ("%s portG=%d portN=%d portP=%d\n",get_node_name(pn),
            pn->portG,pn->portN,pn->portP);
    if (pn->map==NULL) continue;
    if (!pn->portN || !pn->portP) continue;
    printf(" inverter %s -> %s-\n",get_node_name(pn->map),get_node_name(pn));
    }

  /*** MARK_INVERSE/MARK_SHARED_INVERSE for inverters of VICTIM or SHARED nodes ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->seed!=pn) continue;
    if (pn->mark==MARK_GLOBAL) continue;
    if (pn->map==NULL) continue;
    if (!pn->portN || !pn->portP) continue;
    if (pn->mark==MARK_VICTIM) is_inverter=1;
    if (pn->map->mark==MARK_VICTIM) pn->mark=MARK_INVERSE;
    if (pn->map->mark==MARK_SHARED || pn->map->mark==MARK_SHARED_GATE)
      pn->mark=MARK_SHARED_INVERSE;
    }

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

  /*** mark internal SP/DP transistor nodes ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (!isTransistor(pdev)) continue;
    pnSP=pdev->sub.trans->nodes[4]->seed;
    pnDP=pdev->sub.trans->nodes[5]->seed;
    if (pnSP->mark==MARK_UNKNOWN) pnSP->mark=MARK_PRIME;
    if (pnDP->mark==MARK_UNKNOWN) pnDP->mark=MARK_PRIME;
    }

  /*** identify coupling nodes ***/
  mark_capacitors(circuit);

  /*** mark resistive subnets too ***/
  set_subnets(circuit);

  /*** automatically add exclcc sets for inverters ***/
  for (i=j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->mark==MARK_UNKNOWN) continue;
    if (pn->map==NULL) continue;
    if (pn->map->mark==MARK_UNKNOWN) continue;
    i++;
    }
  if (i>0)
    circuit->exclusives=
      leak_realloc(circuit->exclusives,(circuit->Nexclusives + i) * sizeof (EXCLUSIVE));
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->mark==MARK_UNKNOWN) continue;
    if (pn->map==NULL) continue;
    if (pn->map->mark==MARK_UNKNOWN) continue;
    pex=&circuit->exclusives[circuit->Nexclusives++];
    pex->type=EXCLCC;
    pex->Nnodes=2;
    pex->nodes=leak_malloc(2*sizeof(NODE *));
    pex->nodes[0]=pn->map;
    pex->nodes[1]=pn;
    }

  /*** big trick -- swap original circuit for relevant subcircuit ***/
  subcircuit = create_subcircuit(circuit);
  circuit = &subcircuit;
  Victim = Victim->map;
  pnGND = pnGND->map;
  pnVdd = pnVdd->map;
  pnEnvGND = pnEnvGND->map;
  pnEnvVdd = pnEnvVdd->map;

  /*** sanity check for unplugged wells ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (!isTransistor(pdev)) continue;
    type=((BSIM4model *)pdev->sub.trans->model)->type;
    pnB=pdev->sub.trans->nodes[3]->seed;
    if (((type==NTYPE)&&(pnB==pnGND || pnB==pnEnvGND)) ||
        ((type==PTYPE)&&(pnB==pnVdd || pnB==pnEnvVdd))) continue;
    err=1;
    fprintf(dsim_err,"ERROR: bad bulk on ");
    print_device(dsim_err,pdev);
    }

  /*** report no fanin ***/
  if (Victim && !Victim->portN)
    {
    fprintf(dsim_err,"WARNING: %s has no NMOS fanin.\n",get_node_name(Victim));
    }
  if (Victim && !Victim->portP)
    {
    fprintf(dsim_err,"WARNING: %s has no PMOS fanin.\n",get_node_name(Victim));
    }

  /*** find fanin nodes ***/
  fanin=list_create(sizeof(NODE *));
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (isMarkGate(pn)&&(pn->seed==pn))
      {
      pn->aggressor=1; // temporarily used to create fanin_groups
      list_append_element(fanin,&pn);
      }
    }
  list_sort(fanin,&node_cmp);

  /*** find exclusive fanin_groups ***/
  fanin_groups=list_create(sizeof(EXCLUSIVE *));
  for (i=0; i<circuit->Nexclusives; i++)
    {
    pex=&circuit->exclusives[i];
    if ((pex->type!=EXCLLO)&&(pex->type!=EXCLHI)) continue;
    group=(EXCLUSIVE *) leak_malloc(sizeof(EXCLUSIVE));
    group->type=pex->type;
    group->Nnodes=0;
    group->nodes=(NODE **) leak_malloc(sizeof(NODE *));
    for (j=0; j<pex->Nnodes; j++)
      {
      pn=pex->nodes[j];
      if (isMarkGate(pn) && pn->aggressor)
        {
        pn->aggressor=0; // make sure it only gets grouped once
        pn->canonical=!pex->type; // force to be consistent with group
        group->nodes = (NODE **)
          leak_realloc(group->nodes,(group->Nnodes+1)*sizeof(NODE *));
        group->nodes[group->Nnodes++]=pn;
        }
      }
    if (group->Nnodes>0) list_append_element(fanin_groups,&group);
    else { free_exclusive(group); leak_free(group); }
    }

  /*** put remaining nodes in their own group ***/
  for (i=0; i<fanin->max; i++)
    {
    pn=fanin->p.pn[i];
    if (pn->aggressor)
      {
      pn->aggressor=0; // make sure it only gets grouped once
      group=(EXCLUSIVE *) leak_malloc(sizeof(EXCLUSIVE));
      group->type=!pn->canonical;
      group->Nnodes=1;
      group->nodes=(NODE **) leak_malloc(sizeof(NODE *));
      group->nodes[0]=pn;
      list_append_element(fanin_groups,&group);
      }
    }

  /*** sort fanin groups by number of elements ***/
  list_sort(fanin_groups,&fanin_group_cmp);

  /*** make list of COUPLING and SHARED seed nodes ***/
  coupling=list_create(sizeof(NODE *));
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    if (pn->seed!=pn) continue;
    if ((pn->mark!=MARK_COUPLING) && (pn->mark!=MARK_SHARED)) continue;
    list_append_element(coupling,&pn);
    }

  /*** summarize external load on victim ***/
  ng=pg=na=pa=np=pp=nw=pw=cw=cg=0;
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (isTransistor(pdev))
      {
      type=((BSIM4model *)pdev->sub.trans->model)->type;
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      pnG=pdev->sub.trans->nodes[2]->seed;
      if (pnG->mark==MARK_VICTIM)
        {
        if      (type==NTYPE) ng+=pdev->sub.trans->W * pdev->sub.trans->L;
        else if (type==PTYPE) pg+=pdev->sub.trans->W * pdev->sub.trans->L;
        }
      else if (pnS->mark==MARK_VICTIM)
        {
        if (type==NTYPE)
          {
          na+=pdev->sub.trans->AS;
          nw+=pdev->sub.trans->W;
          np+=pdev->sub.trans->PS-pdev->sub.trans->W;
          }
        else if (type==PTYPE)
          {
          pa+=pdev->sub.trans->AS;
          pw+=pdev->sub.trans->W;
          pp+=pdev->sub.trans->PS-pdev->sub.trans->W;
          }
        }
      else if (pnD->mark==MARK_VICTIM)
        {
        if (type==NTYPE)
          {
          na+=pdev->sub.trans->AD;
          nw+=pdev->sub.trans->W;
          np+=pdev->sub.trans->PD-pdev->sub.trans->W;
          }
        else if (type==PTYPE)
          {
          pa+=pdev->sub.trans->AD;
          pw+=pdev->sub.trans->W;
          pp+=pdev->sub.trans->PD-pdev->sub.trans->W;
          }
        }
      }
    else if (isDiode(pdev))
      {
      type=((BSIM4model *)pdev->sub.diode->model)->type;
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      if ((pnS->mark==MARK_VICTIM) || (pnD->mark==MARK_VICTIM))
        {
        if      (type==NTYPE) {na+=pdev->sub.diode->AS; np+=pdev->sub.diode->PS;}
        else if (type==PTYPE) {pa+=pdev->sub.diode->AS; pp+=pdev->sub.diode->PS;}
        }
      }
    else if (isCapacitor(pdev))
      {
      pnS=pdev->sub.trans->nodes[0]->seed;
      pnD=pdev->sub.trans->nodes[1]->seed;
      if ((pnS->mark==MARK_VICTIM) || (pnD->mark==MARK_VICTIM))
        cw+=pdev->sub.cap->C;
      }
    }
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->mark==MARK_VICTIM) cg += pn->CG;
    }

  /** print capacitance summary **/
  fprintf(dsim_out,"External load on Victim:\n");
  fprintf(dsim_out,"  Coupling capacitance %gfF\n",1e15*cw);
  fprintf(dsim_out,"  Constant capacitance %gfF\n",1e15*cg);
  fprintf(dsim_out,"  Nmos gate area %gum^2\n",1e12*ng);
  fprintf(dsim_out,"  Ndiffusion area %gum^2\n",1e12*na);
  fprintf(dsim_out,"  Ndiffusion non-gate perimeter %gum\n",1e6*np);
  fprintf(dsim_out,"  Ndiffusion gate perimeter %gum\n",1e6*nw);
  fprintf(dsim_out,"  Pmos gate area %gum^2\n",1e12*pg);
  fprintf(dsim_out,"  Pdiffusion area %gum^2\n",1e12*pa);
  fprintf(dsim_out,"  Pdiffusion non-gate perimeter %gum\n",1e6*pp);
  fprintf(dsim_out,"  Pdiffusion gate perimeter %gum\n",1e6*pw);

  /*** print debugging information **/
  print_fanin(fanin);
  print_fanin_groups(fanin_groups);
  print_marked_nodes(circuit);
  if (verbose) print_exclusive_sets(circuit);
  if (verbose) print_devices(circuit);

  /*** check if important nodes weren't relevant ***/
  if (Victim==NULL) fprintf(dsim_err,"ERROR: Victim node irrelevant in subcircuit.\n");
  if (pnGND==NULL)  fprintf(dsim_err,"ERROR: $GND node irrelevant in subcircuit.\n");
  if (pnVdd==NULL)  fprintf(dsim_err,"ERROR: $Vdd node irrelevant in subcircuit.\n");
  if ((Victim==NULL)||(pnGND==NULL)||(pnVdd==NULL)) err=1;

  /*** save doCouplingCap, VTN, VTP, prs_tau, prs_pct options ***/
  circuit_warnings = 0; // reset warning counter
  old_doCouplingCap = doCouplingCap;
  old_prs_tau = options.prs_tau;
  old_prs_pct = prs_pct = 100;
  old_VTN = options.VTN;
  old_VTP = options.VTP;

  /*** loop through all delay/bump, slow/fast, up/dn simulations ***/
  if (!err)
    for (dir=0; dir<2; dir++) for (delay=0; delay<2; delay++)
      for (cap=0; (delay && delayCap->max>0 ? cap<delayCap->max : cap==0); cap++)
        for (s=0; (delay ? s<delayFast->max : s==0); s++)
          for (cc=0; cc<CC[delay]->max; cc++)
            for (tau=0; tau<Tau[delay]->max; tau++)
              {
              /*** override doCouplingCap and options.prs_tau and CG ***/
              slow = delay ? !delayFast->p.i[s] : 0;
              doCouplingCap = (CC[delay]->p.i[cc]!=0);
              options.prs_tau = Tau[delay]->p.f[tau];
              max_fanin_aggressors = delay ? options.max_delay_fanin_aggressors :
                options.max_bump_fanin_aggressors;
              cg = Victim->CG;
              if (cap<delayCap->max) loadCap = delayCap->p.f[cap];
              else loadCap = 0;
              Victim->CG += loadCap;

              /*** setup names/trace files ***/
              safe_sprintf(sim,"%s_%s",delay?(slow?"slow":"fast"):"bump",dir?"up":"dn");
              safe_sprintf(name,delayCap->max>0 ? "%s/%s:%d:%g:%g" : "%s/%s:%d:%g",
                           AlintVictim,sim,doCouplingCap,
                           options.prs_tau/1e-12,loadCap/1e-15);
              circuit->run=name;
              circuit->trace_started=0; /* start a new names/trace with same circuit */
              circuit->time=0; /* restart time */

              /*** search through scenarios ***/
              fprintf(dsim_out,"Alint %s simulation (CC=%d Tau=%g Cap=%g):\n",sim,
                      doCouplingCap,options.prs_tau,loadCap);
              scenarios=enumerate_scenarios(circuit,dir,delay,slow,0,
                                            max_fanin_aggressors,fanin,
                                            fanin_groups,coupling,sim,
                                            CustomScenarios,CustomForce);
              fprintf(dsim_out,"Total %s scenarios=%d\n",sim,scenarios);
              finish_tracefile(circuit);

              /*** restore doCouplingCap and options.prs_tau and CG ***/
              doCouplingCap   = old_doCouplingCap;
              options.prs_tau = old_prs_tau;
              Victim->CG = cg;
              }

  /*** loop through noise threshold simulations ***/
  if (!err)
    for (dir=0; dir<2; dir++) for (cc=0; cc<threshCC->max; cc++)
      for (tau=0; tau<threshTau->max; tau++) for (h=0; h<threshPercent->max; h++)
        {
        /*** override doCouplingCap and options.prs_tau and CG ***/
        delay=2; // distinguish threshold sims from delay sims
        slow=0;  // fast configuration is worst noise threshold
        doCouplingCap = (threshCC->p.i[cc]!=0);
        options.prs_tau = threshTau->p.f[tau];
        prs_pct = threshPercent->p.f[h];
        max_fanin_aggressors = options.max_delay_fanin_aggressors;

        /*** setup names/trace files ***/
        safe_sprintf(sim,"thresh_%s",dir?"up":"dn");
        safe_sprintf(name,"%s/%s:%d:%g:%g",AlintVictim,sim,doCouplingCap,options.prs_tau/1e-12,prs_pct);
        circuit->run=name;
        circuit->trace_started=0; /* start a new names/trace with same circuit */
        circuit->time=0; /* restart time */

        /*** search through scenarios ***/
        fprintf(dsim_out,"Alint %s simulation (CC=%d Tau=%g Percent=%g):\n",
                sim,doCouplingCap,options.prs_tau,prs_pct);
        scenarios=enumerate_scenarios(circuit,dir,delay,slow,0,
                                      max_fanin_aggressors,fanin,
                                      fanin_groups,coupling,sim,
                                      CustomScenarios,CustomForce);
        fprintf(dsim_out,"Total %s scenarios=%d\n",sim,scenarios);
        finish_tracefile(circuit);

        /*** restore ***/
        doCouplingCap   = old_doCouplingCap;
        options.prs_tau = old_prs_tau;
        prs_pct         = old_prs_pct;
        }

  /*** override doCouplingCap, VTN, VTP ***/
  doCouplingCap = 0;
  options.VTN = 0;
  options.VTP = 0;
  options.leakage_enabled = 1;

  /*** leakage simulations ***/
  if (doLeakage && !err && (!is_inverter || doInverterLeakage))
    for (dir=0; dir<2; dir++)
      {
      /*** setup names/trace files ***/
      safe_sprintf(sim,"leak_%s",dir?"up":"dn");
      safe_sprintf(name,"%s/%s",AlintVictim,sim);
      circuit->run=name;
      circuit->trace_started=0; /* start a new names/trace with same circuit */
      circuit->time=0; /* restart time */

      /*** search through scenarios (a slow bump with 0 aggressors) ***/
      fprintf(dsim_out,"Alint %s simulation:\n",sim);
      scenarios=enumerate_scenarios(circuit,dir,0,1,1,0,fanin,
                                    fanin_groups,coupling,sim,
                                    CustomScenarios,CustomForce);
      fprintf(dsim_out,"Total %s scenarios=%d\n",sim,scenarios);
      finish_tracefile(circuit);
      }

  /*** restore doCouplingCap, VTN, VTP, prs_tau options ***/
  doCouplingCap = old_doCouplingCap;
  options.VTN = old_VTN;
  options.VTP = old_VTP;
  options.leakage_enabled = 0;

  /*** free memory ***/
  if (!err) fprintf(dsim_out,"Alint simulation of %s finished, time=%g\n",
                    get_node_name(Victim),user_time()-start_time);
  list_free(fanin);
  for (i=0; i<fanin_groups->max; i++)
    {
    group=fanin_groups->p.pex[i];
    free_exclusive(group);
    leak_free(group);
    }
  list_free(fanin_groups);
  list_free(coupling);
  free_subcircuit(circuit);
  return err;
  }
