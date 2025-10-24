/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** convert voltages on analog nodes to set events in digital system ***/
DIGITAL_NODE *analog2digital(CIRCUIT *circuit)
  {
  int j,warn;
  unsigned value;
  NODE *pn;
  DIGITAL_NODE *breaknode=NULL,*dnode;
  int digital_timestep;
  int rising=0,falling=0;
  double V,X,lastV;

  /*** convert analog to digital ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    dnode=pn->digital_node;
    if (dnode==NULL) continue;

    // save pending/unstab state before digital cycle
    pn->pending=trivalue(dnode->pending);
    pn->unstab=dnode->unstab;

    // skip env nodes
    if (dnode->has_env_fanin) continue;

    // get shorthand variables
    V=pn->smoothV; // use low pass filtered V
    X=pn->smoothX; // use low pass filtered dVdt
    warn = warnall && (dnode->unstab!=1); // suppress warnings on unstab events

    /*** look for digital transitions ***/
    value=trivalue(dnode->value);
    if (dnode->unstab==0) // normal events
      {
      // eager thresholds, direction sensitive in middle region
      rising  = (value!=Value1) && 
        (V>options.hi_thresholdV) &&
        ((X>0) || (V>options.lo_thresholdV));
      falling = (value!=Value0) && 
        (V<options.lo_thresholdV) &&
        ((X<0) || (V<options.hi_thresholdV));
      }
    else if (dnode->unstab==1) // unstab events
      {
      // eager thresholds, only sensitive to crossing the thresholds
      lastV = V - X;
      rising  = (value!=Value1) &&
        (V>options.hi_thresholdV) &&
        ((lastV<=options.hi_thresholdV) || (V>options.lo_thresholdV));
      falling = (value!=Value0) &&
        (V<options.lo_thresholdV) &&
        ((lastV>=options.lo_thresholdV) || (V<options.hi_thresholdV));
      }
    else if (dnode->unstab==2) // metastab events
      {
      // lazy thresholds
      rising  = (value!=Value1) &&
        (V>options.hi_thresholdV) &&
        (V>options.lo_thresholdV);
      falling = (value!=Value0) &&
        (V<options.lo_thresholdV) &&
        (V<options.hi_thresholdV);
      }

    /*** set digital cosim nodes, possibly with warnings ***/
    if (rising || falling)
      {
      if (warn && dnode->has_digital_fanin)
        {
        if (dnode->event_index<0)
          fprintf(dsim_err,"WARNING: %s %s"
                  " without digital event pending at time %u\n",
                  get_node_name(pn),rising ? "rising" : "falling", digital_time);
        else if (pn->pending!=(rising ? Value1: Value0))
          fprintf(dsim_err,"WARNING: %s rising"
                  " overpowers pending digital event %s at time %u\n",
                  get_node_name(pn),ValueName[pn->pending],digital_time);
        }
      schedule_event(dnode,(rising ? Big1 : Big0),
                     INSTANT_TIME,dnode->unstab,LEVEL_ENV,1,NULL);
      }
    }

  /*** digital step ***/
  if (options.digital_time_unit>0)
    {
    digital_timestep = circuit->time/options.digital_time_unit - digital_time;
    if (digital_timestep>0) breaknode = cycle(circuit->groups,digital_timestep);
    }
  else breaknode = cycle(circuit->groups,0); /*** arbitrarily fast environment ***/

  /*** check that new pending events start from consistent voltage ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    V=pn->smoothV; // use low pass filtered V
    dnode=pn->digital_node;
    if (dnode==NULL) continue;                 // no A/D cosim
    if (!dnode->has_digital_fanin) continue;   // assume driven by analog source
    if (dnode->event_index<0) continue;        // not in queue
    if (trivalue(dnode->pending)==pn->pending) continue; // not new event
    if (pn->unstab) continue;                  // last event was unstable
    if      ((V<options.lo_thresholdV)&&(V<options.hi_thresholdV)) value=Value0;
    else if ((V>options.lo_thresholdV)&&(V>options.hi_thresholdV)) value=Value1;
    else                                                           value=ValueU;
    if (warnall && (value!=trivalue(dnode->value)))
      fprintf(dsim_err,"WARNING: %s %s to %s event enqueued while V=%g at time %u\n",
              get_node_name(pn),
              ValueName[trivalue(dnode->value)],
              ValueName[trivalue(dnode->pending)],
              V,digital_time);
    }

  /* report breakpoint, if any */
  return breaknode;
  }

/*** pull analog node to V with hybrid linear/exponential waveform ***/
void set_analog_node(NODE *pn,
                     double V, double mAq, double mAi, double mBq, double mBi,
                     double time, double h)
  {
  double tau,X,Y,Ys,t;
  int shape;
  if (pn->fixed) return; // don't force fixed nodes
  shape = options.d2a_shape+0.5;
  tau = options.prs_tau;
  Ys = options.d2a_saturation;
  X = fabs(options.trueV-options.falseV)/tau; // linear slope
  if (shape==1) tau = Ys/X;                   // match slope

  // linear ramp towards saturation voltage
  Y = fabs(V-pn->V);
  t = (Y-Ys)/X;
  if (shape!=1 || t<=0) { t=h; }         // in exponential saturation
  else if (t<=h)        { Y=Ys; t=h-t; } // linear ramp to saturation
  else                  { Y-=X*h; t=0; } // linear ramp

  // exponential decay
  if (t>0 && tau>0) Y*=exp(-t/tau);
  else if (t>0) Y=0;

  // set pn->X
  pn->vsource = 1;
  Y = V>pn->V ? V-Y : V+Y;
  pn->X = (Y-pn->V)/h;
  }

/*** convert digital to analog where appropriate ***/
void digital2analog(CIRCUIT *circuit,
                    double mAq, double mAi, double mBq, double mBi,
                    double time, double h)
  {
  int j,value;
  NODE *pn;
  DIGITAL_NODE *dnode;
  double V,midV;
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    dnode=pn->digital_node;
    midV=0.5*(options.falseV+options.trueV);

    /*** for charge sharing analysis ***/
    if      (pn->force==FORCE_UP)  set_analog_node(pn,options.trueV, mAq,mAi,mBq,mBi,time,h);
    else if (pn->force==FORCE_DN)  set_analog_node(pn,options.falseV,mAq,mAi,mBq,mBi,time,h);
    else if (pn->force==FORCE_MID) set_analog_node(pn,midV,mAq,mAi,mBq,mBi,time,h);
    else if (pn->force==FORCE_V)   set_analog_node(pn,pn->forceV,mAq,mAi,mBq,mBi,time,h);

    /*** for analog/digital cosim ***/
    else if ((dnode!=NULL) && dnode->has_env_fanin)
      {
      /*** digital env node drives analog node ***/
      value = trivalue(dnode->value);
      if      (value == Value1) V=options.trueV;
      else if (value == Value0) V=options.falseV;
      else                      V=midV;
      set_analog_node(pn,V,mAq,mAi,mBq,mBi,time,h);
      }
    }
  }
