/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"
#undef DEBUG

unsigned digital_time=1;
DIGITAL_NODE **events=NULL;
HISTORY *History=NULL;
int Nevents=0,Allocated=0,HistoryN=0,HistoryMax=0;
int warnall=0,random_order=0,analog=ANALOG_ENABLED,
    ignoreerror=1,interrupt_sim=0,ERROR=0;
int floating_unstab_is_U=0;
double slow_delay=4.0/3,fast_delay=2.0/3;
BIGINT *Big0=NULL,*Big1=NULL,*BigU=NULL;

/****************************** HEAP HANDLING ******************************/

/*** allocate the event queue ***/
void allocate_event_queue(int size)
  {
  if (size<=Allocated) return;
  Allocated=size;
  if (events==NULL)
    events=(DIGITAL_NODE **) leak_malloc(Allocated*sizeof(DIGITAL_NODE *));
  else
    events=(DIGITAL_NODE **) leak_realloc(events,Allocated*sizeof(DIGITAL_NODE *));
  }
  
/*** free the event queue ***/
void free_event_queue()
  {
  if (events!=NULL) leak_free(events);
  events=NULL;
  Allocated=0;
  Nevents=0;
  }

/*** swap two events ***/
inline void swap_events(int e1, int e2)
  {
  DIGITAL_NODE *temp;
  assert(events[e1]->event_index==e1);
  assert(events[e2]->event_index==e2);
  temp=events[e1];
  events[e1]=events[e2];
  events[e2]=temp;
  events[e1]->event_index=e1;
  events[e2]->event_index=e2;
  }

/*** reheap event ***/
void reheap(int e)
  {
  int p,l,r,l_smaller,r_smaller;

  /*** reheap earlier ***/
  while(1)
    {
    p=(e-1)>>1;
    if ((p>=0)&&(events[e]->event_time<events[p]->event_time)) {swap_events(e,p); e=p;}
    else break;
    }

  /*** reheap later ***/
  while(1)
    {
    l=(e<<1)+1;
    r=l+1;
    l_smaller=((l<Nevents)&&(events[e]->event_time>events[l]->event_time));
    r_smaller=((r<Nevents)&&(events[e]->event_time>events[r]->event_time));
    if      ((!l_smaller)&&(!r_smaller)) break;
    else if (l_smaller&&(!r_smaller)) {swap_events(e,l); e=l;}
    else if (r_smaller&&(!l_smaller)) {swap_events(e,r); e=r;}
    else if (events[l]->event_time<events[r]->event_time) {swap_events(e,l); e=l;}
    else                                                  {swap_events(e,r); e=r;}
    }

#ifdef DEBUG
  /*** check heap property ***/
  for (e=0; e<Nevents; e++)
    {
    l=(e<<1)+1;
    r=l+1;
    assert(events[e]->event_index==e);
    if (l<Nevents) assert(events[l]->event_time>=events[e]->event_time);
    if (r<Nevents) assert(events[r]->event_time>=events[e]->event_time);
    }
#endif
  }

/*** schedule event on event queue ***/
void schedule_event(DIGITAL_NODE *dnode, BIGINT *pending,
                    unsigned time, int unstab, int level, int normal,
                    DIGITAL_NODE *lastevent)
  {
  /*** set unstab, enabler ***/
  dnode->unstab=unstab;
  if (lastevent!=NULL) // don't change history if NULL
    {
    dnode->enabler_node=lastevent;
    dnode->enabler_tcount=lastevent->tcount;
    }

  /*** handle cosimulation of boundary nodes ***/
  dnode->effective=1;
  if ((dnode->level==LEVEL_BOUNDARY) && 
      (analog==ANALOG_ENABLED) &&
      (level!=LEVEL_ENV))
    {
    if (!normal)
      {
      // abnormal events happen without cosim
      assign_bigint(dnode->pending,pending);
      if (dnode->has_analog_fanin) dnode->effective=0; // dequeue with no effect
      dnode->event_level=LEVEL_ENV;
      }
    else if ((dnode->event_index>=0) && (dnode->event_level!=level))
      {
      // was partially enabled, now enabled by both levels
      if (compare_bigint(dnode->pending,pending)!=0)
        {
        // cosim error
        ERROR=1;
        if (warnall)
          {
          fprintf(dsim_err,"WARNING: %s was pending ",
                  get_digital_node_name(dnode));
          print_value(dsim_err,dnode->pending,dnode->string);
          fprintf(dsim_err," from %s level, now pending ",
                  LevelName[dnode->event_level]);
          print_value(dsim_err,pending,dnode->string);
          fprintf(dsim_err," at time %u.\n",digital_time);
          }
        }

      // fully enable the event to fire
      if (level!=LEVEL_LOWER) assign_bigint(dnode->pending,pending);
      if (dnode->event_time-LAZY_TIME>time) time=dnode->event_time-LAZY_TIME;
      dnode->event_level=LEVEL_ENV;
      }
    else
      {
      // partially enqueue/requeue event after LAZY_TIME
      dnode->event_level=level;
      time+=LAZY_TIME; // enqueue until other level enables
      assign_bigint(dnode->pending,pending);
      }
    }
  else
    {
    // schedule non-cosim events normally
    dnode->event_level=LEVEL_ENV;
    assign_bigint(dnode->pending,pending);
    }

  /*** schedule or reschedule ***/
  if (dnode->event_index<0) /*** for new event, add to end and reheap ***/
    {
    if (Nevents+1>Allocated) allocate_event_queue(Nevents+1);
    dnode->event_index=Nevents;
    events[Nevents]=dnode;
    Nevents++;
    dnode->event_time=time;
    reheap(dnode->event_index);
#ifdef VERBOSE
    fprintf(dsim_err,"Adding %s->",get_digital_node_name(dnode));
    print_value(dsim_err,dnode->pending,dnode->string);
    fprintf(dsim_err," to queue at with unstab=%d at time=%u.\n",dnode->unstab,time);
#endif
    }
  else if (time<dnode->event_time) /*** reschedule event earlier ***/
    {
    dnode->event_time=time;
    reheap(dnode->event_index);
#ifdef VERBOSE
    fprintf(dsim_err,"Rescheduling %s->",get_digital_node_name(dnode));
    print_value(dsim_err,dnode->pending,dnode->string);
    fprintf(dsim_err," in queue with unstab=%d at time=%u.\n",dnode->unstab,time);
#endif

    }
  }

/*** remove the first event from the queue ***/
DIGITAL_NODE *first_event()
  {
  DIGITAL_NODE *dnode;
  int e;

  /*** pick node to take from heap ***/
  assert(Nevents>0);
  assert(Allocated>=Nevents);
  if (events[0]->event_time==INSTANT_TIME) e=0; /*** fire isochronic rules first ***/
  else
    {
    e=rand()%Nevents;
    if (events[e]->event_time!=RANDOM_TIME) e=0; /*** fire timed rules in order ***/
    }
  dnode=events[e];

  /*** swap with last event and remove ***/
  swap_events(e,Nevents-1);
  Nevents--;

  /*** reheap last event (now at index e) ***/
  if (Nevents>0) reheap(e);

  /*** clear dnode fields ***/
  assert(dnode->event_index==Nevents);
  assert(dnode==events[Nevents]);
  dnode->event_index=-1;

#ifdef VERBOSE
  fprintf(dsim_err,"Removing %s->",get_digital_node_name(dnode));
  print_value(dsim_err,dnode->pending,dnode->string);
  fprintf(dsim_err," from queue at time=%u.\n",dnode->event_time);
#endif
  return dnode;
  }

/****************************** SIMULATION ******************************/

/*** handle time wrapping case gracefully ***/
inline unsigned wrapped_time(unsigned time)
  {
  int i,offset;
  DIGITAL_NODE *dnode;
  if (time<RANDOM_TIME) return time; /* time within legal bounds */
  fprintf(dsim_err,"WARNING: time wrapped, renormalizing times in event queue.\n");
  offset=1-digital_time; /* renormalize to digital_time=1 */
  for (i=0; i<Nevents; i++)
    {
    dnode=events[i];
    if (dnode->event_time==INSTANT_TIME) continue; /* isochronic */
    if (dnode->event_time==RANDOM_TIME) continue; /* random */
    if (dnode->event_time==INSTANT_TIME+LAZY_TIME) continue; /* ioschronic lazy */
    if (dnode->event_time==RANDOM_TIME+LAZY_TIME) continue; /* random lazy */
    if (dnode->event_time>LAZY_TIME)
      {
      dnode->event_time+=offset;
      if (dnode->event_time<LAZY_TIME) dnode->event_time=LAZY_TIME;
      }
    else dnode->event_time+=offset;
    }
  time+=offset;
  digital_time+=offset;
  return time;
  }

/*** pick event time for target of a rule or gma ***/
inline unsigned target_event_time(int delay, int instant, int isochronic,
                                  int timed, int realtime)
  {
  if (instant)
    return INSTANT_TIME; // always instant
  else if ((random_order==0) || realtime) // norandom or realtime
    return wrapped_time(digital_time + delay);
  else if (isochronic && (random_order == 1))
    // Isochronic rules fire "instantly" in random simulations to
    // model timing assumptions in otherwise DI circuits.
    return INSTANT_TIME;
  else if (timed || (random_order==2)) // timed rule or timed_random mode
    {
    double jitter = fast_delay + 
      ((double) rand()/(double) RAND_MAX)*(slow_delay-fast_delay);
    return wrapped_time(digital_time + delay*jitter);
    }
  else return RANDOM_TIME;
  }

/*** get 0/1/U value from BIGINT value ***/
inline unsigned trivalue(BIGINT *value)
  {
  if      (bigint_is_zero(value)) return Value0;
  else if (bigint_is_one(value))  return Value1;
  else return ValueU;
  }

/*** fire a rule and enqueue target ***/
void fire_rule(DIGITAL_NODE *lastevent, DIGITAL_RULE *drule)
  {
  DIGITAL_NODE *dnode=drule->target;
  BIGINT *bigdir;
  unsigned dir,inqueue=(dnode->event_index>=0);
  unsigned time,value,pending;
  int warn;
  assert(drule->old!=drule->new);
  value = trivalue(dnode->value);
  pending = trivalue(dnode->pending);

  /*** figure out whats happening to the node ***/
  if ((dnode->Count[0]+dnode->UCount[0]>0)&&(dnode->Count[1]+dnode->UCount[1]>0)) /*** interfering ***/
    {
    if ((drule->old==RuleOFF)&&(((!inqueue)&&(value!=ValueU)) || 
                                (((inqueue)&&(pending!=ValueU)))))
      {
      /*** newly became interfering ***/
      schedule_event(dnode,BigU,INSTANT_TIME,dnode->unstab,drule->level,0,lastevent);
      event_warning(lastevent,dnode,"interfering");
      }
    }
  else if ((dnode->Count[0]>0) || (dnode->Count[1]>0)) /*** driven ***/
    {
    dir=(dnode->Count[1]>0);
    bigdir = dir ? Big1 : Big0;
    time = target_event_time(drule->delay,drule->instant,drule->isochronic,
                             drule->timed,drule->realtime);
    if (!inqueue)
      {
      if (value==dir); /*** vacuous ***/
      else if (value==ValueU)
	{
        /*** new event due to disappearance of interference ***/
        schedule_event(dnode,bigdir,digital_time,dnode->unstab,drule->level,0,lastevent);
        event_notice(lastevent,dnode,"changes from U");
        }
      else if (drule->new==RuleON)
	{
        /*** new event ***/
        schedule_event(dnode,bigdir,time,drule->unstab,drule->level,1,lastevent);
        event_notice(lastevent,dnode,"scheduled normally");
        }
      }
    else
      {
      if (pending==ValueU)
	{
        /*** new event due to disappearance of interference ***/
        schedule_event(dnode,bigdir,digital_time,dnode->unstab,drule->level,0,lastevent);
        event_notice(lastevent,dnode,"changes from U");
        }
      else if ((drule->old==RuleON)&&(pending!=dir))
	{
        /*** instability ***/
        warn=!dnode->unstab;
        schedule_event(dnode,bigdir,INSTANT_TIME,dnode->unstab,drule->level,0,lastevent);
        if (warn) event_warning(lastevent,dnode,"unstable (driven)");
        }
      else if ((drule->new==RuleON)&&(pending==dir)&&(time<dnode->event_time))
	{
        /*** reschedule event ***/
        schedule_event(dnode,bigdir,time,drule->unstab,drule->level,1,lastevent);
        event_notice(lastevent,dnode,"rescheduled earlier");
        }
      }
    }
  else /*** floating or driven by unknown rule ***/
    {
    if ((drule->old==RuleON)&&(inqueue)&&(pending!=ValueU))
      {
      /*** instability ***/
      warn=!dnode->unstab;
      schedule_event(dnode,floating_unstab_is_U ? BigU : dnode->value,
                     INSTANT_TIME,dnode->unstab,drule->level,0,lastevent);
      if (warn) event_warning(lastevent,dnode,"unstable (floating)");
      }
    else if ((drule->new==RuleU)&&(((dnode->UCount[0]>0)&&(value==Value1)) ||
                                   ((dnode->UCount[1]>0)&&(value==Value0))))
      {
      /*** set to U ***/
      schedule_event(dnode,BigU,INSTANT_TIME,dnode->unstab,drule->level,0,lastevent);
      event_warning(lastevent,dnode,"set to U");
      }
    }
  }

/*** evaluate user defined functions or groups in a bigint expression ***/
void eval_user(int type, int *tos, BIGINT **stack, void *data)
  {
  DIGITAL_GROUP *groups = (DIGITAL_GROUP *) data;
  DIGITAL_GROUP *dgrp;
  DIGITAL_NODE *dnode;
  int igrp,inode;
  char msg[STRMAX];
  if (type==BIOP_TIME)
    {
    stack[(*tos)++]=bigint_from_int(digital_time);
    return;
    }
  igrp = type-BIOP_GRP;
  dgrp = &groups[igrp];
  if (*tos<1)
    {
    safe_sprintf(msg,"WARNING: stack underflow in %s().\n",
                 dgrp->name->name);
    event_warning(NULL,NULL,msg);
    move_bigint(stack[*tos-1],bigint_from_int(0));
    return;
    }
  inode = int_from_bigint(stack[*tos-1]);
  if ((inode<0) || (inode>=dgrp->nodes->max))
    {
    safe_sprintf(msg,"WARNING: out-of-bounds set %s(%d).\n",
                 dgrp->name->name,inode);
    event_warning(NULL,NULL,msg);
    move_bigint(stack[*tos-1],bigint_from_int(0));
    return;
    }
  dnode=dgrp->nodes->p.dnode[inode];
  move_bigint(stack[*tos-1],copy_bigint(dnode->value));
  }

/*** test if GMA firing should produce an instability warning ***/
int unstab_gma_warning(DIGITAL_GMA *gma, DIGITAL_NODE *dnode)
  {
  return (!dnode->unstab) && (dnode->event_index>=0) &&
    ((dnode->event_level==LEVEL_ENV) || (dnode->event_level==gma->level));
  }

/*** fire a gma ***/
void fire_gma(DIGITAL_GROUP *groups, DIGITAL_NODE *lastevent, DIGITAL_GMA *gma)
  {
  BIGINT *value,*t;
  DIGITAL_NODE *dnode;
  DIGITAL_GROUP *dgrp;
  LIST *iexpr,*dexpr,*expr;
  int enabled,i,j,warn;
  unsigned delay,time,index;
  int flags,instant,isochronic,timed,realtime,unstab,level;

  /*** test for enabled guard ***/
  t = evaluate_bigint_expression(gma->guard,groups,eval_user);
  enabled = bigint_is_nonzero(t);
#ifdef VERBOSE
  fprintf(dsim_err,"Evaluating guard for gma %p: ",gma);
  print_bigint_expression(dsim_err,gma->guard);
  fprintf(dsim_err," = ");
  print_bigint_hex(dsim_err,t);
  fprintf(dsim_err,"\n");
  fprintf(dsim_err,"Node %s changes, gma %p is %s.\n",
          lastevent!=NULL ? get_digital_node_name(lastevent) : "NULL",gma,
          enabled ? "enabled" : "disabled");
#endif
  free_bigint(t);

  /*** fire when guard transitions from false to true ***/
  if (gma->enabled==enabled) return; // no change in state of guard
  gma->enabled=enabled;
  if (!enabled) return; // not enabled now
  
  /*** evaluate expressions, enqueue nodes ***/
  for (i=0; i<gma->nodes->max; i++)
    {
    /*** prefetch common data ***/
    prefetch_load_transient(&gma->flags->p.i[i+1]);
    prefetch_load_transient(&gma->nodes->p.dnode[i+1]);
    prefetch_load_transient(&gma->expressions->p.list[i+1]);

    /*** get flags ***/
    flags   = gma->flags->p.i[i];
    instant = (flags & FLAG_INSTANT) !=0;
    unstab  = (flags / FLAG_UNSTAB) & 3;
    level   = (flags & FLAG_ENV)!=0 ? LEVEL_ENV : gma->level;

    /*** get event time ***/
    if (instant) time = INSTANT_TIME;
    else
      {
      /*** evaluate delay expression ***/
      dexpr = gma->delays->p.list[i];
      t = evaluate_bigint_expression(dexpr,groups,eval_user);
      delay = t->value;
      free_bigint(t);

      /*** pick event time ***/
      isochronic = (flags & FLAG_ISOCHRONIC) !=0;
      timed      = (flags & FLAG_TIMED)      !=0;
      realtime   = (flags & FLAG_REALTIME)   !=0;
      time = target_event_time(delay,instant,isochronic,timed,realtime);
      }

    /*** right hand side expression ***/
    expr = gma->expressions->p.list[i];
    value = evaluate_bigint_expression(expr,groups,eval_user);

    /*** identify the lvalue of the assignment ***/
    dnode = gma->nodes->p.dnode[i]; // get node lvalue, if one exists
    dgrp  = gma->groups->p.dgrp[i]; // get group lvalue, if one exists
    if ((dnode==NULL)&&(dgrp!=NULL)) // a group lvalue
      {
      iexpr = gma->indices->p.list[i];
      if (iexpr->max==0) // on empty index, assign all elements of the group
        {
        for (j=0; j<dgrp->nodes->max; j++)
          {
          /*** schedule event, warn about unstable events ***/
          dnode = dgrp->nodes->p.dnode[j];
          if (dnode->group) continue; // already enqueued by this group
          dnode->group=1; // set group bit
          warn = unstab_gma_warning(gma,dnode);
          schedule_event(dnode,value,time,unstab,level,1,lastevent);
          if (warn) event_warning(lastevent,dnode,"unstable (GMA)");
          }
        for (j=0; j<dgrp->nodes->max; j++)
          {
          dnode = dgrp->nodes->p.dnode[j];
          dnode->group=0; // clear group bit
          }
        dnode = NULL;
        }
      else // assign one member of the group
        {
        /*** index expression ***/
        t = evaluate_bigint_expression(iexpr,groups,eval_user);
        index = t->value;
        free_bigint(t);
        if ((index<0)||(index>=dgrp->nodes->max))
          {
          char msg[STRMAX];
          safe_sprintf(msg,"WARNING: out-of-bounds get %s(%d).\n",
                       dgrp->name->name,index);
          event_warning(lastevent,NULL,msg);
          }
        else dnode = dgrp->nodes->p.dnode[index];
        }
      }
    if (dnode!=NULL)
      {
      /*** schedule event, warn about unstable events ***/
      warn = unstab_gma_warning(gma,dnode);
      schedule_event(dnode,value,time,unstab,level,1,lastevent);
      if (warn) event_warning(lastevent,dnode,"unstable (GMA)");
      }
    free_bigint(value);
    }
  }

/*** set a node, enqueue dependents ***/
void set_digital_node(DIGITAL_GROUP *groups, DIGITAL_NODE *dnode, BIGINT *big_pending)
  {
  int j,s;
  DIGITAL_RULE *drule;
  unsigned value,pending;

  /*** get info ***/
  value = trivalue(dnode->value);
  pending = trivalue(big_pending);
  dnode->tcount++;

  /*** special case for 0 to 1 transitions ***/
  if ((value==Value0)&&(pending==Value1))
    {
    /*** enable normal sense rules ***/
    for (j=0; j<dnode->Nrules[1]; j++)
      {
      drule=dnode->rules[1][j];
      drule->new=drule->old=RuleOFF;
      drule->FalseConjuncts--;
      if (drule->FalseConjuncts==0)
	{
	if (drule->UnknownConjuncts==0)
          {drule->new=RuleON; drule->target->Count[drule->dir]++;}
	else
          {drule->new=RuleU; drule->target->UCount[drule->dir]++;}
	}
      }

    /*** disable opposite sense rules ***/
    for (j=0; j<dnode->Nrules[0]; j++)
      {
      drule=dnode->rules[0][j];
      drule->new=drule->old=RuleOFF;
      if (drule->FalseConjuncts==0)
	{
        if (drule->UnknownConjuncts==0)
          {drule->old=RuleON; drule->target->Count[drule->dir]--;}
	else
          {drule->old=RuleU; drule->target->UCount[drule->dir]--;}
	}
      drule->FalseConjuncts++;
      }
    }
  /*** special case for 1 to 0 transistions ***/
  else if ((value==Value1)&&(pending==Value0))
    {
    /*** enable opposite sense rules ***/
    for (j=0; j<dnode->Nrules[0]; j++)
      {
      drule=dnode->rules[0][j];
      drule->new=drule->old=RuleOFF;
      drule->FalseConjuncts--;
      if (drule->FalseConjuncts==0)
	{
	if (drule->UnknownConjuncts==0)
          {drule->new=RuleON; drule->target->Count[drule->dir]++;}
	else
          {drule->new=RuleU; drule->target->UCount[drule->dir]++;}
	}
      }

    /*** disable normal sense rules ***/
    for (j=0; j<dnode->Nrules[1]; j++)
      {
      drule=dnode->rules[1][j];
      drule->new=drule->old=RuleOFF;
      if (drule->FalseConjuncts==0)
	{
        if (drule->UnknownConjuncts==0)
          {drule->old=RuleON; drule->target->Count[drule->dir]--;}
	else
          {drule->old=RuleU; drule->target->UCount[drule->dir]--;}
	}
      drule->FalseConjuncts++;
      }
    }
  /*** general case for all state transitions between 0,1,U ***/
  else
    {
    /*** compute drule->old ***/
    for (s=0; s<2; s++) for (j=0; j<dnode->Nrules[s]; j++)
      {
      drule=dnode->rules[s][j];
      if      ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts==0))
        drule->old=RuleON;
      else if ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts>0))
        drule->old=RuleU;
      else
        drule->old=RuleOFF;
      }

    /*** update dependent rule and node counters ***/
    for (s=0; s<2; s++) for (j=0; j<dnode->Nrules[s]; j++)
      {
      drule=dnode->rules[s][j];

      /*** reverse last transition on node ***/
      if      ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts==0))
        drule->target->Count[drule->dir]--;
      else if ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts>0))
        drule->target->UCount[drule->dir]--;

      /*** update rule conjunct counts ***/
      drule->UnknownConjuncts         += (pending==ValueU) - (value==ValueU);
      if (s==0) drule->FalseConjuncts += (pending==Value1) - (value==Value1);
      else      drule->FalseConjuncts += (pending==Value0) - (value==Value0);

      /*** update counts of target node ***/
      if      ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts==0))
        drule->target->Count[drule->dir]++;
      else if ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts>0))
        drule->target->UCount[drule->dir]++;
      }

    /*** compute drule->new ***/
    for (s=0; s<2; s++) for (j=0; j<dnode->Nrules[s]; j++)
      {
      drule=dnode->rules[s][j];
      if      ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts==0))
        drule->new=RuleON;
      else if ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts>0))
        drule->new=RuleU;
      else
        drule->new=RuleOFF;
      }
    }

  /*** set node value ***/
  assign_bigint(dnode->value,big_pending);

  /*** record history ***/
  if (History!=NULL)
    {
    s=HistoryN%HistoryMax;
    History[s].node=dnode;
    History[s].enabler_node=dnode->enabler_node;
    History[s].enabler_tcount=dnode->enabler_tcount;
    History[s].time=digital_time;
    History[s].tcount=dnode->tcount;
    if (History[s].value!=NULL) assign_bigint(History[s].value,dnode->value);
    else History[s].value = copy_bigint(dnode->value);
    HistoryN++;
    }

  /*** debugging printouts ***/
  if (dnode->watch) print_dnode(dsim_out,dnode);
  if ((dsim_log!=NULL) && dnode->log) log_dnode(dsim_log,dnode);

  /*** fire modified rules ***/
  for (s=0; s<2; s++) for (j=0; j<dnode->Nrules[s]; j++)
    {
    drule=dnode->rules[s][j];
    if (drule->new!=drule->old) fire_rule(dnode,drule);
    }

  /*** fire affected gmas ***/
  for (j=0; j<dnode->Ngmas; j++) fire_gma(groups,dnode,dnode->gmas[j]);
  }

/*** allocate a History buffer ***/
void allocate_history(int Max)
  {
  int j;
  if (History!=NULL)
    {
    for (j=0; j<HistoryMax; j++)
      if (History[j].value!=NULL)
        free_bigint(History[j].value);  
    leak_free(History);
    }
  History=NULL;
  HistoryN=0;
  HistoryMax=0;
  if (Max>0)
    {
    size_t size;
    size=Max*sizeof(HISTORY);
    History=leak_malloc_no_check(size);
    HistoryN=0;
    HistoryMax=Max;
    if (History==NULL)
      fprintf(dsim_err,"WARNING: unable to allocate %ldMB for history.\n",
              ((long) size)/1024/1024);
    else for (j=0; j<HistoryMax; j++) History[j].value=NULL;
    }
  }

/**
 * Cycle until event queue is empty of non-lazy events, timestep
 * exceeded, or breakpt.  Returns breakpt node or NULL if none.
 **/
DIGITAL_NODE *cycle(DIGITAL_GROUP *groups, int timestep)
  {
  unsigned next,endtime=digital_time+timestep;
  DIGITAL_NODE *dnode;
  ERROR=0;
  while (Nevents>0)
    {
    /* check time of next event */
    next=events[0]->event_time;
    if (next>=LAZY_TIME) break;
    if ((timestep>0)&&(next>endtime)&&(next!=RANDOM_TIME)) break;

    /* fire next event */
    dnode=first_event();
    if ((dnode->event_time!=INSTANT_TIME)&&
        (dnode->event_time!=RANDOM_TIME)&&
        (dnode->event_time>digital_time))
      digital_time=dnode->event_time;
    if (dnode->effective || (analog!=ANALOG_ENABLED))
      set_digital_node(groups,dnode,dnode->pending);

    /* handle breakpts, errors, and interrupts */
    if (dnode->breakpt>1) dnode->breakpt--;
    else if (dnode->breakpt==1) {dnode->breakpt=0; return dnode; }
    if (!ignoreerror && ERROR) return NULL;
    if (interrupt_sim) return NULL;
    }
  if (timestep>0) digital_time=endtime; /* advance to endtime if appropriate */
  return NULL;
  }

/****************************** CREATE NODES AND RULES ******************************/

/*** initialize a digital node ***/
void create_digital_node(DIGITAL_NODE *dnode, PARSENODE *name)
  {
  dnode->value=bigint_from_int(ValueU);
  dnode->pending=bigint_from_int(ValueU);
  dnode->event_index=-1;
  dnode->event_time=0;
  dnode->group=0;
  dnode->level=0;
  dnode->event_level=0;
  dnode->watch=0;
  dnode->log=0;
  dnode->breakpt=0;
  dnode->unstab=0;
  dnode->effective=0;
  dnode->string=0;
  dnode->has_digital_fanin=0;
  dnode->has_env_fanin=0;
  dnode->has_analog_fanin=0;
  dnode->tcount=0;
  dnode->Count[0]=dnode->Count[1]=0;
  dnode->UCount[0]=dnode->UCount[1]=0;
  dnode->Nrules[0]=dnode->Nrules[1]=0;
  dnode->rules[0]=dnode->rules[1]=NULL;
  dnode->name=name;
  dnode->enabler_node=NULL;
  dnode->enabler_tcount=0;
  dnode->Ngmas=0;
  dnode->gmas=NULL;
#ifdef SST2
  dnode->fiber=NULL;
  dnode->transaction=NULL;
#endif
  }

/*** free a node ***/
void free_digital_node(DIGITAL_NODE *dnode)
  {
  if (dnode->rules[0]!=NULL) leak_free(dnode->rules[0]);
  if (dnode->rules[1]!=NULL) leak_free(dnode->rules[1]);
  if (dnode->gmas!=NULL)     leak_free(dnode->gmas);
  dnode->name=NULL;
  dnode->rules[0]=NULL;
  dnode->rules[1]=NULL;
  dnode->gmas=NULL;
  }

/*** reallocate memory for rules dependencies ***/
DIGITAL_RULE **realloc_rules(DIGITAL_RULE **drules, int N)
  {
  if (drules==NULL) drules=(DIGITAL_RULE **) leak_malloc(N*sizeof(DIGITAL_RULE *));
  else drules=(DIGITAL_RULE **) leak_realloc(drules,N*sizeof(DIGITAL_RULE *));
  return drules;
  }

/*** add a new conjunctive production rule to the system ***/
void create_digital_rule(DIGITAL_RULE *drule,
                         int Nguards, DIGITAL_NODE **guards, int *sense,
                         DIGITAL_NODE *target, int dir,
                         int delay, int flags, int level)
  {
  int j,k,s,value;
  DIGITAL_NODE *dnode;
  if (flags & FLAG_ENV) level=LEVEL_ENV;
  drule->dir=dir;
  drule->delay=delay;
  drule->level=level;
  drule->target=target;
  drule->target->has_digital_fanin=1;
  if (level==LEVEL_ENV) drule->target->has_env_fanin=1;
  drule->target->level |= level;
  drule->instant = (flags & FLAG_INSTANT) !=0;
  drule->timed = (flags & FLAG_TIMED) !=0;
  drule->isochronic = (flags & FLAG_ISOCHRONIC) !=0;
  drule->realtime = (flags & FLAG_REALTIME) !=0;
  if      (flags & FLAG_METASTAB) drule->unstab=2;
  else if (flags & FLAG_UNSTAB)   drule->unstab=1;
  else                            drule->unstab=0;
  drule->FalseConjuncts=0;
  drule->UnknownConjuncts=0;
  assert(Nguards<256);
  for (j=0; j<Nguards; j++)
    {
    dnode=guards[j];
    s=sense[j];
    for (k=0; k<dnode->Nrules[s]; k++) if (dnode->rules[s][k]==drule) break;
    if (k<dnode->Nrules[s]) continue; /* don't put in duplicate dependencies */
    dnode->Nrules[s]++;
    dnode->rules[s]=realloc_rules(dnode->rules[s],dnode->Nrules[s]);
    dnode->rules[s][dnode->Nrules[s]-1]=drule;
    value=trivalue(dnode->value);
    if       (value==ValueU)          drule->UnknownConjuncts++;
    else if ((value==Value0)&&(s==1)) drule->FalseConjuncts++;
    else if ((value==Value1)&&(s==0)) drule->FalseConjuncts++;
    }
  drule->old=RuleOFF;
  if      ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts==0))
    {drule->new=RuleON; drule->target->Count[dir]++;}
  else if ((drule->FalseConjuncts==0)&&(drule->UnknownConjuncts>0))
    {drule->new=RuleU; drule->target->UCount[dir]++;}
  else drule->new=RuleOFF;
  if (drule->new!=drule->old) fire_rule(NULL,drule);
  }

/*** free a rule ***/
void free_digital_rule(DIGITAL_RULE *drule){}

/*** reallocate memory for gmas dependencies ***/
DIGITAL_GMA **realloc_gmas(DIGITAL_GMA **gmas, int N)
  {
  if (gmas==NULL) gmas=(DIGITAL_GMA **) leak_malloc(N*sizeof(DIGITAL_GMA *));
  else gmas=(DIGITAL_GMA **) leak_realloc(gmas,N*sizeof(DIGITAL_GMA *));
  return gmas;
  }

/*** create a gma ***/
void create_digital_gma(DIGITAL_GROUP *groups,
                        DIGITAL_GMA *gma,
                        LIST *guard, LIST *nodes, LIST *lgrps,
                        LIST *delays, LIST *flags,
                        LIST *indices, LIST *expressions,
                        LIST *fanin_nodes,
                        int level)
  {
  int j,k;
  DIGITAL_NODE *dnode;
  DIGITAL_GROUP *dgrp;

  // set flags and lists
  gma->level=level;
  gma->enabled=0;
  gma->guard=guard;
  gma->nodes=nodes;
  gma->groups=lgrps;
  gma->delays=delays;
  gma->flags=flags;
  gma->indices=indices;
  gma->expressions=expressions;
  
  // create dependencies for fanin nodes
  for (j=0; j<fanin_nodes->max; j++)
    {
    dnode = fanin_nodes->p.dnode[j];
    for (k=0; k<dnode->Ngmas; k++) { if (dnode->gmas[k]==gma) continue; }
    dnode->gmas=realloc_gmas(dnode->gmas,dnode->Ngmas+1);
    dnode->gmas[dnode->Ngmas]=gma;
    dnode->Ngmas++;
#ifdef VERBOSE
    fprintf(dsim_err,"Adding gma %p to dependencies of node %s.\n",
            gma,get_digital_node_name(dnode));
#endif
    }

  // process lvalue nodes
  for (j=0; j<gma->nodes->max; j++)
    {
    dnode = gma->nodes->p.dnode[j];
    if (dnode==NULL) continue;
    // set level of target node
    if (gma->flags->p.i[j]&FLAG_ENV) dnode->has_env_fanin=1;
    else                             dnode->level |= level;
    dnode->has_digital_fanin=1;
    if (level==LEVEL_ENV) dnode->has_env_fanin=1;
    // set dnode->string flag if driven by a GMA with a string expression
    if (bigint_expression_is_string(expressions->p.list[j])) dnode->string=1;
    }

  // process lvalue groups
  for (j=0; j<gma->groups->max; j++)
    {
    dgrp = gma->groups->p.dgrp[j];
    if (dgrp==NULL) continue;
    for (k=0; k<dgrp->nodes->max; k++)
      {
      dnode=dgrp->nodes->p.dnode[k];
      // set level of target node
      dnode->level |= level;
      // set dnode->string flag if driven by a GMA with a string expression
      if (bigint_expression_is_string(expressions->p.list[j])) dnode->string=1;
      }
    }

  // fire it up
  fire_gma(groups,NULL,gma);
  }

/*** free a gma ***/
void free_digital_gma(DIGITAL_GMA *gma)
  {
  int k;
  list_free(gma->nodes);
  free_bigint_expression(gma->guard,0,0,0,0,0,0);
  for (k=0; k<gma->delays->max; k++)
    free_bigint_expression(gma->delays->p.list[k],0,0,0,0,0,0);
  for (k=0; k<gma->indices->max; k++)
    free_bigint_expression(gma->indices->p.list[k],0,0,0,0,0,0);
  for (k=0; k<gma->expressions->max; k++)
    free_bigint_expression(gma->expressions->p.list[k],0,0,0,0,0,0);
  list_free(gma->delays);
  list_free(gma->flags);
  list_free(gma->indices);
  list_free(gma->expressions);
  }

/*** create a digital group ***/
void create_digital_group(DIGITAL_GROUP *group, PARSENODE *name)
  {
  group->name=name;
  group->nodes=NULL;
  }

/*** free a digital group ***/
void free_digital_group(DIGITAL_GROUP *group)
  {
  if (group->nodes!=NULL) list_free(group->nodes);
  group->nodes=NULL;
  }

/*** re-initialize digital simulator state ***/
void digital_initialize(int Ndnodes, DIGITAL_NODE *dnodes, DIGITAL_GROUP *groups)
  {
  int j;
  DIGITAL_NODE *dnode;

  // reset some settings
  allocate_history(0);
  warnall=0;
  ignoreerror=1;

  // set all digital nodes to BigU (preserves Count's)
  for (j=0; j<Ndnodes; j++)
    {
    dnode=&dnodes[j];
    dnode->watch=0;
    dnode->log=0;
    dnode->breakpt=0;
    set_digital_node(groups,dnode,BigU);
    }

  // clear dynamic digital node fields
  for (j=0; j<Ndnodes; j++)
    {
    dnode=&dnodes[j];
    dnode->tcount=0;
    dnode->unstab=0;
    dnode->event_level=0;
    dnode->event_index=-1;
    dnode->event_time=0;
    dnode->effective=0;
    }

  // reset other settings
  Nevents=0;
  digital_time=0;
  warnall=1;
  random_order=0;
  analog=ANALOG_ENABLED;
  ignoreerror=1;
  interrupt_sim=0;
  ERROR=0;
  slow_delay=4.0/3;
  fast_delay=2.0/3;
  }
