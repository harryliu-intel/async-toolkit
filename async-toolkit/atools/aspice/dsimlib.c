/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/****************************** GLOBALS ******************************/
char *prefix;
LIST *bumpCC,*bumpTau; /* alint sweeps */
LIST *delayCC,*delayTau,*delayFast,*delayCap; /* alint sweeps */
LIST *threshCC,*threshTau,*threshPercent; /* alint sweeps */
LIST *CustomScenarios;  /* list of SCENARIO -- user specified scenarios for alint */
int doLeakage=0; /* enable alint leakage simulations */
int doInverterLeakage=0; /* enable alint leakage simulations of inverters */
int monte_carlo_seed=0; /* set > 0 to enable monte-carlo simulation */

/*********************** ASIM commands, usage, help ****************************/
char *command[][3]=
  {
    // general UI commands
    {"help","[command]",
     "List all commands.  If a command is specified, give detailed help on\n"
     "that command."},
    {"settings","",
     "Reports current values of simulation modes and settings."},
    {"echo","[text]",
     "Echo text to stdout."},
    {"comment","[text]",
     "Ignores this command line."},

    // files
    {"source","\"file\"",
     "Include commands from the specified file."},
    {"output","[\"file\"]",
     "Redirect stdout to the specified file.  If none given, redirect back\n"
     "to stdout."},
    {"outerr","[\"file\"]",
     "Redirect stderr to the specified file.  If none given, redirect back\n"
     "to stderr.  Does not effect parsing errors."},
    {"workdir","[\"dir\"]",
     "Create names/trace files in the specified directory.\n"},

    // set/get
    {"initialize","",
     "Reinitialize all state and settings of the digital simulator."},
    {"with","[prefix]",
     "Change the default prefix applied to all subsequent command nodelists.\n"
     "Clears the prefix if none given."},
    {"set","nodelist:value",
     "Set nodes to the specified value."},
    {"get","nodelist[:value]",
     "Get node values.  If a value is given, only list nodes which have\n"
     "that value."},
    {"get_named","nodelist[:value]",
     "Get values of nodes with user names.  If a value is given, only list\n"
     "nodes which have that value."},
    {"status","value",
     "List all nodes which have a specified value."},

    // watching and logging
    {"watch","nodelist",
     "Start watching all transitions on specified nodes."},
    {"nowatch","nodelist",
     "Stop watching transitions on specified nodes."},
    {"log_sst2","[\"dir\"]",
     "Create an SST2 logging database in the specified directory.  If none\n"
     "given, close any open database"},
    {"log","nodelist",
     "Start logging transitions of nodes to SST2 database."},
    {"nolog","nodelist",
     "Stop logging transitions of nodes to SST2 database."},
    {"warnall","",
     "Print warnings for all simulation errors."},
    {"nowarnall","",
     "Disable warnings for simulation errors.  Especially useful during\n"
     "reset."},

    // simulation start/stop
    {"cycle","[timestep]",
     "Cycle until a breakpoint or error.  If timestep is specified, also\n"
     "halts after that amount of time."},
    {"ignoreerror","",
     "Don't halt simulation on errors.  Especially useful during reset."},
    {"noignorerror","",
     "Halt simulation on errors."},
    {"breakpt","nodelist [:tcount]",
     "Set or clear a breakpoint on a set of nodes.  If the tcount is 0,\n"
     "clears breakpoints.  If the tcount is positive, will store that in each\n"
     "nodes' breakpoint counter.  Each time a node transitions, it\n"
     "decrements its breakpoint counter.  When the counter transitions from\n"
     "1 to 0, the breakpoint occurs and stops the simulation.  The tcount\n"
     "defaults to 1.  Breakpoints are always cleared after they fire."},
    {"showbreakpts","",
     "Display all nodes which have a nonzero breakpoint counter."},

    // timing mode
    {"norandom","",
     "Select norandom timing mode.  All transitions occur after their\n"
     "nominal delays."},
    {"random","[seed]",
     "Select random timing mode.  Can specify random number seed."},
    {"timed_random","[seed]",
     "Select timed_random mode.  Can specify random number seed."},
    {"timed_delay","[fast slow]",
     "Adjust jitter in timed_random mode.  Fast and slow are floating point\n"
     "values around 1.  Delay will be uniformly distributed between fast and\n"
     "slow times the nominal delay.  If no fast or slow given, displays the\n"
     "current settings."},

    // miscellaneous actions and information
    {"clear_tcounts","",
     "Clear all node transition counts back to 0."},
    {"aliases","nodelist",
     "List all aliases of the given nodes."},
    {"subnets","nodelist",
     "List all resistive subnets of the given nodes."},
    {"fanout","nodelist",
     "List all targets of production rules gated by these nodes."},
    {"fanin","nodelist",
     "List all nodes which gate production rules with the specified nodes\n"
     "as targets."},
    {"drivers","nodelist",
     "List all nodes which gate currently enabled production rules with\n"
     "the specified nodes as targets."},
    {"pending","nodelist",
     "Report all pending events in the event queue, including which level\n"
     "enqueued them if they are waiting for the other cosim level."},
    {"floating_unstab_is_U","[0|1]",
     "If 1, a PRS that goes unstable and left floating will be set to U.\n"
     "If 0, it will keep its current value."},

    // trace buffer
    {"trace","[EventsPerNode]",
     "Enable a trace buffer holding recent events on all nodes.  This is\n"
     "needed by the history and critical commands.  The EventsPerNode scales\n"
     "how big a buffer to allocate, and defaults to 10 events per digital\n"
     "node overall."},
    {"history","nodelist",
     "Report the recent history of specified nodes using the trace buffer."},
    {"critical","nodelist",
     "Report the critical path from specified nodes using the trace buffer."},

    // alint
    {"bumpCC","[list of 0|1]",
     "Set alint sweep options for cap-coupling in bump scenarios."},
    {"bumpTau","[list of tau]",
     "Set alint sweep options for input taus in bump scenarios."},
    {"delayCC","[list of 0|1]",
     "Set alint sweep options for cap-coupling in delay scenarios."},
    {"delayTau","[list of tau]",
     "Set alint sweep options for input taus in delay scenarios."},
    {"delayFast","[list of 0|1]",
     "Set alint sweep options for fast/slow assumptions in delay scenarios."},
    {"delayCap","[list of cap]",
     "Set alint sweep options for output load capacitances in delay scenarios."},
    {"threshCC","[list of 0|1]",
     "Set alint sweep options for cap-coupling in thresh scenarios."},
    {"threshTau","[list of tau]",
     "Set alint sweep options for input taus in thresh scenarios."},
    {"threshPercent","[list of percents]",
     "Set alint sweep options for threshold characterization input step height "
     "as a percentage of true."},
    {"leakage","[0|1]",
     "Disable or enable alint leakage simulations."},
    {"inverterLeakage","[0|1]",
     "Disable or enable alint leakage simulations of inverters."},
    {"max_bump_fanin_aggressors","integer",
     "Set maximum number of fanins which can switch simultaneously\n"
     "for alint bump scenarios."},
    {"max_delay_fanin_aggressors","integer",
     "Set maximum number of fanins which can switch simultaneously\n"
     "for alint delay scenarios."},
    {"scenario","[:delay|:bump|:leak] [:up|:dn] fanin1[=0|=1|+|-] fanin2[=0|=1|+|-] ...",
     "Specify another custom scenario for alint.  The scenario is described\n"
     "by a list of fanin nodes with an initial state or aggressor direction.\n"
     "Unspecified fanins are recursively enumerated.  An empty fanin list\n"
     "does default scenario enumeration.  The scenario list is cleared\n"
     "after the alint command.  Optionally restrict this scenario to a given\n"
     "simulation type (delay, bump, leak) and a given victim direction (up, dn).\n"
     "Globbing is supported in the fanins.  If the same fanin occurs multiple\n"
     "times, only the last one takes effect."},
    {"leaky","nodelist",
     "Declares analog nodes to be more leaky than default.  Used in alint\n"
     "leakage simulations."},
    {"leaky_file","\"filename\"",
     "Reads a list of leaky nodes from a file."},
    {"alint","nodelist [:force]",
     "Run an alint simulation on specified nodes using current alint sweep\n"
     "settings.  Will run simulations for all combinations of bumpTau\n"
     "versus bumpCC and delayTau versus delayCC versus delayFast versus\n"
     "delayCap and threshTau versus threshCC versus threshPercent, plus leakage.\n\n"
     "If custom scenarios have been specified, will simulate only those\n"
     "scenarios.  Otherwise, it will simulate all reasonably interesting\n"
     "scenarios.  Any custom scenarios are cleared after the alint command.\n"
     "If the force keyword is used, alint will simulate custom scenarios\n"
     "even if they are interfering or otherwise driven the wrong way."},

    // marked subcircuit
    {"mark_node","nodelist",
     "Marks nodes of interest for marked_subcircuit."},
    {"mark_barrier","nodelist",
     "Marks nodes as barriers that block recursion of mark_fanin."},
    {"mark_fanin","[levels]",
     "Recursively marks the fanin of marked nodes until barrier nodes\n"
     "are encountered.  Optionally limit the depth of recursiont to levels.\n"
     "Emit results with marked_subcircuit."},
    {"marked_subcircuit","[\"basefilename\"]",
     "Writes out the marked subcircuit, including all gate loads and\n"
     "capacitive coupling nets.  Optionally writes files to disk using\n"
     "the base filename provided."},
    {"unmark_all","",
     "Clears all marked nodes for marked_subcircuit."},
    {"unmark_node","",
     "Clears specified marked nodes for marked_subcircuit."},

    // aspice
    {"force","nodelist [:[v|t|f|u]]",
     "Force analog nodes to specified voltage if given, otherwise stop forcing.\n"
     "Can use t,f,u to force to digital voltage levels, which also schedules/sets\n"
     "the digital node, if any, depending on wheather analog is on/off."},
    {"probe","nodelist",
     "Probe voltage of analog nodes."},
    {"prsTau","tau",
     "Set switching time constant of forced or digital transitions."},
    {"lumpCap","nodelist :C",
     "Add C farads to lumped capacitance on analog nodes."},
    {"analog","[on|off]",
     "If turned on, enables normal analog/digital cosimulation.  If turned\n"
     "off, allows circuit to become all digital temporarily, allowing you to\n"
     "advance the simulation to an interesting period and turn analog back\n"
     "on then.  Don't turn this off in digital vs digital cosimulation."},

    // NULL terminate
    {NULL,NULL,NULL}
  };

/****************************** HANDLE NODE NAMES ******************************/

/*** add dnode to node set if its not already there ***/
void add_dnode(LIST *list, DIGITAL_NODE *pn)
  {
  int j;
  j=find_element_lazy_sort(list,&pn,&dnode_cmp);
  if (j<0) list_insert_element_lazy_sort(list,&pn,&dnode_cmp);
  }

/****************************** PARSE USER INTERFACE ******************************/

/*** parse a node value ***/
BIGINT *parse_value(LEX *lex)
  {
  BIGINT *value;
  if (lex_eatif_sym(lex,"u") || lex_eatif_sym(lex,"U")) return bigint_from_int(ValueU);
  lex_push_position(lex);
  lex_do_integer(lex);
  value = parse_bigint(lex_get_token(lex));
  lex_pop_position(lex);
  return value;
  }

/*** handle a bad command ***/
void bad_command(LEX *lex, char *msg)
  {
  lex_warning(lex,msg);
  lex_eat_until(lex,"\n");
  }

/*** report a bad filename ***/
void bad_file(LEX *lex, char *name)
  {
  lex_file_warning(lex,name);
  lex_eat_until(lex,"\n");
  }

/*** report a bad node name ***/
void bad_node(LEX *lex, char *name)
  {
  char msg[STRMAX];
  safe_sprintf(msg,"node %s undefined\n",name);
  lex_message(lex,"WARNING",msg);
  }

/*** convert analog node name to pointer **/
NODE *parse_anode(CIRCUIT *circuit, LEX *lex)
  {
  char *name;
  NODE *pn;
  name=parse_nodename(lex);
  pn=find_node(circuit,prefix,name);
  leak_free(name);
  return pn;
  }

/*** convert node name to pointer ***/
DIGITAL_GROUP *parse_digital_group(CIRCUIT *circuit, LEX *lex)
  {
  char *name;
  DIGITAL_GROUP *dgrp;
  lex_do_whitespace(lex);
  lex_push_position(lex);
  lex_eat_until(lex," \t\n:");
  name=lex_get_token(lex);
  if (name[0]==0) {lex_restore_position(lex); return NULL;}
  dgrp=find_digital_group(circuit,prefix,name);
  lex_pop_position(lex);
  return dgrp;
  }

/*** parse a list of digital nodes, possibly using globbing ***/
LIST *parse_dnode_list(CIRCUIT *circuit, LEX *lex)
  {
  LIST *args;
  PARSENODE *pname;
  DIGITAL_NODE *pn;
  DIGITAL_GROUP *dgrp;
  char *glob;
  int j;
  char fullglob[STRMAX];
  args=list_create(sizeof(DIGITAL_NODE *));
  do
    {
    lex_do_whitespace(lex);
    lex_push_position(lex);
    /*** look for a group ***/
    dgrp=parse_digital_group(circuit,lex);
    if (dgrp!=NULL) /*** found a group ***/
      {
      lex_pop_position(lex);
      for (j=0; j<dgrp->nodes->max; j++)
        {
        pn=dgrp->nodes->p.dnode[j];
        if (find_element_lazy_sort(args,&pn,&dnode_cmp)<0)
          list_insert_element_lazy_sort(args,&pn,&dnode_cmp);
        }
      }
    else /*** look for a glob ***/
      {
      lex_restore_position(lex);
      glob=lex_eat_until(lex,"\n :\t");
      if (is_glob(glob)) for (j=0; j<circuit->names->max; j++)
        {
        pname = circuit->names->p.parsenode[j];
        if ((pname->dnum<0)||circuit->names->p.parsenode[j]->group_used) continue;
        pn=&circuit->dnodes[pname->dnum];
        safe_sprintf(fullglob,"%s%s",prefix,glob);
        if (glob_matches(fullglob,pname->name))
          if (find_element_lazy_sort(args,&pn,&dnode_cmp)<0)
            list_insert_element_lazy_sort(args,&pn,&dnode_cmp);
        }
      else /*** look for exact node name ***/
        {
        pn=find_digital_node(circuit,prefix,glob);
        if (pn==NULL) bad_node(lex,glob);
        else if (find_element_lazy_sort(args,&pn,&dnode_cmp)<0)
          list_insert_element_lazy_sort(args,&pn,&dnode_cmp);
        }
      }
    } while (!lex_is_eof(lex)&&!lex_is_sym(lex,":"));
  list_finish_lazy_sort(args,&dnode_cmp);
  return args;
  }
	
/***
 * Parse a list of analog nodes, possibly using globbing.  Assumes
 * each whitespace separated field may be a glob or node name.  Stops
 * at a : preceeded by whitespace, or the -+= symbols without
 * whitespace.
 ***/
LIST *parse_anode_list(CIRCUIT *circuit, LEX *lex, int ignoreBadNode)
  {
  LIST *args;
  PARSENODE *pname;
  NODE *pn;
  char *glob;
  int j;
  char fullglob[STRMAX];
  args=list_create(sizeof(NODE *));
  do
    {
    lex_do_whitespace(lex);
    glob=lex_eat_until(lex,"\n \t=-+");
    if (is_glob(glob)) for (j=0; j<circuit->names->max; j++)
      {
      pname = circuit->names->p.parsenode[j];
      if (pname->num<0) continue;
      pn=&circuit->nodes[pname->num];
      safe_sprintf(fullglob,"%s%s",prefix,glob);
      if (glob_matches(fullglob,pname->name))
        if (find_element_lazy_sort(args,&pn,&node_cmp)<0)
          list_insert_element_lazy_sort(args,&pn,&node_cmp);
      }
    else
      {
      pn=find_node(circuit,prefix,glob);
      if (pn==NULL) {if (!ignoreBadNode) bad_node(lex,glob);}
      else if (find_element_lazy_sort(args,&pn,&node_cmp)<0)
        list_insert_element_lazy_sort(args,&pn,&node_cmp);
      }
    } while (!lex_is_eof(lex)&&!lex_is_sym(lex,":")&&
             !lex_is_sym(lex,"+")&&!lex_is_sym(lex,"-")&&!lex_is_sym(lex,"="));
  list_finish_lazy_sort(args,&node_cmp);
  return args;
  }
	
/*** parse a list of PARSENODEs, possibly using globbing ***/
LIST *parse_name_list(CIRCUIT *circuit, LEX *lex)
  {
  LIST *args;
  PARSENODE *pname;
  char *glob;
  int j;
  char fullglob[STRMAX];
  args=list_create(sizeof(PARSENODE *));
  do
    {
    lex_do_whitespace(lex);
    glob=lex_eat_until(lex,"\n \t");
    if (is_glob(glob)) for (j=0; j<circuit->names->max; j++)
      {
      pname = circuit->names->p.parsenode[j];
      safe_sprintf(fullglob,"%s%s",prefix,glob);
      if (glob_matches(fullglob,pname->name))
        if (find_element_lazy_sort(args,&pname,&pparsenodecmp)<0)
          list_insert_element_lazy_sort(args,&pname,&pparsenodecmp);
      }
    else
      {
      pname=find_parsenode(circuit,prefix,glob);
      if (pname==NULL) bad_node(lex,glob);
      else if (find_element_lazy_sort(args,&pname,&pparsenodecmp)<0)
        list_insert_element_lazy_sort(args,&pname,&pparsenodecmp);
      }
    } while (!lex_is_eof(lex)&&!lex_is_sym(lex,":"));
  list_finish_lazy_sort(args,&pparsenodecmp);
  return args;
  }

/** parse comma separated integers into an existing list **/
void parse_int_list(LEX *lex, LIST *list)
  {
  int x;
  list_realloc(list,0);
  while (lex_is_real(lex))
    {
    x = lex_eat_integer(lex);
    list_append_element(list,&x);
    lex_eatif_sym(lex,",");
    }
  }

/** parse comma separated real numbers into an existing list **/
void parse_real_list(LEX *lex, LIST *list)
  {
  double x;
  list_realloc(list,0);
  while (lex_is_real(lex))
    {
    x = lex_eat_real(lex);
    list_append_element(list,&x);
    lex_eatif_sym(lex,",");
    }
  }

/** print comma separated list of integers **/
void print_int_list(char *name, LIST *list)
  {
  int k;
  fprintf(dsim_out,"%s ",name);
  for (k=0; k<list->max; k++)
    fprintf(dsim_out,"%d%s",list->p.i[k],k+1<list->max ? "," : "");
  fprintf(dsim_out,"\n");
  }

/** print comma separated list of reals **/
void print_real_list(char *name, LIST *list)
  {
  int k;
  fprintf(dsim_out,"%s ",name);
  for (k=0; k<list->max; k++)
    fprintf(dsim_out,"%g%s",list->p.f[k],k+1<list->max ? "," : "");
  fprintf(dsim_out,"\n");
  }

/*** compare FANIN's by pn only ***/
int fanin_node_cmp(void *p1, void *p2)
  {
  FANIN *pf1 = ((FANIN *) p1), *pf2 = ((FANIN *) p2);
  return node_cmp(&pf1->pn,&pf2->pn);
  }

/*** handle user commands ***/
void parse_user(char *line, CIRCUIT *circuit)
  {
  int j,k,l,dnum;
  FILE *newfile;
  LEX *newlex;
  char *filename,*str;
  DIGITAL_NODE *pn,*pn2;
  BIGINT *value;
  LIST *fan,*args;
  LEX *lex;

  lex=lex_string(line);
  while(!lex_is_eof(lex))
    {
    if (lex_eatif_keyword(lex,"initialize"))
      digital_initialize(circuit->Ndnodes,circuit->dnodes,circuit->groups);
    else if (lex_eatif_keyword(lex,"set"))
      {
      args=parse_dnode_list(circuit,lex);
      if ((args->max>0)&&lex_eatif_sym(lex,":"))
        {
        value = parse_value(lex);
        for (j=0; j<args->max; j++)
          set_digital_node(circuit->groups,args->p.dnode[j],value);
        free_bigint(value);
        }
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"get"))
      {
      args=parse_dnode_list(circuit,lex);
      if (lex_eatif_sym(lex,":")) value = parse_value(lex);
      else value = NULL;
      for (j=0; j<args->max; j++)
        if ((value==NULL) || (compare_bigint(args->p.dnode[j]->value,value)==0))
          print_dnode(dsim_out,args->p.dnode[j]);
      if (value!=NULL) free_bigint(value);
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"get_named"))
      {
      args=parse_dnode_list(circuit,lex);
      if (lex_eatif_sym(lex,":")) value = parse_value(lex);
      else value = NULL;
      for (j=0; j<args->max; j++)
        if ((value==NULL) || (compare_bigint(args->p.dnode[j]->value,value)==0))
          if (args->p.dnode[j]->name->unnamed==0)
            print_dnode(dsim_out,args->p.dnode[j]);
      if (value!=NULL) free_bigint(value);
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"status"))
      {
      value = parse_value(lex);
      for (j=0; j<circuit->names->max; j++)
        {
        dnum = circuit->names->p.parsenode[j]->dnum;
        if (circuit->names->p.parsenode[j]->proot!=NULL) continue;
        if (dnum<0) continue;
        pn=&circuit->dnodes[dnum];
        if (compare_bigint(pn->value,value)==0) print_dnode(dsim_out,pn);
        }
      free_bigint(value);
      }
    else if (lex_eatif_keyword(lex,"cycle"))
      {
      if (lex_is_integer(lex)) j=lex_eat_integer(lex);
      else j=-1;
      if ((circuit->Nnodes==0)||(analog==ANALOG_DISABLED))
        pn=cycle(circuit->groups,j); /* pure digital */
      else /* mixed analog/digital simulation */
        {
        double old_timemax = options.timemax; // save timemax
        unsigned endtime = digital_time + j;
        if (j>=0) options.timemax = endtime * options.digital_time_unit;
        pn=simulate_circuit_inner(circuit);
        if (j>=0 && pn==NULL && endtime>digital_time && !interrupt_sim)
          digital_time = endtime; // advance time precisely
        options.timemax = old_timemax; // restore timemax
        }
      circuit->time = digital_time * options.digital_time_unit;
      if (pn!=NULL) {fprintf(dsim_out,"BREAKING: "); print_dnode(dsim_out,pn);}
      flush_log(dsim_log);
      }
    else if (lex_eatif_keyword(lex,"breakpt"))
      {
      args=parse_dnode_list(circuit,lex);
      k=1;
      if (lex_eatif_sym(lex,":"))
        if (lex_is_integer(lex))
          k=lex_eat_integer(lex);
      for (j=0; j<args->max; j++) args->p.dnode[j]->breakpt=k;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"watch"))
      {
      args=parse_dnode_list(circuit,lex);
      for (j=0; j<args->max; j++) args->p.dnode[j]->watch=1;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"nowatch"))
      {
      args=parse_dnode_list(circuit,lex);
      for (j=0; j<args->max; j++) args->p.dnode[j]->watch=0;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"log"))
      {
      args=parse_dnode_list(circuit,lex);
      for (j=0; j<args->max; j++) log_dnode_on(dsim_log,args->p.dnode[j]);
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"nolog"))
      {
      args=parse_dnode_list(circuit,lex);
      for (j=0; j<args->max; j++) log_dnode_off(dsim_log,args->p.dnode[j]);
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"showbreakpts"))
      {
      for (j=0; j<circuit->Ndnodes; j++)
        if (circuit->dnodes[j].breakpt)
          fprintf(dsim_out,"%s : %d\n",
                  get_digital_node_name(&circuit->dnodes[j]),
                  circuit->dnodes[j].breakpt);
      }
    else if (lex_eatif_keyword(lex,"random"))
      {
      if (lex_is_integer(lex)) srand(lex_eat_integer(lex));
      random_order=1;
      }
     else if (lex_eatif_keyword(lex,"timed_random"))
      {
      if (lex_is_integer(lex)) srand(lex_eat_integer(lex));
      random_order=2;
      }
    else if (lex_eatif_keyword(lex,"norandom")) random_order=0;
    else if (lex_eatif_keyword(lex,"timed_delay"))
      {
      int error=0;
      if (lex_is_real(lex)) fast_delay=lex_eat_real(lex); else error=1;
      if (lex_is_real(lex)) slow_delay=lex_eat_real(lex); else error=1;
      if (error) fprintf(dsim_out,"fast_delay=%g slow_delay=%g\n",fast_delay,slow_delay);
      }
    else if (lex_eatif_keyword(lex,"warnall")) warnall=1;
    else if (lex_eatif_keyword(lex,"nowarnall")) warnall=0;
    else if (lex_eatif_keyword(lex,"ignoreerror")) ignoreerror=1;
    else if (lex_eatif_keyword(lex,"noignoreerror")) ignoreerror=0;
    else if (lex_eatif_keyword(lex,"floating_unstab_is_U"))
      {
      if (lex_is_integer(lex)) floating_unstab_is_U=lex_eat_integer(lex);
      else fprintf(dsim_out,"floating_unstab_is_U=%d\n",floating_unstab_is_U);
      }
    else if (lex_eatif_keyword(lex,"clear_tcounts"))
      {
      for (j=0; j<circuit->Ndnodes; j++) circuit->dnodes[j].tcount=0;
      }
    else if (lex_eatif_keyword(lex,"source"))
      {
      if (!lex_is_quote(lex)) bad_command(lex,"quoted filename");
      else
	{
        str=lex_eat_quote(lex);
        filename=find_filename(str,"",".:$ASPICE_PATH");
        newfile=fopen(filename,"r");
        if (newfile==NULL) bad_file(lex,str);
        else
          {
          newlex=lex_file_with_name(newfile,filename);
          while(!lex_is_eof(newlex)) parse_user(lex_eat_until(newlex,"\n"),circuit);
          lex_free(newlex);
          leak_free(filename);
          fclose(newfile);
          }
	}
      }
    else if (lex_eatif_keyword(lex,"output"))
      {
      if (dsim_out!=stdout) fclose(dsim_out);
      dsim_out = stdout;
      if (lex_is_quote(lex))
        {
        filename=lex_eat_quote(lex);
        dsim_out=mkdir_fopen(filename,"wt");
        if (dsim_out==NULL)
          {
          bad_file(lex,filename);
          dsim_out=stdout;
          }
        }
      }
    else if (lex_eatif_keyword(lex,"outerr"))
      {
      if (dsim_err!=stderr) fclose(dsim_err);
      dsim_err = stderr;
      if (lex_is_quote(lex))
        {
        filename=lex_eat_quote(lex);
        dsim_err=mkdir_fopen(filename,"wt");
        if (dsim_err==NULL)
          {
          bad_file(lex,filename);
          dsim_err=stderr;
          }
        }
      }
    else if (lex_eatif_keyword(lex,"workdir"))
      {
      if (!lex_is_quote(lex)) bad_command(lex,"quoted directory name");
      else
        {
        leak_free(WorkDir);
        WorkDir = leak_strdup(lex_eat_quote(lex));
        }
      }
    else if (lex_eatif_keyword(lex,"log_sst2"))
      {
      if (dsim_log!=NULL)
        stop_log(dsim_log,circuit->Ndnodes,circuit->dnodes);
      dsim_log = NULL;
      if (lex_is_quote(lex))
        {
        filename=lex_eat_quote(lex);
        dsim_log=start_log(filename,circuit->Ndnodes,circuit->dnodes);
        if (dsim_log==NULL) bad_file(lex,filename);
        }
      }
    else if (lex_eatif_keyword(lex,"echo"))
      fprintf(dsim_out,"%s\n",lex_eat_until(lex,"\n"));
    else if (lex_eatif_keyword(lex,"comment")) lex_eat_until(lex,"\n");
    else if (lex_eatif_keyword(lex,"aliases"))
      {
      args=parse_name_list(circuit,lex);
      for (k=0; k<args->max; k++)
	{
        PARSENODE *pname[2];
        pname[1] = pname[0] = args->p.parsenode[k];
        fprintf(dsim_out,"Aliases of %s:\n",pname[0]->name);
        do
          {
          fprintf(dsim_out,"  %s\n",pname[1]->name);
          pname[1] = pname[1]->palias;
          } while (pname[1]!=pname[0]);
        }
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"subnets"))
      {
      args=parse_anode_list(circuit,lex,0);
      for (k=0; k<args->max; k++)
        {
        NODE *seed = args->p.pn[k]->seed;
        fprintf(dsim_out,"Subnets of %s:\n",get_node_name(seed));
        for (j=0; j<circuit->Nnodes; j++)
          if (circuit->nodes[j].seed==seed)
            fprintf(dsim_out,"  %s\n",get_node_name(&circuit->nodes[j]));
        }
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"fanout"))
      {
      args=parse_dnode_list(circuit,lex);
      for (k=0; k<args->max; k++)
	{
        pn=args->p.dnode[k];
        fprintf(dsim_out,"Fanout of %s:\n",get_digital_node_name(pn));
	if (pn!=NULL)
	  {
	  fan=list_create(sizeof(DIGITAL_NODE *));
	  for (j=0; j<pn->Nrules[0]; j++) add_dnode(fan,pn->rules[0][j]->target);
	  for (j=0; j<pn->Nrules[1]; j++) add_dnode(fan,pn->rules[1][j]->target);
	  for (j=0; j<fan->max; j++)
            fprintf(dsim_out,"  %s\n",get_digital_node_name(fan->p.dnode[j]));
	  list_free(fan);
	  }
	}
      list_free(args);
      }
    else if (lex_is_keyword(lex,"drivers")||lex_is_keyword(lex,"fanin"))
      {
      int drivers=0;
      if      (lex_eatif_keyword(lex,"drivers")) drivers=1;
      else if (lex_eatif_keyword(lex,"fanin"))   drivers=0;
      args=parse_dnode_list(circuit,lex);
      for (l=0; l<args->max; l++)
	{
        pn=args->p.dnode[l];
        fprintf(dsim_out,"%s of %s:\n",drivers ? "Drivers" : "Fanin",
                get_digital_node_name(pn));
        if (pn!=NULL)
	  {
          fan=list_create(sizeof(DIGITAL_NODE *));
          for (j=0; j<circuit->Ndnodes; j++)
	    {
            pn2=&circuit->dnodes[j];
	    for (k=0; k<pn2->Nrules[0]; k++)
              if (pn2->rules[0][k]->target==pn)
		if ((!drivers) || (pn2->rules[0][k]->FalseConjuncts==0))
		  add_dnode(fan,pn2);
	    for (k=0; k<pn2->Nrules[1]; k++)
              if (pn2->rules[1][k]->target==pn)
		if ((!drivers) || (pn2->rules[1][k]->FalseConjuncts==0))
		  add_dnode(fan,pn2);
	    }
	  for (j=0; j<fan->max; j++)
            fprintf(dsim_out,"  %s\n",get_digital_node_name(fan->p.dnode[j]));
	  list_free(fan);
	  }
	}
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"pending"))
      {
      args=parse_dnode_list(circuit,lex);
      for (k=0; k<args->max; k++)
        {
        pn=args->p.dnode[k];
        if (pn->event_index<0) continue;
        print_pending_dnode(dsim_out,pn);
        }
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"with"))
      {
      leak_free(prefix);
      lex_do_whitespace(lex);
      prefix=leak_strdup(lex_eat_until(lex,"\n\t "));
      }
    else if (lex_eatif_keyword(lex,"trace"))
      {
      if (lex_is_integer(lex)) allocate_history(lex_eat_integer(lex)*circuit->Ndnodes);
      else allocate_history(10*circuit->Ndnodes);
      }
    else if (lex_eatif_keyword(lex,"history"))
      {
      unsigned last1=LAZY_TIME,last0=LAZY_TIME;
      args=parse_dnode_list(circuit,lex);
      for (k=0; k<args->max; k++)
	{
        pn=args->p.dnode[k];
        fprintf(dsim_out,"History of %s:\n",get_digital_node_name(pn));
        for (j=HistoryN-1; (j>=HistoryN-HistoryMax)&&(j>=0); j--)
	  {
          HISTORY *hist;
          hist=&History[j%HistoryMax];
          if (hist->node==pn)
            {
            fprintf(dsim_out," @%u #%d %s:",
                    hist->time,hist->tcount,get_digital_node_name(pn));
            print_value(dsim_out,hist->value,hist->node->string);
            if (hist->tcount&1)
              {
              if (last1!=LAZY_TIME) fprintf(dsim_out," (tau=%u)",last1-hist->time);
              last1=hist->time;
              }
            else
              {
              if (last0!=LAZY_TIME) fprintf(dsim_out," (tau=%u)",last0-hist->time);
              last0=hist->time;
              }
            fprintf(dsim_out,"\n");
            }
          }
	}
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"critical"))
      {
      unsigned tcount;
      args=parse_dnode_list(circuit,lex);
      for (k=0; k<args->max; k++)
	{
        pn=args->p.dnode[k];
        tcount=pn->tcount;
        fprintf(dsim_out,"Critical path for %s:\n",get_digital_node_name(pn));
        for (j=HistoryN-1; (j>=HistoryN-HistoryMax)&&(j>=0); j--)
	  {
          HISTORY *hist;
          hist=&History[j%HistoryMax];
          if ((hist->node==pn)&&(hist->tcount==tcount))
            {
            fprintf(dsim_out," @%u #%d %s:",
                    hist->time,hist->tcount,get_digital_node_name(pn));
            print_value(dsim_out,hist->value,hist->node->string);
            fprintf(dsim_out,"\n");
            pn=hist->enabler_node;
            tcount=hist->enabler_tcount;
	    }
	  }
	}
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"prsTau"))
      {
      if (lex_is_real(lex)) options.prs_tau = lex_eat_real(lex);
      }
    else if (lex_eatif_keyword(lex,"bumpCC"))
      parse_int_list(lex,bumpCC);
    else if (lex_eatif_keyword(lex,"bumpTau"))
      parse_real_list(lex,bumpTau);
    else if (lex_eatif_keyword(lex,"delayCC"))
      parse_int_list(lex,delayCC);
    else if (lex_eatif_keyword(lex,"delayFast"))
      parse_int_list(lex,delayFast);
    else if (lex_eatif_keyword(lex,"delayTau"))
      parse_real_list(lex,delayTau);
    else if (lex_eatif_keyword(lex,"delayCap"))
      parse_real_list(lex,delayCap);
    else if (lex_eatif_keyword(lex,"threshCC"))
      parse_int_list(lex,threshCC);
    else if (lex_eatif_keyword(lex,"threshTau"))
      parse_real_list(lex,threshTau);
    else if (lex_eatif_keyword(lex,"threshPercent"))
      parse_real_list(lex,threshPercent);
    else if (lex_eatif_keyword(lex,"leakage"))
      {
      if (lex_is_integer(lex)) doLeakage = lex_eat_integer(lex);
      }
    else if (lex_eatif_keyword(lex,"inverterLeakage"))
      {
      if (lex_is_integer(lex)) doInverterLeakage = lex_eat_integer(lex);
      }
    else if (lex_eatif_keyword(lex,"max_bump_fanin_aggressors"))
      {
      if (lex_is_integer(lex))
        options.max_bump_fanin_aggressors = (double) lex_eat_integer(lex);
      }
    else if (lex_eatif_keyword(lex,"max_delay_fanin_aggressors"))
      {
      if (lex_is_integer(lex))
        options.max_delay_fanin_aggressors = (double) lex_eat_integer(lex);
      }
    else if (lex_eatif_keyword(lex,"scenario"))
      {
      FANIN fanin;
      SCENARIO scenario;
      int bad=0;
      scenario.sim=SIM_ALL;
      scenario.dir=2;
      scenario.fanin = list_create(sizeof(FANIN));
      if      (lex_eatif_sym(lex,":bump"))  scenario.sim=SIM_BUMP;
      else if (lex_eatif_sym(lex,":delay")) scenario.sim=SIM_DELAY;
      else if (lex_eatif_sym(lex,":leak"))  scenario.sim=SIM_LEAK;
      if      (lex_eatif_sym(lex,":up"))    scenario.dir=1;
      else if (lex_eatif_sym(lex,":dn"))    scenario.dir=0;
      while(!lex_is_eof(lex))
        {
        args = parse_anode_list(circuit,lex,0);
        fanin.initial = 0;
        fanin.aggressor = 0;
        if      (lex_eatif_sym(lex,"=0")) {fanin.initial=0; fanin.aggressor=0;}
        else if (lex_eatif_sym(lex,"=1")) {fanin.initial=1; fanin.aggressor=0;}
        else if (lex_eatif_sym(lex,"+"))  {fanin.initial=0; fanin.aggressor=1;}
        else if (lex_eatif_sym(lex,"-"))  {fanin.initial=1; fanin.aggressor=1;}
        else {bad=1; bad_command(lex,"[=0|=1|+|-]");}
        for (j=0; j<args->max; j++)
          {
          fanin.pn=args->p.pn[j];
          k=find_element_lazy_sort(scenario.fanin,&fanin,&fanin_node_cmp);
          if (k<0) list_insert_element_lazy_sort(scenario.fanin,&fanin,&fanin_node_cmp);
          else scenario.fanin->p.fanin[k]=fanin;
          }
        list_free(args);
        }
      if (bad) list_free(scenario.fanin);
      else list_append_element(CustomScenarios,&scenario);
      }
    else if (lex_eatif_keyword(lex,"force"))
      {
      NODE *anode;
      int force = FORCE_NONE;
      BIGINT *dir = NULL;
      REAL forceV = 0;
      args = parse_anode_list(circuit,lex,0);
      if (lex_eatif_sym(lex,":"))
        {
        if      (lex_eatif_sym(lex,"f")||lex_eatif_sym(lex,"F"))
          {dir=Big0; force=FORCE_DN;}
        else if (lex_eatif_sym(lex,"t")||lex_eatif_sym(lex,"T"))
          {dir=Big1; force=FORCE_UP;}
        else if (lex_eatif_sym(lex,"u")||lex_eatif_sym(lex,"U"))
          {dir=BigU; force=FORCE_MID;}
        else if (lex_is_real(lex))
          {force=FORCE_V; forceV=lex_eat_real(lex); }
        }
      for (j=0; j<args->max; j++)
        {
        anode=args->p.pn[j];
        if (anode->fixed && force!=FORCE_NONE)
          {
          char msg[STRMAX];
          safe_sprintf(msg,"can't force fixed node %s\n",get_node_name(anode));
          lex_message(lex,"WARNING",msg);
          continue;
          }
        anode->force=force;
        anode->forceV=forceV;
        pn=anode->digital_node;
        if ((pn!=NULL)&&(dir!=NULL)) // handle digital node too
          {
          if (analog==ANALOG_ENABLED)
            schedule_event(pn,dir,LAZY_TIME,0,LEVEL_ENV,1,NULL);
          else
            set_digital_node(circuit->groups,pn,dir);
          }
        }
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"probe"))
      {
      args=parse_anode_list(circuit,lex,0);
      for (j=0; j<args->max; j++)
        fprintf(dsim_out," @%d %s %g\n",
                digital_time,get_node_name(args->p.pn[j]),args->p.pn[j]->V);
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"lumpCap"))
      {
      double CG=0;
      args = parse_anode_list(circuit,lex,0);
      if (lex_eatif_sym(lex,":")) if (lex_is_real(lex)) CG = lex_eat_real(lex);
      for (j=0; j<args->max; j++) args->p.pn[j]->CG+=CG;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"leaky"))
      {
      args = parse_anode_list(circuit,lex,0);
      for (j=0; j<args->max; j++) args->p.pn[j]->leaky=1;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"leaky_file"))
      {
      if (!lex_is_quote(lex)) bad_command(lex,"quoted filename");
      else
	{
        str=lex_eat_quote(lex);
        filename=find_filename(str,"",".:$ASPICE_PATH");
        newfile=fopen(filename,"r");
        if (newfile==NULL) bad_file(lex,str);
        else
          {
          newlex=lex_file_with_name(newfile,filename);
          while (!lex_is_eof(newlex))
            {
            args = parse_anode_list(circuit,newlex,1);
            for (j=0; j<args->max; j++) args->p.pn[j]->leaky=1;
            list_free(args);
            }
          lex_free(newlex);
          leak_free(filename);
          fclose(newfile);
          }
	}
      }
    else if (lex_eatif_keyword(lex,"alint"))
      {
      int CustomForce = 0;
      args = parse_name_list(circuit,lex);
      if (lex_eatif_sym(lex,":")) CustomForce = lex_eatif_keyword(lex,"force");
      for (j=0; j<args->max; j++) if (root_alias(args->p.parsenode[j])->num>=0)
        alint(circuit,args->p.parsenode[j]->name,CustomScenarios,CustomForce);
      list_free(args);
      for (k=0; k<CustomScenarios->max; k++)
        list_free(CustomScenarios->p.scenario[k].fanin);
      list_realloc(CustomScenarios,0);
      }
    else if (lex_eatif_keyword(lex,"unmark_all"))
      {
      mark_clear(circuit);
      }
    else if (lex_eatif_keyword(lex,"unmark_node"))
      {
      args=parse_anode_list(circuit,lex,0);
      for (j=0; j<args->max; j++)
        args->p.pn[j]->seed->mark=MARK_UNKNOWN;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"mark_node"))
      {
      args=parse_anode_list(circuit,lex,0);
      for (j=0; j<args->max; j++)
        args->p.pn[j]->seed->mark=MARK_VICTIM;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"mark_barrier"))
      {
      args=parse_anode_list(circuit,lex,0);
      for (j=0; j<args->max; j++)
        args->p.pn[j]->seed->mark=MARK_BARRIER;
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"get_node_marking")) // undocumented
      {
      args=parse_anode_list(circuit,lex,0);
      for (j=0; j<args->max; j++)
        fprintf(dsim_out,"%s %s\n",get_node_name(args->p.pn[j]->seed),
                mark_name[args->p.pn[j]->seed->mark]);
      list_free(args);
      }
    else if (lex_eatif_keyword(lex,"mark_fanin"))
      {
      int levels=-1;
      if (lex_is_integer(lex)) levels=lex_eat_integer(lex);
      mark_fanin(circuit,levels);
      }
    else if (lex_eatif_keyword(lex,"marked_subcircuit"))
      {
      filename="";
      if (lex_is_quote(lex)) filename=lex_eat_quote(lex);
      marked_subcircuit(circuit,filename);
      }
    else if (lex_eatif_keyword(lex,"analog"))
      {
      int on=-1;
      if      (lex_eatif_keyword(lex,"on"))  on=1;
      else if (lex_eatif_keyword(lex,"off")) on=0;
      if (on<0) bad_command(lex,"[on|off]");
      else if ((analog==ANALOG_ENABLED) && (on==0)) // disable analog simulation
        {
        analog = ANALOG_DISABLED;
        // reschedule pending lazy digital events to non LAZY_TIME
        for (j=0; j<circuit->Ndnodes; j++)
          {
          pn=&circuit->dnodes[j];
          if ((pn->event_index>=0)&&(pn->event_time>=LAZY_TIME))
            {
            pn->event_time-=LAZY_TIME;
            if (pn->event_time<digital_time) pn->event_time=digital_time;
            reheap(pn->event_index);
            }
          }
        }
      else if ((analog==ANALOG_DISABLED) && (on==1)) // enable analog simulation
        {
        // save old simulation state
        double old_timemax = options.timemax;
        for (j=0; j<circuit->Nnodes; j++)
          circuit->nodes[j].old_force=circuit->nodes[j].force;

        // re-initialize analog node state
        for (j=0; j<circuit->Nnodes; j++)
          {
          NODE *anode;
          anode = &circuit->nodes[j];
          anode->V=anode->X=anode->smoothV=anode->smoothX = 0;
          }

        // partition circuit when all voltages are 0
        if (partition_circuit(circuit)) summarize_circuit(circuit,stdout);

        // force all seed analog nodes to digital value
        for (j=0; j<circuit->Nnodes; j++)
          {
          NODE *anode;
          anode = &circuit->nodes[j];
          if (anode->digital_node!=NULL)
            {
            if      (trivalue(anode->digital_node->value)==Value0)
              anode->seed->force=FORCE_DN;
            else if (trivalue(anode->digital_node->value)==Value1)
              anode->seed->force=FORCE_UP;
            else anode->seed->force=FORCE_MID;
            }
          }

        // set initial voltages using alint code
        set_true_false_mid();
        propagate_voltages(circuit,0,2);
        set_subnets(circuit);

        // simulate to settle internal nodes
        analog = ANALOG_SETTLING;
        options.timemax = circuit->time + options.settle_time;
        simulate_circuit_inner(circuit);

        // restore old simulation state
        circuit->time   = digital_time * options.digital_time_unit;
        options.timemax = old_timemax;
        analog = ANALOG_ENABLED;
        for (j=0; j<circuit->Nnodes; j++)
          circuit->nodes[j].force=circuit->nodes[j].old_force;

        // reschedule pending boundary events back to LAZY_TIME
        for (j=0; j<circuit->Ndnodes; j++)
          {
          pn=&circuit->dnodes[j];
          if ((pn->event_index>=0)&&(pn->level==LEVEL_BOUNDARY))
            {
            assert(pn->event_time<LAZY_TIME);
            pn->event_time+=LAZY_TIME;
            pn->event_level=LEVEL_UPPER;
            reheap(pn->event_index);
            }
          }
        }
      }
    else if (lex_eatif_keyword(lex,"settings"))
      {
      fprintf(dsim_out,"prefix %s\n",prefix);
      if (warnall) fprintf(dsim_out,"warnall\n");
      else         fprintf(dsim_out,"nowarnall\n");
      if (ignoreerror) fprintf(dsim_out,"ignoreerror\n");
      else             fprintf(dsim_out,"noignorerror\n");
      if      (random_order==0) fprintf(dsim_out,"norandom\n");
      else if (random_order==1) fprintf(dsim_out,"random\n");
      else if (random_order==2) fprintf(dsim_out,"timed_random\n");
      fprintf(dsim_out,"timed_delay %g %g\n",fast_delay,slow_delay);
      if (analog==ANALOG_DISABLED) fprintf(dsim_out,"analog off\n");
      else                         fprintf(dsim_out,"analog on\n");
      fprintf(dsim_out,"floating_unstab_is_U %d\n",floating_unstab_is_U);
      if (circuit->Ndnodes>0)
        fprintf(dsim_out,"trace %d\n",HistoryMax/circuit->Ndnodes);
      fprintf(dsim_out,"prsTau %g\n",options.prs_tau);
      print_int_list("bumpCC",bumpCC);
      print_real_list("bumpTau",bumpTau);
      print_int_list("delayCC",delayCC);
      print_real_list("delayTau",delayTau);
      print_int_list("delayFast",delayFast);
      print_real_list("delayCap",delayCap);
      print_int_list("threshCC",threshCC);
      print_real_list("threshTau",threshTau);
      print_real_list("threshPercent",threshPercent);
      fprintf(dsim_out,"leakage %d\n",doLeakage);
      fprintf(dsim_out,"inverterLeakage %d\n",doInverterLeakage);
      fprintf(dsim_out,"max_bump_fanin_aggressors %d\n",
              (int) options.max_bump_fanin_aggressors);
      fprintf(dsim_out,"max_delay_fanin_aggressors %d\n",
              (int) options.max_delay_fanin_aggressors);
      }
    else if (lex_eatif_keyword(lex,"help"))
      {
      if (lex_is_id(lex))
        {
        // expanded help for a specific command
        char *str = lex_eat_id(lex);
        for (j=0; command[j][0]!=NULL; j++)
          if (strcmp(str,command[j][0])==0)
            fprintf(dsim_out,"%s %s\n\n%s\n\n",
                    command[j][0],command[j][1],command[j][2]);
        }
      else
        {
        // print usage for all commands
        fprintf(dsim_out,"Commands:\n");
        fprintf(dsim_out,"  (nodelist is a space separated list of nodes, groups, or single * globs)\n");
        for (j=0; command[j][0]!=NULL; j++)
          fprintf(dsim_out,"  %s %s\n",command[j][0],command[j][1]);
        }
      }
    else bad_command(lex,"command");
    }
  lex_free(lex);
  }

/*** set digital_sim globals to defaults ***/
void dsim_init()
  {
  dsim_in  = stdin;
  dsim_out = stdout;
  dsim_err = stderr;
  dsim_log = NULL;
  prefix=leak_strdup("");
  bumpCC    = list_create(sizeof(int));
  delayCC   = list_create(sizeof(int));
  delayFast = list_create(sizeof(int));
  bumpTau   = list_create(sizeof(double));
  delayTau  = list_create(sizeof(double));
  delayCap  = list_create(sizeof(double));
  threshCC  = list_create(sizeof(int));
  threshTau = list_create(sizeof(double));
  threshPercent = list_create(sizeof(double));
  CustomScenarios = list_create(sizeof(SCENARIO));
  srand(1);
  Big0 = bigint_from_int(Value0);
  Big1 = bigint_from_int(Value1);
  BigU = bigint_from_int(ValueU);
  }

/*** finish digital_sim ***/
void dsim_free(CIRCUIT *circuit)
  {
  int k;
  if (dsim_in!=stdin)   fclose(dsim_in);
  if (dsim_out!=stdout) fclose(dsim_out);
  if (dsim_err!=stderr) fclose(dsim_err);
  if (dsim_log!=NULL)   stop_log(dsim_log,circuit->Ndnodes,circuit->dnodes);
  dsim_in  = NULL;
  dsim_out = NULL;
  dsim_err = NULL;
  dsim_log = NULL;
  leak_free(prefix);
  list_free(bumpCC);
  list_free(delayCC);
  list_free(delayFast);
  list_free(bumpTau);
  list_free(delayTau);
  list_free(delayCap);
  list_free(threshCC);
  list_free(threshTau);
  list_free(threshPercent);
  for (k=0; k<CustomScenarios->max; k++)
    list_free(CustomScenarios->p.scenario[k].fanin);
  list_free(CustomScenarios);
  free_event_queue();
  allocate_history(0);
  free_bigint_memory();
  }
