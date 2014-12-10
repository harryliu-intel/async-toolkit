/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <signal.h>
#include "aspice.h"

void signal_exit(int junk)
  {
  fprintf(stderr,"Received exit signal.\n");
  exit(0);
  }

void banner()
  { 
  fprintf(stderr,"USAGE: aspice base\n"
          "  [-include file]\n"
          "  [-path path] [-dir dir]\n"
          "  [-batch]\n" 
          "  [-noreorder] [-restore]\n"
          "  [-noR] [-minCC C]\n"
	  "  [-traceInternal] [-traceR] [-traceAll]\n"
          "  [-verbose] [-debug_alint]\n"
          "  [-smoothV]\n"
	  "  [-count_scenarios]\n"
          "  [-top TopCellType]\n"
          "  [-seed srand_seed]\n"
	  "Aspice reads a circuit description from base.asp,\n"
	  "and writes results to base.names and base.trace.\n"
          "-include file sources commands from the file instead of stdin.\n"
          "-path specifies a colon separated search path for include files.\n"
          "-dir specifies working directory for names/trace/check files.\n"
          "-batch disables the command line interface.\n"
	  "-noreorder does not reorder output trace file.\n"
	  "-restore restores from a checkpoint.\n"
	  "-noR turns all resistors into wires.\n"
	  "-minCC C converts coupling capacitors less than C farads to lumped.\n"
	  "-traceInternal watches unnamed internal nodes.\n"
	  "-traceR watches all nodes that touch active devices.\n"
          "-traceAll watches all nodes.\n"
          "-verbose enables more output information (used for alint EM).\n"
          "-debug_alint enables debugging output for alint.\n"
          "-smoothV saves the EWMA waveforms used for analog to digital conversion.\n"
	  "-count_scenarios just enumerates scenarios for alint.\n"
          "-top instantiates TopCellType's contents with no prefix.\n"
          "-seed srand_seed enables Monte-Carlo AGAUSS function when srand_seed>0\n"
	  "\n(c) Andrew Lines, 2005 Fulcrum Microsystems, 2011 Intel\n\n");
  exit(1);
  }

int main(int argc, char *argv[])
  {
  LIST *cells,*globals,*criticalnets,*modifiers;
  char *basename=NULL,filename[STRMAX],*pc;
  int interactive=1,err=0;
  int do_reorder,do_restore,i;
  double start_time,setup_time;
  CIRCUIT circuit;

  /** system setup **/
  signal(SIGUSR1,signal_exit); /* exit when sent a kill -USR1 (for profiling) */
  setlinebuf(stdin);
  setlinebuf(stdout);
  setlinebuf(stderr);

  /*** banner ***/
  if (argc<2) banner();

  /*** initialize dsim ***/
  dsim_init();

  /*** get command line arguments ***/
  WorkDir=leak_strdup(".");
  do_reorder=1;
  do_restore=0;
  for (i=1; i<argc; i++)
    {
    if      (strcmp(argv[i],"-batch")==0) interactive=0;
    else if ((strcmp(argv[i],"-include")==0)&&(i+1<argc)) 
      {
      i++;
      interactive = 1;
      pc=find_filename(argv[i],"",".:$ASPICE_PATH");
      dsim_in = fopen(pc,"r");
      if (dsim_in==NULL) error("Can't read include file.");
      leak_free(pc);
      }
    else if (strcmp(argv[i],"-noreorder")==0) do_reorder=0;
    else if (strcmp(argv[i],"-restore")==0) do_restore=1;
    else if (strcmp(argv[i],"-smoothV")==0) trace_smoothV=1;
    else if (strcmp(argv[i],"-count_scenarios")==0) CountScenarios=1;
    else if (strcmp(argv[i],"-noR")==0) noResistors=1;
    else if (strcmp(argv[i],"-traceR")==0) traceResistiveSubnets=1;
    else if (strcmp(argv[i],"-traceInternal")==0) traceInternalNodes=1;
    else if (strcmp(argv[i],"-traceAll")==0) traceAllNodes=1;
    else if (strcmp(argv[i],"-verbose")==0) verbose=1;
    else if (strcmp(argv[i],"-debug_alint")==0) debug_alint=1;
    else if ((strcmp(argv[i],"-path")==0)&&(i+1<argc)) 
      {i++; safe_sprintf(filename,"ASPICE_PATH=%s",argv[i]); putenv(strdup(filename));}
    else if ((strcmp(argv[i],"-dir")==0)&&(i+1<argc))
      {i++; leak_free(WorkDir); WorkDir=leak_strdup(argv[i]);}
    else if ((strcmp(argv[i],"-minCC")==0)&&(i+1<argc)) 
      {i++; sscanf(argv[i],"%lf",&minCouplingCapacitance);}
    else if ((strcmp(argv[i],"-seed")==0)&&(i+1<argc)) 
      {i++; sscanf(argv[i],"%d",&monte_carlo_seed);
        srand48(monte_carlo_seed);}
    else if ((strcmp(argv[i],"-top")==0)&&(i+1<argc))
      {i++; topCell=argv[i];}
    else if (basename==NULL) basename=argv[i];
    else banner();
    }
  if (basename==NULL) banner();

  /*** parse circuit description ***/
  setup_time=start_time=user_time();
  printf("Parsing...\n");
  cells=list_create(sizeof(PARSECELL *));
  globals=list_create(sizeof(char *));
  criticalnets=list_create(sizeof(char *));
  modifiers=list_create(sizeof(MODIFIER *));
  parse_circuit(basename,cells,globals,criticalnets,modifiers);
  printf("Parsing time=%g\n",user_time()-start_time);

  /*** construct flattened circuit ***/
  start_time=user_time();
  printf("Constructing...\n");
  circuit=construct_circuit(basename,cells,globals,criticalnets,modifiers);
  circuit.do_restore = do_restore;
  circuit.do_reorder = do_reorder;
  circuit.run = basename;
  free_parse_memory(cells,globals,criticalnets,modifiers);
  printf("Constructing time=%g\n",user_time()-start_time);
 
  /*** partition and setup data structures ***/
  start_time=user_time();
  printf("Preparing...\n");
  prepare_circuit(&circuit);
  summarize_circuit(&circuit,stdout);
  printf("Preparing time=%g\n",user_time()-start_time);
  printf("Total setup time=%g\n",user_time()-setup_time);

  /*** simulate ***/
  start_time=user_time();
  simulate_circuit(&circuit,interactive,1);
  printf("Simulation time=%g\n",user_time()-start_time);

  /*** close files and free memory ***/
  dsim_free(&circuit);
  free_circuit(&circuit);
  MODEL_FreeMemory();
  free_temp_list();

  /*** reorder tracefile for transient runs ***/
  if (do_reorder)
    {
    start_time=user_time();
    reorder_tracefile(basename);
    printf("Reorder time=%g\n",user_time()-start_time);
    }
  else printf("Reorder time=NA\n");

  /*** check for memory leaks ***/
  leak_free(WorkDir);
  leak_check();
  return err;
  }
