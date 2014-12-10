/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

char *WorkDir=NULL; /* global for trace/names/check directory */

void save_state(CIRCUIT *circuit)
  {
  int j,x;
  FILE *fcheck;
  char checkfile[STRMAX],tempname[STRMAX];

  /*** get check file name ***/
  safe_sprintf(checkfile,"%s/%s.checkpoint",WorkDir,circuit->run);

  /*** open temp file ***/
  safe_sprintf(tempname,"%s.XXXXXX",checkfile);
  mkstemp(tempname);

  fcheck=mkdir_fopen(tempname,"wb");
  if (fcheck==NULL) 
    {
    fprintf(stderr,"ERROR: can't write checkpoint to %s.\n",tempname);
    return;
    }
  bigbuffer(fcheck);

  /*** save analog state ***/
  write_float(fcheck,circuit->time);
  for (j=0; j<circuit->Nnodes; j++) write_float(fcheck,circuit->nodes[j].V);

  /*** save digital state ***/
  write_int(fcheck,digital_time);
  x=0;
  for (j=0; j<circuit->Ndnodes; j++)
    {
    x|=(trivalue(circuit->dnodes[j].value))<<(2*(j%16));
    if ((j%16)==15) {write_int(fcheck,x); x=0;}
    }
  if ((j%16)>0) write_int(fcheck,x);

  /*** save digital event queue ***/
  write_int(fcheck,Nevents);
  for (j=0; j<Nevents; j++) 
    {
    write_int(fcheck,events[j]-circuit->dnodes);
    write_int(fcheck,events[j]->event_time);
    write_char(fcheck,trivalue(events[j]->pending));
    }

  /*** rename to file.check ***/
  fclose(fcheck);
  if (rename(tempname,checkfile)!=0)
    {
    fprintf(stderr,"ERROR: can't rename %s to %s.\n",tempname,checkfile);
    unlink(tempname);
    }
  }

void restore_state(CIRCUIT *circuit)
  {
  int j,x=0;
  char checkfile[STRMAX];
  FILE *fcheck;
  BIGINT *t;
  int old_warnall;

  /*** get check file name ***/
  safe_sprintf(checkfile,"%s/%s.checkpoint",WorkDir,circuit->run);

  /*** open check file ***/
  fcheck=fopen(checkfile,"rb");
  if (fcheck==NULL)
    {
    fprintf(stderr,"ERROR: can't open %s to restore from checkpoint.\n",
            checkfile);
    exit(1);
    }
  bigbuffer(fcheck);

  /*** restore analog state ***/
  circuit->time=read_float(fcheck);
  for (j=0; j<circuit->Nnodes; j++) circuit->nodes[j].V=read_float(fcheck);

  /*** restore digital state ***/
  old_warnall=warnall;
  warnall=0;
  digital_time=read_int(fcheck);
  for (j=0; j<circuit->Ndnodes; j++)
    {
    if ((j%16)==0) x=read_int(fcheck);
    t = bigint_from_int((x>>(2*(j%16)))&3);
    set_digital_node(circuit->groups,&circuit->dnodes[j],t);
    free_bigint(t);
    }
  warnall=old_warnall;

  /*** restore digital event queue ***/
  Nevents=read_int(fcheck);
  for (j=0; j<circuit->Ndnodes; j++) circuit->dnodes[j].event_index=-1;
  for (j=0; j<Nevents; j++)
    {
    events[j]=circuit->dnodes+read_int(fcheck);
    events[j]->event_time=read_int(fcheck);
    events[j]->pending=bigint_from_int(read_char(fcheck));
    events[j]->event_index=j;
    }
  }

/*** append to end of tracefile ***/
void append_tracefile(CIRCUIT *circuit)
  {
  char filename[STRMAX];
  int Nnodes,Nsteps,order,timestamp,j,k,step;
  safe_sprintf(filename,"%s/%s.trace",WorkDir,circuit->run);
  k=1; for (j=0; j<circuit->Nnodes; j++) if (circuit->nodes[j].watch) k++;
  circuit->ftrace=mkdir_fopen(filename,"r+b");
  if (circuit->ftrace==NULL) 
    {
    fprintf(stderr,"WARNING: can't append to %s.\n",filename);
    return;
    }
  bigbuffer(circuit->ftrace);
  if ((!get_header(circuit->ftrace,&timestamp,&Nnodes,&Nsteps,&order))||(order!=ORDER_ORIGINAL)||(Nnodes!=k))
    {
    fprintf(stderr,"WARNING: can't append to %s.\n",filename);
    fclose(circuit->ftrace);
    return;
    }
  for (step=0; step<Nsteps; step++)
    {
    fseek(circuit->ftrace,HEADERSIZE+step*Nnodes*sizeof(float),SEEK_SET);
    if (read_float(circuit->ftrace)>=circuit->time) break;
    }
  fseek(circuit->ftrace,HEADERSIZE+step*Nnodes*sizeof(float),SEEK_SET);
  if (step==Nsteps) fprintf(stderr,"WARNING: checkpoint time not reached by tracefile.\n");
  }

/*** start a new tracefile ***/
void start_tracefile(CIRCUIT *circuit)
  {
  int j,k;
  time_t timestamp;
  char filename[STRMAX];
  safe_sprintf(filename,"%s/%s.trace",WorkDir,circuit->run);
  circuit->ftrace=mkdir_fopen(filename,"wb");
  if (circuit->ftrace==NULL) error("Can't open trace file.");
  bigbuffer(circuit->ftrace);
  write_int(circuit->ftrace,0); /*** order is 0 ***/
  timestamp=0; time(&timestamp);
  write_int(circuit->ftrace,timestamp); /*** timestamp when file is started ***/
  k=1; // time
  for (j=0; j<circuit->Nnodes; j++) // watched nodes
    if (circuit->nodes[j].watch) k++;
  for (j=0; j<circuit->Ndevices; j++) // named resistors and sources
    {
    DEVICE *pdev = &circuit->devices[j];
    if (pdev->name==NULL) continue;
    if (isResistor(pdev) || isSource(pdev)) k++;
    }
  write_int(circuit->ftrace,k); /*** number of output variables ***/
  }

/*** either start or restore from checkpoint and append a tracefile ***/
void prepare_tracefile(CIRCUIT *circuit)
  {
  if (circuit->trace_started) return;
  if (circuit->ftrace!=NULL) fclose(circuit->ftrace);
  circuit->ftrace=NULL;
  if (circuit->Nnodes==0) return;
  circuit->trace_started=1;
  write_names(circuit);
  if (circuit->do_restore)
    {
    restore_state(circuit);
    printf("Restored from checkpoint at time %g.\n",circuit->time);
    append_tracefile(circuit);
    }
  else start_tracefile(circuit);
  }

/*** reorder the tracefile ***/
void reorder_tracefile(char *run)
  {
  char filename[STRMAX];
  safe_sprintf(filename,"%s/%s.trace",WorkDir,run);
  if (!reorder_trace_in_place(filename,REORDERMEM))
    fprintf(stderr,"WARNING: unable to reorder trace file.\n");
  }

/*** close and reorder the tracefile ***/
void finish_tracefile(CIRCUIT *circuit)
  {
  if (!circuit->trace_started) return;
  if (circuit->ftrace!=NULL) fclose(circuit->ftrace);
  circuit->ftrace=NULL;
  if (circuit->run==NULL) return;
  if (circuit->do_reorder) reorder_tracefile(circuit->run);
  }
