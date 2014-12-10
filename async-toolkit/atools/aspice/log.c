/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"
#ifdef SST2
#include "sdi2c.h"
#endif

const char *LevelName[4]={"env","lower","upper","boundary"};
const char *ValueName[3]={"0","1","U"};
FILE *dsim_out=NULL,*dsim_err=NULL,*dsim_in=NULL;
LOGFILE *dsim_log=NULL;

/*** returns debugging name for a digital node ***/
char *get_digital_node_name(DIGITAL_NODE *dnode)
  {
  return dnode->name->name;
  }

/*** format a dnode value as a hex or string ***/
void print_value(FILE *fout, BIGINT *value, int string) 
  {
  if (string) print_bigint_str(fout,value);
  else print_bigint_hex(fout,value);
  }

/*** format a dnode value as a hex or string ***/
void sprint_value(char *str, BIGINT *value, int string) 
  {
  BIGINT *t;
  if (string) sprint_bigint_str(str,value);
  else
    {
    t=hexstr_bigint(value);
    sprint_bigint_str(str,t);
    free_bigint(t);
    }
  }

/*** print value of a node ***/
void print_dnode(FILE *fout, DIGITAL_NODE *dnode)
  {
  if (digital_time>0) fprintf(fout," @%u",digital_time);
  fprintf(fout," #%d",dnode->tcount);
  fprintf(fout," %s:",get_digital_node_name(dnode));
  print_value(fout,dnode->value,dnode->string);
  fprintf(fout,"\n");
  }

/*** print pending value of a node ***/
void print_pending_dnode(FILE *fout, DIGITAL_NODE *dnode)
  {
  if (dnode->event_index<0) return; // not pending
  if (dnode->event_time>0) fprintf(fout," @%u",dnode->event_time);
  fprintf(fout," #%d",dnode->tcount+1);
  fprintf(fout," %s %s:",LevelName[dnode->event_level],get_digital_node_name(dnode));
  print_value(fout,dnode->value,dnode->string);
  fprintf(fout," -> ");
  print_value(fout,dnode->pending,dnode->string);
  fprintf(fout,"\n");
  }

/*** print members of a digital group ***/
void print_digital_group(FILE *fout, DIGITAL_GROUP *dgrp)
  {
  DIGITAL_NODE *dnode;
  int j;
  fprintf(fout," group %s:\n",dgrp->name->name);
  for (j=0; j<dgrp->nodes->max; j++)
    {
    dnode=dgrp->nodes->p.dnode[j];
    print_dnode(fout,dnode);
    }
  }

/*** print an event or error ***/
void print_event(FILE *fout, DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message)
  {
  fprintf(fout,"WARNING:");
  if (dnode!=NULL)
    {
    fprintf(fout," %s->",get_digital_node_name(dnode));
    print_value(fout,dnode->pending,dnode->string);
    }
  fprintf(fout," %s at time %u",message,digital_time);
  if (lastevent!=NULL)
    {
    fprintf(fout," caused by %s->",get_digital_node_name(lastevent));
    print_value(fout,lastevent->value,lastevent->string);
    }
  fprintf(fout,".\n");
  }

/*** SST: create a fiber for logging ***/
void start_fiber(LOGFILE *log, DIGITAL_NODE *dnode)
  {
#ifdef SST2
  int j;
  char scope[STRMAX],name[STRMAX];
  if (dnode->fiber!=NULL) return; // fiber already exists

  // replace illegal characters with SST2 acceptable ones
  sprintf(name,"%s",get_digital_node_name(dnode));
  for (j=0; j<strlen(name); j++)
    {
    if      (isalnum(name[j]));
    else if (name[j]=='.');
    else if (name[j]==',');
    else if (name[j]=='[') name[j]='(';
    else if (name[j]==']') name[j]=')';
    else name[j]='_';
    }

  // split scope and name apart at last dot
  sprintf(scope,"%s",name);
  for (j=strlen(name)-1; j>=0; j--) if (scope[j]=='.') break;
  if (j<0) scope[0]=0;
  else     scope[j]=0;
 
  // create the fiber
  dnode->fiber=sdi_create_fiber(log,name+j+1,"USER_DEFINED_FIBER",scope);
#endif
  }

/*** SST2: end a transaction and free the handle ***/
void end_transaction(LOGFILE *log, DIGITAL_NODE *dnode)
  {
#ifdef SST2
  sdi_timeT sdi_time;
  sdi_time = (sdi_timeT) (digital_time * options.digital_time_unit / 1e-12);
  sdi_set_time(log,sdi_time);
  if (dnode->transaction!=NULL)
    {
    sdi_end_transaction(dnode->transaction);
    sdi_free_transaction_handle(dnode->transaction);
    }
  dnode->transaction=NULL;
#endif
  }

/*** create a link between transactions ***/
void link_transactions(LOGFILE *log, DIGITAL_NODE *from, DIGITAL_NODE *to)
  {
#ifdef SST2
  if ((from==NULL) || (from->transaction==NULL)) return;
  if ((to==NULL) || (to->transaction==NULL)) return;
  sdi_link_transaction(from->transaction,to->transaction,"enables");
#endif
  }

/*** log an error ***/
void log_error(FILE *log, DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message)
  {
#ifdef SST2
  char label[STRMAX+1];
  if (dnode==NULL) return; // need a dnode (TODO: use global fiber)
  start_fiber(log,dnode); // start a fiber

  // set label
  label[0]='=';
  sprint_value(label+1,dnode->pending,dnode->string);

  // advance time, close old transaction, record sdiError
  end_transaction(log,dnode);
  dnode->transaction=sdi_transaction(sdiError,dnode->fiber,"Error",label,message);

  // link cause of error
  link_transactions(log,lastevent,dnode);

  // free transaction
  sdi_free_transaction_handle(dnode->transaction);
  dnode->transaction=NULL;
#else
  print_event(log,lastevent,dnode,message);
#endif
  }

/*** log an event ***/
void log_event(FILE *log, DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message)
  {
#ifdef SST2
  char label[STRMAX+1];
  if (dnode==NULL) return; // need a dnode (TODO: use global fiber)
  start_fiber(log,dnode); // start a fiber

  // set label
  label[0]='=';
  sprint_value(label+1,dnode->pending,dnode->string);

  // advance time, close old transaction, record sdiEvent
  end_transaction(log,dnode);
  dnode->transaction=sdi_transaction(sdiEvent,dnode->fiber,"Event",label,message);

  // link cause of event
  link_transactions(log,lastevent,dnode);

  // free transaction
  sdi_free_transaction_handle(dnode->transaction);
  dnode->transaction=NULL;
#else
  print_event(log,lastevent,dnode,message);
#endif
  }

/*** enable notices for all events, not just bad ones ***/
void event_notice(DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message)
  {
#ifdef VERBOSE
  if (!warnall) return;
  print_event(dsim_err,lastevent,dnode,message);
  if (dsim_log!=NULL) log_event(dsim_log,lastevent,dnode,message);
#endif
  }

/*** print a warning for a suspicious event ***/
void event_warning(DIGITAL_NODE *lastevent, DIGITAL_NODE *dnode, char *message)
  {
  ERROR=1;
  if (!warnall) return;
  print_event(dsim_err,lastevent,dnode,message);
  if (dsim_log!=NULL) log_error(dsim_log,lastevent,dnode,message);
  }

/*** log value of a node to logging file ***/
void log_dnode(LOGFILE *log, DIGITAL_NODE *dnode)
  {
#ifdef SST2
  char label[STRMAX+1],description[STRMAX];

  // set value and description
  label[0]='=';
  sprint_value(label+1,dnode->value,dnode->string);
  sprintf(description,"#%d",dnode->tcount);

  // advance time, close old transaction, start new one
  end_transaction(log,dnode);
  dnode->transaction=sdi_transaction(sdiBeginEnd,dnode->fiber,
                                     "BigInt",label,description);

  // link critical path information if possible
  link_transactions(log,dnode->enabler_node,dnode);
#else
  fprintf(log," @%u",digital_time);
  fprintf(log," #%d",dnode->tcount);
  fprintf(log," %s:",get_digital_node_name(dnode));
  print_value(log,dnode->value,dnode->string);
  if (dnode->enabler_node!=NULL)
    fprintf(log," enabled by #%u %s",
            dnode->enabler_tcount,get_digital_node_name(dnode->enabler_node));
  fprintf(log,"\n");
#endif
  }

/*** start logging a digital node ***/
void log_dnode_on(LOGFILE *log, DIGITAL_NODE *dnode)
  {
  if (dnode->log) return; // vacuous
  dnode->log=1;
#ifdef SST2
  if (log==NULL) return;
  start_fiber(log,dnode); // start fiber
  log_dnode(log,dnode); // log initial value
#endif
  }

/*** stop logging a digital node ***/
void log_dnode_off(LOGFILE *log, DIGITAL_NODE *dnode)
  {
  if (!dnode->log) return; // vacuous
  dnode->log=0;
#ifdef SST2
  if (log==NULL) return;
  end_transaction(log,dnode);
#endif
  }

/*** start logging to a file ***/
LOGFILE *start_log(char *filename, int Ndnodes, DIGITAL_NODE *dnodes)
  {
#ifdef SST2
  return sdi_set_database(filename,-12,0);
#else
  int j;
  LOGFILE *log;
  log = mkdir_fopen(filename,"wt");
  if (log==NULL) return log;
  for (j=0; j<Ndnodes; j++) if (dnodes[j].log)
    {
    start_fiber(log,&dnodes[j]); // start fiber
    log_dnode(log,&dnodes[j]); // log initial value
    }
  return log;
#endif
  }

/*** stop logging to a file ***/
void stop_log(LOGFILE *log, int Ndnodes, DIGITAL_NODE *dnodes)
  {
#ifdef SST2
  int j;
  for (j=0; j<Ndnodes; j++) if (dnodes[j].log)
    {
    end_transaction(log,&dnodes[j]);
    dnodes[j].fiber=NULL; // invalidate fiber as well
    }
  sdi_close_database(log);
#else
  fclose(log);
#endif
  }

/*** flush logging file ***/
void flush_log(LOGFILE *log)
  {
#ifdef SST2
  sdi_simulation_paused(log);
#else
  fflush(log);
#endif
  }
