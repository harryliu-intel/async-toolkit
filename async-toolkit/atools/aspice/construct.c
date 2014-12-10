/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** globals to decide what to save to tracefile ***/
int traceResistiveSubnets=0; /* include active_used nodes ***/
int traceInternalNodes=0; /* include active_used internal non-subnet nodes */
int traceAllNodes=0; /* watch every damn thing */
double minCouplingCapacitance=0; /* threshold to ignore capacitors */
char *topCell=NULL; /* flatten this cell type with empty prefix */
int constructedNames=0;
int constructedDevices=0;
int skipInstances=0,envInstances=0;

/*** return a pointer to a MODIFIER for this cell, or NULL ***/
MODIFIER *get_modifier(LIST *modifiers, char *type, char *name)
  {
  MODIFIER m,*pm = &m;
  int k;

  // look for type only modifier first
  pm->type = type;
  pm->name = NULL;
  k = find_element_lazy_sort(modifiers,&pm,&pmodifierprefixcmp);

  // look for type and instance modifier next
  if (k<0)
    {
    pm->name = name;
    k = find_element_lazy_sort(modifiers,&pm,&pmodifierprefixcmp);
    }

  // return modififer if found
  if (k>=0)
    {
    pm = modifiers->p.modifier[k];
    pm->applied=1;
    if (pm->env)  envInstances++;
    if (pm->skip) skipInstances++;
    return pm;
    }

  // neither found
  return NULL;
  }

/*** get the namespace, also count devices and digital rules ***/
void get_names(PARSECELL *pcell, LIST *cells,
               LIST *globals, LIST *criticalnets, LIST *modifiers,
               CIRCUIT *circuit, char *prefix, int un)
  {
  PARSEACTION *pact;
  PARSENODE *pnode[4];
  PARSECELL *pnewcell;
  char newprefix[STRMAX],*str;
  int j,k,l,newun;

  MODIFIER m,*pm; pm=&m;
        
  for (j=0; j<pcell->actions->max; j++)
    {
    if (circuit->names->max>=constructedNames+NUM_NOTIFY)
      {
      constructedNames=circuit->names->max;
      printf("Constructed %d names...\n",constructedNames);
      }
    pact=&pcell->actions->p.parseaction[j];
    if (pact->action==ACTION_WIRE)
      {
      pnode[0]=NULL;
      for (k=0; k<pact->nodes->max; k++)
        {
        pnode[1]=add_parsenode(circuit->names,globals,criticalnets,prefix,
                               pact->nodes->p.pc[k],0,un,0,0,0,0);
        if (pnode[0]!=NULL) alias_parsenodes(pnode[0],pnode[1]);
        pnode[0]=pnode[1];
	}
      }
    else if (pact->action==ACTION_EXCLUSIVE)
      {
      circuit->Nexclusives++;
      }
    else if (pact->action==ACTION_DEVICE)
      {
      int active,used=1,critical=pact->type!=TYPE_RES && pact->type!=TYPE_CAP;
      circuit->Ndevices++;
      // first pass to create nodes and check if any are critical
      for (k=0; k<pact->nodes->max; k++)
        {
        pnode[1]=add_parsenode(circuit->names,globals,criticalnets,prefix,
                               pact->nodes->p.pc[k],0,un,0,0,0,0);
        critical |= pnode[1]->critical;
        }
      // set critical, active, used
      active=(pact->type==TYPE_N_DIODE)||(pact->type==TYPE_P_DIODE)||
        (pact->type==TYPE_N_TRANSISTOR)||(pact->type==TYPE_P_TRANSISTOR);
      if (!critical) { circuit->Ndevices--; used=active=0; }
      // second pass to mark nodes used/active or short out noncritical resistors
      pnode[0]=NULL;
      for (k=0; k<pact->nodes->max; k++)
        {
        pnode[1]=add_parsenode(circuit->names,globals,criticalnets,prefix,
                               pact->nodes->p.pc[k],0,un,used,active,0,0);
        if (pnode[0]!=NULL && pact->type==TYPE_RES && !critical)
          alias_parsenodes(pnode[0],pnode[1]); // short out noncritical resistors
        pnode[0]=pnode[1];
        }
      }
    else if (pact->action==ACTION_PRS)
      {
      circuit->Ndrules++;
      for (k=0; k<pact->nodes->max; k++)
        add_parsenode(circuit->names,globals,criticalnets,prefix,
                      pact->nodes->p.pc[k],0,un,0,0,1,0);
      }
    else if (pact->action==ACTION_GMA)
      {
      circuit->Ngmas++;
      for (k=0; k<pact->nodes->max; k++) // look for lvalues of gma
        add_parsenode(circuit->names,globals,criticalnets,prefix,
                      pact->nodes->p.pc[k],0,un,0,0,1,0);
      for (k=0; k<pact->parms->max; k++) // look for variables inside expressions
        {
        LIST *expr = pact->parms->p.list[k];
        for (l=0; l<expr->max; l++)
          {
          BIOP op = expr->p.biop[l];
          if (op.type==BIOP_VARSTR)
            {
            str = sprint_bigint_str(NULL,op.value);
            add_parsenode(circuit->names,globals,criticalnets,prefix,
                          str,0,un,0,0,1,0);
            leak_free(str);
            }
          }
        }
      }
    else if (pact->action==ACTION_GROUP)
      {
      add_parsenode(circuit->names,globals,criticalnets,prefix,
                    pact->name,0,un,0,0,1,1);
      for (k=0; k<pact->nodes->max; k++)
        add_parsenode(circuit->names,globals,criticalnets,prefix,
                      pact->nodes->p.pc[k],0,un,0,0,1,0);
      }
    else if (pact->action==ACTION_FMLA)
      {
      circuit->Ndevices++;
      for (k=0; k<pact->nodes->max; k++)
        if ((k==0)||(find_element_sorted(globals,&pact->nodes->p.pc[k],&pstrcmp)>=0))
          add_parsenode(circuit->names,globals,criticalnets,
                        prefix,pact->nodes->p.pc[k],0,un,1,1,0,0);
      }
    else if (pact->action==ACTION_INSTANCE)
      {
      /*** check modifiers for this subcell ***/
      MODIFIER *pm;
      safe_sprintf(newprefix,"%s%s",prefix,pact->name);
      pm = get_modifier(modifiers,pact->type,newprefix);
      if (pm!=NULL && pm->skip) continue; // modifier=skip

      /*** fill in subcell info ***/
      safe_sprintf(newprefix,"%s%s.",prefix,pact->name);
      k=find_element_sorted(cells,pact,&pcellactioncmp);
      if (k<0)
        {
        fprintf(stderr,
          "ERROR: Instance %s refers to undefined cell %s (line %d).\n",
          pact->name,pact->type,pact->line);
        exit(1);
	}
      pnewcell=cells->p.parsecell[k];
      newun=un||pact->unnamed;

      /*** create and alias the node list ***/
      if (((pact->nodes->max!=0) && (pact->nodes->max!=pnewcell->nodes->max)) ||
          (pact->parms->max!=pnewcell->parms->max))
        {
        fprintf(stderr,
          "ERROR: Instance %s doesn't match definition of cell %s (line %d).\n",
          pact->name,pact->type,pact->line);
        exit(1);
        }
      for (k=0; k<pact->nodes->max; k++)
        {
        pnode[0]=add_parsenode(circuit->names,globals,criticalnets,prefix,
                               pact->nodes->p.pc[k],0,un,0,0,0,0);
        pnode[1]=add_parsenode(circuit->names,globals,criticalnets,newprefix,
                               pnewcell->nodes->p.pc[k],0,newun,0,0,0,0);
        alias_parsenodes(pnode[0],pnode[1]);
        }

      /*** recurse into instance ***/
      get_names(pnewcell,cells,globals,NULL,modifiers,circuit,newprefix,newun);
      }
    }
  }

/*** test if any nodes of an ACTION are critical in get_bodies pass ***/
int device_critical(CIRCUIT *circuit, PARSEACTION *pact, char *prefix)
  {
  PARSENODE *pnode;
  int k;
  if (pact->type!=TYPE_RES && pact->type!=TYPE_CAP) return 1;
  for (k=0; k<pact->nodes->max; k++)
    {
    pnode=find_parsenode(circuit,prefix,pact->nodes->p.pc[k]);
    assert(pnode!=NULL);
    if (pnode->critical) return 1;
    }
  return 0;
  }

/*** process the devices ***/
void get_bodies(PARSECELL *pcell, LIST *cells, LIST *modifiers,
                CIRCUIT *circuit, char *prefix, LIST *assignments)
  {
  PARSECELL *pnewcell;
  PARSEACTION *pact;
  DEVICE *pdev;
  NODE *pn;
  OP op,*pop;
  DIGITAL_NODE *guards[256],*dnode;
  DIGITAL_GROUP *dgrp;
  LIST *parms,*nodes;
  int j,k,l,m,n;
  char *str,newprefix[STRMAX];
  double x;

  /*** do ACTION_GROUP's first ***/
  for (j=0; j<pcell->actions->max; j++)
    {
    pact=&pcell->actions->p.parseaction[j];
    if (pact->action==ACTION_GROUP)
      {
      // get group name
      str = pact->name;
      dgrp = find_digital_group(circuit,prefix,str);
      assert(dgrp!=NULL);
      if (pact->nodes->max>0)
        {
        // check for redefining group members
        if (dgrp->nodes!=NULL)
          {
          fprintf(stderr,"ERROR: group %s redefined (line %d).\n",dgrp->name->name,pact->line);
          exit(1);
          }

        // map member node names to DIGITAL_NODE *'s
        dgrp->nodes = list_create(sizeof(DIGITAL_NODE *));
        for (k=0; k<pact->nodes->max; k++)
          {
          str = pact->nodes->p.pc[k];
          dnode = find_digital_node(circuit,prefix,str);
          list_append_element(dgrp->nodes,&dnode);
          }
        }
      }
    }

  /*** do remaining parse actions in order ***/
  for (j=0; j<pcell->actions->max; j++)
    {
    if (circuit->Ndevices>=constructedDevices+NUM_NOTIFY)
      {
      constructedDevices=circuit->Ndevices;
      printf("Constructed %d devices...\n",constructedDevices);
      }
    pact=&pcell->actions->p.parseaction[j];
    if (pact->action==ACTION_WIRE);
    else if (pact->action==ACTION_ASSIGNMENT)
      assign(assignments,pact->name,
             evaluate_by_name(assignments,pact->parms,pact->nodes));
    else if (pact->action==ACTION_INSTANCE)
      {
      /*** check modifiers for this subcell ***/
      MODIFIER *pm;
      safe_sprintf(newprefix,"%s%s",prefix,pact->name);
      pm = get_modifier(modifiers,pact->type,newprefix);
      if (pm!=NULL && pm->skip) continue; // modifier=skip

      /*** recurse into subcells ***/
      safe_sprintf(newprefix,"%s%s.",prefix,pact->name);
      k=find_element_sorted(cells,pact,&pcellactioncmp);
      assert(k>=0);
      pnewcell=cells->p.parsecell[k];
      parms=evaluate_parms(assignments,pact->parms);
      add_assignment_level(assignments);
      for (k=0; k<parms->max; k++)
        assign(assignments,pnewcell->parms->p.pc[k],parms->p.f[k]);
      if (pm!=NULL && pm->env) assign(assignments,"level",LEVEL_ENV); // modifier=env
      list_free(parms);
      get_bodies(pnewcell,cells,modifiers,circuit,newprefix,assignments);
      remove_assignment_level(assignments);
      }
    else if (pact->action==ACTION_ADDLEVEL)
      add_assignment_level(assignments);
    else if (pact->action==ACTION_REMOVELEVEL)
      remove_assignment_level(assignments);
    else if ((pact->action==ACTION_EXCLUSIVE) && (pact->type==TYPE_NOCC))
      {
      for (k=0; k<pact->nodes->max; k++)
        {
        pn = find_node(circuit,prefix,pact->nodes->p.pc[k]);
        if (pn!=NULL) pn->nocc=1;
        }
      }
    else if ((pact->action==ACTION_EXCLUSIVE) && (pact->type!=TYPE_NOCC))
      {
      EXCLUSIVE ex;
      LIST *ex_nodes;
      if      (pact->type==TYPE_EXCLHI) ex.type=EXCLHI;
      else if (pact->type==TYPE_EXCLLO) ex.type=EXCLLO;
      else                              ex.type=EXCLCC;
      ex_nodes = list_create(sizeof(NODE *));
      for (k=0; k<pact->nodes->max; k++)
        {
        pn = find_node(circuit,prefix,pact->nodes->p.pc[k]);
        if (pn!=NULL)
          {
          if      (ex.type==1) pn->canonical=0;
          else if (ex.type==0) pn->canonical=1;
          if (find_element_lazy_sort(ex_nodes,&pn,&paddrcmp)<0)
            list_insert_element_lazy_sort(ex_nodes,&pn,&paddrcmp);
          }
        }
      if (ex_nodes->max>=2)
        {
        ex.Nnodes=ex_nodes->max;
        ex.nodes=(NODE **) leak_malloc(ex_nodes->max*sizeof(NODE *));
        for (k=0; k<ex_nodes->max; k++) ex.nodes[k]=ex_nodes->p.pn[k];
        circuit->exclusives[circuit->Nexclusives++]=ex;
        }
      list_free(ex_nodes);
      }
    else if (pact->action==ACTION_MODEL)
      {
      if      (pact->type == TYPE_CORNER) MODEL_SetCorner(pact->name);
      else if (pact->type == TYPE_BSIM4)
        {
        double temperature;
        char *lib="",*dn="";
        if (pact->nodes->max>=1) lib=pact->nodes->p.pc[0];
        if (pact->nodes->max>=2) dn=pact->nodes->p.pc[1];
        if (pact->parms->max>=1) temperature=pact->parms->p.f[0];
        else if (get(assignments,"temperature",&temperature));
        else temperature=27;
        get(assignments,"vmin",&options.vmin);
        get(assignments,"vmax",&options.vmax);
        MODEL_SetModel(4,assignments,pact->name,lib,dn,temperature);
        }
      }
    else if (pact->action==ACTION_DEVICE)
      {
      parms=evaluate_parms(assignments,pact->parms);
      nodes=list_create(sizeof(NODE *));
      list_realloc(nodes,pact->nodes->max);
      for (k=0; k<pact->nodes->max; k++)
        nodes->p.pn[k]=find_node(circuit,prefix,pact->nodes->p.pc[k]);

      /*** create desired device ***/
      pdev=&circuit->devices[circuit->Ndevices];
      if (!device_critical(circuit,pact,prefix));
      else if      (pact->type == TYPE_CAP)
        {
        assert(parms->max==1);
        assert((nodes->max==1)||(nodes->max==2));
        if (nodes->max==1)
          {
          /*** explicitly lumped capacitor ***/
          nodes->p.pn[0]->CG += parms->p.f[0];
          }
        else if (nodes->p.pn[0]!=nodes->p.pn[1])
	  {
          if ((parms->p.f[0] < minCouplingCapacitance) ||
              nodes->p.pn[0]->fixed || nodes->p.pn[1]->fixed)
            {
            /*** lumped capacitor ***/
            nodes->p.pn[0]->CG += parms->p.f[0];
            nodes->p.pn[1]->CG += parms->p.f[0];
            }
          else
            {
            /*** coupling capacitor ***/
            create_capacitor(pdev,nodes->p.pn[0],
                             nodes->p.pn[1],parms->p.f[0]);
            circuit->Ndevices++;
            }
          }
        }
      else if (pact->type == TYPE_RES)
        {
        assert((nodes->max==2)&&(parms->max==1));
	if (nodes->p.pn[0]!=nodes->p.pn[1])
	  {
	  create_resistor(pdev,nodes->p.pn[0],nodes->p.pn[1],parms->p.f[0]);
	  circuit->Ndevices++;
	  }
        }
      else if (pact->type == TYPE_N_TRANSISTOR)
	{
        assert(nodes->max==6);
        create_transistor(pdev,NTYPE,nodes->max,nodes->p.pn,parms->max,parms->p.f);
	circuit->Ndevices++;
        }
      else if (pact->type == TYPE_P_TRANSISTOR)
	{
        assert(nodes->max==6);
        create_transistor(pdev,PTYPE,nodes->max,nodes->p.pn,parms->max,parms->p.f);
	circuit->Ndevices++;
        }
      else if (pact->type == TYPE_N_DIODE)
        {
        assert((nodes->max==2)&&(parms->max==4));
	if (nodes->p.pn[0]!=nodes->p.pn[1])
	  {
	  create_diode(pdev,NTYPE,nodes->p.pn[0],nodes->p.pn[1],
		       parms->p.f[0],parms->p.f[1],parms->p.f[2],parms->p.f[3]);
	  circuit->Ndevices++;
	  }
        }
      else if (pact->type == TYPE_P_DIODE)
        {
        assert((nodes->max==2)&&(parms->max==4));
	if (nodes->p.pn[0]!=nodes->p.pn[1])
	  {
	  create_diode(pdev,PTYPE,nodes->p.pn[0],nodes->p.pn[1],
		       parms->p.f[0],parms->p.f[1],parms->p.f[2],parms->p.f[3]);
	  circuit->Ndevices++;
	  }
        }
      else assert(0);
      if (pact->name!=NULL) // named device
        {
        safe_sprintf(newprefix,"%s%s",prefix,pact->name);
        pdev->name=leak_strdup(newprefix);
        }
      list_free(nodes);
      list_free(parms);
      }
    else if (pact->action==ACTION_PRS) // production rule
      {
      int flags,delay,level;
      n=pact->nodes->max;
      assert(n<=256);

      // get level
      x=LEVEL_UPPER;
      get(assignments,"level",&x);
      level=(int) x;

      // get flags and delay, scale dealy
      flags=pact->flags;
      delay=pact->delay;
      x=1;
      if (flags&FLAG_REALTIME) get(assignments,"scale_after_ps_delay",&x);
      else get(assignments,"scale_after_delay",&x);
      delay = (int) (x * delay + 0.5);
      
      // create rule
      for (k=0; k<n; k++)
        guards[k]=find_digital_node(circuit,prefix,pact->nodes->p.pc[k]);
      create_digital_rule(&circuit->drules[circuit->Ndrules],
                          n-1,guards,pact->parms->p.i,
                          guards[n-1],pact->parms->p.i[n-1],
                          delay,flags,level);
      circuit->Ndrules++;
      }
    else if (pact->action==ACTION_GMA) // guarded multiple assignment
      {
      int level;
      LIST *guard=NULL, *nodes, *groups, *delays, *flags, *indices, *expressions, *fanin;
      LIST *expr1, *expr2;
      BIOP op;
      BIGINT *tmp,*n16;
      double scale_after_ps,scale_after;

      // convert variables in expression from strings to BIGINT *'s
      delays = list_create(sizeof(LIST *));
      flags = list_create(sizeof(int));
      indices = list_create(sizeof(LIST *));
      expressions = list_create(sizeof(LIST *));
      fanin = list_create(sizeof(DIGITAL_NODE *));
      for (k=0; k<pact->parms->max; k++)
        {
        expr1 = pact->parms->p.list[k];
        expr2 = list_create(sizeof(BIOP));
        for (l=0; l<expr1->max; l++)
          {
          BIOP op;
          op = expr1->p.biop[l];
          if (op.type==BIOP_VARSTR) // translate VARSTR's to VAR's
            {
            str = sprint_bigint_str(NULL,op.value);
            dnode = find_digital_node(circuit,prefix,str);
            leak_free(str);
            op.type = BIOP_VAR;
            op.value = dnode->value;
            // add node to fanin list of guard
            if (k==0) if (find_element_lazy_sort(fanin,&dnode,&dnode_cmp)<0)
              list_insert_element_lazy_sort(fanin,&dnode,&dnode_cmp);
            }
          else if (op.type==BIOP_USRSTR) // translate USRSTR's to USR's
            {
            str = sprint_bigint_str(NULL,op.value);
            op.value=NULL;
            if (strcmp(str,"TIME")==0) // reserved GMA functions
              op.type = BIOP_TIME;
            else // convert to group access
              {
              dgrp = find_digital_group(circuit,prefix,str);
              if ((dgrp==NULL) || (dgrp->nodes==NULL))
                {
                fprintf(stderr,"ERROR: group %s%s used before it was defined (line %d).\n",
                        prefix,str,pact->line);
                exit(1);
                }
              op.type = BIOP_GRP + (dgrp - circuit->groups);
              // add all member nodes of group to fanin list of guard
              if (k==0) for (m=0; m<dgrp->nodes->max; m++)
                {
                dnode=dgrp->nodes->p.dnode[m];
                if (find_element_lazy_sort(fanin,&dnode,&dnode_cmp)<0)
                  list_insert_element_lazy_sort(fanin,&dnode,&dnode_cmp);
                }
              }
            leak_free(str);
            }
          list_append_element(expr2,&op);
          }
        if (k==0) guard=expr2;
        else if (k%4==1) list_append_element(delays,&expr2);
        else if (k%4==2) list_append_element(flags,&(expr2->p.biop[0].value->value));
        else if (k%4==3) list_append_element(indices,&expr2);
        else if (k%4==0) list_append_element(expressions,&expr2);
        }

      // scale delays
      scale_after=1;    get(assignments,"scale_after_delay",&scale_after);
      scale_after_ps=1; get(assignments,"scale_after_ps_delay",&scale_after_ps);
      for (k=0; (k<delays->max) && (k<flags->max); k++)
        {
        if (flags->p.i[k]&FLAG_INSTANT) continue; // instant delay
        if (flags->p.i[k]&FLAG_REALTIME) x = scale_after_ps;
        else                             x = scale_after;
        if (x==1) continue; // no scaling
        expr1 = delays->p.list[k];
        tmp = bigint_from_int((int) (x*(1<<16)+0.5)); // round to nearest 1/16384
        n16 = bigint_from_int(16);
        if ((expr1->max==1) && (expr1->p.biop[0].type==BIOP_CONST))
          {
          // change constant delay to (value*scale*16384)/16384
          move_bigint(tmp,mul_bigint(expr1->p.biop[0].value,tmp));
          move_bigint(tmp,shr_bigint(tmp,n16));
          move_bigint(expr1->p.biop[0].value,tmp);
          free_bigint(n16);
          }
        else
          {
          // change delay expression to (expression*scale*16384)/16384
          op.type=BIOP_CONST;
          op.value=tmp;
          list_append_element(expr1,&op);
          op.type=BIOP_MUL;
          op.value=NULL;
          list_append_element(expr1,&op);
          op.type=BIOP_CONST;
          op.value=n16;
          list_append_element(expr1,&op);
          op.type=BIOP_SHR;
          op.value=NULL;
          list_append_element(expr1,&op);
          }
        }

      // map lvalue node names to DIGITAL_NODE *'s or DIGITAL_GROUP *'s
      nodes  = list_create(sizeof(DIGITAL_NODE *));
      groups = list_create(sizeof(DIGITAL_GROUP *));
      for (k=0; k<pact->nodes->max; k++)
        {
        str = pact->nodes->p.pc[k];
        dgrp = find_digital_group(circuit,prefix,str);
        dnode = find_digital_node(circuit,prefix,str);
        if ((dnode==NULL) && ((dgrp==NULL) || (dgrp->nodes==NULL)))
          {
          fprintf(stderr,"ERROR: group %s%s used before it was defined (line %d).\n",
                  prefix,str,pact->line);
          exit(1);
          }
        list_append_element(nodes,&dnode);
        list_append_element(groups,&dgrp);
        }

      // assert stuff
      assert((guard!=NULL) && 
             (nodes->max>0) && 
             (groups->max==nodes->max) && 
             (delays->max==nodes->max) && 
             (flags->max==nodes->max) &&
             (expressions->max==nodes->max) &&
             (indices->max==nodes->max));

      // get level
      x=LEVEL_UPPER;
      get(assignments,"level",&x);
      level=(int) x;

      // create the GMA
      create_digital_gma(circuit->groups,&circuit->gmas[circuit->Ngmas],
                         guard,nodes,groups,delays,flags,indices,expressions,
                         fanin,level);
      list_free(fanin);
      circuit->Ngmas++;
      }
    else if (pact->action==ACTION_GROUP); // skip it this time
    else if (pact->action==ACTION_FMLA)
      {
      nodes=list_create(sizeof(NODE *));
      pn=find_node(circuit,prefix,pact->nodes->p.pc[0]);
      list_append_element(nodes,&pn);

      /*** create fmla, set true/false values, substitute parameters ***/
      parms=list_create(sizeof(OP));
      op.type=OP_TRUE;
      get(assignments,"true",&op.value);
      list_append_element(parms,&op);
      op.type=OP_FALSE;
      get(assignments,"false",&op.value);
      list_append_element(parms,&op);
      list_append_list(parms,pact->parms);

      /*** substitute parameters, change to port NODE *'s ***/
      for (k=0; k<parms->max; k++)
        {
        pop=&parms->p.op[k];
        if (pop->type<0) continue; // not a variable or parameter
        str=pact->nodes->p.pc[pop->type];
        if (get(assignments,str,&pop->value)) {pop->type=OP_CONST; continue;}
        pn=find_node(circuit,prefix,str);
        if (pn==NULL)
          {
          fprintf(stderr,"ERROR: formula refers to undefined var/parm %s%s (line %d).\n",
                  prefix,str,pact->line);
          exit(1);
          }
        n=find_element(nodes,&pn,&paddrcmp);
        if (n>=0) pop->type=n;
        else
          {
          pop->type=nodes->max;
          list_append_element(nodes,&pn);
          }
        }

      /*** create the current source ***/
      pdev=&circuit->devices[circuit->Ndevices++];
      create_source(pdev,nodes->max,nodes->p.pn,parms->max,parms->p.op,
                    pact->type==TYPE_VOLTAGE_SOURCE);
      if (pact->name!=NULL) // named device
        {
        safe_sprintf(newprefix,"%s%s",prefix,pact->name);
        pdev->name=leak_strdup(newprefix);
        }

      /*** free parms and nodes lists ***/
      list_free(parms);
      list_free(nodes);
      }
    else assert(0);
    }
  }

/*** number the parse nodes, create nodes and dnodes ***/
void make_nodes(CIRCUIT *circuit)
  {
  PARSENODE *pnode[3];
  DIGITAL_NODE *dnode;
  DIGITAL_GROUP *dgrp;
  int j,named,fixed,top_port;

  /*** count digital nodes, digital groups, and analog nodes ***/
  circuit->Nnodes=0;
  circuit->Ndnodes=0;
  circuit->Ngroups=0;
  for (j=0; j<circuit->names->max; j++)
    {
    pnode[0]=circuit->names->p.parsenode[j];
    if (pnode[0]->created==1) continue;

    /*** mark all aliased nodes as created==1 ***/
    pnode[1]=pnode[0];
    do {pnode[1]->created=1; pnode[1]=pnode[1]->palias;} while (pnode[1]!=pnode[0]);

    /*** find root alias ***/
    pnode[0]=root_alias(pnode[0]);

    /*** warn if node is unused ***/
    if ((!pnode[0]->digital_used)&&(!pnode[0]->analog_used)&&(!pnode[0]->group_used))
      {
      if (!pnode[0]->fixed)
        fprintf(stderr,"WARNING: node %s not used.\n",pnode[0]->name);
      continue;
      }

    /*** count groups or digital nodes ***/
    if      (pnode[0]->group_used)   circuit->Ngroups++;
    else if (pnode[0]->digital_used) circuit->Ndnodes++;

    /*** count an analog node ***/
    if (pnode[0]->analog_used) circuit->Nnodes++;
    }
  circuit->nodes=(NODE *) leak_malloc(circuit->Nnodes*sizeof(NODE));
  circuit->dnodes=(DIGITAL_NODE *) leak_malloc(circuit->Ndnodes*sizeof(DIGITAL_NODE));
  circuit->groups=(DIGITAL_GROUP *) leak_malloc(circuit->Ngroups*sizeof(DIGITAL_GROUP));
  circuit->Nnodes=0;
  circuit->Ndnodes=0;
  circuit->Ngroups=0;

  /*** second pass to create the nodes ***/
  for (j=0; j<circuit->names->max; j++)
    {
    pnode[0]=circuit->names->p.parsenode[j];
    if (pnode[0]->created==2) continue;

    /*** mark all aliased nodes as created==2 ***/
    pnode[1]=pnode[0];
    do {pnode[1]->created=2; pnode[1]=pnode[1]->palias;} while (pnode[1]!=pnode[0]);

    /*** find best name for the node ***/
    pnode[2]=pnode[1]=pnode[0];
    do
      {
      if (better_name_cmp(pnode[1],pnode[2])>0) pnode[2]=pnode[1];
      pnode[1]=pnode[1]->palias;
      } while(pnode[1]!=pnode[0]);

    /*** find root alias ***/
    pnode[0]=root_alias(pnode[0]);
    dnode=NULL;

    /*** create a digital group ***/
    if (pnode[0]->group_used)
      {
      if (pnode[0]->analog_used)
        {
        fprintf(stderr,"ERROR: group name %s is also used as an analog node name.\n",
                pnode[0]->name);
        exit(1);
        }
      pnode[1]=pnode[0];
      do 
        {
        pnode[1]->dnum=circuit->Ngroups;
        pnode[1]=pnode[1]->palias;
        } while (pnode[1]!=pnode[0]);
      dgrp=&circuit->groups[circuit->Ngroups++];
      create_digital_group(dgrp,pnode[2]);
      }

    /*** create a digital node ***/
    else if (pnode[0]->digital_used)
      {
      pnode[1]=pnode[0];
      do 
        {
        pnode[1]->dnum=circuit->Ndnodes;
        pnode[1]=pnode[1]->palias;
        } while (pnode[1]!=pnode[0]);
      dnode=&circuit->dnodes[circuit->Ndnodes++];
      create_digital_node(dnode,pnode[2]);
      }

    /*** create an analog node ***/
    if (pnode[0]->analog_used)
      {
      fixed=0;
      named=0;
      top_port=0;
      pnode[1]=pnode[2];
      do /* for all aliases */
        {
        pnode[1]->num=circuit->Nnodes; /* set node number */
        if (pnode[1]->fixed) fixed=1;
        if (traceAllNodes) named = 1;
        if (pnode[0]->active_used &&
            (pnode[1]->unnamed==1 || pnode[1]->unnamed==2) &&
            (!pnode[1]->subnet    || traceResistiveSubnets) &&
            (!pnode[1]->internal  || traceInternalNodes)) named=1;
        if ((pnode[1]->global!=2)&&(pnode[1]->unnamed==0)) named=1;
        if (pnode[1]->top_port) top_port=1;
        pnode[1]=pnode[1]->palias;
        } while(pnode[1]!=pnode[2]);

      /*** initialize and add the node ***/
      create_node(&circuit->nodes[circuit->Nnodes++],dnode,named,fixed,top_port,pnode[2]);
      }
    }
  }

/*** write names to a file ***/
void write_names(CIRCUIT *circuit)
  {
  DEVICE *pdev;
  PARSENODE *pnode[2];
  int j;
  FILE *fnames;
  char filename[STRMAX];

  /*** open names file ***/
  if (circuit->Nnodes==0) return;
  safe_sprintf(filename,"%s/%s.names",WorkDir,circuit->run);
  fnames=mkdir_fopen(filename,"w");
  if (fnames==NULL) error("Can't write names file!");
  bigbuffer(fnames);

  /*** second pass to create the nodes ***/
  fprintf(fnames,"time\n\n");
  for (j=0; j<circuit->Nnodes; j++)
    {
    if (!circuit->nodes[j].watch) continue; /* unwatched node */
    pnode[0]=pnode[1]=circuit->nodes[j].name; /* canonical name */
    fprintf(fnames,"%s\n",pnode[0]->name); /* always print canonical name */
    pnode[0]=pnode[0]->palias;
    
    /*** print additional aliases to names file ***/
    while (pnode[0]!=pnode[1])
      {
      if ((pnode[0]->global!=2)&&(pnode[0]->unnamed<=1))
        fprintf(fnames,"=%s\n",pnode[0]->name);
      pnode[0]=pnode[0]->palias;
      }
    fprintf(fnames,"\n");
    }

  /*** write names for currents through named resistors or sources ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if (pdev->name==NULL) continue;
    if (isResistor(pdev) || isSource(pdev)) 
      fprintf(fnames,"%s|\n\n",pdev->name);
    }
  fclose(fnames);
  }

/*** top level for converting parse tree into simulation data structures ***/
CIRCUIT construct_circuit(char *basename, LIST *cells,
                          LIST *globals, LIST *criticalnets, LIST *modifiers)
  {
  CIRCUIT circuit;
  LIST *assignments;
  PARSECELL *ptop,*ptopCell=NULL;
  PARSENODE *pnode;
  int j;
  double x;
  char *topname="";

  /*** initialize circuit ***/
  circuit=create_circuit();

  /*** find default unnamed top cell ***/
  j=find_element_sorted(cells,topname,&pcellnamecmp);
  assert(j>=0);
  ptop=cells->p.parsecell[j];

  /*** find named top cell ***/
  if (topCell!=NULL)
    {
    j=find_element_sorted(cells,topCell,&pcellnamecmp);
    if (j>=0) ptopCell=cells->p.parsecell[j];
    else {fprintf(stderr,"ERROR: no -top %s\n",topCell); exit(1);}
    }

  /*** first pass to construct names and wires ***/
  circuit.names=list_create(sizeof(PARSENODE *));
  add_parsenode(circuit.names,globals,criticalnets,"",
                "",1,1,0,0,0,0); /*** magic ground ***/
  get_names(ptop,cells,globals,criticalnets,modifiers,&circuit,"",0);
  if (ptopCell!=NULL)
    {
    for (j=0; j<ptopCell->nodes->max; j++) /* mark top_port property */
      {
      pnode=add_parsenode(circuit.names,globals,criticalnets,"",
                          ptopCell->nodes->p.pc[j],0,0,0,0,0,0);
      pnode->top_port=1;
      }
    get_names(ptopCell,cells,globals,criticalnets,modifiers,&circuit,"",0);
    }
  list_finish_lazy_sort(circuit.names,&pparsenodecmp);

  /*** feebdack on modify commands **/
  if (skipInstances) printf("Modify skip applied to %d instances\n",skipInstances);
  if (envInstances)  printf("Modify env  applied to %d instances\n",envInstances);
  for (j=0; j<modifiers->max; j++)
    {
    MODIFIER *pm=modifiers->p.modifier[j];
    if (!pm->applied)
      {
      printf("UNUSED: modify %s%s\"%s\"",
             pm->env?"env ":"" ,pm->skip?"skip ":"",pm->type);
      if (pm->name) printf(" \"%s\"",pm->name);
      printf("\n");
      }
    }

  /*** create nodes ***/
  make_nodes(&circuit);

  /*** preallocate the event queue ***/ 
  allocate_event_queue(circuit.Ndnodes);

  /*** allocate devices, drules, gmas ***/
  circuit.devices=(DEVICE *) leak_malloc(circuit.Ndevices*sizeof(DEVICE));
  circuit.drules=(DIGITAL_RULE *) leak_malloc(circuit.Ndrules*sizeof(DIGITAL_RULE));
  circuit.gmas=(DIGITAL_GMA *) leak_malloc(circuit.Ngmas*sizeof(DIGITAL_GMA));
  circuit.exclusives=(EXCLUSIVE *) leak_malloc(circuit.Nexclusives*sizeof(EXCLUSIVE));
  circuit.Ndevices=0;
  circuit.Ndrules=0;
  circuit.Ngmas=0;
  circuit.Nexclusives=0;

  /*** create parameter assignment scope ***/
  assignments=list_create(sizeof(LIST *));
  add_assignment_level(assignments);

  /*** set default option parameters ***/
  options=default_options();
  assign(assignments,"timestep",options.timestep);
  assign(assignments,"timeratio",options.timeratio);
  assign(assignments,"poststep",options.poststep);
  assign(assignments,"timemax",options.timemax);
  assign(assignments,"maxerr",options.maxerr);
  assign(assignments,"partition_mode",options.partition_mode);
  assign(assignments,"coupling_cutoff",options.coupling_cutoff);
  assign(assignments,"checkpoint_interval",options.checkpoint_interval);
  assign(assignments,"false",options.falseV);
  assign(assignments,"true",options.trueV);
  assign(assignments,"loth",options.lo_thresholdV);
  assign(assignments,"hith",options.hi_thresholdV);
  assign(assignments,"prstau",options.prs_tau);
  assign(assignments,"digital_time_unit",options.digital_time_unit);
  assign(assignments,"d2a_shape",options.d2a_shape);
  assign(assignments,"d2a_saturation",options.d2a_saturation);
  assign(assignments,"a2d_ewma_tau",options.a2d_ewma_tau);
  assign(assignments,"scale_after_delay",options.scale_after_delay);
  assign(assignments,"scale_after_ps_delay",options.scale_after_ps_delay);
  assign(assignments,"settle_time",options.settle_time);
  assign(assignments,"vmin",options.vmin);
  assign(assignments,"vmax",options.vmax);
  assign(assignments,"VTN",options.VTN);
  assign(assignments,"VTP",options.VTP);
  assign(assignments,"stable_dVdt",options.stable_dVdt);
  assign(assignments,"small_leak_voltage",options.small_leak_voltage);
  assign(assignments,"large_leak_voltage",options.large_leak_voltage);
  assign(assignments,"max_bump_fanin_aggressors",options.max_bump_fanin_aggressors);
  assign(assignments,"max_delay_fanin_aggressors",options.max_delay_fanin_aggressors);
  assign(assignments,"bump_event_interval",options.bump_event_interval);
  assign(assignments,"delay_event_interval",options.delay_event_interval);
  assign(assignments,"scenario_timemax",options.scenario_timemax);
  assign(assignments,"thresh_scenario_timemax",options.thresh_scenario_timemax);
  assign(assignments,"relevant_cap_ratio",options.relevant_cap_ratio);
  assign(assignments,"alint_voltage_margin",options.alint_voltage_margin);
  assign(assignments,"delay_slow_inverse",options.delay_slow_inverse);
  assign(assignments,"level",LEVEL_UPPER); // backward compatible for A/D cosim

  /*** second pass to construct capacitors and sources ***/
  get_bodies(ptop,cells,modifiers,&circuit,"",assignments);
  if (ptopCell!=NULL) get_bodies(ptopCell,cells,modifiers,&circuit,"",assignments);

  /*** get simulate parseoptions from special parameters ***/
  get(assignments,"timestep",&options.timestep);
  get(assignments,"timeratio",&options.timeratio);
  get(assignments,"timemax",&options.timemax);
  get(assignments,"poststep",&options.poststep);
  get(assignments,"maxerr",&options.maxerr);
  get(assignments,"partition_mode",&options.partition_mode);
  get(assignments,"coupling_cutoff",&options.coupling_cutoff);
  get(assignments,"checkpoint_interval",&options.checkpoint_interval);
  get(assignments,"true",&options.trueV);
  get(assignments,"false",&options.falseV);
  get(assignments,"loth",&options.lo_thresholdV);
  get(assignments,"hith",&options.hi_thresholdV);
  get(assignments,"prstau",&options.prs_tau);
  get(assignments,"digital_time_unit",&options.digital_time_unit);
  get(assignments,"d2a_shape",&options.d2a_shape);
  get(assignments,"d2a_saturation",&options.d2a_saturation);
  get(assignments,"a2d_ewma_tau",&options.a2d_ewma_tau);
  get(assignments,"settle_time",&options.settle_time);
  get(assignments,"scale_after_delay",&options.scale_after_delay);
  get(assignments,"scale_after_ps_delay",&options.scale_after_ps_delay);
  get(assignments,"vmin",&options.vmin);
  get(assignments,"vmax",&options.vmax);
  get(assignments,"VTN",&options.VTN);
  get(assignments,"VTP",&options.VTP);
  get(assignments,"stable_dVdt",&options.stable_dVdt);
  get(assignments,"small_leak_voltage",&options.small_leak_voltage);
  get(assignments,"large_leak_voltage",&options.large_leak_voltage);
  get(assignments,"max_bump_fanin_aggressors",&options.max_bump_fanin_aggressors);
  get(assignments,"max_delay_fanin_aggressors",&options.max_delay_fanin_aggressors);
  get(assignments,"bump_event_interval",&options.bump_event_interval);
  get(assignments,"delay_event_interval",&options.delay_event_interval);
  get(assignments,"scenario_timemax",&options.scenario_timemax);
  get(assignments,"thresh_scenario_timemax",&options.thresh_scenario_timemax);
  get(assignments,"relevant_cap_ratio",&options.relevant_cap_ratio);
  get(assignments,"alint_voltage_margin",&options.alint_voltage_margin);
  get(assignments,"delay_slow_inverse",&options.delay_slow_inverse);

  /*** get some digital_sim settings too ***/
  if (get(assignments,"random_order",&x)) random_order=(int)x;
  if (get(assignments,"slow_delay",&x)) slow_delay=x;
  if (get(assignments,"fast_delay",&x)) fast_delay=x;
  if (get(assignments,"warnall",&x))  warnall=(x!=0);

  /*** link models to their decks ***/
  MODEL_Finish();

  /*** remove assignment scope ***/
  remove_assignment_level(assignments);
  list_free(assignments);

  /*** initialize remaining circuit fields ***/
  free_temp_list(); // free sorting memory
  return circuit;
  }
