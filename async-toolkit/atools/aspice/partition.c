/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** puts a LIST structure wrapping around an array ***/
LIST list_wrapper(int max, int size, int mem, void *p)
  {
  LIST list;
  list.size=size;
  list.max=max;
  list.mem=mem;
  list.shrink=0;
  list.p.b=p;
  return list;
  }

/*** sort devices by type, then by model, then by parameters ***/
void sort_devices(CIRCUIT *circuit)
  {
  LIST list;
  list=list_wrapper(circuit->Ndevices,sizeof(DEVICE),
                    circuit->Ndevices*sizeof(DEVICE),circuit->devices);
  list_sort(&list,&pdevcmp);
  }

/*** compare fanin groups by size ***/
int exclusive_length_cmp(void *p1, void *p2)
  {
  EXCLUSIVE *pex1 = (EXCLUSIVE *) p1, *pex2 = (EXCLUSIVE *) p2;
  return pex2->Nnodes - pex1->Nnodes;
  }  

/*** sort exclusives so that longest sets come first ***/
void sort_exclusives(CIRCUIT *circuit)
  {
  LIST list;
  list=list_wrapper(circuit->Nexclusives,sizeof(EXCLUSIVE),
                    circuit->Nexclusives*sizeof(EXCLUSIVE),circuit->exclusives);
  list_sort(&list,&exclusive_length_cmp);
  }

/*** find root seed and shorten tree ***/
NODE *root_seed(NODE *pn)
  {
  NODE *proot,*pnext;
  proot=pn; while (proot->seed!=proot) proot=proot->seed;
  while (pn->seed!=pn) {pnext=pn->seed; pn->seed=proot; pn=pnext;}
  return proot;
  }

/*** link a pair of resistive subnets ***/
int link_subnet_pair(NODE *pnS, NODE *pnD, int link_fixed)
  {
  if (!link_fixed && (pnS->fixed || pnD->fixed || pnS->vsource || pnD->vsource)) return 0;
  pnS=root_seed(pnS);
  pnD=root_seed(pnD);
  if (pnS==pnD) return 0; // already linked
  if (better_name_cmp(pnS->name,pnD->name)>0) pnD->seed=pnS;
  else pnS->seed=pnD;
  return 1; // progress
  }

/*** make all resistive subnets point at a single seed node ***/
void link_subnets(CIRCUIT *circuit, int link_fixed, int link_prime)
  {
  int i,progress,shorted_yet=0;
  NODE *pn,*pnVdd,*pnGND;
  DEVICE *pdev;

  /*** check for resistive shorts between Vdd and GND ***/
  pnGND = find_node(circuit,"","$GND");
  pnVdd = find_node(circuit,"","$Vdd");
  if (pnGND!=NULL && pnVdd!=NULL && pnGND==pnVdd)
    {
    shorted_yet=1;
    fprintf(stderr,"WARNING: $Vdd and $GND shorted via aliases.\n");
    }

  /*** start all nodes as seeds ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->seed=pn;
    }

  /*** propagate seed information through resistors ***/
  do
    {
    progress=0;
    for (i=0; i<circuit->Ndevices; i++)
      {
      pdev=&circuit->devices[i];
      if (isResistor(pdev))
        progress += link_subnet_pair(pdev->sub.res->nodes[0],pdev->sub.res->nodes[1],link_fixed);
      if (!shorted_yet && pnGND!=NULL && pnVdd!=NULL && pnVdd->seed==pnGND->seed)
        {
        shorted_yet = 1;
        fprintf(stderr,"WARNING: $Vdd and $GND shorted by:\n");
        print_device(stderr,pdev);
        }
      }
    } while (progress);

  /*** link prime nodes of transistors ***/
  if (link_prime) for (i=0; i<circuit->Ndevices; i++)
    {
    pdev=&circuit->devices[i];
    if (isTransistor(pdev))
      {
      link_subnet_pair(pdev->sub.trans->nodes[4],pdev->sub.trans->nodes[0],link_fixed); // sp+s
      link_subnet_pair(pdev->sub.trans->nodes[5],pdev->sub.trans->nodes[1],link_fixed); // dp+s
      }
    }

  /*** make sure nodes point directly to their seed ***/
  for (i=0; i<circuit->Nnodes; i++)
    {
    pn=&circuit->nodes[i];
    pn->seed=root_seed(pn);
    }
  }

/**
 * Return the pointer to a sparse matrix entry.  Finds pn2 in pn1->A
 * or returns NULL if not there.  Assumes local nodes and non-local
 * nodes are each sorted by address.
 **/
int find_matrix_entry(NODE *pn1, NODE *pn2)
  {
  BLOCK *pblock;
  LIST list;
  int j;
  pblock = pn1->pblock;

  /*** search in local nodes ***/
  list = list_wrapper(pblock->Nlocal,
                      sizeof(NODE *),
                      pblock->Nlocal*sizeof(NODE *),
                      pblock->nodes);
  j = find_element_sorted(&list,&pn2,&paddrcmp);
  if (j>=0) return j;

  /*** search in non-local nodes ***/
  list = list_wrapper(pblock->Nnodes-pblock->Nlocal,
                      sizeof(NODE *),
                      (pblock->Nnodes-pblock->Nlocal)*sizeof(NODE *),
                      pblock->nodes+pblock->Nlocal);
  j = find_element_sorted(&list,&pn2,&paddrcmp);
  if (j>=0) return j+pblock->Nlocal;

  /*** didn't find pn2 in pn1->A ***/
  return -1;
  }

/*** return the value of a sparse matrix entry ***/
REAL get_matrix_entry(NODE *pn1, NODE *pn2)
  {
  int j;
  j=find_matrix_entry(pn1,pn2);
  if (j>=0) return pn1->A[j];
  return 0;
  }

/*** compute coupling between two nodes ***/
double check_coupling(NODE *pn1, NODE *pn2)
  {
  double x,y11,y12,y21,y22;
  if (pn1->fixed || pn2->fixed || pn1->vsource || pn2->vsource) return 0;
  y11=get_matrix_entry(pn1,pn1);
  y12=get_matrix_entry(pn1,pn2);
  y21=get_matrix_entry(pn2,pn1);
  y22=get_matrix_entry(pn2,pn2);
  assert(y11!=0);
  assert(y22!=0);
  x=fabs((y12*y21)/(y11*y22));
  assert(x<=1);
  return x;
  }

/*** attach tightly coupled nodes to the same block ***/
void grow_seed(NODE *pn1, int jblock)
  {
  NODE *pn2;
  BLOCK *pblock;
  double x = options.coupling_cutoff;
  int m = options.partition_mode;
  int j;
  if (pn1->jblock>=0) return; // already in a block
  pn1->jblock=jblock;
  pblock=pn1->pblock;
  for (j=pblock->Nlocal; j<pblock->Nnodes; j++)
    {
    pn2=pblock->nodes[j];
    if (pn1->fixed || pn2->fixed || pn1->vsource || pn2->vsource) continue;
    if ((m==2 && pn1->seed==pn2->seed) || (m>=1 && check_coupling(pn1,pn2)>x*x))
      grow_seed(pn2,jblock);
    }
  }

/*** identify neighboring nodes and blocks ***/
void find_neighbors(CIRCUIT *circuit)
  {
  BLOCK *pblock;
  DEVICE *pdev;
  NODE *pn1,*pn2;
  int j,k,l;
  LIST **neighbors,list;

  /*** allocate neighbors lists ***/
  neighbors=(LIST **) leak_malloc(circuit->Nblocks*sizeof(LIST *));
  for (j=0; j<circuit->Nblocks; j++) neighbors[j]=list_create(sizeof(NODE *));

  /*** find neighbor nodes ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    for (k=0; k<device_Nnodes(pdev); k++)
      {
      pn1=device_nodes(pdev)[k];
      for (l=0; l<device_Nnodes(pdev); l++)
	{
        pn2=device_nodes(pdev)[l];
        if ((pn2->pblock!=pn1->pblock)&&
            (find_element_lazy_sort(neighbors[pn1->jblock],&pn2,&paddrcmp)<0))
          list_insert_element_lazy_sort(neighbors[pn1->jblock],&pn2,&paddrcmp);
        }
      }
    }

  /*** append neighbor nodes to pblock->nodes, sort properly ***/
  for (j=0; j<circuit->Nblocks; j++)
    {
    pblock=&circuit->blocks[j];
    list = list_wrapper(pblock->Nlocal,sizeof(NODE *),
                        pblock->Nlocal*sizeof(NODE *),pblock->nodes);
    list_sort(&list,&paddrcmp);
    list_finish_lazy_sort(neighbors[j],&paddrcmp);
    pblock->Nnodes = pblock->Nlocal + neighbors[j]->max;
    pblock->nodes = leak_realloc(pblock->nodes,pblock->Nnodes*sizeof(NODE *));
    for (k=pblock->Nlocal; k<pblock->Nnodes; k++)
      pblock->nodes[k] = neighbors[j]->p.pn[k-pblock->Nlocal];
    list_free(neighbors[j]);
    }
  leak_free(neighbors);
  }

/*** re/allocate sparse matrix rows ***/
void create_rows(CIRCUIT *circuit)
  {
  NODE *pn;
  int j,k,N;

  /*** free old sparse matrix ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    free_node(pn);
    }

  /*** allocate new sparse matrix ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    N=4*((pn->pblock->Nnodes+3)/4);      // round up to multiple of 4
    pn->A=leak_malloc16(N*sizeof(REAL)); // align to 16 byte boundary
    for (k=0; k<N; k++) pn->A[k]=0;      // fill with 0's
    }
  }

/*** fill in pdev->Aentry tables and pn->diagonal_index ***/
void make_Aentry_table(CIRCUIT *circuit)
  {
  int j,k,l,m,Nnodes;
  DEVICE *pdev;
  REAL **Aentry;
  NODE **nodes,*pn;

  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    Nnodes=device_Nnodes(pdev);
    nodes=device_nodes(pdev);
    Aentry=device_Aentry(pdev);
    for (k=0; k<Nnodes; k++) for (l=0; l<Nnodes; l++)
      {
      m=find_matrix_entry(nodes[k],nodes[l]);
      assert(m>=0);
      Aentry[Nnodes*k+l]=&nodes[k]->A[m];
      }
    }
  
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    pn->diagonal_index = find_matrix_entry(pn,pn);
    assert((pn->diagonal_index>=0) && (pn->diagonal_index<pn->pblock->Nlocal));
    }
  }

/*** debugging ***/
void print_blocks(CIRCUIT *circuit)
  {
  int j,k;
  BLOCK *pblock;
  for (j=0; j<circuit->Nblocks; j++)
    {
    pblock=&circuit->blocks[j];
    printf("Block %d: Nlocal=%d Nnodes=%d\n",j,pblock->Nlocal,pblock->Nnodes);
    printf("  Local:");
    for (k=0; k<pblock->Nlocal; k++)
      printf(" %s",get_node_name(pblock->nodes[k]));
    printf("\n  Neighbors:");
    for (k=pblock->Nlocal; k<pblock->Nnodes; k++)
      printf(" %s",get_node_name(pblock->nodes[k]));
    printf("\n\n");
    }
  }

/*** create initial blocks ***/
void initial_blocks(CIRCUIT *circuit)
  {
  int j;
  BLOCK block;

  /*** allocate 1 block per node ***/
  circuit->Nblocks = circuit->Nnodes;
  circuit->blocks = (BLOCK *) leak_malloc(circuit->Nblocks*sizeof(BLOCK));
  block.active=1;
  block.Nlocal=1;
  block.Nnodes=1;
  block.nodes=NULL;
  for (j=0; j<circuit->Nnodes; j++)
    {
    block.nodes=leak_malloc(sizeof(NODE **));
    block.nodes[0]=&circuit->nodes[j];
    circuit->blocks[j]=block;
    circuit->nodes[j].jblock=j;
    circuit->nodes[j].pblock=&circuit->blocks[j];
    }

  /*** finish up ***/
  find_neighbors(circuit);
  create_rows(circuit);
  make_Aentry_table(circuit);
  }

/*** create blocks and link them to nodes ***/
void final_blocks(CIRCUIT *circuit)
  {
  int j,Nblocks=0;
  BLOCK block,*pblock;
  NODE *pn;

  /*** initialize pn->jblock ***/
  for (j=0; j<circuit->Nnodes; j++) circuit->nodes[j].jblock=-1;

  /*** assign jblock numbers to tightly coupled nodes ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->jblock==-1) grow_seed(pn,Nblocks++);
    }

  /*** free old blocks ***/
  for (j=0; j<circuit->Nblocks; j++) free_block(&circuit->blocks[j]);
  leak_free(circuit->blocks);

  /*** allocate new blocks ***/
  circuit->Nblocks = Nblocks;
  circuit->blocks = (BLOCK *) leak_malloc(circuit->Nblocks*sizeof(BLOCK));
  block.active=1;
  block.Nlocal=0;
  block.Nnodes=0;
  block.nodes=NULL;
  for (j=0; j<circuit->Nblocks; j++) circuit->blocks[j]=block;

  /*** update pn->pblock, count Nlocal ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    pn->pblock=&circuit->blocks[pn->jblock];
    pn->pblock->Nlocal++;
    }

  /*** allocate local block->nodes ***/
  for (j=0; j<circuit->Nblocks; j++)
    {
    pblock=&circuit->blocks[j];
    pblock->nodes=(NODE **) leak_malloc(pblock->Nlocal*sizeof(NODE *));
    pblock->Nlocal=0;
    }

  /*** fill in local block->nodes ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    pblock=pn->pblock;
    pblock->nodes[pblock->Nlocal++]=pn;
    pblock->Nnodes=pblock->Nlocal;
    }

  /*** finish up ***/
  find_neighbors(circuit);
  create_rows(circuit);
  make_Aentry_table(circuit);
  }

/** report if any matrix entries are still over coupling_cutoff **/
void check_all_coupling(CIRCUIT *circuit)
  {
  int j,k,l;
  double x;
  BLOCK *pblock;
  NODE *pn1,*pn2;
  for (j=0; j<circuit->Nblocks; j++)
    {
    pblock=&circuit->blocks[j];
    for (k=0; k<pblock->Nlocal; k++)
      {
      pn1=pblock->nodes[k];
      for (l=pblock->Nlocal; l<pblock->Nnodes; l++)
        {
        pn2=pblock->nodes[l];
        x=check_coupling(pn1,pn2);
        if (x>options.coupling_cutoff*options.coupling_cutoff)
          fprintf(stderr,"WARNING: coupling between %s and %s is %g\n",
                  get_node_name(pn1),get_node_name(pn2),sqrt(x));
        }
      }
    }
  }

/*** look for bizarre circuit features ***/
void check_circuit(CIRCUIT *circuit)
  {
  int j,k,errors=0,type;
  NODE *pn;
  DEVICE *pdev;

  /*** set floatingC, portG, portN, portP fields ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    pn->floatingC=1;
    pn->portG=0;
    pn->portN=0;
    pn->portP=0;
    if (pn->force!=FORCE_NONE) pn->floatingC=0; // forced by alint
    else if ((pn->digital_node!=NULL)&&
             (pn->digital_node->UCount[0]+pn->digital_node->UCount[1]>0))
      pn->floatingC=0; // driven by prs
    }
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    for (k=0; k<device_Nnodes(pdev); k++)
      {
      pn=device_nodes(pdev)[k];
      if (!isCapacitor(pdev)) pn->floatingC=0;
      if (isTransistor(pdev))
        {
        type=((BSIM4model *)pdev->sub.trans->model)->type;
        if (k==2)                      pn->seed->portG=1;
        else if ((k<2)&&(type==NTYPE)) pn->seed->portN=1;
        else if ((k<2)&&(type==PTYPE)) pn->seed->portP=1;
        }
      }
    }

  /*** examine nodes for warnings and errors ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->floatingC==1)
      fprintf(dsim_err,"WARNING: node %s is a floating capacitor.\n",get_node_name(pn));
    }
  if (errors>0) exit(1);
  }

/*** examine matrix for errors ***/
void check_matrix(CIRCUIT *circuit)
  {
  int j,errors=0;
  NODE *pn;
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->pblock->Nnodes==1)
      {
      fprintf(dsim_err,"WARNING: node %s depends on no other nodes.\n",get_node_name(pn));
      continue;
      }
    if (get_matrix_entry(pn,pn)==0 && !pn->fixed)
      {
      fprintf(dsim_err,"ERROR: voltage on node %s has no effect on its current or charge.\n",get_node_name(pn));
      errors++;
      continue;
      }
    }
  if (errors>0) exit(1);
  }

/*** summarize properties of a circuit ***/
void summarize_circuit(CIRCUIT *circuit, FILE *out)
  {
  int j,k,Ntransistors=0,Ndiodes=0,Nsources=0,Ncapacitors=0,Nresistors=0;
  double cost;
  long mem[15],T=0;
  NODE *pn;
  DEVICE *pdev;
  BLOCK *pblock;
  DIGITAL_NODE *dnode;

  /*** count nodes and devices ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    if      (isTransistor(pdev)) Ntransistors++;
    else if (isDiode(pdev)) Ndiodes++;
    else if (isCapacitor(pdev)) Ncapacitors++;
    else if (isResistor(pdev)) Nresistors++;
    else if (isSource(pdev)) Nsources++;
    else assert(0);
    }
  fprintf(out,"Circuit statistics:\n");
  fprintf(out,"  %10d analog nodes\n",circuit->Nnodes);
  fprintf(out,"  %10d transistors\n",Ntransistors);
  fprintf(out,"  %10d diodes\n",Ndiodes);
  fprintf(out,"  %10d capacitors\n",Ncapacitors);
  fprintf(out,"  %10d resistors\n",Nresistors);
  fprintf(out,"  %10d sources\n",Nsources);
  fprintf(out,"  %10d digital nodes\n",circuit->Ndnodes);
  fprintf(out,"  %10d digital rules\n",circuit->Ndrules);
  fprintf(out,"  %10d digital gmas\n",circuit->Ngmas);
  fprintf(out,"  %10d digital groups\n",circuit->Ngroups);

  /*** partitioning ***/
  cost=0;
  for (j=0; j<circuit->Nblocks; j++)
    {
    int NL=circuit->blocks[j].Nlocal, NN=circuit->blocks[j].Nnodes;
    cost+=NL*NL*NN; // approximate cost of conditioning
    }
  if (circuit->blocks==NULL)
    fprintf(out,"  block partitioning deferred\n");
  else
    fprintf(out,"  %d blocks, cost=%g cost/node=%g\n",circuit->Nblocks,cost,cost/circuit->Nnodes);

  /*** compute memory usage ***/
  for (j=0; j<15; j++) mem[j]=0;
  mem[1]=circuit->Nnodes*sizeof(NODE);
  mem[2]=circuit->Nblocks*sizeof(BLOCK);
  mem[3]=circuit->names->max*(sizeof(PARSENODE *)+sizeof(PARSENODE));
  for (j=0; j<circuit->names->max; j++)
    {
    mem[3]+=strlen(circuit->names->p.parsenode[j]->name)+1;
    }
  if (circuit->blocks!=NULL) for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    mem[4]+=pn->pblock->Nnodes*sizeof(REAL);
    }
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    mem[5]+=device_mem(pdev);
    }
  for (j=0; j<circuit->Nblocks; j++)
    {
    pblock=&circuit->blocks[j];
    mem[6]+=pblock->Nnodes*sizeof(NODE *);
    }
  for (j=0; j<circuit->Ndnodes; j++)
    {
    dnode=&circuit->dnodes[j];
    mem[8]+=sizeof(DIGITAL_NODE) +
      sizeof(DIGITAL_RULE *)*(dnode->Nrules[0]+dnode->Nrules[1]);
    }
  mem[10]=circuit->Ndrules*sizeof(DIGITAL_RULE);
  mem[11]=circuit->Ndnodes*sizeof(DIGITAL_NODE *);
  mem[12]=circuit->Ngmas*sizeof(DIGITAL_GMA);
  for (j=0; j<circuit->Ngmas; j++)
    {
    DIGITAL_GMA *gma = &circuit->gmas[j];
    mem[12] += sizeof(LIST) + gma->nodes->max*sizeof(DIGITAL_NODE *);
    mem[12] += sizeof(LIST) + gma->guard->max*sizeof(BIOP);
    mem[12] += sizeof(LIST) + gma->expressions->max*sizeof(LIST *);
    mem[12] += sizeof(LIST) + gma->delays->max*sizeof(LIST *);
    for (k=0; k<gma->expressions->max; k++)
      mem[12] += sizeof(LIST) + sizeof(BIOP) * gma->expressions->p.list[k]->max;
    for (k=0; k<gma->delays->max; k++)
      mem[12] += sizeof(LIST) + sizeof(BIOP) * gma->delays->p.list[k]->max;
    }
  mem[13]=total_bigint_memory();
  mem[14]=circuit->Ngroups*sizeof(DIGITAL_GROUP);
  for (j=0; j<circuit->Ngroups; j++)
    {
    DIGITAL_GROUP *group = &circuit->groups[j];
    mem[14] += sizeof(LIST) + group->nodes->max*sizeof(DIGITAL_NODE *);
    }
  for (j=1; j<15; j++) T+=mem[j];

  /*** print memory usage ***/
  fprintf(out,"Total simulation memory %ld bytes.\n",T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for names\n",mem[3],100.0*mem[3]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for nodes\n",mem[1],100.0*mem[1]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for sparse matrix\n",mem[4],100.0*mem[4]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for devices\n",mem[5],100.0*mem[5]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for blocks\n",mem[2],100.0*mem[2]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for block node lists\n",mem[6],100.0*mem[6]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for digital nodes\n",mem[8],100.0*mem[8]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for digital rules\n",mem[10],100.0*mem[10]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for digital gmas\n",mem[12],100.0*mem[12]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for digital groups\n",mem[14],100.0*mem[14]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for digital event queue\n",mem[11],100.0*mem[11]/T);
  fprintf(out,"  %10ld bytes (%2.0f%%) for big integers\n",mem[13],100.0*mem[13]/T);
  }

/*** do the minimum necessary preparation for the full circuit before alint ***/
void prepare_circuit(CIRCUIT *circuit)
  {
  /*** fill in pn->seed to link resistive subnets ***/
  link_subnets(circuit,1,0);

  /*** check for bad circuit characteristics ***/
  check_circuit(circuit);
  }

/*** create partitioning blocks and additional structures before simulation ***/
int partition_circuit(CIRCUIT *circuit)
  {
  /*** skip if already partitioned ***/
  if (circuit->blocks!=NULL) return 0;

  /*** sort devices by type, model, L, W ***/
  sort_devices(circuit);

  /*** sort exclusives so largest sets come first ***/
  sort_exclusives(circuit);

  /*** good time to free memory associated with sorting ***/
  free_temp_list();

  /*** initial partition ***/
  initial_blocks(circuit);
  // print_blocks(circuit);

  /*** evaluate devices in order to partition ***/
  bsim4_warn=1;
  evaluate_devices(circuit,1,-0.5*options.timestep,0,1,0,options.timestep);
  bsim4_warn=0;
  // check_all_coupling(circuit);

  /*** check for bad matrix ***/
  check_matrix(circuit);
  
  /*** final partition ***/
  if (options.partition_mode>0)
    {
    link_subnets(circuit,0,1); // link subnets but not through vsource or fixed nodes, link prime nodes
    final_blocks(circuit); // repartition
    // print_blocks(circuit);
    evaluate_devices(circuit,1,-0.5*options.timestep,0,1,0,options.timestep);
    check_all_coupling(circuit); // check again just to be sure
    link_subnets(circuit,1,0); // link subnets normally again
    }

  /*** good time to free memory associated with sorting ***/
  free_temp_list();

  /*** done ***/
  return 1;
  }
