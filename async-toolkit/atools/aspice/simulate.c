/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/*** global profiling counters ***/
double device_evaluations=0,bsim_evaluations=0,total_steps=0,
  total_iterations=0,node_relaxations=0,block_conditionings=0;
int trace_smoothV=0; /* for debugging A2D conversion */
int circuit_warnings=0, max_circuit_warnings=1000;

/****************************** SIMULATION DEVICES ******************************/

/*** accumulate from device to a sparse matrix entries ***/
inline void accumulate_device(int N, NODE **pn, REAL **Aentry,
                              double *dQdV, double *dIdV, double *Q, double *I,
                              double mAq, double mAi, double mBq, double mBi)
  {
  int j,k,l;
  for (l=0, j=0; j<N; j++)
    {
    pn[j]->B += mBq*Q[j] + mBi*I[j];
    for (k=0; k<N; k++, l++) *Aentry[l] += mAq*dQdV[l] + mAi*dIdV[l];
    }
  }

/*** nodes=a,b parms=C ***/
inline void capacitor(DEVICE *pdev, double mAq, double mAi, double mBq, double mBi, double time)
  {
  NODE **pn=pdev->sub.cap->nodes;
  REAL **Aentry=pdev->sub.cap->Aentry,C=pdev->sub.cap->C;
  double x,y;
  x = mBq*(pn[0]->V - pn[1]->V)*C;
  y = mAq*C;
  pn[0]->B   += x;
  pn[1]->B   -= x;
  *Aentry[0] += y;
  *Aentry[1] -= y;
  *Aentry[2] -= y;
  *Aentry[3] += y;
  }

/*** nodes=a,b parms=1/R ***/
inline void resistor(DEVICE *pdev, double mAq, double mAi, double mBq, double mBi, double time)
  {
  NODE **pn=pdev->sub.res->nodes;
  REAL **Aentry=pdev->sub.res->Aentry,G=pdev->sub.res->G;
  double x,y;
  x = (pn[0]->V - pn[1]->V)*G;
  pdev->sub.res->I = x; // save current
  x = mBi*x;
  y = mAi*G;
  pn[0]->B   -= x;
  pn[1]->B   += x;
  *Aentry[0] -= y;
  *Aentry[1] += y;
  *Aentry[2] += y;
  *Aentry[3] -= y;
  }

/*** nodes=s,d,g,b,sp,dp parms=W,L,AS,PS,NRS,AD,PD,NRD,SA,SB,SC,SD,NF,SCA,SCB,SCC ***/
void bsim4_transistor(DEVICE *pdev, double mAq, double mAi, 
                      double mBq, double mBi, double time)
  {
  NODE **pn=pdev->sub.trans->nodes;
  double Q[6],I[6],dQ[36],dI[36];
  BSIM4_Transistor(pdev,bsim4_warn,Q,I,dQ,dI);
  accumulate_device(6,pn,pdev->sub.trans->Aentry,dQ,dI,Q,I,
		    mAq,mAi,mBq,mBi);
  }

/*** nodes=s,b parms=W,L,AS,PS */
void bsim4_diode(DEVICE *pdev, double mAq, double mAi, 
                 double mBq, double mBi, double time)
  {
  error("BSIM4 diodes are integrated into transistor model.\n");
  }

/*** first node is output, parms are OP list ***/
/*** nodes must be unique, or derivative fails! ***/
void current_source(DEVICE *pdev, double mAq, double mAi, double mBq, double mBi, double time)
  {
  NODE **pn=pdev->sub.source->nodes;
  OP *ops=pdev->sub.source->ops;
  int j,N=pdev->sub.source->Nnodes,Nops=pdev->sub.source->Nops;
  double x,ov,G;
  x=evaluate_by_nodes(Nops,ops,N,pn,time);
  pdev->sub.source->I=x; // save current
  pn[0]->B += x*mBi;
  for (j=0; j<N; j++) // accumulate dIdV
    {
    ov=pn[j]->V;
    pn[j]->V+=TINY;
    G=(evaluate_by_nodes(Nops,ops,N,pn,time)-x)/TINY;
    *pdev->sub.source->Aentry[j] += mAi*G;
    pn[j]->V=ov;
    }
  }

/*** first node is output, parms are OP list ***/
/*** nodes must be unique, or derivative fails! ***/
void voltage_source(DEVICE *pdev, double mAq, double mAi, double mBq, double mBi, double time)
  {
  NODE **pn=pdev->sub.source->nodes;
  OP *ops=pdev->sub.source->ops;
  int N=pdev->sub.source->Nnodes,Nops=pdev->sub.source->Nops;
  double x;
  if (pn[0]->fixed) return; // don't force fixed nodes
  x=evaluate_by_nodes(Nops,ops,N,pn,time);
  if (pn[0]->vsource)
    {
    fprintf(dsim_err,"WARNING: multiple voltage sources drive node %s\n",
            get_node_name(pn[0]));
    circuit_warning();
    }
  pn[0]->vsource = 1; // voltage source sets pn->X directly
  pn[0]->X = (x-pn[0]->V)/options.timestep;
  }

/**
 * Measure current injected into a node by a voltage source.  Relies
 * on the original AX=B where B is current, not normalized,
 * conditioned, or relaxed.  For this node, X is given, and we solve
 * for an additional I that satisfies AX=B+I.  That I gets saved to
 * the trace file.
 **/
double measure_node_current(NODE *pn)
  {
  int k;
  double I;
  I = -pn->B;
  for (k=0; k<pn->pblock->Nnodes; k++)
    I += pn->A[k] * pn->pblock->nodes[k]->X;
  return I;
  }

/** abort if too many warnings **/
void circuit_warning()
  {
  if (++circuit_warnings>max_circuit_warnings)
    {
    fprintf(stderr,"ERROR: more than %d warnings\n",max_circuit_warnings);
    exit(1);
    }
  }

/***************************** EVALUATE DEVICES **********************************/

/*** evaluate all devices and accumulate on to nodes ***/
void evaluate_devices(CIRCUIT *circuit, double mAq, double mAi,
                      double mBq, double mBi, double time, double h)
  {
  int j,k,N,N4;
  NODE *pn;
  DEVICE *pdev;
  REAL *a;

  /*** clear A,B of nodes, evaluate lumped capacitance ***/
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    pn->B = 0;
    pn->vsource = 0;
    a = pn->A;
    N = pn->pblock->Nnodes;
    N4 = (N+3)/4;
#if SIMD
    {
    v4sf *v = (v4sf *) a, y;
    scalar_to_v4sf(y,0);
    for (k=N4; k>0; k--) {*v = y; v++;}
    }
#else
    for (k=0; k<N; k++) a[k] = 0;
#endif
    if (pn->CG>0) // evaluate lumped capacitor
      {
      pn->B += mBq*pn->V*pn->CG;
      pn->A[pn->diagonal_index] +=  mAq*pn->CG;
      }
    }

  /*** evaluate/accumulate devices Q,I,A ***/
  for (j=0; j<circuit->Ndevices; j++)
    {
    pdev=&circuit->devices[j];
    device_evaluations++;
    if (pdev->device_func==&capacitor) /** optimized capacitors **/
      capacitor(pdev,mAq,mAi,mBq,mBi,time);
    else if (pdev->device_func==&resistor) /** optimized resistors **/
      resistor(pdev,mAq,mAi,mBq,mBi,time);
    else pdev->device_func(pdev,mAq,mAi,mBq,mBi,time); /** other devices **/
    }

  /*** evaluate digital devices and convert to analog ***/
  digital2analog(circuit,mAq,mAi,mBq,mBi,time,h);
  }

/************************** CONDITION MATRIX *******************************/

/*** debugging ***/
void print_rows(CIRCUIT *circuit)
  {
  int j,k;
  NODE *pn;
  BLOCK *pblock;
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    pblock = pn->pblock;
    printf("%s B=%g A=",get_node_name(pn),pn->B);
    for (k=0; k<pblock->Nnodes; k++)
      printf("%s:%g ",get_node_name(pblock->nodes[k]),pn->A[k]);
    printf("\n\n");
    }
  }

/*** do gauss-jordan elimation on nodes in blocks ***/
void condition(CIRCUIT *circuit)
  {
  NODE *pn1,*pn2;
  int i,j,k,l,Nnodes,Nlocal,N4;
  BLOCK *pblock;
  REAL x,*a1,*a2;

  /*** for all blocks ***/
  for (i=0; i<circuit->Nblocks; i++)
    {
    pblock=&circuit->blocks[i];
    pblock->active=1;
    Nnodes=pblock->Nnodes;
    Nlocal=pblock->Nlocal;
    N4=(Nnodes+3)/4; // round up to multiple of 4

    /*** make local nodes block identity matrix ***/
    for (j=0; j<Nlocal; j++)
      {
      /*** normalize this row ***/
      pn1 = pblock->nodes[j];
      if (pn1->fixed || pn1->vsource) continue; // don't modify voltage sources
      a1 = pn1->A;
      x = 1/a1[j];
#ifdef SIMD
      {
      v4sf *v1 = (v4sf *) a1, y;
      scalar_to_v4sf(y,x);
      for (k=N4; k>0; k--) // 4 at a time
        {
        *v1 *= y;
        v1++;
        }
      }
#else
      for (k=0; k<Nnodes; k++) a1[k] *= x;
#endif
      a1[j] = 1;
      pn1->B *= x;

      /*** subtract rows to cancel nondiagonal terms in this column ***/
      for (k=0; k<Nlocal; k++) if (j!=k)
        {
        pn2 = pblock->nodes[k];
        a2 = pn2->A;
        x = a2[j];
        if (x==0) continue; // nothing to cancel out
#ifdef SIMD
        {
        v4sf *v1 = (v4sf *) a1, *v2 = (v4sf *) a2, y;
        scalar_to_v4sf(y,x);
        for (l=N4; l>0; l--) // 4 at a time
          {
          *v2 -= y * (*v1);
          v1++;
          v2++;
          }
        }
#else
        for (l=0; l<Nnodes; l++) a2[l] -= x * a1[l];
#endif
        a2[j] = 0;
        pn2->B -= x * pn1->B;
        }
      }
    block_conditionings++;
    }
  }

/***************************** SOLVE MATRIX ******************************/

/*** solve AX=B using node relaxation on active blocks ***/
void relax(CIRCUIT *circuit)
  {
  int active,j,k,l,iterations=0,halflife=100,Nnodes,Nlocal,bad;
  BLOCK *pblock;
  NODE *pn,**nodes;
  double dx,alpha=1,minalpha=0.25;
  REAL *A;
  do
    {
    active=0;
    iterations++;

    /** check for poor convergence **/
    bad = (iterations%halflife) == 0;
    if (bad) {
      alpha = minalpha + (1-minalpha)*pow(2,-iterations/(double) halflife);
      fprintf(dsim_err,"NOTE: relaxation iterations=%d, alpha=%g, time=%gns\n",
              iterations,alpha,circuit->time*1e9);
      circuit_warning();
    }

    /** relax blocks **/
    for (j=0; j<circuit->Nblocks; j++)
      {
      pblock=&circuit->blocks[j];
      if (!pblock->active) continue;
      pblock->active=0;
      Nnodes=pblock->Nnodes;
      Nlocal=pblock->Nlocal;
      
      /*** relax nodes in block ***/
      for (k=0; k<Nlocal; k++)
        {
        pn=pblock->nodes[k];
        if (pn->fixed || pn->vsource) continue; // don't modify voltage sources
        dx = pn->B - pn->X;
        A = &pn->A[Nlocal];
        nodes = pblock->nodes + Nlocal;
        for (l=Nnodes-Nlocal; l>0; l--)
          {dx -= A[0] * nodes[0]->X; A++; nodes++;}
        pn->X += alpha*dx;
        node_relaxations++;
        if (fabs(dx)>options.maxerr)
          {
          pblock->active=active=1;
          if (bad) fprintf(dsim_err,"  active_node=%s\n",get_node_name(pn));
          }
        }

      /*** if nodes moved, activate all neighboring blocks ***/
      if (pblock->active)
        {
        nodes = pblock->nodes + Nlocal;
        for (k=Nnodes-Nlocal; k>0; k--) {nodes[0]->pblock->active=1; nodes++;}
        }
      }
    } while (active);
  total_iterations+=iterations;
  }

/**************************** INTEGRATION ALGORITHM *****************************/

/*** update V/smoothV/smoothX ***/
void update(CIRCUIT *circuit, double h)
  {
  int j;
  NODE *pn;
  double alpha,beta;
  if (options.a2d_ewma_tau<=0) alpha=0;
  else alpha = exp(-h/options.a2d_ewma_tau);
  beta=1-alpha;
  for (j=0; j<circuit->Nnodes; j++)
    {
    pn=&circuit->nodes[j];
    if (pn->fixed && pn->X!=0)
      {
      fprintf(stderr,"ERROR: fixed node %s has X=%g\n",get_node_name(pn),pn->X);
      exit(1);
      }
    if (pn->fixed) continue;

    /*** update V ***/
    pn->V += pn->X * h;

    /*** clamp V between vmin and vmax ***/
    if (pn->V<options.vmin)
      {
      fprintf(dsim_err,"WARNING: Clamped %s from %gV to %gV at time %gns\n",
                  get_node_name(pn),pn->V,options.vmin,circuit->time*1e9);
      pn->V=options.vmin;
      circuit_warning();
      }
    else if (pn->V>options.vmax)
      {
      fprintf(dsim_err,"WARNING: Clamped %s from %gV to %gV at time %gns\n",
              get_node_name(pn),pn->V,options.vmax,circuit->time*1e9);
      pn->V=options.vmax;
      circuit_warning();
      }

    /*** update smoothV/smoothX ***/
    pn->smoothV = alpha * pn->smoothV + beta * pn->V;
    pn->smoothX = alpha * pn->smoothX + beta * pn->X;
    }
  }

/***
 * Second order forward integration.  Given C=dQ/dV capacitance
 * matrix, G=dI/dV conductance matrix, I current vector, and h
 * timestep, solve for dV/dt which satsifies:
 *
 * C*dV/dt = I + 0.5*h*G*dV/dt
 *
 * Formulate as sparse matrix equation AX=B,
 * where A=C-0.5*h*G, X=dV/dt, B=I.
 ***/
DIGITAL_NODE *semi_midpoint(CIRCUIT *circuit, double h)
  {
  /*** evaluate analog devices and accumulate to nodes ***/
  evaluate_devices(circuit,1,-0.5*h,0,1,circuit->time,h);

  /*** condition blocks ***/
  condition(circuit);

  /*** solve AX=B using node relaxation on active blocks ***/
  relax(circuit);

  /*** update voltages (V+=X) ***/
  update(circuit,h);

  /*** convert analog voltages into digital events ***/
  if (analog!=ANALOG_SETTLING) return analog2digital(circuit);
  return NULL;
  }

/**************************** DRIVE SIMULATION *******************************/

/** inner loop of simulation ***/
DIGITAL_NODE *simulate_circuit_inner(CIRCUIT *circuit)
  {
  int j,dotrace;
  double lastpost=-options.poststep,h=options.timestep,lastcheck,usertime;
  DIGITAL_NODE *breaknode=NULL;
  DEVICE *pdev;

  /*** start trace/names files, partition circuit if you haven't already ***/
  if (partition_circuit(circuit)) summarize_circuit(circuit,stdout);
  prepare_tracefile(circuit);

  /*** simulate ***/
  lastcheck=user_time();
  while (circuit->time < options.timemax)
    {
    dotrace = (circuit->ftrace!=NULL)&&(analog!=ANALOG_SETTLING)&&
      (circuit->time>=lastpost+0.999*options.poststep);

    /*** write time and node voltages ***/
    if (dotrace)
      {
      /*** write time ***/
      write_float(circuit->ftrace,circuit->time);

      /*** write voltages ***/
      for (j=0; j<circuit->Nnodes; j++)
	if (circuit->nodes[j].watch)
          write_float(circuit->ftrace,
                      trace_smoothV ? circuit->nodes[j].smoothV : circuit->nodes[j].V);
      }

    /*** advance simulation to next time step ***/
    breaknode = semi_midpoint(circuit,h);
    circuit->time += h;
    total_steps++;

    /*** write currents through named resistors and sources ***/
    if (dotrace)
      {
      for (j=0; j<circuit->Ndevices; j++)
        {
        pdev = &circuit->devices[j];
        if (pdev->name==NULL) continue;
        if (isResistor(pdev))
          write_float(circuit->ftrace,pdev->sub.res->I);
        else if (isCurrentSource(pdev))
          write_float(circuit->ftrace,pdev->sub.source->I);
        else if (isVoltageSource(pdev))
          write_float(circuit->ftrace,measure_node_current(pdev->sub.source->nodes[0]));
        }

      /*** advance time ***/
      lastpost=circuit->time;

      /*** checkpoint ***/
      usertime=user_time();
      if ((options.checkpoint_interval>0) && 
          (usertime>lastcheck+options.checkpoint_interval))
        {
        lastcheck=usertime;
        save_state(circuit);
        }
      }

    /*** break for interactive sims ***/
    if (interrupt_sim || (breaknode!=NULL)) break;
    }
  return breaknode;
  }

/*** handle ctrl-c's ***/
static void interruptHandler(){interrupt_sim=1;} 

/** interactive inner loop of simulation ***/
void simulate_circuit_interactive(CIRCUIT *circuit)
  {
  char *oldline=NULL,templine[STRMAX],*line;
  int tty;

  /*** user interface ***/
  tty=isatty(0) && (dsim_in == stdin);

  /*** interactive loop ***/
  if (tty) signal(SIGINT,interruptHandler);
  while(1)
    {
    if (tty)
      {
      interrupt_sim=0;
      line=readline("(asim) ");
      if (line==NULL) {printf("\n"); break;}
      if ((line[0]!=0)&&((oldline==NULL)||
                         (strcmp(line,oldline)!=0))) add_history(line);
      if (oldline!=NULL) free(oldline);
      oldline=line;
      }
    else
      {
      line=fgets(templine,STRMAX,dsim_in);
      if (line==NULL) break;
      printf("(asim) %s",line);
      }
    parse_user(line,circuit);
    if (circuit->ftrace!=NULL) fflush(circuit->ftrace); // flush trace file
    }
  }

/*** main loop of simulation ***/
void simulate_circuit(CIRCUIT *circuit, int interactive, int report)
  {
  /*** clear statistics ***/
  device_evaluations=0;
  bsim_evaluations=0;
  total_steps=0;
  total_iterations=0;
  node_relaxations=0;
  block_conditionings=0;

  /*** inner loop simulation ***/
  if (!interactive) simulate_circuit_inner(circuit);
  else              simulate_circuit_interactive(circuit);

  /*** report statistics ***/
  if (report && (total_steps>0))
    {
    printf("Simulations statistics:\n");
    printf("  %10.0f total time steps\n",total_steps);
    printf("  %10.0f device evaluations\n",device_evaluations);
    if (bsim_evaluations>0) 
      printf("  %10.0f bsim evaluations\n",bsim_evaluations);
    printf("  %10.0f block conditionings\n",block_conditionings);
    printf("  %10.0f relaxation passes\n",total_iterations);
    printf("  %10.2f relaxation passes/timestep\n",total_iterations/total_steps);
    printf("  %10.2f relaxations per node per timestep\n",
            node_relaxations/circuit->Nnodes/total_steps);
    }
  }
