#ifdef JNI
#include <jni.h>
#endif
#include "solve.h"

int ERROR = 0, INTERRUPT = 0;
FILE *minout = NULL;
FILE *output;
SOLVE Solve;

/************************** output/debugging ***************************/

/*** print all variables ***/
void print_variables(FILE *fp, LIST *variables) {
  int i;
  VAR *pv;
  for (i=0; i<variables->max; i++) {
    pv = variables->p.pvar[i];
    print_var(fp,pv);
  }
}

/*** print all independent variables ***/
void print_independent_variables(FILE *fp, LIST *variables) {
  int i;
  VAR *pv;
  for (i=0; i<variables->max; i++) {
    pv = variables->p.pvar[i];
    if (pv->formula->max>0) continue;
    fprintf(fp,"set %s %g\n",pv->name,get(pv));
  }
}

/*** set interrupt flag on Ctrl-C ***/
void interrupt() {
  INTERRUPT = 1;
}

/*** save intermediate state ***/
void snapshot() {
  FILE *fp;
  fprintf(stderr,"Energy=%g, Constraint=%g, Wrote snapshot.fmla\n",
          get(Solve.energy),get(Solve.constraint));
  fp = fopen("snapshot.fmla","wt");
  print_variables(fp,Solve.variables);
  fclose(fp);
}

/*** print group results ***/
void write_groups(FILE *fp) {
  int i;
  for (i=0; i<Solve.groups->max; i++)
    print_independent_variables(fp,Solve.groups->p.plist[i]);
}

/********************** interface to minimize.c ********************/

/*** unpack double array into independent variables ***/
void unpack_state(double *p) {
  int i;
  for (i=0; i<Solve.current_group->max; i++)
    set(Solve.current_group->p.pvar[i],p[i]);
}

/*** pack independent variables into double array ***/
void pack_state(double *p) {
  int i;
  for (i=0; i<Solve.current_group->max; i++)
    p[i]=get(Solve.current_group->p.pvar[i]);
}

/*** evaluate combined energy ***/
double get_energy(void *parms, double *p) {
  double E,C;
  unpack_state(p);
  E = get(Solve.energy);
  C = get(Solve.constraint);
  return E + 2 * Solve.constraint_weight / Solve.constraint->formula->max * C;
}

/*** free saved gradients ***/
void free_gradient() {
  if (Solve.dE!=NULL) leak_free(Solve.dE);
  if (Solve.dC!=NULL) leak_free(Solve.dC);
  Solve.dE=Solve.dC=NULL;
}

/*** compute and save gradient variables for current_group **/
void cache_gradient() {
  int i,N;
  VAR *pv;
  N = Solve.current_group->max;
  free_gradient();
  Solve.dE = (VAR **) leak_malloc(N*sizeof(VAR *));
  Solve.dC = (VAR **) leak_malloc(N*sizeof(VAR *));
  for (i=0; i<N; i++) {
    pv = Solve.current_group->p.pvar[i];
    fprintf(stderr,"Computing derivative with respect to %s (%d of %d)\n",pv->name,i,N);
    Solve.dE[i] = derivative_var(Solve.variables,Solve.derivatives,Solve.energy,pv);
    Solve.dC[i] = derivative_var(Solve.variables,Solve.derivatives,Solve.constraint,pv);
  }
}

/*** evaluate gradient of combined energy ***/
void get_down(void *parms, double *p, double *d) {
  double TINY=1e-6,E0,E,op;
  int i;
  if ((Solve.dE!=NULL) && (Solve.dC!=NULL)) {
    unpack_state(p);
    for (i=0; i<Solve.current_group->max; i++)
      d[i] = - get(Solve.dE[i]) - 
        2 * Solve.constraint_weight / Solve.constraint->formula->max * get(Solve.dC[i]);
  }
  else {
    E0=get_energy(parms,p);
    for (i=0; i<Solve.current_group->max; i++) {
      op=p[i];
      p[i]+=TINY;
      E=get_energy(parms,p);
      d[i]=(E0-E)/TINY;
      p[i]=op;
    }
  }
}

/*************************** main solver ****************************/

/*** print summary of solving run ***/
void summary(int group, int steps, int N, double weight) {
  if (group>=0) fprintf(stderr,"Solved Group=%d, ",group);
  else fprintf(stderr,"Solved ");
  fprintf(stderr,"N=%d, Steps=%d, Weight=%g, Energy=%g, Constraint=%g\n",
          N,steps,weight,get(Solve.energy),get(Solve.constraint));
}

/*** solve constrained optimization with simulated annealing ***/
void solve() {
  int i,N,steps;
  double *p,w,start_time;
  VAR *pv;

  /** Ctrl-C forces a snapshot **/
  signal(SIGINT,interrupt);
  start_time=user_time();

  /** set current_group list to all independent variables **/
  Solve.current_group = list_create(sizeof(VAR *));
  for (i=0; i<Solve.variables->max; i++)
    {
    pv = Solve.variables->p.pvar[i];
    if(pv->formula->max==0) list_append_element(Solve.current_group,&pv);
    }

  /** allocate and pack state **/
  N=Solve.current_group->max;
  p=(double *) leak_malloc(sizeof(double)*N);
  pack_state(p);
  if (Solve.analytic) cache_gradient();

  /** report setup time separately **/
  fprintf(stderr,"derivative time=%gs\n",user_time()-start_time);
  start_time=user_time();

  /** solve **/
  for (w=Solve.initial_weight; w<=Solve.final_weight; w*=Solve.step_weight) {
    Solve.constraint_weight=w;
    steps=minimize(minout,NULL,
                   (ENERGY *)&get_energy,(DOWN *)&get_down,
                   N,p,
                   ceil(N*Solve.iteration_ratio),Solve.min_gradient,Solve.tolerance);
    if (INTERRUPT) {snapshot(); INTERRUPT=0;}
    summary(-1,steps,N,w);
  }
  
  /** unpack and free **/
  free_gradient();
  unpack_state(p);
  leak_free(p);
  fprintf(stderr,"solve time=%gs\n",user_time()-start_time);
  signal(SIGINT,SIG_DFL); /* Ctrl-C back to normal */

  /** free current_group **/
  list_free(Solve.current_group);
  Solve.current_group = NULL;
}

/*** solve constrained optimization with simulated annealing and groups ***/
void solve_grouped() {
  int i,j,N,steps;
  double *p,w,start_time;
  signal(SIGINT,interrupt); /* Ctrl-C forces a snapshot */
  start_time=user_time();
  N=0;
  for (i=0; i<Solve.groups->max; i++) {
    j = Solve.groups->p.plist[i]->max;
    if (j>N) N=j;
  }
  p=(double *) leak_malloc(sizeof(double)*N);
  for (w=Solve.initial_weight; w<=Solve.final_weight; w*=Solve.step_weight) {
    Solve.constraint_weight=w;
    for (i=0; i<Solve.groups->max; i++) {
      Solve.current_group = Solve.groups->p.plist[i];
      N=Solve.current_group->max;
      pack_state(p);
      steps=minimize(minout,NULL,
                     (ENERGY *)&get_energy,(DOWN *)&get_down,
                     N,p,
                     ceil(N*Solve.iteration_ratio),Solve.min_gradient,Solve.tolerance);
      unpack_state(p);
      if (INTERRUPT) {snapshot(); INTERRUPT=0;}
      summary(i,steps,N,w);
    }
  }
  leak_free(p);
  fprintf(stderr,"solve time=%gs\n",user_time()-start_time);
  signal(SIGINT,SIG_DFL); /* Ctrl-C back to normal */
}

/*** solve constrained optimization with simulated annealing and groups ***/
void solve_parallel(int NG, int IG, int argc, char *argv[]) {
  int i,j,N,iter=0,steps;
  double *p,w,start_time;
  FILE *fp;
  char str[STRMAX],str2[STRMAX];
  signal(SIGINT,interrupt); /* Ctrl-C forces a snapshot */
  start_time=user_time();
  N=0;
  for (i=0; i<Solve.groups->max; i++) {
    j = Solve.groups->p.plist[i]->max;
    if (j>N) N=j;
  }
  p=(double *) leak_malloc(sizeof(double)*N);
  for (w=Solve.initial_weight; w<=Solve.final_weight; w*=Solve.step_weight) {
    Solve.constraint_weight=w;

    // do one step of minimization over your own groups
    for (i=0; i<Solve.groups->max; i++) {
      Solve.current_group = Solve.groups->p.plist[i];
      N=Solve.current_group->max;
      pack_state(p);
      steps=minimize(minout,NULL,
                     (ENERGY *)&get_energy,(DOWN *)&get_down,
                     N,p,
                     ceil(N*Solve.iteration_ratio),Solve.min_gradient,Solve.tolerance);
      unpack_state(p);
      if (INTERRUPT) {snapshot(); INTERRUPT=0;}
      summary(i,steps,N,w);
    }
    
    // write your own results atomically
    safe_sprintf(str,"%d:%d.temp.fmla",iter,IG);
    safe_sprintf(str2,"%d:%d.fmla",iter,IG);
    fp = fopen(str,"wt");
    write_groups(fp);
    fclose(fp);
    rename(str,str2);
    
    // read all other results
    for (i=0; i<NG; i++) if (i!=IG) {
      FILE *file;
      safe_sprintf(str,"%d:%d.fmla",iter,i);
      do {
        file = fopen(str,"rt");
        sleep(1);
      } while (file==NULL);
      parse(file,argc,argv);
    }
    iter++;
  }
  leak_free(p);
  fprintf(stderr,"solve time=%gs\n",user_time()-start_time);
  signal(SIGINT,SIG_DFL); /* Ctrl-C back to normal */
}

/************************* parsing and setup **************************/

/*** set defaults ***/
void init_solve() {
  /*** annealing settings ***/
  Solve.initial_weight=0.1;
  Solve.final_weight=1e6;
  Solve.step_weight=10;
  Solve.iteration_ratio=1;
  Solve.min_gradient=1e-20;
  Solve.tolerance=0;
  Solve.analytic=0;

  /*** grouping ***/
  Solve.groups = list_create(sizeof(LIST *));
  Solve.current_group = NULL;
  Solve.dE = NULL;
  Solve.dC = NULL;

  /*** allocate recalc functions ***/
  Solve.functions = alloc_functions();

  /*** variables ***/
  Solve.energy = new_var("ENERGY",0);
  Solve.constraint = new_var("CONSTRAINT",0);
  fmla_constant(Solve.energy,0);
  fmla_constant(Solve.constraint,0);
  Solve.variables = list_create(sizeof(VAR *));
  Solve.derivatives = list_create(sizeof(DERIVATIVE *));
  list_insert_element_lazy_sort(Solve.variables,&Solve.energy,&var_cmp);
  list_insert_element_lazy_sort(Solve.variables,&Solve.constraint,&var_cmp);
}

/*** free Solve data ***/
void free_solve() {
  int i;
  free_functions(Solve.functions);
  free_variables(Solve.variables);
  free_derivatives(Solve.derivatives);
  for (i=0; i<Solve.groups->max; i++) list_free(Solve.groups->p.plist[i]);
  list_free(Solve.groups);
  free_temp_list();
}

/*** report options ***/
void print_options() {
  fprintf(output,"initial_weight=%g\n",Solve.initial_weight);
  fprintf(output,"final_weight=%g\n",Solve.final_weight);
  fprintf(output,"step_weight=%g\n",Solve.step_weight);
  fprintf(output,"iteration_ratio=%g\n",Solve.iteration_ratio);
  fprintf(output,"min_gradient=%g\n",Solve.min_gradient);
  fprintf(output,"tolerance=%g\n",Solve.tolerance);
  fprintf(output,"analytic=%d\n",Solve.analytic);
}

/*** read a command like argument ***/
char *arg(LEX *lex, int argc, char *argv[]) {
  int i;
  if (lex_eatif_sym(lex,"$")) {
    i=lex_eat_integer(lex);
    if (i<argc) return argv[i];
  }
  lex_warning(lex,"command line argument expected.");
  lex_eat_until(lex,"\n");
  return "";
}

/*** either get a quoted filename, or use command line arguments ***/
char *lex_eat_quote_arg(LEX *lex, int argc, char *argv[]) {
  if (lex_is_quote(lex)) return lex_eat_quote(lex);
  else return arg(lex,argc,argv);
}

/*** an int or a command line argument ***/
int lex_eat_integer_arg(LEX *lex, int argc, char *argv[]) {
  if (lex_is_integer(lex)) return lex_eat_integer(lex);
  else return atoi(arg(lex,argc,argv));
}

/*** an double or command line argument ***/
double lex_eat_real_arg(LEX *lex, int argc, char *argv[]) {
  if (lex_is_real(lex)) return lex_eat_real(lex);
  else return atof(arg(lex,argc,argv));
}

/*** help banner ***/
void help() {
  fprintf(stderr,
         "Solve implements a subminimal recalc engine, plus a constrained\n"
	 "optimization algorithm using soft constraints, simulated annealing,\n"
	 "and conjugate-gradient descent.\n"
	 "\n"
	 "General commands:\n"
	 "  help                (print this help screen)\n"
	 "  include \"file\"      (reads commands from a file)\n"
	 "  echo text           (echoes text)\n"
	 "  // comment          (a comment)\n"
         "  verbose [0|1]       (enable verbosity)\n"
         "  output \"file\"       (redirect stdout to a file)\n"
         "  stdout              (sends output back to stdout)\n"
	 "Annealing options:\n"
	 "  initial_weight x    (starting constraint weight)\n"
	 "  final_weight x      (ending constraint weight)\n"
	 "  step_weight x       (multipliciative increase in weight per step)\n"
	 "  iteration_ratio x   (minimization iterations = iteration_ratio*variables)\n"
         "  min_gradient x      (minimum |gradient|^2 to terminate)\n"
         "  tolerance x         (minimum step in any independent var to terminate)\n"
         "  analytic [0|1]      (enables analytic derivatives)\n"
	 "  options             (print current values of these options)\n"
	 "Creating/using variables:\n"
	 "  functions           (list available RPN operators)\n"
	 "  var x               (create an independent variable)\n"
	 "  var y = rpn         (create a dependent variable)\n"
         "  deriv x y           (create derivative of x with respect to y)\n"
	 "  energy e = rpn      (create a dependent variable, accumulate on ENERGY)\n"
	 "  constraint c = rpn  (create a <=0 CONSTRAINT)\n"
         "  equality c = rpn    (create a == CONSTRAINT)\n"
	 "  set x value         (assign an independent variable)\n"
	 "  get y               (evaluate a variable)\n"
	 "  print x             (print variable expression and value)\n"
	 "  debug               (print all variables)\n"
         "  write_all           (print value of all variables)\n"
	 "  write_results       (print independent variables)\n"
	 "Grouping:\n"
	 "  group x, y, ...     (creates a group of independent variables)\n"
	 "  groups              (prints out all groups)\n"
         "  write_groups        (print group variables)\n"
	 "  clear_groups        (deletes all groups)\n"
	 "Optimization:\n"
	 "  solve               (solves over all independent variables)\n"
	 "  solve_grouped       (solves with relaxation over specified groups)\n"
	 "  solve_parallel N id (solves in parallel with N total cpu's)\n");
}

/*** main parsing loop ***/
void parse(FILE *fin, int argc, char *argv[]) {
  char *filename,name[STRMAX];
  LEX *lex;
  VAR *pv;
  LIST *group;
  int i,j;

  /*** parse command line ***/
  lex=lex_file(fin);
  while (1) {
    /*** handle included files ***/
    if (lex_is_eof(lex)) {
      lex_free(lex);
      if (fin!=stdin) fclose(fin);
      break;
    }
    else if (lex_eatif_keyword(lex,"include"))   {
      char *pc;
      FILE *fnew;
      filename=lex_eat_quote_arg(lex,argc,argv);
      pc=find_filename(filename,"",".");
      if (pc!=NULL) fnew=fopen(pc,"rt");
      else fnew=NULL;
      if (fnew==NULL) {
        ERROR=1;
        fprintf(stderr,"ERROR: can't include %s.\n",filename);
      }
      else parse(fnew,argc,argv);
      leak_free(pc);
    }

    /*** set parameters ***/
    else if (lex_eatif_keyword(lex,"analytic"))
      Solve.analytic=lex_eat_integer_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"initial_weight"))
      Solve.initial_weight=lex_eat_real_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"final_weight"))
      Solve.final_weight=lex_eat_real_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"step_weight"))
      Solve.step_weight=lex_eat_real_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"iteration_ratio"))
      Solve.iteration_ratio=lex_eat_real_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"min_gradient"))
      Solve.min_gradient=lex_eat_real_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"tolerance"))
      Solve.tolerance=lex_eat_real_arg(lex,argc,argv);
    else if (lex_eatif_keyword(lex,"options"))
      print_options();
    else if (lex_eatif_keyword(lex,"verbose")) {
      i = lex_eat_integer_arg(lex,argc,argv);
      if (i!=0) minout = stdout;
      else minout = NULL;
    }
    else if (lex_eatif_keyword(lex,"output")) {
      if (output!=stdout) fclose(output);
      filename = lex_eat_quote_arg(lex,argc,argv);
      output = fopen(filename,"wt");
      if (output==NULL) output = stdout;
    }
    else if (lex_eatif_keyword(lex,"stdout")) {
      if (output!=stdout) fclose(output);
      output = stdout;
    }

    /*** create/debug variables ***/
    else if (lex_eatif_keyword(lex,"var")) {
      pv = parse_rpn(Solve.variables,Solve.functions,lex_eat_until(lex,"\n"));
    }
    else if (lex_eatif_keyword(lex,"deriv")) {
      VAR *top,*bot;
      top = find_var(Solve.variables,lex_eat_id(lex));
      bot = find_var(Solve.variables,lex_eat_id(lex));
      if ((top!=NULL) && (bot!=NULL))
        pv = derivative_var(Solve.variables,Solve.derivatives,top,bot);
      else {
        ERROR=1;
        fprintf(stderr,"WARNING: undefined variable\n");
      }
    }
    else if (lex_is_keyword(lex,"constraint") || lex_is_keyword(lex,"equality")) {
      if      (lex_eatif_keyword(lex,"constraint")) i=1;
      else if (lex_eatif_keyword(lex,"equality")) i=0;
      else i=0;
      pv = parse_rpn(Solve.variables,Solve.functions,lex_eat_until(lex,"\n"));
      if (pv!=NULL) {
        // create variable to hold constraint to energy conversion expression
        VAR *pv2;
        safe_sprintf(name,"CE_%s",pv->name);
        pv2 = new_var(name,0);
        fmla_var(pv2,pv);
        if (i) {
          fmla_constant(pv2,0);
          fmla_func(pv2,func_max);
        }
        fmla_func(pv2,func_sqr);
        list_insert_element_lazy_sort(Solve.variables,&pv2,&var_cmp);

        // accumulate to constraint variable
        fmla_var(Solve.constraint,pv2);
        fmla_func(Solve.constraint,func_add);
      }
    }
    else if (lex_eatif_keyword(lex,"energy")) {
      pv = parse_rpn(Solve.variables,Solve.functions,lex_eat_until(lex,"\n"));
      if (pv!=NULL) {
        fmla_var(Solve.energy,pv);
        fmla_func(Solve.energy,func_add);
      }
    }
    else if (lex_eatif_keyword(lex,"set")) {
      pv = find_var(Solve.variables,lex_eat_id(lex));
      if (pv!=NULL) set(pv,lex_eat_real_arg(lex,argc,argv));
    }
    else if (lex_eatif_keyword(lex,"get")) {
      pv = find_var(Solve.variables,lex_eat_id(lex));
      if (pv!=NULL) fprintf(output,"%g\n",get(pv));
    }

    /*** debugging/output ***/
    else if (lex_eatif_keyword(lex,"print")) {
      pv = find_var(Solve.variables,lex_eat_id(lex));
      if (pv!=NULL) print_var(output,pv);
    }
    else if (lex_eatif_keyword(lex,"debug")) {
      list_finish_lazy_sort(Solve.variables,&var_cmp);
      print_variables(output,Solve.variables);
    }
    else if (lex_eatif_keyword(lex,"write_all")) {
      list_finish_lazy_sort(Solve.variables,&var_cmp);
      for (i=0; i<Solve.variables->max; i++) {
        pv = Solve.variables->p.pvar[i];
        fprintf(output,"set %s %g\n",pv->name,get(pv));
      }
    }
    else if (lex_eatif_keyword(lex,"write_results")) {
      list_finish_lazy_sort(Solve.variables,&var_cmp);
      print_independent_variables(output,Solve.variables);
    }
    else if (lex_eatif_keyword(lex,"write_groups")) {
      write_groups(output);
    }
    else if (lex_eatif_keyword(lex,"groups")) {
      for (i=0; i<Solve.groups->max; i++) {
        fprintf(output,"group ");
        group=Solve.groups->p.plist[i];
        for (j=0; j<group->max; j++) {
          fprintf(output,"%s",group->p.pvar[j]->name);
          if (j+1<group->max) fprintf(output,", ");
        }
        fprintf(output,"\n");
      }
    }
    else if (lex_eatif_keyword(lex,"functions")) {
      for (i=0; i<Solve.functions->max; i++) {
        fprintf(output,"%s\n",Solve.functions->p.pfunc[i]->name);
      }
    }
 
    /*** solve constrained optimization problem ***/
    else if (lex_eatif_keyword(lex,"clear_groups")) {
      for (i=0; i<Solve.groups->max; i++) {
        group=Solve.groups->p.plist[i];
        list_free(group);
      }
      list_realloc(Solve.groups,0);
    }
    else if (lex_eatif_keyword(lex,"group")) {
      group=list_create(sizeof(VAR *));
      do {
        char *pc;
        pc = lex_eat_id(lex);
        pv = find_var(Solve.variables,pc);
        if ((pv!=NULL)&&(pv->formula->max==0))
          {
          if (find_element_lazy_sort(group,&pv,&pvar_cmp)<0)
            list_insert_element_lazy_sort(group,&pv,&pvar_cmp);
          }
        else {
          ERROR=1;
          fprintf(stderr,"WARNING: independent variable %s doesn't exist\n",pc);
        }
      } while (lex_eatif_sym(lex,","));
      list_append_element(Solve.groups,&group);
    }
    else if (lex_eatif_keyword(lex,"solve")) solve();
    else if (lex_eatif_keyword(lex,"solve_grouped")) solve_grouped();
    else if (lex_eatif_keyword(lex,"solve_parallel")) {
      i = lex_eat_integer_arg(lex,argc,argv);
      j = lex_eat_integer_arg(lex,argc,argv);
      solve_parallel(i,j,argc,argv);
    }

    /*** comments, echos ***/
    else if (lex_is_sym(lex,"//"))
      while (lex_eat_char(lex)!='\n');
    else if (lex_eatif_keyword(lex,"echo")) {
      lex_push_position(lex);
      while (lex_eat_char(lex)!='\n');
      fprintf(output,"%s",lex_get_token(lex));
      lex_pop_position(lex);
    }

    /*** help ***/
    else if (lex_eatif_keyword(lex,"help")) {
      help();
    }

    /*** syntax error ***/
    else {
      lex_push_position(lex);
      while (lex_eat_char(lex)!='\n');
      ERROR=1;
      fprintf(stderr,"SYNTAX ERROR: %s",lex_get_token(lex));
      lex_pop_position(lex);
    }
  }
}

#ifdef JNI
/** Java native interface to source given file **/
JNIEXPORT jint JNICALL Java_com_avlsi_tools_jauto_CGSolverInterface_runSolve
  (JNIEnv *env, jclass clazz, jstring directory, jstring filename) {
  jboolean isCopy;
  char *str_filename, *str_directory;
  FILE *input;
  str_filename = (char *) (*env)->GetStringUTFChars(env,filename,&isCopy);
  str_directory = (char *) (*env)->GetStringUTFChars(env,directory,&isCopy);
  
  /*** setup ***/
  setlinebuf(stdout);
  setlinebuf(stderr);
  output = stdout;
  init_solve();

  /*** open specified input file ***/
  if (chdir(str_directory)) return 1;
  input = fopen(str_filename,"rt");
  if (input == NULL) return 1;
  
  /*** parse input file ***/
  parse(input,0,NULL);
  fflush(stdout);
  fflush(stderr);
  if (output!=stdout) fclose(output);
  free_solve();
  return ERROR;
}
#endif
