#include "auto.h"

DNF parse_dnf_expression(LEX *lex, int precedence);

/*** free dnf memory ***/
void free_dnf(DNF dnf)
  {
  int i;
  for (i=0; i<dnf.conjuncts->max; i++)
    list_free(dnf.conjuncts->p.plist[i]);
  list_free(dnf.conjuncts);
  }

/*** negate a dnf (NOTE: assumes inverse monotonicity!) ***/
DNF negate_dnf(DNF dnf1)
  {
  DNF dnf;
  int i,j,n=1,d=0,a,b;
  LIST *conjunct,*pcon;

  /*** count depth and number of conjunctions ***/
  dnf.conjuncts=list_create(sizeof(LIST *));
  d=dnf1.conjuncts->max;
  for (i=0; i<d; i++) n*=dnf1.conjuncts->p.plist[i]->max;

  /*** generate negated dnf ***/
  for (i=0; i<n; i++)
    {
    conjunct=list_create(sizeof(char *));
    a=i;
    for (j=0; j<d; j++)
      {
      pcon=dnf1.conjuncts->p.plist[j];
      b=a/pcon->max;
      a=a%pcon->max;
      list_append_element(conjunct,&pcon->p.pc[a]);
      a=b;
      }
    list_append_element(dnf.conjuncts,&conjunct);
    }

  free_dnf(dnf1);
  return dnf;
  }

/*** and two dnfs ***/
DNF and_dnf(DNF dnf1, DNF dnf2)
  {
  int i,j;
  DNF dnf;
  LIST *conjunct;

  dnf.conjuncts=list_create(sizeof(LIST *));
  for (i=0; i<dnf1.conjuncts->max; i++)
    for (j=0; j<dnf2.conjuncts->max; j++)
      {
      conjunct=list_dup(dnf1.conjuncts->p.plist[i]);
      list_append_list(conjunct,dnf2.conjuncts->p.plist[j]);
      list_append_element(dnf.conjuncts,&conjunct);
      }

  free_dnf(dnf1);
  free_dnf(dnf2);
  return dnf;
  }

/*** or two dnfs ***/
DNF or_dnf(DNF dnf1, DNF dnf2)
  {
  int i;
  DNF dnf;
  LIST *conjunct;
  dnf.conjuncts=list_create(sizeof(LIST *));
  for (i=0; i<dnf1.conjuncts->max; i++)
    {
    conjunct=list_dup(dnf1.conjuncts->p.plist[i]);
    list_append_element(dnf.conjuncts,&conjunct);
    }
  for (i=0; i<dnf2.conjuncts->max; i++)
    {
    conjunct=list_dup(dnf2.conjuncts->p.plist[i]);
    list_append_element(dnf.conjuncts,&conjunct);
    }

  free_dnf(dnf1);
  free_dnf(dnf2);
  return dnf;
  }

/*** parse a single item of an expression ***/
DNF parse_item(LEX *lex)
  {
  char *str;
  DNF dnf;
  LIST *conjunct;

  if (lex_eatif_sym(lex,"("))
    {
    dnf=parse_dnf_expression(lex,0);
    lex_eat_sym(lex,")");
    }
  else if (lex_eatif_sym(lex,"~"))
    {
    dnf=parse_item(lex);
    dnf=negate_dnf(dnf);
    }
  else
    {
    str=get_name(parse_nodename(lex));
    dnf.conjuncts=list_create(sizeof(LIST *)); 
    conjunct=list_create(sizeof(char *));
    list_append_element(conjunct,&str);
    list_append_element(dnf.conjuncts,&conjunct);
    }
  return dnf;
  }

/*** parse an infix expression into DNF ***/
DNF parse_dnf_expression(LEX *lex, int precedence)
  {
  DNF dnf1,dnf2;
  dnf1=parse_item(lex);
  while (!lex_is_eof(lex))
    {
    if ((precedence<2)&&lex_eatif_sym(lex,"&"))
      {
      dnf2=parse_dnf_expression(lex,2);
      dnf1=and_dnf(dnf1,dnf2);
      }
    else if ((precedence<1)&&lex_eatif_sym(lex,"|"))
      {
      dnf2=parse_dnf_expression(lex,1);
      dnf1=or_dnf(dnf1,dnf2);
      }
    else break;
    }
  return dnf1;
  }

/*** print a DNF expression ***/
void print_dnf_expression(DNF dnf)
  {
  int i,j;
  LIST *con;
  char *pc;
  for (i=0; i<dnf.conjuncts->max; i++)
    {
    con=dnf.conjuncts->p.plist[i];
    if (i>0) printf("| ");
    for (j=0; j<con->max; j++)
      {
      pc=con->p.pc[j];
      if (j>0) printf("& ");
      printf("%s ",pc);
      }
    }
  printf("\n");
  }

/*** parse a rule ***/
void parse_rule(LEX *lex, LIST *rules, LIST *never, LIST *feet, int library)
  {
  RULE rule,*pr=NULL;
  DNF dnf;
  int i,j;
  char str[STRMAX];
  double width;

  /*** parse width information ***/
  width=30;
  rule.delaybias=1;
  rule.setdelay=0;
  rule.extrabias=1;
  rule.num=rules->max;
  rule.width_var=NULL;
  rule.min_pnt=rule.max_pnt=NULL;
  rule.unshared=rule.strict=rule.reverse=rule.endpath=rule.beginpath=rule.continuepath=0;
  if (lex_eatif_sym(lex,"<"))
    {
    if (library) rule.width_var=leak_strdup(lex_eat_id(lex));
    else if (lex_eatif_keyword(lex,"inf")||lex_eatif_keyword(lex,"nan"));
    else width=lex_eat_real(lex);
    if (width<4) width=4;
    if (width>Size.max_width) width=Size.max_width;
    if (lex_eatif_sym(lex,",")) rule.extrabias=lex_eat_real(lex);
    lex_eat_sym(lex,">");
    }

  /*** parse expression, target, dir ***/
  dnf=parse_dnf_expression(lex,0);
  lex_eat_sym(lex,"->");
  rule.target=get_name(parse_nodename(lex));
  if (strcmp(rule.target,"ERROR")==0) rule.target=NULL;
  if (lex_eatif_sym(lex,"+")) rule.dir=1;
  else if (lex_eatif_sym(lex,"-")) rule.dir=0;
  else lex_error(lex,"+ OR -");

  /*** append a rule for each conjunct of expression ***/
  for (i=0; i<dnf.conjuncts->max; i++)
    {
    /*** add guard ***/
    rule.guard=dnf.conjuncts->p.plist[i];

    /*** search for an exisiting rule with same target and gates in series ***/
    for (j=0; j<rules->max; j++)
      {
      pr=&rules->p.rule[j];
      if ((pr->target==rule.target)&&(pr->dir==rule.dir)&&(pr->guard->max==rule.guard->max)) break;
      }

#if 1
    if (j<rules->max) /*** reuse variables of existing independent rule ***/
      {
      rule.independent=0;
      rule.lnw=pr->lnw;
      rule.width=pr->width;
      rule.delay=pr->delay;
      }
    else /*** create recalc VAR's for width and delay ***/
#endif
      {
      rule.independent=1;
      safe_sprintf(str,"LNW(%s%s)",rule.target,rule.dir?"+":"-");
      rule.lnw=new_var(str,log(width));
      safe_sprintf(str,"W(%s%s)",rule.target,rule.dir?"+":"-");
      rule.width=new_var(str,0);
      fmla_var(rule.width,rule.lnw);
      fmla_func(rule.width,func_exp); /* monotonic transformation of independent variables */
      safe_sprintf(str,"D(%s%s)",rule.target,rule.dir?"+":"-");
      rule.delay=new_var(str,0);
      }

    /*** add guard, add to rules list ***/
    if (rule.target!=NULL) list_append_element(rules,&rule);
    else                   list_append_element(never,&rule);
    if (rule.guard->max==1) /*** keep track of feet ***/
      {
      rule.guard=list_dup(rule.guard);
      list_append_element(feet,&rule);
      }
    }
  list_free(dnf.conjuncts);
  }

/*** HACK: connect _SReset and _PReset for sake of detecting combinational operators ***/
int is_reset(char *name)
  {
  return (strcmp(name,"_SReset")==0) || (strcmp(name,"_PReset")==0);
  }

/*** test if a bunch of RULE *'s make a combinational operator ***/
int is_combinational(LIST *uprules, LIST *dnrules)
  {
  LIST *guard;
  DNF up,dn,notup;
  int i,j,ret;
  char *res="_Reset";

  /*** early out ***/
  if ((uprules->max==0)&&(dnrules->max==0)) return 1;
  if ((uprules->max==0)||(dnrules->max==0)) return 0;

  /*** rebuild sorted DNF's for up and dn expressions ***/
  up.conjuncts=list_create(sizeof(LIST *));
  dn.conjuncts=list_create(sizeof(LIST *));
  for (i=0; i<uprules->max; i++)
    {
    guard=list_dup(uprules->p.prule[i]->guard);
    for (j=0; j<guard->max; j++) if (is_reset(guard->p.pc[j])) guard->p.pc[j]=res;
    list_sort(guard,&str_cmp);
    list_append_element(up.conjuncts,&guard);
    }
  for (i=0; i<dnrules->max; i++)
    {
    guard=list_dup(dnrules->p.prule[i]->guard);
    for (j=0; j<guard->max; j++) if (is_reset(guard->p.pc[j])) guard->p.pc[j]=res;
    list_sort(guard,&str_cmp);
    list_append_element(dn.conjuncts,&guard);
    }

  /*** evaluate ~up, resort conjuncts ***/
  notup=negate_dnf(up);
  for (i=0; i<notup.conjuncts->max; i++) list_sort(notup.conjuncts->p.plist[i],&str_cmp);

  /*** compare dn to ~up ***/
  for (i=0; i<dn.conjuncts->max; i++)
    {
    guard=dn.conjuncts->p.plist[i];
    for (j=0; j<notup.conjuncts->max; j++)
      {
      if (list_compare(guard,notup.conjuncts->p.plist[j],&str_cmp)==0)
	{
	list_remove_element(dn.conjuncts,i);
	list_remove_element(notup.conjuncts,j);
	i--;
	break;
	}
      }
    }

  /*** free and return ***/
  ret=((dn.conjuncts->max==0)&&(notup.conjuncts->max==0));
  free_dnf(dn);
  free_dnf(notup);
  return ret;
  }

/*** print a rule ***/
void print_rule(FILE *fout, RULE rule)
  {
  int i;
  double w=0,b;
  if (rule.width!=NULL) w=get(rule.width);
  b=rule.extrabias;
  if ((w!=0)||(b!=1))
    {
    fprintf(fout,"<%0.0f",w);
    if (b!=1) fprintf(fout,",%0.6g",b);
    fprintf(fout,"> ");
    }
  for (i=0; i<rule.guard->max; i++)
    {
    if (i>0) fprintf(fout,"& ");
    fprintf(fout,"%s%s ",rule.dir?"~":"",rule.guard->p.pc[i]);
    }
  fprintf(fout,"-> %s%s\n",(rule.target!=NULL?rule.target:"ERROR"),rule.dir?"+":"-");
  }

/*** parse an exclhi, excllo property ***/
PROPERTY parse_property(LEX *lex)
  {
  PROPERTY prop;
  char *name;
  prop.items=list_create(sizeof(char *));
  if      (lex_eatif_keyword(lex,"exclhi")) prop.sense=1;
  else if (lex_eatif_keyword(lex,"excllo")) prop.sense=0;
  else lex_error(lex,"exclhi OR excllo");
  lex_eat_sym(lex,"(");
  while (!lex_eatif_sym(lex,")"))
    {
    name=get_name(parse_nodename(lex));
    list_append_element(prop.items,&name);
    lex_eatif_sym(lex,",");
    }
  return prop;
  }

/*** parse fullauto layout port directives ***/
void parse_directives(LEX *lex, int type, LIST *directives)
  {
  DIRECTIVE dir;
  lex_eat_sym(lex,"(");
  while (!lex_eatif_sym(lex,")"))
    {
    dir.name=get_name(parse_nodename(lex));
    dir.pos=-1; /* undefined by default */
    dir.type=type;
    if (lex_eatif_sym(lex,":")) dir.pos=lex_eat_integer(lex);
    list_append_element(directives,&dir);
    lex_eatif_sym(lex,",");
    }
  }

/*** print a directive ***/
void print_directive(FILE *fout, DIRECTIVE *pdir)
  {
  char *type;
  if      (pdir->type==0) type="strut";
  else if (pdir->type==1) type="top";
  else if (pdir->type==2) type="bottom";
  else if (pdir->type==3) type="vertical";
  else if (pdir->type==4) type="left";
  else if (pdir->type==5) type="right";
  else if (pdir->type==6) type="pin";
  else if (pdir->type==7) type="pinfixme";
  else if (pdir->type==8) type="inplace";
  else assert(0);
  if (pdir->pos>=0) fprintf(fout,"@%s(%s:%d)\n",type,pdir->name,pdir->pos);
  else              fprintf(fout,"@%s(%s)\n",type,pdir->name);
  }

/*** compare two load types ***/
int load_cmp(void *p1, void *p2)
  {
  return strcmp(((LOAD *)p1)->name,((LOAD *)p2)->name);
  }

/*** compare two load types ***/
int load_str_cmp(void *p1, void *p2)
  {
  return strcmp(((LOAD *)p1)->name,(char *)p2);
  }

/*** lex2dp: find width_var and return pointer to width ***/
double *lex2dp(LEX *lex, void *data)
  {
  char *name;
  double *pv;
  LIST *rules=(LIST *)data;
  int i;
  name=lex_eat_id(lex);
  pv=get_parm(name); if (pv!=NULL) return pv; /* user defined Parms */
  for (i=0; i<rules->max; i++)
    if (rules->p.rule[i].width_var!=NULL)
      if (strcmp(rules->p.rule[i].width_var,name)==0)
        return &rules->p.rule[i].width->val; /* should really get() every time */
  return NULL;
  }

/*** set a rule's unshared/strict/endpath/delaybias attribute ***/
void set_rule_attributes(LEX *lex, LIST *rules, int attribute, double value)
  {
  char *target,str[STRMAX];
  int i,dir=0,found;
  RULE *pr;
  lex_eat_sym(lex,"(");
  do
    {
    found=0;
    target=get_name(parse_nodename(lex));
    if      (lex_eatif_sym(lex,"+")) dir=1;
    else if (lex_eatif_sym(lex,"-")) dir=0;
    else lex_error(lex,"+ OR -");
    for (i=0; i<rules->max; i++)
      {
      pr=&rules->p.rule[i];
      if ((pr->target==target)&&(pr->dir==dir))
	{
	found++;
        if      (attribute==0) pr->unshared=1;
        else if (attribute==1) pr->strict=1;
	else if (attribute==2) pr->endpath=value;
	else if (attribute==3) pr->delaybias=value;
	else if (attribute==4) pr->setdelay=value;
	}
      }
    if (found==0)
      {
      safe_sprintf(str,"rule for %s%s",target,dir?"+":"-");
      lex_error(lex,str);
      }
    } while(lex_eatif_sym(lex,","));
  lex_eat_sym(lex,")");
  }

/*** parse a circuit or library circuit ***/
void parse_circuit(CIRCUIT *pcircuit, LEX *lex, int library)
  {
  RULE *pr;
  PROPERTY prop;
  LOAD load,*pl;
  int i,j;
  LIST *expression;
  STRETCH stretch;
  USE use;
  LIST *uprules,*dnrules;
  double x;
  STATICIZER staticizer;

  /*** parse standard PRS/exclhi/excllo contents ***/
  while(!lex_is_eof(lex))
    {
    if (lex_is_sym(lex,"@")) break;
    else if (lex_is_keyword(lex,"exclhi")||lex_is_keyword(lex,"excllo"))
      {
      prop=parse_property(lex);
      list_append_element(pcircuit->properties,&prop);
      }
    else if (lex_eatif_keyword(lex,"connect")) lex_eat_until(lex,"\n");
    else if (lex_eatif_keyword(lex,"export"))  lex_eat_until(lex,"\n");
    else parse_rule(lex,pcircuit->rules,pcircuit->never,pcircuit->feet,library);
    }

  /*** parse extra @ directives ***/
  while(!lex_is_eof(lex))
    {
    lex_eat_sym(lex,"@");
    if (lex_eatif_keyword(lex,"staticize"))
      {
      lex_eat_sym(lex,"(");
      do
	{
	staticizer.name=get_name(parse_nodename(lex));
        staticizer.rulenum=10000;
	list_insert_element_lazy_sort(pcircuit->staticizers,&staticizer,&staticizer_cmp);
	list_insert_element_lazy_sort(pcircuit->explicit_staticizers,&staticizer,&staticizer_cmp);
        } while(lex_eatif_sym(lex,","));
      lex_eat_sym(lex,")");
      }
    else if (library)
      {
      if (lex_eatif_keyword(lex,"require"))
	{
	lex_eat_sym(lex,"(");
	expression=parse_expression(lex,lex2dp,(void *)pcircuit->rules);
	list_append_element(pcircuit->requirements,&expression);
	lex_eat_sym(lex,")");
	}
      else if (lex_eatif_keyword(lex,"stretch"))
	{
	lex_eat_sym(lex,"(");
	stretch.label=leak_strdup(lex_eat_id(lex));
	lex_eat_sym(lex,",");
	stretch.amount=0;
	stretch.expression=parse_expression(lex,lex2dp,(void *)pcircuit->rules);
	list_append_element(pcircuit->stretches,&stretch);
	lex_eat_sym(lex,")");
	}
      else lex_error(lex,"staticize, require, stretch");
      }
    else
      {
      if (lex_eatif_keyword(lex,"unshared"))
	set_rule_attributes(lex,pcircuit->rules,0,0);
      else if (lex_eatif_keyword(lex,"strict"))
	set_rule_attributes(lex,pcircuit->rules,1,0);
      else if (lex_eatif_keyword(lex,"endpath"))
	set_rule_attributes(lex,pcircuit->rules,2,1);
      else if (lex_eatif_keyword(lex,"catpath"))
	set_rule_attributes(lex,pcircuit->rules,2,2);
      else if (lex_eatif_keyword(lex,"ruledelaybias"))
	{
        lex_eat_sym(lex,"(");
	x=lex_eat_real(lex);
	lex_eat_sym(lex,")");
	set_rule_attributes(lex,pcircuit->rules,3,x);
	}
      else if (lex_eatif_keyword(lex,"setdelay"))
	{
        lex_eat_sym(lex,"(");
	x=lex_eat_real(lex);
	lex_eat_sym(lex,")");
	set_rule_attributes(lex,pcircuit->rules,4,x);
	}
      else if (lex_eatif_keyword(lex,"magic"))
	{
        lex_eat_keyword(lex,"use");
	use=parse_use(lex,1);
        list_append_element(pcircuit->subcells,&use);
	}
      else if (lex_eatif_keyword(lex,"xpitch"))
	{
	lex_eat_sym(lex,"(");
	pcircuit->xpitch=lex_eat_real(lex);
	lex_eat_sym(lex,")");
	}
      else if (lex_eatif_keyword(lex,"ypitch"))
	{
	lex_eat_sym(lex,"(");
	pcircuit->ypitch=lex_eat_real(lex);
	lex_eat_sym(lex,")");
	}
      else if (lex_eatif_keyword(lex,"delaybias"))
	{
	lex_eat_sym(lex,"(");
	pcircuit->delaybias=lex_eat_real(lex);
	lex_eat_sym(lex,")");
	}
      else if (lex_eatif_keyword(lex,"lessfolding"))
	{
	pcircuit->lessfolding=1;
	}
      else if (lex_eatif_keyword(lex,"nostaticizer"))
	{
        lex_eat_sym(lex,"(");
        do
	  {
	  staticizer.name=get_name(parse_nodename(lex));
          staticizer.rulenum=10000;
          list_insert_element_lazy_sort(pcircuit->nostaticizers,&staticizer,&staticizer_cmp);
	  } while(lex_eatif_sym(lex,","));
	lex_eat_sym(lex,")");
	}
      else if (lex_eatif_keyword(lex,"strut"))    parse_directives(lex,0,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"top"))      parse_directives(lex,1,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"bottom"))   parse_directives(lex,2,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"vertical")) parse_directives(lex,3,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"left"))     parse_directives(lex,4,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"right"))    parse_directives(lex,5,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"pin"))      parse_directives(lex,6,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"pinfixme"))  parse_directives(lex,7,pcircuit->directives);
      else if (lex_eatif_keyword(lex,"inplace"))  parse_directives(lex,8,pcircuit->directives);
      else /*** default: set a parameter ***/
	{
	PARM parm;
	parm.name=leak_strdup(lex_eat_id(lex));
	lex_eat_sym(lex,"(");
	parm.value=lex_eat_real(lex);
	lex_eat_sym(lex,")");
	set_parm(parm.name,parm.value);
	list_append_element(pcircuit->parms,&parm);
	}
      }
    }

  /*** multiply rule->delaybias by circuit->delaybias ***/
  for (i=0; i<pcircuit->rules->max; i++)
    {
    pr=&pcircuit->rules->p.rule[i];
    pr->delaybias*=pcircuit->delaybias;
    pr->setdelay*=pcircuit->delaybias;
    }

  /*** allocate LOAD structures for all internal nodes used in rules ***/
  for (i=0; i<pcircuit->rules->max; i++)
    {
    pr=&pcircuit->rules->p.rule[i];
    load.name=pr->target;
    if (find_element_lazy_sort(pcircuit->loads,&load,&load_cmp)<0)
      list_insert_element_lazy_sort(pcircuit->loads,&load,&load_cmp);
    for (j=0; j<pr->guard->max; j++)
      {
      load.name=pr->guard->p.pc[j];
      if (find_element_lazy_sort(pcircuit->loads,&load,&load_cmp)<0)
        list_insert_element_lazy_sort(pcircuit->loads,&load,&load_cmp);
      }
    }
  list_finish_lazy_sort(pcircuit->loads,&load_cmp);

  /*** add in staticizers (except library) ***/
  if (!library) for (i=0; i<pcircuit->loads->max; i++)
    {
    pl=&pcircuit->loads->p.load[i];

    /*** find all rules which target this load ***/
    staticizer.name=pl->name;
    staticizer.rulenum=10000;
    uprules=list_create(sizeof(RULE *));
    dnrules=list_create(sizeof(RULE *));
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (pr->target==pl->name)
	{
	if (pr->dir) list_append_element(uprules,&pr);
	else         list_append_element(dnrules,&pr);
	staticizer.rulenum=min(staticizer.rulenum,pr->num);
        }
      }

    /*** add a staticizer if rules is not a combinational operator ***/
    if (!is_combinational(uprules,dnrules)&&
	(find_element_lazy_sort(pcircuit->nostaticizers,&staticizer,&staticizer_cmp)<0)&&
	(find_element_lazy_sort(pcircuit->staticizers,&staticizer,&staticizer_cmp)<0))
	list_insert_element_lazy_sort(pcircuit->staticizers,&staticizer,&staticizer_cmp);
    list_free(uprules);
    list_free(dnrules);
    }

  /*** sort pin directives ***/
  list_sort(pcircuit->directives,&directive_cmp);
  }

/*** initialize fields of a circuit ***/
CIRCUIT create_circuit()
  {
  CIRCUIT circuit;
  circuit.name=NULL;
  circuit.filename=NULL;
  circuit.instances=0;
  circuit.fixed=0;
  circuit.lessfolding=0;
  circuit.xpitch=circuit.ypitch=0;
  circuit.delaybias=1;
  circuit.delay=0;
  circuit.rules=list_create(sizeof(RULE));
  circuit.never=list_create(sizeof(RULE));
  circuit.staticizers=list_create(sizeof(STATICIZER));
  circuit.explicit_staticizers=list_create(sizeof(STATICIZER));
  circuit.nostaticizers=list_create(sizeof(STATICIZER));
  circuit.properties=list_create(sizeof(PROPERTY));
  circuit.loads=list_create(sizeof(LOAD));
  circuit.requirements=list_create(sizeof(LIST *));
  circuit.stretches=list_create(sizeof(STRETCH));
  circuit.subcells=list_create(sizeof(USE));
  circuit.paths=NULL;
  circuit.catpaths=NULL;
  circuit.metric=NULL;
  circuit.directives=list_create(sizeof(DIRECTIVE));
  circuit.feet=list_create(sizeof(RULE));
  circuit.parms=list_create(sizeof(PARM));
  return circuit;
  }

/*** read circuit or library circuit from filename.prs ***/
CIRCUIT read_circuit(char *name, char *filename, int library)
  {
  CIRCUIT circuit;
  FILE *fin,*ftest;
  LEX *lex;

  /*** create an empty circuit ****/
  circuit=create_circuit();
  circuit.name=leak_strdup(name);
  circuit.filename=find_filename(filename,"",SEARCH_PATH);
  circuit.fixed=library;

  /*** find the file ***/
  if (circuit.filename!=NULL) fin=fopen(circuit.filename,"rt");
  else fin=NULL;
  if (fin==NULL)
    {
    fprintf(stderr,"WARNING: circuit %s assumed to be empty.\n",circuit.name);
    circuit.fixed=1;
    return circuit;
    }
  if (!library)
    {
    ftest=fopen(circuit.filename,"at");
    if (ftest==NULL)
      {
      fprintf(stderr,"WARNING: circuit %s assumed to have fixed sizes.\n",circuit.name);
      circuit.fixed=1;
      }
    else fclose(ftest);
    }
  lex=lex_file_with_name(fin,circuit.filename);
  parse_circuit(&circuit,lex,library);
  lex_free(lex);
  fclose(fin);
  if (!library)
    {
    xpitch=circuit.xpitch;
    ypitch=circuit.ypitch;
    }
  return circuit;
  }

/*** free circuit ***/
void free_circuit(CIRCUIT *circuit)
  {
  int i;
    RULE *pr;
  for (i=0; i<circuit->requirements->max; i++)
    list_free(circuit->requirements->p.plist[i]);
  leak_free(circuit->name);
  leak_free(circuit->filename);
  for (i=0; i<circuit->rules->max; i++)
    {
    pr=&circuit->rules->p.rule[i];
    list_free(pr->guard);
    if (pr->independent)
      {
      free_var(pr->lnw);
      free_var(pr->width);
      free_var(pr->delay);
      }
    }
  list_free(circuit->rules);
  for (i=0; i<circuit->feet->max; i++)
    {
    pr=&circuit->feet->p.rule[i];
    list_free(pr->guard);
    }
  list_free(circuit->feet);
  for (i=0; i<circuit->never->max; i++)
    list_free(circuit->never->p.rule[i].guard);
  for (i=0; i<circuit->parms->max; i++)
    leak_free(circuit->parms->p.parm[i].name);
  list_free(circuit->parms);
  list_free(circuit->never);
  list_free(circuit->explicit_staticizers);
  list_free(circuit->staticizers);
  for (i=0; i<circuit->properties->max; i++)
    list_free(circuit->properties->p.prop[i].items);
  list_free(circuit->properties);
  list_free(circuit->loads);
  for (i=0; i<circuit->requirements->max; i++)
    list_free(circuit->requirements->p.plist[i]);
  list_free(circuit->requirements);
  for (i=0; i<circuit->stretches->max; i++)
    {
    leak_free(circuit->stretches->p.stretch[i].label);
    list_free(circuit->stretches->p.stretch[i].expression);
    }
  list_free(circuit->stretches);
  for (i=0; i<circuit->subcells->max; i++)
    {
    leak_free(circuit->subcells->p.use[i].name);
    leak_free(circuit->subcells->p.use[i].type);
    }
  list_free(circuit->subcells);
  list_free(circuit->nostaticizers);
  if (circuit->paths!=NULL) free_paths(circuit);
  if (circuit->metric!=NULL) free_var(circuit->metric);
  list_free(circuit->directives);
  leak_free(circuit);
  }

/*** compare rules by target,dir ***/
int rule_td_cmp(void *p1, void *p2)
  {
  int c;
  RULE *pr1=(RULE *)p1,*pr2=(RULE *)p2;
  c=pr1->target-pr2->target;
  if (c!=0) return c;
  c=pr1->dir-pr2->dir;
  return c;
  }

/*** print a property ***/
void print_property(FILE *fout, PROPERTY *pprop)
  {
  int j;
  if (pprop->sense) fprintf(fout,"exclhi(");
  else              fprintf(fout,"excllo(");
  for (j=0; j<pprop->items->max; j++)
    {
    if (j>0) fprintf(fout,",");
    fprintf(fout,"%s",pprop->items->p.pc[j]);
    }
  fprintf(fout,")\n");
  }

/*** print a single path ***/
void print_path(FILE *fout, PATH *pp)
  {
  int j;
  RULE *pr;
  for (j=pp->rules->max-1; j>=0; j--)
    {
    pr=pp->rules->p.prule[j];
    fprintf(fout,"%s%s ",pr->target,pr->dir?"+":"-");
    }
  }

/*** print paths ***/
void print_paths(FILE *fout, LIST *paths)
  {
  int i;
  PATH *pp;
  if (paths==NULL) return;
  fprintf(fout,"/*** PATHS:\n");
  for (i=0; i<paths->max; i++)
    {
    pp=&paths->p.path[i];
    print_path(fout,pp);
    fprintf(fout,"\n");
    }
  fprintf(fout,"***/\n");
  }

/*** print a single catpath ***/
void print_catpath(FILE *fout, CATPATH *pcp)
  {
  int j;
  for (j=pcp->paths->max-1; j>=0; j--)
    {
    print_path(fout,pcp->paths->p.ppath[j]);
    if (j>0) fprintf(fout,":: ");
    }
  }

/*** print catpaths ***/
void print_catpaths(FILE *fout, LIST *catpaths)
  {
  int i;
  CATPATH *pcp;
  if (catpaths==NULL) return;
  fprintf(fout,"/*** CATPATHS:\n");
  for (i=0; i<catpaths->max; i++)
    {
    pcp=&catpaths->p.catpath[i];
    print_catpath(fout,pcp);
    fprintf(fout,"\n");
    }
  fprintf(fout,"***/\n");
  }

/*** print out rule attributes ***/
void print_attributes(FILE *fout, char *type, LIST *rules)
  {
  int i;
  RULE *pr;
  if (rules->max>0)
    {
    fprintf(fout,"@%s(",type);
    for (i=0; i<rules->max; i++)
      {
      pr=&rules->p.rule[i];
      if (i>0) fprintf(fout,",");
      fprintf(fout,"%s%s",pr->target,pr->dir?"+":"-");
      }
    fprintf(fout,")\n");
    }
  }

/*** print a circuit ***/
void print_circuit(FILE *fout, CIRCUIT *circuit,
		   int do_rules, int do_staticizers,
                   int do_properties, int do_attributes, int do_magic, int do_path)
  {
  RULE *pr;
  LIST *unshared,*strict,*endpath,*catpath,*ruledelaybias,*setdelay;
  int i;
  fprintf(fout,"/* NAME: %s */\n/* FILE: %s */\n/* INSTANCES: %d */\n/* DELAY: %g*%g*/\n/* METRIC: %g */\n",
	  circuit->name,circuit->filename,circuit->instances,
	  circuit->delaybias,circuit->delay,circuit->metric!=NULL ? get(circuit->metric) : 0);
  if (do_rules) for (i=0; i<circuit->rules->max; i++)
    print_rule(fout,circuit->rules->p.rule[i]);
  if (do_rules) for (i=0; i<circuit->never->max; i++)
    print_rule(fout,circuit->never->p.rule[i]);
  if (do_properties) for (i=0; i<circuit->properties->max; i++)
    print_property(fout,&circuit->properties->p.prop[i]);
  if (do_magic) for (i=0; i<circuit->subcells->max; i++)
    print_use(fout,circuit->subcells->p.use[i],1);
  if (do_staticizers) for (i=0; i<circuit->staticizers->max; i++)
    fprintf(fout,"@staticize(%s)\n",circuit->staticizers->p.staticizer[i].name);
  if (do_rules&&!do_staticizers) for (i=0; i<circuit->explicit_staticizers->max; i++)
    fprintf(fout,"@staticize(%s)\n",circuit->explicit_staticizers->p.staticizer[i].name);
  if (circuit->xpitch!=0) fprintf(fout,"@xpitch(%g)\n",circuit->xpitch);
  if (circuit->ypitch!=0) fprintf(fout,"@ypitch(%g)\n",circuit->ypitch);
  if (circuit->delaybias!=1) fprintf(fout,"@delaybias(%g)\n",circuit->delaybias);
  if (circuit->lessfolding!=0) fprintf(fout,"@lessfolding\n");

  /*** attributes ***/
  if (do_attributes)
    {
    for (i=0; i<circuit->parms->max; i++)
      fprintf(fout,"@%s(%g)\n",circuit->parms->p.parm[i].name,circuit->parms->p.parm[i].value);
    for (i=0; i<circuit->directives->max; i++)
      print_directive(fout,&circuit->directives->p.directive[i]);
    for (i=0; i<circuit->nostaticizers->max; i++)
      fprintf(fout,"@nostaticizer(%s)\n",circuit->nostaticizers->p.staticizer[i].name);

    /*** rule related attributes ***/
    unshared=list_create(sizeof(RULE));
    strict=list_create(sizeof(RULE));
    endpath=list_create(sizeof(RULE));
    catpath=list_create(sizeof(RULE));
    ruledelaybias=list_create(sizeof(RULE));
    setdelay=list_create(sizeof(RULE));
    for (i=0; i<circuit->rules->max; i++)
      {
      pr=&circuit->rules->p.rule[i];
      if ((pr->unshared)&&(find_element_sorted(unshared,pr,&rule_td_cmp)<0))
        list_insert_element_sorted(unshared,pr,&rule_td_cmp);
      if ((pr->strict)&&(find_element_sorted(strict,pr,&rule_td_cmp)<0))
        list_insert_element_sorted(strict,pr,&rule_td_cmp);
      if ((pr->endpath==1)&&(find_element_sorted(endpath,pr,&rule_td_cmp)<0))
        list_insert_element_sorted(endpath,pr,&rule_td_cmp);
      if ((pr->endpath==2)&&(find_element_sorted(catpath,pr,&rule_td_cmp)<0))
        list_insert_element_sorted(catpath,pr,&rule_td_cmp);
      if ((pr->delaybias!=circuit->delaybias)&&(find_element_sorted(ruledelaybias,pr,&rule_td_cmp)<0))
	list_insert_element_sorted(ruledelaybias,pr,&rule_td_cmp);
      if ((pr->setdelay!=0)&&(find_element_sorted(setdelay,pr,&rule_td_cmp)<0))
	list_insert_element_sorted(setdelay,pr,&rule_td_cmp);
      }
    print_attributes(fout,"unshared",unshared);
    print_attributes(fout,"strict",strict);
    print_attributes(fout,"endpath",endpath);
    print_attributes(fout,"catpath",catpath);
    for (i=0; i<ruledelaybias->max; i++)
      {
      pr=&ruledelaybias->p.rule[i];
      fprintf(fout,"@ruledelaybias(%g)(%s%s)\n",
	      pr->delaybias/circuit->delaybias,pr->target,pr->dir?"+":"-");
      }
    for (i=0; i<setdelay->max; i++)
      {
      pr=&setdelay->p.rule[i];
      fprintf(fout,"@setdelay(%g)(%s%s)\n",
	      pr->setdelay/circuit->delaybias,pr->target,pr->dir?"+":"-");
      }      
    list_free(unshared);
    list_free(strict);
    list_free(endpath);
    list_free(catpath);
    list_free(ruledelaybias);
    list_free(setdelay);
    }

  /*** path comments ***/
  if (do_path)
    {
    print_paths(fout,circuit->paths);
    print_catpaths(fout,circuit->catpaths);
    }
  }
