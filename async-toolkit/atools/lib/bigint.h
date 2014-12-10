/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** structures ***/
typedef struct BIGINT
  {
  int value;
  struct BIGINT *next;
  } BIGINT;

typedef struct BIOP
  {
  int type;
  BIGINT *value;
  } BIOP;

/*** op codes ***/

#define BIOP_VAR    0
#define BIOP_VARSTR 1
#define BIOP_CONST  2
#define BIOP_STR    3
#define BIOP_USRSTR 4

#define BIOP_E   10
#define BIOP_NE  11
#define BIOP_GT  12
#define BIOP_GTE 13
#define BIOP_LT  14
#define BIOP_LTE 15

#define BIOP_BOOL_AND 20
#define BIOP_BOOL_OR  21
#define BIOP_BOOL_NOT 22

#define BIOP_BIT_AND  30
#define BIOP_BIT_OR   31
#define BIOP_BIT_NOT  32
#define BIOP_BIT_XOR  33
#define BIOP_SHL      34
#define BIOP_SHR      35
#define BIOP_BIT_INS  36
#define BIOP_BIT_EXT  37

#define BIOP_ADD 40
#define BIOP_SUB 41
#define BIOP_NEG 42
#define BIOP_MUL 43
#define BIOP_POW 44
#define BIOP_DIV 45
#define BIOP_MOD 46
#define BIOP_POSMOD 47

#define BIOP_ABS 50
#define BIOP_MAX 51
#define BIOP_MIN 52
#define BIOP_IF  53
#define BIOP_RAND 54

#define BIOP_STRCAT 61
#define BIOP_HEXSTR 62
#define BIOP_DECSTR 63

#define BIOP_USR 100

/*** function prototypes ***/

int int_from_bigint(BIGINT *b);
BIGINT *bigint_from_int(int value);
BIGINT *bigint_from_longlong(long long value);
void free_bigint(BIGINT *b);
void free_bigint_memory();
int total_bigint_memory();

void assign_bigint(BIGINT *b, BIGINT *new);
void move_bigint(BIGINT *b, BIGINT *new);
BIGINT *copy_bigint(BIGINT *b1);

BIGINT *parse_bigint_dec(char *str);
BIGINT *parse_bigint_str(char *str);
BIGINT *parse_bigint_hex(char *str);
BIGINT *parse_bigint(char *str);
void print_bigint_dec(FILE *fout, BIGINT *b);
void print_bigint_str(FILE *fout, BIGINT *b);
char *sprint_bigint_str(char *str, BIGINT *b);
void print_bigint_hex(FILE *fout, BIGINT *b);

struct _list *parse_bigint_expression(LEX *lex, char *parse_var(LEX *));
void free_bigint_expression(struct _list *expression,
                            int freeVAR, int freeVARSTR,
                            int freeUSR, int freeUSRSTR,
                            int freeCONST, int freeSTR);
void print_bigint_expression(FILE *fout, struct _list *expression);
BIGINT *evaluate_bigint_expression(struct _list *expression, void *data,
                                   void eval_user(int type, int *tos,
                                                  BIGINT **stack, void *data));
int bigint_expression_is_string(struct _list *expression);

void append_bigint_operator(struct _list *expression, int type);
void append_bigint_constant(struct _list *expression, char *str);
void append_bigint_string(struct _list *expression, char *str);
void append_bigint_variable(struct _list *expression, char *str);

int compare_bigint(BIGINT *b1, BIGINT *b2);
int bigint_is_zero(BIGINT *b);
int bigint_is_nonzero(BIGINT *b);
int bigint_is_one(BIGINT *b);
int bigint_is_two(BIGINT *b);
int bigint_is_negative(BIGINT *b);

BIGINT *eq_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *neq_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *gt_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *lt_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *gte_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *lte_bigint(BIGINT *b1, BIGINT *b2);

BIGINT *bool_bigint(BIGINT *b);
BIGINT *bool_or_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *bool_and_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *bool_not_bigint(BIGINT *b);

BIGINT *bit_or_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *bit_and_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *bit_xor_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *bit_not_bigint(BIGINT *b1);
BIGINT *shl_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *shr_bigint(BIGINT *b1, BIGINT *b2);

BIGINT *add_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *sub_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *neg_bigint(BIGINT *b1);
BIGINT *mul_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *pow_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *div_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *mod_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *posmod_bigint(BIGINT *b1, BIGINT *b2);
void divmod_bigint(BIGINT *n, BIGINT *d, BIGINT **pq, BIGINT **pr);

BIGINT *abs_bigint(BIGINT *b1);
BIGINT *min_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *max_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *rand_bigint(BIGINT *b1);

BIGINT *strcat_bigint(BIGINT *b1, BIGINT *b2);
BIGINT *decstr_bigint(BIGINT *b1);
BIGINT *hexstr_bigint(BIGINT *b1);
