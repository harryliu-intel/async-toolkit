/* *****************************************************************************
// [read_analog.cpp]
//
//  Copyright 2006-2009 SPRINGSOFT. All Rights Reserved.
//
// Except as specified in the license terms of SPRINGSOFT, this material may not be copied, modified,
// re-published, uploaded, executed, or distributed in any way, in any medium,
// in whole or in part, without prior written permission from SPRINGSOFT.
// ****************************************************************************/
//
// Program Name	: read_analog.cpp
//
// Purpose	: Demonstrate how to call fsdb reader APIs to access 
//		  the value changes of analog type fsdb.
//


//
// NOVAS_FSDB is internally used in NOVAS
//
#ifdef NOVAS_FSDB
#undef NOVAS_FSDB
#endif

#include "ffrAPI.h"
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <sys/wait.h>

#ifndef FALSE
#define FALSE	0
#endif

#ifndef TRUE
#define TRUE	1
#endif

#define MIN(a,b) ((a)<(b)?(a):(b))
#define MAX(a,b) ((a)>(b)?(a):(b))

static unsigned loid = (unsigned)-1;
static unsigned hiid = 0;

//
// The tree callback function, it's used to traverse the design 
// hierarchies. 
//
static bool_T __MyTreeCB(fsdbTreeCBType cb_type, 
			 void *client_data, void *tree_cb_data);

static char *verbosestr=(getenv("DEBUGnanosimrd"));
static int verbose=(verbosestr ? atoi(verbosestr) : 0);

#define CMDBUFSIZ 2048

static pid_t       mypid     = getpid();
static const char *myname    = "nanosimrd";

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//
// dump scope definition
//
static void 
__DumpScope(fsdbTreeCBDataScope *scope);


//
// dump var definition 
// 
static void 
__DumpVar(fsdbTreeCBDataVar *var);

static void 
BuildVar(fsdbTreeCBDataVar *var);


void
myfree(void *p)
{
  if(verbose>=100)fprintf(stderr, "%s %d : free(0x%x)\n", myname, mypid, p);
  free(p);
}

void
free_argv(char **argv)
{
  int j = 0;
  while(argv[j])
    myfree(argv[j++]);

  myfree(argv);
}

//////////////////////////////////////////////////////////////////////
//
// integer dequeue
//

typedef struct int_dq {
  int val;
  struct int_dq *prev, *next;
} int_dq;

int_dq *
new_int_dq(void)
{
  // allocate and set up sentinel
  int_dq *res = (int_dq *)malloc(sizeof(int_dq)); 
  res->val = 0xdeadbeef;
  res->next = res;
  res->prev = res;

  return res;
}

void
int_dq_addlo(int_dq *q, int val)
{
  int_dq *newd = (int_dq *)malloc(sizeof(int_dq));

  newd->val = val;

  q->next->prev = newd;
  newd->next = q->next;

  newd->prev = q;
  q->next = newd;
}

void
int_dq_addhi(int_dq *q, int val)
{
  int_dq *newd = (int_dq *)malloc(sizeof(int_dq));

  newd->val = val;
  
  q->prev->next = newd;
  newd->prev = q->prev;

  newd->next = q;
  q->prev = newd;
}

void
int_dq_free(int_dq *q)
{
  int_dq *op, *p=q->next;

  while (p != q) {
    op = p;
    p = p->next;
    myfree(op);
  }

  q->next = q;
  q->prev = q;
}


//////////////////////////////////////////////////////////////////////

typedef struct {
  fsdbTag32 htime, ltime;
  float     sigval;
} extended_t;

//////////////////////////////////////////////////////////////////////
//
// extended dequeue
//

typedef struct ext_dq {
  extended_t val;
  struct ext_dq *prev, *next;
} ext_dq;

ext_dq *
new_ext_dq(void)
{
  // allocate and set up sentinel
  ext_dq *res = (ext_dq *)malloc(sizeof(ext_dq)); 
  res->val.htime =  0xdeadbeef;
  res->val.ltime =  0xbeefdead;
  
  res->next = res;
  res->prev = res;

  return res;
}

void
ext_dq_addlo(ext_dq *q, const extended_t *valp)
{
  ext_dq *newd = (ext_dq *)malloc(sizeof(ext_dq));

  newd->val = *valp;

  q->next->prev = newd;
  newd->next = q->next;

  newd->prev = q;
  q->next = newd;
}

void
ext_dq_addhi(ext_dq *q, const extended_t *valp)
{
  ext_dq *newd = (ext_dq *)malloc(sizeof(ext_dq));

  newd->val = *valp;
  
  q->prev->next = newd;
  newd->prev = q->prev;

  newd->next = q;
  q->prev = newd;
}

void
ext_dq_free(ext_dq *q)
{
  ext_dq *op, *p=q->next;

  while (p != q) {
    op = p;
    p = p->next;
    myfree(op);
  }

  q->next = q;
  q->prev = q;
}

//////////////////////////////////////////////////////////////////////

const int debug = 2 * !!getenv("DEBUGnanosimrd");

ffrObject      *fsdb_obj;
fsdbXTagType    xtag_type; 

int
setup(char *fn)
{
    // 
    // check the file to see if it's a fsdb file or not.
    //
    if (FALSE == ffrObject::ffrIsFSDB(fn)) {
        fprintf(stderr, "%s %d : %s is not an fsdb file.\n", myname, mypid, fn);
	return FSDB_RC_FAILURE;
    }

    ffrFSDBInfo fsdb_info;

    ffrObject::ffrGetFSDBInfo(fn, fsdb_info);

    if (FSDB_FT_NANOSIM != fsdb_info.file_type) {
  	fprintf(stderr, "%s %d : file type is not nanosim but %d.\n",
                myname, mypid, fsdb_info.file_type);
	return FSDB_RC_FAILURE;
    }

    //
    // Open the fsdb file.
    //
    // From fsdb v2.0(Debussy 5.0), there are two APIs to open a 
    // fsdb file: ffrOpen() and ffrOpen2(). Both APIs take three 
    // parameters, the first one is the fsdb file name, the second 
    // one is a tree callback function written by application, the 
    // last one is the client data that application would like 
    // fsdb reader to pass it back in tree callback function.
    //
    // Open a fsdb file with ffrOpen(), the tree callback function
    // will be activated many times during open session; open a fsdb
    // file with ffrOpen2(), the tree callback function will not be
    // activated during open session, applicaiton has to call an API
    // called "ffrReadScopeVarTree()" to activate the tree callback
    // function. 
    // 
    // In tree callback function, application can tell what the
    // callback data is, based on the callback type. For example, if 
    // the callback type is scope(FFR_TREE_CBT_SCOPE), then 
    // applicaiton knows that it has to perform (fsdbTreeCBDataScope*) 
    // type case on the callback data so that it can read the scope 
    // defition.
    //
    fsdb_obj =  ffrObject::ffrOpen3(fn);
    if (NULL == fsdb_obj) {
        fprintf(stderr, "%s %d ffrObject::ffrOpen() failed.\n", myname, mypid);
	exit(FSDB_RC_OBJECT_CREATION_FAILED);
    }
    fsdb_obj->ffrSetTreeCBFunc(__MyTreeCB, NULL);

    if (FSDB_FT_NANOSIM != fsdb_obj->ffrGetFileType()) {
	fprintf(stderr, 
		"%s %d : %s is not nanosim type fsdb, just return.\n",
                myname, mypid, fn);
	fsdb_obj->ffrClose();
	return FSDB_RC_SUCCESS;
    }

    xtag_type = fsdb_obj->ffrGetXTagType();


    //
    // Activate the tree callback funciton, read the design 
    // hierarchies. Application has to perform proper type case 
    // on tree callback data based on the callback type, then uses 
    // the type case structure view to access the wanted data.
    //
    fsdb_obj->ffrReadScopeVarTree();
}


int
get_max_idcode(void)
{
  return fsdb_obj->ffrGetMaxVarIdcode();
}

str_T
get_scaleunit(void)
{
  return fsdb_obj->ffrGetScaleUnit();
}

//////////////////////////////////////////////////////////////////////

static int_dq *active=new_int_dq();

int
open_signal_range(void)
{
  if (verbose) fprintf(stderr, "%s %d : open_signal_range()\n", myname, mypid);

  for(int_dq *p=active->next; p != active; p = p->next) {
    if (verbose>=10) fprintf(stderr, "%s %d : signal %u\n", myname, mypid, p->val);
    fsdb_obj->ffrAddToSignalList(p->val);
  }

}

int
load_signals(void)
{
  fprintf(stderr, "%s %d : loading signals\n", myname, mypid);
  fsdb_obj->ffrLoadSignals();

  fprintf(stderr, "%s %d : signals loaded\n", myname, mypid);
}

#define TRAVERSE_TIME     1
#define TRAVERSE_SIGNAL   2
#define TRAVERSE_BINARY   4
#define TRAVERSE_EXTENDED 8
#define TRAVERSE_FILTER  16

static void 
PrintTimeValChng(ffrVCTrvsHdl   vc_trvs_hdl, 
                 void          *time,
                 byte_T        *vc_ptr,
                 int            bytesPerBit,
                 unsigned       mode,
                 char          *buff,
                 unsigned       tidx,
                 unsigned       lim,
                 extended_t    *xval)
{ 
    FILE           *stream=stdout;

    // print time

    if (mode & TRAVERSE_BINARY) {

      if (tidx >= lim) {
        fprintf(stderr, "%s %d : error: time array overrun @ %s\n", myname, mypid, tidx);
        exit(-1);
      }
      
      switch (bytesPerBit) {
      case FSDB_BYTES_PER_BIT_4B:
        ((float *)buff)[tidx] = *(float*)vc_ptr;	
        break;
        
      case FSDB_BYTES_PER_BIT_8B:
        ((float *)buff)[tidx] = *(double*)vc_ptr;	
        
      default:
        ((float *)buff)[tidx] = 0.0;
        break;
      }
    } else if (mode & TRAVERSE_EXTENDED) {
      xval->htime = ((fsdbTag64*)time)->H;
      xval->ltime = ((fsdbTag64*)time)->L;

      switch (bytesPerBit) {
      case FSDB_BYTES_PER_BIT_4B:
        xval->sigval = *(float*)vc_ptr;	
        break;
          
      case FSDB_BYTES_PER_BIT_8B:
        xval->sigval = *(double*)vc_ptr;
        break;
          
      default:
        fprintf(stream, "ERROR DIGITAL\n");
        exit(-1);
        break;
      }
        
    } else {
      
      if (mode & TRAVERSE_TIME) 
        fprintf(stream, "%u %u",
                ((fsdbTag64*)time)->H,
                ((fsdbTag64*)time)->L);
      
      // print value
      if (mode & TRAVERSE_SIGNAL)
        switch (bytesPerBit) {
        case FSDB_BYTES_PER_BIT_4B:
          fprintf(stream, " %15e", *(float*)vc_ptr);	
          break;
          
        case FSDB_BYTES_PER_BIT_8B:
          fprintf(stream, " %15e", *(double*)vc_ptr);	
          
        default:
          fprintf(stream, " DIGITAL");
          break;
        }
      
      if (mode) fprintf(stream, "\n");
    }
}

#define TIME_MEMORIZE 1
#define TIME_CHECK    2

//////////////////////////////////////////////////////////////////////

typedef struct {
  unsigned   len;
  unsigned   p;
  fsdbTag64 *times;
} time_memory_t;

static time_memory_t *the_timemem=NULL;

static time_memory_t *
timemem_new(void)
{
  time_memory_t *mem;
  
  mem        = (time_memory_t *)malloc(sizeof(time_memory_t));
  mem->len   = 1;
  mem->p     = 0;
  mem->times = (fsdbTag64 *)malloc(sizeof(fsdbTag64) * 1);
  return mem;
}

static void
timemem_free(time_memory_t *mem)
{
  myfree(mem->times);
  myfree(mem);
}

static void
timemem_addhi(time_memory_t *mem, fsdbTag64 *val)
{
  if(mem->p == mem->len) {
    // we are at the end of the allocated space,
    // grow the space by double, copy over and free the old
    
    unsigned   newlen   = mem->len * 2;
    fsdbTag64 *newtimes = (fsdbTag64 *)malloc(sizeof(fsdbTag64) * newlen);

    for(int i=0; i < mem->len; ++i) // copy over
      newtimes[i] = mem->times[i];

    myfree(mem->times); // free the old data
    mem->times = newtimes;
    mem->len   = newlen;
  }

  // remember the new value
  mem->times[mem->p] = *val;

  (mem->p)++;
}

static unsigned
timemem_equal(const time_memory_t     *mem,
              const unsigned           idx,
              const fsdbTag64         *val)
{
  if(idx >= mem->p)
    return 0;
  
  return (mem->times[idx].H == val->H && mem->times[idx].L == val->L);
}

//////////////////////////////////////////////////////////////////////

void
fput32(unsigned u, FILE *stream)
{
  fputc((u           ) & 0xff, stream);
  fputc((u      >>  8) & 0xff, stream);
  fputc((u      >> 16) & 0xff, stream);
  fputc((u      >> 24) & 0xff, stream);
}

void
do_timecheck(unsigned            *timecheck_ok,
             const time_memory_t *mem,
             unsigned             idx,
             fsdbTag64           *time)
{
  unsigned this_ok = timemem_equal(mem, idx, time);

  if (*timecheck_ok && !this_ok)
    fprintf(stderr, "%s %d : time check failed idx=%u time=%u %u\n",
            myname, mypid, idx, time->H, time->L);

  *timecheck_ok &= this_ok;
}

//////////////////////////////////////////////////////////////////////

FILE *filterstr;

int pipefds1[2] = { -1, -1 };               // pipefds2[2];

int filterfd = fileno(stdout); // by default filter output goes to stdout

pid_t childpid;

//
// do we filter or not?
char  *filterpath    = NULL;
char **filterargv;

void
start_filter(void)
{
  if (verbose)
    fprintf(stderr, "%s %d : starting filter\n", myname, mypid);
  if (pipe(pipefds1) == -1) {
    perror("pipe");
    exit(1);
  }
  
  if((childpid = fork()) == -1) {
    perror("fork");
    exit(1);
  }
  
  if(childpid == 0) {
    close(pipefds1[1]);   // close write end
    dup2(pipefds1[0], 0); // make stdin the read end
    
    //        close(pipefds2[0]);   // close read end
    //        dup2(pipefds2[1], 1); // make stdout the write end
    
    // try to run the filter
    if ((execvp(filterpath, filterargv)) == -1) {
      // exec failed
      perror("execvp");
      fprintf(stderr, "%s %d : filterpath \"%s\"\n", myname, mypid, filterpath);
      _exit(1); // child exit, use _exit...
    }
    assert(0); // cant get here
  } else {
    // parent needs to write to pipe
    filterfd = pipefds1[1];
    close(pipefds1[0]);
  }
  filterstr = fdopen(filterfd, "a");
  // open the filter file descriptor in stdio
}

void
stop_filter(void)
{
  if (verbose)
    fprintf(stderr, "%s %d : stopping filter\n", myname, mypid);
  close(pipefds1[1]);
  pipefds1[1] = -1;
  wait(NULL);
}

int
filter_running(void)
{
  return (pipefds1[1] != -1);
}

int
traverse_one_signal(int        idcode,
                    unsigned   mode,
                    unsigned   time_mode)
{
  byte_T *vc_ptr;
  fsdbTag64 *time = (fsdbTag64 *)calloc(8, sizeof(byte_T));
  char *buff = NULL;
  extended_t x;
  ext_dq *extdata = NULL;
  unsigned nx=0;

  if(verbose)fprintf(stderr, "%s %d : traverse_one_signal(%d,0x%x,0x%x)\n", myname, mypid, idcode, mode, time_mode);

  if (mode & TRAVERSE_EXTENDED)
    extdata = new_ext_dq();

  assert (sizeof(float) == 4);

  if (mode & TRAVERSE_BINARY) {
    if (!the_timemem) the_timemem = timemem_new();
    
    buff = (char *)malloc(the_timemem->p * sizeof(float));
  }
  
  if (time_mode & TIME_MEMORIZE) {
    if (the_timemem)
      myfree(the_timemem);

    the_timemem = timemem_new();
  }
  

  ffrVCTrvsHdl vc_trvs_hdl =
    fsdb_obj->ffrCreateVCTraverseHandle(idcode);

  int bytesPerBit = vc_trvs_hdl->ffrGetBytesPerBit();

  if (NULL == vc_trvs_hdl) {
    fprintf(stderr, "%s %d : Failed to create a traverse handle for var (%u)\n", myname, mypid, 
            idcode);
    exit(FSDB_RC_OBJECT_CREATION_FAILED);
  }
  
  if (FSDB_RC_SUCCESS != 
      fsdb_obj->ffrGetMinFsdbTag64((fsdbTag64*)time)) {
    fprintf(stderr, "%s %d : should not happen.\n", myname, mypid);
    exit(FSDB_RC_FAILURE);
  }
  
  if(verbose)
    fprintf(stderr, "%s %d : trvs hdl(%u): minimum time is (%u %u).\n", myname, mypid, 
            idcode,
            ((fsdbTag64*)time)->H, 
            ((fsdbTag64*)time)->L);
  
  
  //
  // Jump to the specific time specified by the parameter of 
  // ffrGotoXTag(). The specified time may have or have not 
  // value change; if it has value change, then the return time 
  // is exactly the same as the specified time; if it has not 
  // value change, then the return time will be aligned forward
  // (toward smaller time direction). 
  //
  // There is an exception for the jump alignment: If the 
  // specified time is smaller than the minimum time where has 
  // value changes, then the return time will be aligned to the 
  // minimum time.
  //

  if (FSDB_RC_SUCCESS != vc_trvs_hdl->ffrGotoXTag((void*)time)) {
    fprintf(stderr, "%s %d : (%u) jump to tag H=%u L=%u failed : should not happen.\n", myname, mypid, idcode, time->H, time->L);
    exit(FSDB_RC_FAILURE);
  }	
  
  //
  // Get the value change. 
  //
  if (time_mode & TIME_MEMORIZE) 
    timemem_addhi(the_timemem, time);

  unsigned timecheck_ok = 1;

  if ((time_mode & TIME_CHECK) && !the_timemem) {
    the_timemem = timemem_new();
  }
    
  if (time_mode & TIME_CHECK)
    do_timecheck(&timecheck_ok, the_timemem, 0, time);
  
  if (FSDB_RC_SUCCESS == vc_trvs_hdl->ffrGetVC(&vc_ptr)) {
    PrintTimeValChng(vc_trvs_hdl,
                     time,
                     vc_ptr,
                     bytesPerBit,
                     mode,
                     buff,
                     0,
                     0,
                     &x);
    if (mode & TRAVERSE_EXTENDED) {
      ext_dq_addhi(extdata, &x);
      ++nx;
    }
  }

  //
  // Value change traverse handle keeps an internal index
  // which points to the current time and value change; each
  // traverse API may move that internal index backward or
  // forward.
  // 
  // ffrGotoNextVC() moves the internal index backward so
  // that it points to the next value change and the time
  // where the next value change happened.
  //  
  for ( int i = 1; FSDB_RC_SUCCESS == vc_trvs_hdl->ffrGotoNextVC(); ++i) {
    vc_trvs_hdl->ffrGetXTag(time);
    vc_trvs_hdl->ffrGetVC(&vc_ptr);
    
    if (time_mode & TIME_MEMORIZE)
      timemem_addhi(the_timemem, time);
    
    unsigned timecheck_ok = 1;
    
    if (time_mode & TIME_CHECK)
      timecheck_ok &= timemem_equal(the_timemem, i, time);
    
    PrintTimeValChng(vc_trvs_hdl,
                     time,
                     vc_ptr,
                     bytesPerBit,
                     mode,
                     buff,
                     i,
                     the_timemem ? the_timemem->p : 0,
                     &x);

    if (mode & TRAVERSE_EXTENDED) {
      ext_dq_addhi(extdata, &x);
      ++nx;
    }
  }

  vc_trvs_hdl->ffrFree();
  myfree(time);


  if (mode & TRAVERSE_BINARY) {
    if (timecheck_ok) {
      unsigned n = the_timemem->p;
      
      if(verbose)
      fprintf(stderr, "%s %d : binary traversal tag N nodeid %u count %u\n", myname, mypid,
              idcode, n);
      
      fprintf(stdout, "OK\n"); // we have to tell the driver data is coming
      fprintf(stdout, "N");

      fput32(idcode, stdout);
      fput32(n, stdout);
      
      fflush(stdout);

      if(verbose)
        fprintf(stderr, "%s %d : writing binary data, value[0] %f\n", myname, mypid,
                ((float *)buff)[0]);

      write(fileno(stdout), buff, n * sizeof(float)); // just dump the buffer
      
    } else {
      fprintf(stdout, "E TIMECHECK %d\n", timecheck_ok);
    }
  } else {
    if (time_mode & TIME_CHECK)
      fprintf(stdout, "TIMECHECK %d\n", timecheck_ok);
  }

  if (mode & TRAVERSE_EXTENDED) {
    ext_dq *p = extdata->next;
    unsigned *ht = (unsigned *)malloc(sizeof(unsigned) * nx);
    unsigned *lt = (unsigned *)malloc(sizeof(unsigned) * nx);
    float    *vv = (float *)   malloc(sizeof(float)    * nx);
    int i=0;

    if(verbose)
      fprintf(stderr, "%s %d : extended binary traversal tag N nodeid %u count %u\n", myname, mypid,
              idcode, nx);

    //
    // the following block of code matches
    // Fsdb.ReadInterpolatedBinaryNodeDataG()
    // 
    // THE FILTER NEEDS TO START READING HERE ==========================
    
    fprintf(filterstr, "OK\n"); // we have to tell the driver data is coming
    fprintf(filterstr, "x");    // this is the response tag

    fput32(idcode, filterstr);  // write nodeid
    fput32(nx,     filterstr);  // write # of records
   
    while (p != extdata) {

      ht[i] = p->val.htime;
      lt[i] = p->val.ltime;
      vv[i] = p->val.sigval;

      ++i;
      p = p->next;
    }

    fflush(filterstr);
    
    write(fileno(filterstr), ht, nx * sizeof(unsigned)); 
    write(fileno(filterstr), lt, nx * sizeof(unsigned)); 
    write(fileno(filterstr), vv, nx * sizeof(float)); 

    // THE FILTER NEEDS TO STOP READING HERE ==========================

    myfree(ht);
    myfree(lt);
    myfree(vv);
    
  }
  
  if (buff)    myfree(buff);
  if (extdata) ext_dq_free(extdata);
}

typedef struct namerec {
  char           *name;
  unsigned        idcode;
  unsigned        bytes_per_bit;
  unsigned        type;
  struct namerec *next;
} namerec_t;

static namerec_t *names=NULL;

const char *
decode_type(unsigned int tid)
{
  const char *type;
  
  switch (tid) {
  case FSDB_VT_NANOSIM_VOLTAGE:
    type = (str_T) "nanosim_voltage"; 
    break;
    
  case FSDB_VT_NANOSIM_INSTANTANEOUS_CURRENT:
    type = (str_T) "nanosim_instantaneous_current";
    break;
    
  case FSDB_VT_NANOSIM_AVERAGE_RMS_CURRENT:
    type = (str_T) "nanosim_average_rms_current";
    break;
    
  case FSDB_VT_NANOSIM_DI_DT:
    type = (str_T) "nanosim_di_dt";
    break;
    
  case FSDB_VT_NANOSIM_MATHEMATICS:
    type = (str_T) "nanosim_mathematics"; 
    break;
    
  case FSDB_VT_NANOSIM_POWER:
    type = (str_T) "nanosim_power";
    break;
    
  default:
    type = (str_T) "nanosim_others";
    break;
  }
  
  return type;
}

void
traverse_names(unsigned lo, unsigned hi, unsigned aliases)
// print a single name for each id if aliases = 0
// print all aliases for each id if aliases = 1
{
  namerec_t *p = names;

  // for some reason we can get duplicates
  unsigned *got=(unsigned *)malloc(sizeof(unsigned) * (hi + 1));
  
  int i=0;

  memset(got, 0, sizeof(unsigned) * (hi + 1));
  
  fprintf(stderr, "%s %d : traverse_names %u %u\n", myname, mypid, lo, hi);
  
  while(p) {
    if (p->idcode >= lo && p->idcode <= hi && !got[p->idcode]) {
      got[p->idcode] = !aliases;
      fprintf(stdout,
              "%u %s %s\n",
              p->idcode,
              p->name,
              decode_type(p->type));
    }
    ++i;
              
    p = p->next;
  }

  fprintf(stderr, "%s %d : traverse_names : %d records\n", myname, mypid, i);
  myfree(got);

}

typedef struct cons_t {
  const char    *car;
  struct cons_t *cdr;
} cons_t;

struct cons_t *
cons(const char *a, cons_t *tail)
{
  struct cons_t *ret = (cons_t *)malloc(sizeof(cons_t));
  ret->car = a;
  ret->cdr = tail;
  
  return ret;
}

struct cons_t *
reverse(const cons_t *old)
// careful about memory leaks!
{
  cons_t *res = NULL;
  const cons_t *p = old;

  while (p) {
    res = cons(p->car, res);
    p   = p->cdr;
  }

  return res;
}

static char  *scopesep    = strdup(".");
static int    scopeseplen = 1;
static int    stripX      = 0;

int 
main(int argc, char *argv[])
{
  
    if (2 != argc) {
	fprintf(stderr, "usage: nanosimrd verilog_type_fsdb\n");
	return FSDB_RC_FAILURE;
    }

    char buff[CMDBUFSIZ];
    
    char *tok;
    
    while(fgets(buff, CMDBUFSIZ, stdin)) {
      if(verbose)fprintf(stderr, "%s %d : got line \"%s\"\n", myname, mypid, buff);

      tok = strtok(buff, " ");
      
      switch (buff[0]) {
        
      case '\0':
      case '#': // skip --- comment
        break;

      case 'B': // parse the file
        (void)setup(argv[1]);
        fprintf(stdout, "BR\n");
        break;

      case 'S': // query range of IDCODES
        fprintf(stdout,
                "SR %d %d %s\n",
                FSDB_MIN_VAR_IDCODE,
                get_max_idcode(),
                get_scaleunit());
        break;
 
      case 'r': // interesting signal
        {
          unsigned code=atoi(strtok(NULL, " "));
          int_dq_addhi(active, code);
          fprintf(stdout, "rR\n");
        }
        break;

      case 'F': // stream filter
        {
          char *str = strtok(NULL, " \n");
          cons_t *filterargs = NULL;
          char *new_filterpath;
          char **new_filterargv;
          
          int nargs = 1;
          
          if(verbose)fprintf(stderr, "%s %d : got filter \"%s\"\n", myname, mypid, str);

          new_filterpath = strdup(str);

          while ((str = strtok(NULL, " \n"))) {
            char *arg = strdup(str);
            if(verbose)fprintf(stderr, "%s %d : got arg \"%s\"\n", myname, mypid, arg);
            
            filterargs = cons(arg, filterargs);
            ++nargs;
          }

          filterargs = reverse(filterargs);

          new_filterargv = (char **)malloc((nargs + 1) * sizeof(char *));

          new_filterargv[0] = new_filterpath;
          
          int i = 1;
          for (cons_t *p = filterargs; p ; ++i, p = p->cdr) {
            new_filterargv[i] = strdup(p->car);
          }
          new_filterargv[i] = NULL;

          /****************************************/

          if (filterpath != NULL) 
            free_argv(filterargv);

          filterpath=new_filterpath;
          filterargv=new_filterargv;
          
          fprintf(stdout, "FR\n");
          break;
        }
        
      case 'R': // interesting signal range
        {
          unsigned lo=atoi(strtok(NULL, " "));
          unsigned hi=atoi(strtok(NULL, " "));
          for (int i=lo; i<= hi; ++i)
            int_dq_addhi(active, i);
        }
        fprintf(stdout, "RR\n");
        break;

      case 'L': // load signals
        open_signal_range();
        fprintf(stdout, "LR\n");
        break;  

      case 'U':                 // unload signals
        fsdb_obj->ffrUnloadSignals();
        fprintf(stdout, "UR\n");
        int_dq_free(active);
        break;

      case 'T': // traverse signals
        for (int_dq *p=active->next; p != active; p = p->next) {
          traverse_one_signal(p->val,
                              TRAVERSE_TIME | TRAVERSE_SIGNAL,
                              0);
        }
        fprintf(stdout, "UT\n");
        break;

      case 't': // traverse signals (binary)
        for (int_dq *p=active->next; p != active; p = p->next) {
          traverse_one_signal(p->val,
                              TRAVERSE_BINARY | TRAVERSE_SIGNAL,
                              TIME_CHECK);
        }
        fprintf(stdout, "tR\n");
        break;

      case 'x': // traverse signals (binary extended)
        for (int_dq *p=active->next; p != active; p = p->next) {
          traverse_one_signal(p->val,
                              TRAVERSE_EXTENDED |
                              0,
                              0);
        }
        fprintf(stdout, "%cR\n", buff[0]);
        break;

      case 'y': // traverse signals (binary extended, filtered)
        start_filter();
        for (int_dq *p=active->next; p != active; p = p->next) {
          traverse_one_signal(p->val,
                              TRAVERSE_EXTENDED |
                              (buff[0] == 'y' ? TRAVERSE_FILTER : 0),
                              0);
        }
        stop_filter();
        fprintf(stdout, "%cR\n", buff[0]);
        break;

      case 'N': // get names
        {
          unsigned lo=atoi(strtok(NULL, " "));
          unsigned hi=atoi(strtok(NULL, " ")); 
          traverse_names(lo, hi, 0);
          fprintf(stdout, "NR\n");
          break;
        }

      case 'A': // get names (all aliases)
        {
          unsigned lo=atoi(strtok(NULL, " "));
          unsigned hi=atoi(strtok(NULL, " ")); 
          traverse_names(lo, hi, 1);
          fprintf(stdout, "AR\n");
          break;
        }

      case 'I': // traverse signal times for a given signal, print,  & remember
        {
          int code=atoi(strtok(NULL, " "));
          traverse_one_signal(code, TRAVERSE_TIME, TIME_MEMORIZE);
          fprintf(stdout, "IR\n");
        }
        break;

      case 'i': // traverse signal times for a given signal & remember
        {
          int code=atoi(strtok(NULL, " "));
          traverse_one_signal(code, 0, TIME_MEMORIZE);
          fprintf(stdout, "iR\n");
        }
        break;

      case 'C': // traverse signal times for a given signal & check
        {
          int code=atoi(strtok(NULL, " "));
          traverse_one_signal(code, 0, TIME_CHECK);
        }
        break;

      case 'Q': // quit
        fprintf(stdout, "QR\n");
        goto done;
        break;

      case 's': // set scope separator
        myfree(scopesep);
        scopesep    = strdup(strtok(NULL, " "));
        stripX      = atoi(strtok(NULL, " \n"));
        scopeseplen = strlen(scopesep);
        fprintf(stdout, "sR\n");
        break;

      default:
        fprintf(stderr, "???line not understood\n");
        break;
      }
      fflush(stdout);
    }

 done:
    fsdb_obj->ffrClose();
    
    return 0;
}

// scoping

typedef struct arc_t {
  char          *name;
  struct arc_t  *up;
} arc_t;

static arc_t *curscope    = NULL;
static char  *curpfx      = strdup("");
static int    curpfxlen   = 0;

static void update_pfx(void)
// call this whenever updating curscope
{
  myfree(curpfx);
  
  if (!curscope) {
    curpfx    = strdup("");
    curpfxlen = 0;
  } else {
      int len = 0;
      for (arc_t *p = curscope; p; p = p->up) 
        len = len + strlen(p->name) + scopeseplen;  // add length + period

      // terminating null
      ++len; 

      // alloc new string
      curpfx    = (char *)malloc(len);
      curpfxlen = len - 1; // string lengths do NOT contain null

      char *q = curpfx + len;

      // invariant : q is the limit of the next string to print
      
      *(--q) = '\000';

      int tlen;
      for (arc_t *p = curscope; p; p = p->up) {
        memcpy(q -= scopeseplen, scopesep, scopeseplen);
        tlen = strlen(p->name);

        memcpy(q -= tlen, p->name, tlen);
      }
      assert(q == curpfx);
    }

}

static void up_scope(void)
{
  arc_t *up = curscope->up;
  myfree(curscope->name);
  myfree(curscope);
  curscope = up;
  update_pfx();
}

static void down_scope(const fsdbTreeCBDataScope *tree_cb_data)
{
  arc_t *down = (arc_t *)malloc(sizeof(arc_t));

  if (stripX && tree_cb_data->name[0] == 'X') 
    down->name = strdup(tree_cb_data->name + 1); // strip leading X
  else
    down->name = strdup(tree_cb_data->name);
  
  down->up   = curscope;
  curscope   = down;
  update_pfx();
}

static bool_T __MyTreeCB(fsdbTreeCBType cb_type, 
			 void *client_data, void *tree_cb_data)
{
    switch (cb_type) {
    case FSDB_TREE_CBT_BEGIN_TREE:
      if (debug >= 2) fprintf(stderr, "%s %d : <BeginTree>\n", myname, mypid);
      break;

    case FSDB_TREE_CBT_SCOPE:
      if (debug >= 2) __DumpScope((fsdbTreeCBDataScope*)tree_cb_data);
      down_scope((fsdbTreeCBDataScope*)tree_cb_data);
      break;

    case FSDB_TREE_CBT_VAR:
      BuildVar((fsdbTreeCBDataVar*)tree_cb_data);
      if (debug >= 2) __DumpVar((fsdbTreeCBDataVar*)tree_cb_data);
      break;

    case FSDB_TREE_CBT_UPSCOPE:
      if (debug >= 2) fprintf(stderr, "%s %d : <Upscope>\n", myname, mypid);
      up_scope();
      break;

    case FSDB_TREE_CBT_END_TREE:
	if (debug >= 2) fprintf(stderr, "%s %d : <EndTree>\n\n", myname, mypid);
	break;

    case FSDB_TREE_CBT_FILE_TYPE:
	break;

    case FSDB_TREE_CBT_SIMULATOR_VERSION:
	break;

    case FSDB_TREE_CBT_SIMULATION_DATE:
	break;

    case FSDB_TREE_CBT_X_AXIS_SCALE:
	break;

    case FSDB_TREE_CBT_END_ALL_TREE:
	break;

    case FSDB_TREE_CBT_ARRAY_BEGIN:
        if (debug >= 2) fprintf(stderr, "%s %d : <BeginArray>\n", myname, mypid);
        break;
        
    case FSDB_TREE_CBT_ARRAY_END:
        if (debug >= 2) fprintf(stderr, "%s %d : <EndArray>\n\n", myname, mypid);
        break;

    case FSDB_TREE_CBT_RECORD_BEGIN:
        if (debug >= 2) fprintf(stderr, "%s %d : <BeginRecord>\n", myname, mypid);
        break;
        
    case FSDB_TREE_CBT_RECORD_END:
        if (debug >= 2) fprintf(stderr, "%s %d : <EndRecord>\n\n", myname, mypid);
        break;
             
    default:
	return FALSE;
    }

    return TRUE;
}

static void 
__DumpScope(fsdbTreeCBDataScope* scope)
{
    str_T type;

    switch (scope->type) {
    case FSDB_ST_VCD_MODULE:
	type = (str_T) "module"; 
	break;

    case FSDB_ST_VCD_TASK:
	type = (str_T) "task"; 
	break;

    case FSDB_ST_VCD_FUNCTION:
	type = (str_T) "function"; 
	break;

    case FSDB_ST_VCD_BEGIN:
	type = (str_T) "begin"; 
	break;

    case FSDB_ST_VCD_FORK:
	type = (str_T) "fork"; 
	break;

    default:
      type = (str_T) "other type of scope";
	break;
    }

    fprintf(stderr, "%s %d : <Scope> name:%s  type:%s\n", myname, mypid, 
	    scope->name, type);
}

static
void BuildVar(fsdbTreeCBDataVar *var)
{
    namerec_t *rec = (namerec_t *)malloc(sizeof(namerec_t));

    int varlen         = strlen(var->name);

    // build fully qualified name
    // 1. hierarchy prefix name (including trailing dot, if exists)
    // 2. variable name
    // 3. trailing null
    rec->name          = (char *)malloc(curpfxlen + varlen + 1);
    memcpy(rec->name            , curpfx   , curpfxlen);
    memcpy(rec->name + curpfxlen, var->name, varlen);
    *(rec->name + curpfxlen + varlen) = '\000';
      
    rec->idcode        = var->u.idcode;
    rec->bytes_per_bit = var->bytes_per_bit;
    rec->type          = var->type;

    rec->next          = names;
    names              = rec;

    loid = MIN(loid, var->u.idcode);
    hiid = MAX(hiid, var->u.idcode);
}

static void 
__DumpVar(fsdbTreeCBDataVar *var)
{
    str_T type;
    str_T bpb;

    switch(var->bytes_per_bit) {
    case FSDB_BYTES_PER_BIT_1B:
	bpb = (str_T) "1B";
	break;

    case FSDB_BYTES_PER_BIT_2B:
	bpb = (str_T) "2B";
	break;

    case FSDB_BYTES_PER_BIT_4B:
	bpb = (str_T) "4B";
	break;

    case FSDB_BYTES_PER_BIT_8B:
	bpb = (str_T) "8B";
	break;

    default:
	bpb = (str_T) "XB";
	break;
    }

    switch (var->type) {
    case FSDB_VT_NANOSIM_VOLTAGE:
	type = (str_T) "nanosim_voltage"; 
  	break;

    case FSDB_VT_NANOSIM_INSTANTANEOUS_CURRENT:
        type = (str_T) "nanosim_instantaneous_current";
	break;

    case FSDB_VT_NANOSIM_AVERAGE_RMS_CURRENT:
        type = (str_T) "nanosim_average_rms_current";
	break;

    case FSDB_VT_NANOSIM_DI_DT:
        type = (str_T) "nanosim_di_dt";
	break;

    case FSDB_VT_NANOSIM_MATHEMATICS:
	type = (str_T) "nanosim_mathematics"; 
	break;

    case FSDB_VT_NANOSIM_POWER:
        type = (str_T) "nanosim_power";
	break;

    default:
	type = (str_T) "nanosim_others";
	break;
    }

    fprintf(stderr,
            "%s %d : <Var>  name:%s  l:%u  r:%u  type:%s  ", myname, mypid,
            var->name, var->lbitnum, var->rbitnum, type);
    fprintf(stderr,
            "%s %d : idcode:%u  dtidcode:%u  bpb:%s\n", myname, mypid,
            var->u.idcode, var->dtidcode, bpb);
}
