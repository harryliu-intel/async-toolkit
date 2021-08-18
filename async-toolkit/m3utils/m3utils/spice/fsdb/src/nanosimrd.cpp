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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

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

static int verbose=FALSE;


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
    free(op);
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
    free(op);
  }

  q->next = q;
  q->prev = q;
}

//////////////////////////////////////////////////////////////////////

const int debug = 1;

ffrObject      *fsdb_obj;
fsdbXTagType    xtag_type; 

int
setup(char *fn)
{
    // 
    // check the file to see if it's a fsdb file or not.
    //
    if (FALSE == ffrObject::ffrIsFSDB(fn)) {
	fprintf(stderr, "%s is not an fsdb file.\n", fn);
	return FSDB_RC_FAILURE;
    }

    ffrFSDBInfo fsdb_info;

    ffrObject::ffrGetFSDBInfo(fn, fsdb_info);

    if (FSDB_FT_NANOSIM != fsdb_info.file_type) {
  	fprintf(stderr, "file type is not nanosim but %d.\n",
                fsdb_info.file_type);
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
	fprintf(stderr, "ffrObject::ffrOpen() failed.\n");
	exit(FSDB_RC_OBJECT_CREATION_FAILED);
    }
    fsdb_obj->ffrSetTreeCBFunc(__MyTreeCB, NULL);

    if (FSDB_FT_NANOSIM != fsdb_obj->ffrGetFileType()) {
	fprintf(stderr, 
		"%s is not nanosim type fsdb, just return.\n", fn);
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


#define CMDBUFSIZ 2048

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
  if (verbose) fprintf(stderr, "open_signal_range()\n");

  for(int_dq *p=active->next; p != active; p = p->next) {
    if (verbose) fprintf(stderr, "signal %u\n", p->val);
    fsdb_obj->ffrAddToSignalList(p->val);
  }

}

int
load_signals(void)
{
  fprintf(stderr, "loading signals\n");
  fsdb_obj->ffrLoadSignals();

  fprintf(stderr, "signals loaded\n");
}

#define TRAVERSE_TIME     1
#define TRAVERSE_SIGNAL   2
#define TRAVERSE_BINARY   4
#define TRAVERSE_EXTENDED 8

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
        fprintf(stderr, "Error: time array overrun @ %s\n", tidx);
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
  free(mem->times);
  free(mem);
}

static void
timemem_addhi(time_memory_t *mem, fsdbTag64 *val)
{
  if(mem->p == mem->len) {
    unsigned newlen = mem->len * 2;
    fsdbTag64 *newtimes = (fsdbTag64 *)malloc(sizeof(fsdbTag64) * newlen);

    for(int i=0; i < mem->len; ++i)
      newtimes[i] = mem->times[i];

    free(mem->times);
    mem->times = newtimes;
    mem->len   = newlen;
  }
  mem->times[mem->p] = *val;

  (mem->p)++;
}

static unsigned
timemem_compare(const time_memory_t     *mem,
                const unsigned           idx,
                const fsdbTag64         *val)
{
  if(idx >= mem->p)
    return 0;
  
  return mem->times[idx].H == val->H && mem->times[idx].L == val->L;
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
do_timecheck(unsigned *timecheck_ok,
             const time_memory_t *mem,
             unsigned idx,
             fsdbTag64 *time)
{
  unsigned this_ok = timemem_compare(mem, idx, time);

  if (*timecheck_ok && !this_ok)
    fprintf(stderr, "time check failed idx=%u time=%u %u\n",
            idx, time->H, time->L);

  *timecheck_ok &= this_ok;
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

  if (mode & TRAVERSE_EXTENDED)
    extdata = new_ext_dq();

  assert (sizeof(float) == 4);

  if (mode & TRAVERSE_BINARY) {
    if (!the_timemem) the_timemem = timemem_new();
    
    buff = (char *)malloc(the_timemem->p * sizeof(float));
  }
  
  if (time_mode & TIME_MEMORIZE) {
    if (the_timemem)
      free(the_timemem);

    the_timemem = timemem_new();
  }
  

  ffrVCTrvsHdl vc_trvs_hdl =
    fsdb_obj->ffrCreateVCTraverseHandle(idcode);

  int bytesPerBit = vc_trvs_hdl->ffrGetBytesPerBit();

  if (NULL == vc_trvs_hdl) {
    fprintf(stderr, "Failed to create a traverse handle for var (%u)\n", 
            idcode);
    exit(FSDB_RC_OBJECT_CREATION_FAILED);
  }
  
  if (FSDB_RC_SUCCESS != 
      fsdb_obj->ffrGetMinFsdbTag64((fsdbTag64*)time)) {
    fprintf(stderr, "should not happen.\n");
    exit(FSDB_RC_FAILURE);
  }
  
  if(verbose)
    fprintf(stderr, "trvs hdl(%u): minimum time is (%u %u).\n", 
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
    fprintf(stderr, "(%u) jump to tag H=%u L=%u failed : should not happen.\n", idcode, time->H, time->L);
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
      timecheck_ok &= timemem_compare(the_timemem, i, time);
    
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
  free(time);


  if (mode & TRAVERSE_BINARY) {
    if (timecheck_ok) {
      unsigned n = the_timemem->p;
      
      if(verbose)
      fprintf(stderr, "binary traversal tag N nodeid %u count %u\n",
              idcode, n);
      
      fprintf(stdout, "OK\n"); // we have to tell the driver data is coming
      fprintf(stdout, "N");

      fput32(idcode, stdout);
      fput32(n, stdout);
      
      fflush(stdout);

      if(verbose)
        fprintf(stderr, "writing binary data, value[0] %f\n",
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
      fprintf(stderr, "extended binary traversal tag N nodeid %u count %u\n",
              idcode, nx);
    
    fprintf(stdout, "OK\n"); // we have to tell the driver data is coming
    fprintf(stdout, "x");

    fput32(idcode, stdout);
    fput32(nx,     stdout);
   
    while (p != extdata) {

      ht[i] = p->val.htime;
      lt[i] = p->val.ltime;
      vv[i] = p->val.sigval;

      ++i;
      p = p->next;
    }

    fflush(stdout);
    
    write(fileno(stdout), ht, nx * sizeof(unsigned)); 
    write(fileno(stdout), lt, nx * sizeof(unsigned)); 
    write(fileno(stdout), vv, nx * sizeof(float)); 

    free(ht);
    free(lt);
    free(vv);
    
  }
  
  if (buff)    free(buff);
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
traverse_names(unsigned lo, unsigned hi)
{
  namerec_t *p = names;

  // for some reason we can get duplicates
  unsigned *got=(unsigned *)malloc(sizeof(unsigned) * (hi + 1));
  
  int i=0;

  memset(got, 0, sizeof(unsigned) * (hi + 1));
  
  fprintf(stderr, "traverse_names %u %u\n", lo, hi);
  
  while(p) {
    if (p->idcode >= lo && p->idcode <= hi && !got[p->idcode]) {
      got[p->idcode] = 1;
      fprintf(stdout,
              "%u %s %s\n",
              p->idcode,
              p->name,
              decode_type(p->type));
    }
    ++i;
              
    p = p->next;
  }

  fprintf(stderr, "traverse_names : %d records\n", i);
  free(got);

}

int 
main(int argc, char *argv[])
{
  
    if (2 != argc) {
	fprintf(stderr, "usage: nanosimrd verilog_type_fsdb\n");
	return FSDB_RC_FAILURE;
    }

    (void)setup(argv[1]);

    char buff[CMDBUFSIZ];
    char *tok;
    
    while(fgets(buff, CMDBUFSIZ, stdin)) {
      if(verbose)fprintf(stderr, "got line \"%s\"\n", buff);

      tok = strtok(buff, " ");
      
      switch (buff[0]) {
      case '\0':
      case '#':
        // skip
        break;
        
      case 'S':
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
                              TRAVERSE_EXTENDED,
                              0);
        }
        fprintf(stdout, "xR\n");
        break;

      case 'N':
        {
          unsigned lo=atoi(strtok(NULL, " "));
          unsigned hi=atoi(strtok(NULL, " ")); 
          traverse_names(lo, hi);
          fprintf(stdout, "NR\n");
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

      case 'Q':
        fprintf(stdout, "QR\n");
        goto done;
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

static bool_T __MyTreeCB(fsdbTreeCBType cb_type, 
			 void *client_data, void *tree_cb_data)
{
    switch (cb_type) {
    case FSDB_TREE_CBT_BEGIN_TREE:
	if (debug >= 2) fprintf(stderr, "<BeginTree>\n");
	break;

    case FSDB_TREE_CBT_SCOPE:
	if (debug >= 2) __DumpScope((fsdbTreeCBDataScope*)tree_cb_data);
	break;

    case FSDB_TREE_CBT_VAR:
      BuildVar((fsdbTreeCBDataVar*)tree_cb_data);
      if (debug >= 2) __DumpVar((fsdbTreeCBDataVar*)tree_cb_data);
      break;

    case FSDB_TREE_CBT_UPSCOPE:
        if (debug >= 2) fprintf(stderr, "<Upscope>\n");
	break;

    case FSDB_TREE_CBT_END_TREE:
	if (debug >= 2) fprintf(stderr, "<EndTree>\n\n");
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
        if (debug >= 2) fprintf(stderr, "<BeginArray>\n");
        break;
        
    case FSDB_TREE_CBT_ARRAY_END:
        if (debug >= 2) fprintf(stderr, "<EndArray>\n\n");
        break;

    case FSDB_TREE_CBT_RECORD_BEGIN:
        if (debug >= 2) fprintf(stderr, "<BeginRecord>\n");
        break;
        
    case FSDB_TREE_CBT_RECORD_END:
        if (debug >= 2) fprintf(stderr, "<EndRecord>\n\n");
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

    fprintf(stderr, "<Scope> name:%s  type:%s\n", 
	    scope->name, type);
}

static
void BuildVar(fsdbTreeCBDataVar *var)
{
    namerec_t *rec = (namerec_t *)malloc(sizeof(namerec_t));

    rec->name          = strdup(var->name);
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
            "<Var>  name:%s  l:%u  r:%u  type:%s  ",
            var->name, var->lbitnum, var->rbitnum, type);
    fprintf(stderr,
            "idcode:%u  dtidcode:%u  bpb:%s\n",
            var->u.idcode, var->dtidcode, bpb);
}
