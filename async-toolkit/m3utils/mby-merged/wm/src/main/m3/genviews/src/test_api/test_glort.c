#include <setjmp.h>
#include "mby_top_map.h"

static void
update_flood_glort_entry(flood_glort_table_r           *p,
                         enum ies_domain_floodset_type  type,
                         uint16_t                       glort)
/* could in theory be auto-generated */
{
  switch (type) {
    case IES_DOMAIN_FLOODSET_ALL:
      p->FLOOD_UNICAST_GLORT = glort;
      p->FLOOD_MULTICAST_GLORT = glort;
      p->BROADCAST_GLORT       = glort;
      break;
    case IES_DOMAIN_FLOODSET_UNICAST:
      p->FLOOD_UNICAST_GLORT   = glort;
      break;
    case IES_DOMAIN_FLOODSET_MULTICAST:
      p->FLOOD_MULTICAST_GLORT = glort;
      break;
    case IES_DOMAIN_FLOODSET_BROADCAST:
      p->BROADCAST_GLORT       = glort;
      break;
    default: 
      raise_exception(IES_ERR_INVALID_ARG);
      break;
    }
  /*NOTREACHED*/
}

static void
check_flood_glort_table_idx(int                  l2_domain_id)
{
  /* could be auto-generated */
  if (l2_domain_id <  0 ||
      l2_domain_id >= mby_ppe_nexthop_map_FLOOD_GLORT_TABLE__nd)
    raise_exception(IES_ERR_INVALID_INSTANCE);
}

typedef struct syncrec {
  /* data structure describing the size of the register tree below a 
     specific point in the hierarchy */
  unsigned int nfields;      // number of leaf fields 
  size_t       localbytes;   // number of bytes in local memory
} syncrec_t;

typedef struct op_handle {
  void                *cache;
  void                *workspace;
  void                *base; /* base of struct allocated */
  const syncrec_t     *sync;
  const unsigned int   startfield;
} op_handle_t;

op_handle_t *
open_rw(const void *srcp, void *tgtp, size_t tgtsz)
{
  op_handle_t *handlep = ies_ex_alloc(sizeof(op_handle_t));

  const syncrec_t *syncrec =
    get_sync_by_ptr(&(srcv), &(handlep->startfield));
  
  assert(tgtsz == syncrec->localbytes);
  memcpy(tgtp, srcp, syncrec->localbytes);
  
  handlep->cache = srcp;
  handlep->base  = lookup_cache(srcp);
  handlep->sync  = syncrec;
  return handlep;
}

/* deque for write operations */
typedef struct writeop {
  unsigned long addr;
  unsigned long data;
  struct writeop *next,*prev;
} writeop_t;

void
commit_changes(op_handle_t *handlep)
{
  writeop_t *changelist;
  writeop_t *sentinel;

  sentinel = ies_ex_alloc(sizeof(writeop_t));
  sentinel->next = sentinel;
  sentinel->prev = sentinel;
  changelist = sentinel;
  
  traverse_work_and_cache(handlep->cache,
                          handlep->workspace,
                          handlep->startfield,
                          handlep->sync->nfields,
                          changelist);

  // now changelist holds all the changes that need to be executed
  {
    // what do we do if there is a failure here, in sending the writes
    // out?
    // we could, for example, invalidate the cache of handlep->cache
    ies_exception_context_t q;
    ies_exception_t ex;
    IES_TRY(q, ex, IES_EXCEPTION_ANY)
      execute_changelist(changelist);
    IES_EXCEPT {
      mark_cache_state(handlep->cache, handlep->sync, IES_CACHE_INVALID);
      ies_raise_exception(ex);
    }
    IES_EXEND(q);
  }

  // success -- so, write all the changes locally
  memcpy(handlep->cache, handlep->workspace, handlep->sync->localbytes);

  // recursively mark this sector of cache as being valid
  mark_cache_state(handlep->cache, handlep->sync, IES_CACHE_VALID);
  
  ies_ex_destroy(handlep->workspace);
  ies_ex_destroy(handlep);
  ies_ex_destroy(sentinel);
}

        
#define OPEN_RW(srcv, tgtv)                                   \
  open_rw(&(srcv), &(tgtv), sizeof(tgtv));

/**********************************************************************/

static void
domain_set_flood_glort(
      const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t  table,
      // note that table is a pointer because it's an array
      int                                            l2_domain_id,
      enum ies_domain_floodset_type                  type,
      uint16_t                                       glort)
{
  flood_glort_table_r       w;
  op_handle_t             **handle;

  check_flood_glort_table_idx(l2_domain_id);
  OPEN_RW(table[l2_domain_id], w, handle);
  update_flood_glort_entry(&w, type, glort);
  commit_changes(handle);
}

static void
domain_set_flood_glort_everywhere(
      const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t  table,
      uint16_t                                       glort)
{
  mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t w;
  op_handle_t                           **handle;

  open_array_rw(x, table, w, handle);
  for(int e = 0; e < mby_ppe_nexthop_map_FLOOD_GLORT_TABLE__nd; ++e)
    update_flood_glort_entry(x, &w[e], IES_DOMAIN_FLOODSET_ALL, glort);
  commit_changes(handle);
}

static void
get_flood_glort_entry(flood_glort_table_r           *p,
                      enum ies_domain_floodset_type  type)
/* could in theory be auto-generated */
{
  switch (type) {
    case IES_DOMAIN_FLOODSET_UNICAST:
      return p->FLOOD_UNICAST_GLORT;
      break;
    case IES_DOMAIN_FLOODSET_MULTICAST:
      return p->FLOOD_MULTICAST_GLORT;
      break;
    case IES_DOMAIN_FLOODSET_BROADCAST:
      return p->BROADCAST_GLORT;
      break;
    case IES_DOMAIN_FLOODSET_ALL:
    default: 
      raise_exception(IES_ERR_INVALID_ARG);
      break;
    }
  /*NOTREACHED*/
}

static uint16_t
domain_get_flood_glort(
      const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t  table,
      int                                            l2_domain_id,
      enum ies_domain_floodset_type                  type)
{
  const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t *rp;
  op_handle_t                           **handle;
  uint16_t                                res;

  check_flood_glort_table_idx(l2_domain_id);
  open_r(&table[l2_domain_id], r, handle);
  res = get_flood_glort_entry(rp, type);
  close_r(handle);
  return res;
}
