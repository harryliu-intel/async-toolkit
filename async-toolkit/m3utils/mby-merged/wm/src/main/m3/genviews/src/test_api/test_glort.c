#include <setjmp.h>
#include "mby_top_map.h"

static void
update_flood_glort_entry(exception_context_t           *x,
                         flood_glort_table_r           *p,
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
      raise_exception(x, IES_ERR_INVALID_ARG);
      break;
    }
  /*NOTREACHED*/
}

static void
check_flood_glort_table_idx(exception_context_t *x,
                            int                  l2_domain_id)
{
  /* could be auto-generated */
  if (l2_domain_id <  0 ||
      l2_domain_id >= mby_ppe_nexthop_map_FLOOD_GLORT_TABLE__nd)
    raise_exception(x, IES_ERR_INVALID_INSTANCE);
}
                            
static void
domain_set_flood_glort(
      exception_context_t                           *x,
      const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t  table,
      int                                            l2_domain_id,
      enum ies_domain_floodset_type                  type,
      uint16_t                                       glort
                       )
{
  flood_glort_table_r       w;
  op_handle_t             **handle;

  check_flood_glort_table_idx(x, l2_domain_id);
  open_rw(x, &table[l2_domain_id], &w, handle);
  update_flood_glort_entry(x, &w, type, glort);
  commit_changes(handle);
}

static void
domain_set_flood_glort_everywhere(
      exception_context_t                           *x,
      const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t  table,
      uint16_t                                       glort
                       )
{
  mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t w;
  op_handle_t                           **handle;

  open_array_rw(x, table, w, handle);
  for(int e = 0; e < mby_ppe_nexthop_map_FLOOD_GLORT_TABLE__nd; ++e)
    update_flood_glort_entry(x, &w[e], IES_DOMAIN_FLOODSET_ALL, glort);
  commit_changes(handle);
}

static void
get_flood_glort_entry(exception_context_t           *x,
                      flood_glort_table_r           *p,
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
      raise_exception(x, IES_ERR_INVALID_ARG);
      break;
    }
  /*NOTREACHED*/
}

static uint16_t
domain_get_flood_glort(
      exception_context_t                           *x,
      const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t  table,
      int                                            l2_domain_id,
      enum ies_domain_floodset_type                  type
                       )
{
  const mby_ppe_nexthop_map_FLOOD_GLORT_TABLE_t *rp;
  op_handle_t                           **handle;
  uint16_t                                res;

  check_flood_glort_table_idx(x, l2_domain_id);
  open_r(x, &table[l2_domain_id], r, handle);
  res = get_flood_glort_entry(x, rp, type);
  close_r(handle);
  return res;
}

/********************  EXCEPTION HACKS  ********************/

