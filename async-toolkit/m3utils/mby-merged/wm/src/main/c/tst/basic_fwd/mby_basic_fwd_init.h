#include <mby_common.h>
#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

void basic_fwd_init
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_top_map * const rx_top_map,
#endif
    fm_uint32 fwd_port,
    fm_macaddr dmac
);
