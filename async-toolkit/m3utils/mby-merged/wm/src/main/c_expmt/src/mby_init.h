#include <mby_model.h>
#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

void mby_init_common_regs
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_top_map * const rx_top_map
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE]
#endif
);
