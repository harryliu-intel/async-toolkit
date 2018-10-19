#ifndef _FM_DEFS_H
#define _FM_DEFS_H
#include "fm_types.h"

#define fmModelIsBroadcastMacAddress(addr)                              \
    ( ( (addr) == FM_LITERAL_U64(0xFFFFFFFFFFFF) ) ? TRUE : FALSE )

#define fmModelIsUnicastMacAddress(addr)                                       \
         ( ((addr) & FM_LITERAL_U64(0x010000000000)) == 0 )

#define fmModelIsMulticastMacAddress(addr)                                     \
         ( ( ((addr) & FM_LITERAL_U64(0x010000000000)) != 0 ) &&               \
           !fmModelIsBroadcastMacAddress(addr) )

#endif /* !_FM_DEFS_H */
