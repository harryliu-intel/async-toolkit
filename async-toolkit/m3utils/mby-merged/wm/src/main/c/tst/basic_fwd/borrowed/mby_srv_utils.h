
#ifndef _MBY_SERVER_UTILS_H_
#define _MBY_SERVER_UTILS_H_

#include <string.h>

/** \ingroup macroSystem
 *  Macro used to suppress compiler warnings when a function parameter is
 *  deliberately not used in a function. */
#define FM_NOT_USED(a) (void) (a)

/** \ingroup macroSystem
 *  *  Macro used to zero an array or structure. */
#define FM_CLEAR(object) \
	    memset(&(object), 0, sizeof(object))

// FIXME memcpy_s is not available in C99
#define FM_MEMCPY_S(dst, limit, src, count) \
	memcpy(dst, src, count)


fm_status ParseUint64(char *string, fm_uint64 *result);
fm_status ParseInt(char *string, fm_int *result);
void HexDump(fm_byte *buf, fm_int nbytes);

#endif /* _MBY_SERVER_UTILS_H_ */
