#ifndef _FM_TYPES_H
#define _FM_TYPES_H

// Basic Data Types:
typedef char                  fm_char;
typedef short                 fm_int16;
typedef int                   fm_int32;
typedef long long             fm_int64;
typedef int                   fm_int;

typedef unsigned char         fm_bool;
typedef unsigned char         fm_byte;
typedef unsigned int          fm_uint;
typedef unsigned short        fm_uint16;
typedef unsigned int          fm_uint32;
typedef unsigned long long    fm_uint64;

typedef char                 *fm_text;

// FM Data Types:
typedef fm_int                fm_status;
typedef unsigned long long    fm_macaddr;

// Constants:
#define FM_OK     0
#define FM_FAIL   1
#define TRUE      1
#define FALSE     0

#define FM_LITERAL_U64(x)       (x ## ULL)  // from fm_std.h
#define FM_LITERAL_64(x)        (x ## LL)   // from fm_std.h

#endif /* !_FM_TYPES_H */
