// "$Id: //depot/user/aubrey/src/lib/libaag/c_std.h#1 $ AAG";
#ifndef __c_std_h__
#define __c_std_h__ 1

/***
*    C_STD
*   -------
* Constant and Type definitions to be used when coding in "C".
* This file is setup for: SUN (UNIX) C
***/

#define VOID      void
#define MLOCAL    static
#define LOCAL     auto
#define EXTERN    extern
#define GLOBAL    globaldef
#ifndef MAX
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#endif
#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef ABS
#define ABS(a)   ((a) >  0  ? (a) : -(a))
#endif
#define and &&
#define or ||
#define MAXSSTR    22
#define MAXSTR    255
#define MININT16 -32768
#define MAXINT16  32767
#define MININT32 -2147483648
#define MAXINT32  2147483647

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

typedef short          BOOL;     /* Boolean, TRUE or FALSE only */
typedef char           CHAR;     /* 8 bit character */
typedef char           *STRING;  /* string type */
typedef short int      INT16;    /* 16 bit signed integer */
typedef int            INT32;    /* 32 bit signed integer */
#ifndef NOTGCC
typedef long long int  INT64;    /* 64 bit signed integer */
typedef unsigned long long int  UINT64;    /* 64 bit unsigned integer */
#endif
typedef unsigned short UINT16;   /* 16 bit unsigned integer */
typedef unsigned int   UINT32;   /* 32 bit unsigned integer */

#endif /* _c_std_h */

