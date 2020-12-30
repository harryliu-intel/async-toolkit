#include <mpfr.h>
#include <malloc.h>
#include <stdio.h>

mpfr_ptr 
MpfrC__alloc(void)
{
  mpfr_ptr res=malloc(sizeof(mpfr_t));

#if 0
  printf("%s:%d making %x\n", __FILE__, __LINE__, res);
#endif
  
  return res;
}

mpfr_ptr
MpfrC__deref(mpfr_ptr p)
{
#if 0
  printf("%s:%d passing %x\n", __FILE__, __LINE__, p);
#endif
  
  return p;
}

int
MpfrC__GetPrecFormat(void)
{
  return _MPFR_PREC_FORMAT;
}
