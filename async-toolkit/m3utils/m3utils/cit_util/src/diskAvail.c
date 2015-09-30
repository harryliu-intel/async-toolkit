#ifdef linux
# include <sys/statfs.h>
#endif

#include <sys/param.h>
#include <sys/mount.h>

/* $Id: diskAvail.c,v 1.2 2007/12/16 06:45:05 mika Exp $ */

int
diskAvail(const char * path,
	  int *bsize,
	  double *total, double *avail, double *availNonSuperUser) 
{
  struct statfs buf;

  if (statfs(path,&buf)<0) return -1;

  *bsize = buf.f_bsize;
  *total = buf.f_blocks;
  *avail = buf.f_bfree;
  *availNonSuperUser = buf.f_bavail;

  return 0;
}

