#include <unistd.h>
#include <grp.h>
#include <limits.h>
#include <stdio.h>

int 
main(void)
{
  int i, ngroups, ngroups2, j;
  gid_t mygroups[NGROUPS_MAX];
  gid_t mygroups2[NGROUPS_MAX];

  if((ngroups = getgroups(NGROUPS_MAX, mygroups)) < 0)  {
    perror("getgroups");
    return -1;
  }

  for (i = ngroups; i >= 0; --i) {
    printf("%d groups:\n", i);
    if (setgroups(i, mygroups) < 0) {
      perror("getgroups");
      return -1;
    }
    system("groups");
    if((ngroups2 = getgroups(NGROUPS_MAX, mygroups2)) < 0)  {
      perror("getgroups");
      return -1;
    }
    for (j=0; j<ngroups2; ++j) {
      printf("%d ", mygroups2[j]);
    }
    printf("\n");
  }
}
