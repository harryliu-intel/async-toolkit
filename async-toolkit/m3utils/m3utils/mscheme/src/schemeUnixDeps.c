/* $Id: schemeUnixDeps.c,v 1.2 2009/07/09 17:09:55 mika Exp $ */

#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <errno.h>

char *
SchemeUnixDeps__getCurrentUserWrapper(void) 
{
	uid_t uid = getuid();

	return getpwuid(uid)->pw_name;
}

char *
SchemeUnixDeps__getHomeDirWrapper(char *user)
{
	struct passwd *pw=getpwnam(user);

	if (!pw) 
		return NULL;
	else
		return pw->pw_dir;
}

int
SchemeUnixDeps__getErrno()
{
	return errno;
}
