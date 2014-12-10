#include <unistd.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>

int unlink(const char *pathname) {
    int (*__unlink)(const char *);
    __unlink = dlsym(RTLD_NEXT, "unlink");
    char *slash = strrchr(pathname, '/');
    if (slash && !strcmp(slash + 1, "1.db")) {
        char *newname = getenv("OUTPUT_DB");
        if (newname) {
            link(pathname, newname);
#ifdef DEBUG
            printf("link(%s, %s)\n", pathname, newname);
#endif
        }
    }
    return __unlink(pathname);
}
