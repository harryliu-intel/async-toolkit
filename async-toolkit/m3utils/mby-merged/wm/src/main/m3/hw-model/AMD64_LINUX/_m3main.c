#include <stddef.h>
#if __INITIAL_POINTER_SIZE == 64
typedef __int64 INTEGER;
#else
typedef ptrdiff_t INTEGER;
#endif
#ifdef __cplusplus
extern "C" {
#endif
void RTLinker__InitRuntime(INTEGER, char**, char**, void*);
void RTProcess__Exit(INTEGER);
void* Main_M3(void); /* ? */
void RTLinker__AddUnitImports(void* (*)(void)); /* ? */
void RTLinker__AddUnit(void* (*)(void)); /* ? */
extern void* Main_M3(void);
#ifdef __cplusplus
} /* extern "C" */
#endif
int main (int argc, char** argv, char** envp)
{
  RTLinker__InitRuntime (argc, argv, envp, (void*)0);
  RTLinker__AddUnit (Main_M3);
  RTProcess__Exit (0);
  return 0;
}

