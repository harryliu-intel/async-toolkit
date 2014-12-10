/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#ifdef PREFETCH

#define prefetch(addr,rw,locality) __builtin_prefetch(addr,rw,locality)
#define prefetch_load(addr) __builtin_prefetch(addr,0,3)
#define prefetch_store(addr) __builtin_prefetch(addr,1,3)
#define prefetch_load_transient(addr) __builtin_prefetch(addr,0,0)
#define prefetch_store_transient(addr) __builtin_prefetch(addr,1,0)

#else

#define prefetch(addr,rw,locality) ((void) 0)
#define prefetch_load(addr) ((void) 0)
#define prefetch_store(addr) ((void) 0)
#define prefetch_load_transient(addr) ((void) 0)
#define prefetch_store_transient(addr) ((void) 0)

#endif
