/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <time.h>

void archive_init(char *archiver);
FILE *archive_fopen(char *path, char *mode);
time_t archive_mtime(char *path);
