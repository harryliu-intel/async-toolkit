/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "archive.h"
#include <limits.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "misc.h"
#include "leak.h"

/*** globals and prototypes ***/
typedef struct _entry
  {
  int uses;
  char *archive;
  char *member;
  char *output;
  } ENTRY;

#define CACHE_SIZE 5
static ENTRY *recent=NULL;
static char *archiver=NULL;

/****************************** PRIVATE ***************************/

void archive_cleanup()
  {
  if (archiver) leak_free(archiver);
  if (recent)
    {
    int i;
    for (i=0; i<CACHE_SIZE; ++i)
      {
      if (recent[i].uses>0)
        {
        unlink(recent[i].output);
        leak_free(recent[i].archive);
        leak_free(recent[i].member);
        leak_free(recent[i].output);
        }
      }
    leak_free(recent);
    }
  }

char *archive_member(char *path, char *archive)
  {
  struct stat get_stat;
  char *member;
  strcpy(archive,path);
  while ((stat(archive,&get_stat)!=0)||(!S_ISREG(get_stat.st_mode)))
    {
    char *slash=strrchr(archive,'/');
    if (slash) *slash='\0'; else break;
    }
  if (!S_ISREG(get_stat.st_mode)) return NULL;

  member=path+strlen(archive);
  while (*member=='/') member++; // skip leading slashes
  return member;
  }

void recent_init()
  {
  if (!recent)
    {
    int i;
    recent=leak_malloc(sizeof(ENTRY)*CACHE_SIZE);
    for (i=0; i<CACHE_SIZE; ++i)
      {
      recent[i].uses=0;
      recent[i].archive=recent[i].member=recent[i].output=0;
      }
    }
  }

int match_recent(char *archive, char *member)
  {
  int i;
  recent_init();
  for (i=0; i<CACHE_SIZE; ++i)
    {
    if ((recent[i].uses>0)&&
        (strcmp(recent[i].archive,archive)==0)&&
        (strcmp(recent[i].member,member)==0))
      {
      return i;
      }
    }
  return -1;
  }

void add_recent(char *archive, char *member, char *output)
  {
  int i;
  int min=INT_MAX;
  ENTRY *entry=recent;
  for (i=0; i<CACHE_SIZE; ++i)
    {
    if (recent[i].uses<min)
      {
      min=recent[i].uses;
      entry=&recent[i];
      }
    }

  if (entry->uses>0) unlink(entry->output);

  entry->uses=1;

  entry->archive=leak_realloc(entry->archive,strlen(archive)+1);
  strcpy(entry->archive,archive);

  entry->member=leak_realloc(entry->member,strlen(member)+1);
  strcpy(entry->member,member);

  entry->output=leak_realloc(entry->output,strlen(output)+1);
  strcpy(entry->output,output);
  }

int archive_extract(char *archive, char *member, char *output)
  {
  char cmd[STRMAX];
  safe_sprintf(cmd,"%s '%s' '%s' '%s'",archiver,archive,member,output);
  return system(cmd);
  }

/****************************** PUBLIC ***************************/

void archive_init(char *cmd)
  {
  if (archiver==NULL)
    {
    archiver=leak_malloc(strlen(cmd)+1);
    strcpy(archiver,cmd);
    atexit(archive_cleanup);
    }
  }

FILE *archive_fopen(char *path, char *mode)
  {
  char archive[STRMAX];
  int cached;
  char *member=archive_member(path,archive);
  if (member==NULL) return NULL;
  if (*member=='\0') return fopen(path,mode);

  cached=match_recent(archive,member);
  if (cached==-1)
    {
    char output[STRMAX];
    int fd;
    strcpy(output,"/tmp/aspice_archive.XXXXXX");
    if ((fd=mkstemp(output))==-1)
      {
      perror("Can't create temporary file");
      return NULL;
      }
    close(fd);
    if (archive_extract(archive,member,output)==0)
      {
      add_recent(archive,member,output);
      return fopen(output,mode);
      }
    else
      {
      unlink(output);
      return NULL;
      }
    }
  else
    {
    recent[cached].uses++;
    return fopen(recent[cached].output,mode);
    }
  }

time_t archive_mtime(char *path)
  {
  struct stat get_stat;
  char archive[STRMAX];
  char *member=archive_member(path,archive);
  if ((member==NULL)||(stat(archive,&get_stat)!=0))
    {
    return ((time_t)-1);
    }
  else
    {
    return get_stat.st_mtime;
    }
  }

#if 0
/****************************** TEST ***************************/
void cat(FILE *fp) {
  char buf[4096];
  size_t sz;
  while ((sz = fread(buf, 1, sizeof(buf), fp))) {
    fwrite(buf, 1, sz, stdout);
    if (sz < sizeof(buf)) {
      break;
    }
  }
}

int main(int argc, char **argv) {
  int i;
  archive_init(argv[1]);
  for (i=2; i<argc; ++i) {
    FILE *fp=archive_fopen(argv[i],"r");
    if (fp) {
      cat(fp);
      fclose(fp);
    } else {
      printf("Can't open: %s\n", argv[i]);
    }
  }
}
#endif
