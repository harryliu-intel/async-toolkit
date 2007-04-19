/*****************************************************************
 * outline.c
 *
 * Copyright 1999, Clark Cooper
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the license contained in the
 * COPYING file that comes with the expat distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Read an XML document from standard input and print an element
 * outline on standard output.
 * Must be used with Expat compiled for UTF-8 output.
 */


#include <stdio.h>
#include "/usr/local/include/expat.h"

#define DEBUG 0

#ifdef XML_LARGE_SIZE
#if defined(XML_USE_MSC_EXTENSIONS) && _MSC_VER < 1400
#define XML_FMT_INT_MOD "I64"
#else
#define XML_FMT_INT_MOD "ll"
#endif
#else
#define XML_FMT_INT_MOD "l"
#endif

#define BUFFSIZE        8192

char Buff[BUFFSIZE];

int Depth;

typedef void (startCall)(void *stuff, const char *el);
typedef void (endCall)(void *stuff);
typedef void (attrCall)(void *stuff, const char *tag, const char *attr);

typedef struct {
  void *stuff;
  startCall *s;
  attrCall *a;
  endCall *e;
} UD;

static void XMLCALL
start(void *data, const char *el, const char **attr)
{
  int i;
  UD *m3callbacks = data;

#if DEBUG
  for (i = 0; i < Depth; i++)
    printf("  ");

  printf("%s", el);
#endif
  
  if (m3callbacks->s) m3callbacks->s(m3callbacks->stuff,el);

  for (i = 0; attr[i]; i += 2) {
#if DEBUG
    printf(" %s='%s'", attr[i], attr[i + 1]);
#endif
    if (m3callbacks->a) m3callbacks->a(m3callbacks->stuff,
				       attr[i], attr[i + 1]);
  }

#if DEBUG
  printf("\n");
#endif
  Depth++;
}

static void XMLCALL
end(void *data, const char *el)
{
  UD *m3callbacks = data;

  if (m3callbacks->e) m3callbacks->e(m3callbacks->stuff);

  Depth--;
}

int
xmlParserMain(const char *path,
	      void *stuff, startCall s, attrCall a, endCall e)
{
  FILE *ifp;

  UD *m3callbacks=malloc(sizeof(UD));

  XML_Parser p = XML_ParserCreate(NULL);
  if (! p) {
    fprintf(stderr, "Couldn't allocate memory for parser\n");
    return -1;
  }

  if (path) {
    if ( !(ifp = fopen(path,"r")) ) {
      fprintf(stderr, "xmlParser: FILE %s NOT FOUND.\n", path);
      return -1;
    }
  } else
    ifp = stdin;

  m3callbacks->stuff = stuff;
  m3callbacks->s = s;
  m3callbacks->a = a;
  m3callbacks->e = e;

  XML_SetUserData(p,m3callbacks);

  XML_SetElementHandler(p, start, end);

  { 
    int done;

    do {
      int len;
      
      len = fread(Buff, 1, BUFFSIZE, ifp);
      if (ferror(ifp)) {
	fprintf(stderr, "Read error\n");
	return -1;
      }
      done = feof(ifp);
      
      if (XML_Parse(p, Buff, len, done) == XML_STATUS_ERROR) {
	fprintf(stderr, "Parse error at line %" XML_FMT_INT_MOD "u:\n%s\n",
		XML_GetCurrentLineNumber(p),
		XML_ErrorString(XML_GetErrorCode(p)));
	return -1;
      }
      
    } while (!done);
  }

  if (path) fclose(ifp);

  return 0;
}
