// $Id: //depot/user/aubrey/src/dwg/maplib.h#1 $ AAG
#include <stdio.h>
extern int debug;
extern int mindate, maxdate;

// Color map structure

typedef struct cmap  MAP;

struct cmap {
   unsigned char r;
   unsigned char g;
   unsigned char b;
};

// The actual map
extern MAP **map;

extern MAP white,
           black,
           red,
           blue,
           green,
           magenta,
           cyan,
           yellow,
           orange;

void initmap (int cols, int rows);
void writemap (MAP color, int x, int y);
int writechar (MAP color, int y, int x, int c, FONT *f);
int writestring (MAP color, int y, int x, char *s, FONT *f);
void writeline (MAP color, int x1, int y1, int x2, int y2);
void writerect (MAP color, int x1, int y1, int x2, int y2, int fill);
void writecircle (MAP color, int xc, int yc, int r, int fill);
void writeppmfile (FILE *fppm);
void writegiffile (FILE *fgif, int cols, int rows);
