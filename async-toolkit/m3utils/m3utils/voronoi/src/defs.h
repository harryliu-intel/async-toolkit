#ifndef NULL
#define NULL 0
#endif
#define DELETED -2

int triangulate, sorted, plot, debug;


float xmin, xmax, ymin, ymax, deltax, deltay;


struct Point	{
float x,y;
};

/* structure used both for sites and for vertices */
struct Site	{
struct	Point	coord;
int		sitenbr;
};


struct	Site	*sites;
int		nsites;
int		siteidx;
int		sqrt_nsites;
int		nvertices;
struct	Site	*bottomsite;


struct TripleArg {
  int s1, s2, s3;
};

struct Triple {
  struct TripleArg d;
  struct Triple *next;
};

struct Triple *triples;


struct Edge	{
float		a,b,c;
struct	Site 	*ep[2];
struct	Site	*reg[2];
int		edgenbr;
};
#define le 0
#define re 1
int nedges;

int has_endpoint(),right_of();
struct Site *intersect();
float dist();
struct Point PQ_min();
struct Halfedge *PQextractmin();
struct Edge *bisect();

struct Halfedge {
struct Halfedge	*ELleft, *ELright;
struct Edge	*ELedge;
char		ELpm;
struct	Site	*vertex;
float		ystar;
struct	Halfedge *PQnext;
};

struct	Halfedge *ELleftend, *ELrightend;
int 	ELhashsize;
struct	Halfedge **ELhash;
struct	Halfedge *HEcreate(), *ELleft(), *ELright(), *ELleftbnd();
struct	Site *leftreg(), *rightreg();


int PQhashsize;
struct	Halfedge *PQhash;
struct	Halfedge *PQfind();
int PQcount;
int PQmin;
int PQempty();

char *memmalloc();

/* free lists */
struct Triple *tfl;
struct Halfedge *hfl;
struct Site *sfl;
struct Edge *efl;
