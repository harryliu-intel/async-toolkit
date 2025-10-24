
typedef struct fontdef FONT;
typedef struct chardef CHARDEF;

struct chardef {
   int dwidth;
   int map[32];
};

struct fontdef {
   short ascent, descent, boxx, boxy;
   CHARDEF cmap[128];
};

