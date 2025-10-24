#define DWG_TYPE_IMAG 0
#define DWG_TYPE_POST 1
#define DWG_TYPE_X 2
#define DWG_TYPE_GIF 3

FILE *set_file (FILE *f);
void dwg_docolor(int arg);
void dwg_filename(char *s);
void dwg_setmag(double mag);
void dwg_setoffset(double valx, double valy);
int printer_type (void);
void end_page(void);
void setpen(int i);
void end_file(void);
void set_border(int x, int y);
void draw_box(int x1, int y1, int x2, int y2, int fill);
int set_texture (int fam, int mem);
void draw_line(int x1, int y1, int x2, int y2, int pen);
void draw_point(int x, int y, int pen);
void set_family(int f);
void smove(int d);
void mmove(int d);
void crlf(void);
void text(int x, int y, char *s, int f);
void polygon(unsigned cnt, int *x, int *y, int fill, int line);
void set_adv_dirs(int main, int secondary);
void draw_circle(int x, int y, int r, int pen);
void circ_arc (int r, int x, int y, int a1, int a2, int pen);
void force_portrait (int val);
void force_scale (int val);
