#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define MAX_STR 1024

/** Make power supplies ports **/
#define PORT_SUPPLY
#ifdef  PORT_SUPPLY
char PowerNet[MAX_STR] ="Vdd";
char GroundNet[MAX_STR]="GND";
#endif

/** GDS record constants **/
#define GDS_HEADER       0x00
#define GDS_BGNLIB       0x01
#define GDS_LIBNAME      0x02
#define GDS_UNITS        0x03
#define GDS_ENDLIB       0x04
#define GDS_BGNSTR       0x05
#define GDS_STRNAME      0x06
#define GDS_ENDSTR       0x07
#define GDS_BOUNDARY     0x08
#define GDS_TEXT         0x0C
#define GDS_LAYER        0x0D
#define GDS_DATATYPE     0x0E
#define GDS_XY           0x10
#define GDS_ENDEL        0x11
#define GDS_TEXTTYPE     0x16
#define GDS_PRESENTATION 0x17
#define GDS_LABEL        0x19
#define GDS_MAG          0x1B

/** GDS datatype constants **/
#define GDS_NODATA   0x0
#define GDS_BITFIELD 0x1
#define GDS_INT2     0x2
#define GDS_INT4     0x3
#define GDS_REAL8    0x5
#define GDS_STRING   0x6

/** Map a layer name to GDS layer:purpose **/
typedef struct {
  char name[MAX_STR];
  int layer;
  int purpose;
} layer_map;

/** Table of GDS layer:purpose mappings for 1274.13 shapes **/
layer_map draw_map[] = {
  {"nwell",11,0},
  {"ndiff",1,0},
  {"pdiff",8,0},
  {"nv1",94,0},
  {"pv1",97,0},
  {"nv2",98,0},
  {"pv2",99,0},
  {"nv3",103,0},
  {"pv3",102,0},
  {"slinomn",81,37},
  {"slinomp",81,36},
  {"wirepoly",2,0},
  {"diffcon",5,0},
  {"fti",184,0},
  {"vcg",32,0},
  {"vct",31,0},
  {"metal0",55,0},
  {"metals0",55,0},
  {"metalc0",55,0},
  {"via0",56,0},
  {"metal1",4,0},
  {"via1",13,0},
  {"metal2",14,0},
  {"metalc2",14,0},
  {"prb",50,0},
  {"",0,0} // terminate
};

/** Alternate table used for fill shapes **/
layer_map fill_map[]= {
  {"wirepoly",2,250},
  {"diffcon",5,250},
  {"metal0",55,250},
  {"metals0",55,250},
  {"metalc0",55,250},
  {"metal1",4,250},
  {"metal2",14,250},
  {"metalc2",14,250},
  {"",0,0} // terminate
};

/** Alternate table used for pin shapes **/
layer_map pin_map[]= {
  {"metal0",55,2},
  {"metals0",55,2},
  {"metalc0",55,2},
  {"metal1",4,2},
  {"metal2",14,2},
  {"metalc2",14,2},
  {"",0,0} // terminate
};

/** Translate layer name to layer:purpose pair **/
layer_map *find_layer(layer_map *map, char *name) {
  layer_map *p;
  // NOTE: linear search is inefficient
  for (p=map; p->name[0]!=0; p++)
    if (strcmp(p->name,name)==0) return p;
  return NULL;
}

/** fwrite that swaps endianness of each element **/
size_t big_fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  int i,j;
  size_t n=0;
  for (i=0; i<nmemb; i++) {
    for (j=0; j<size; j++) n+=fwrite(&((char *)ptr)[size-1-j],1,1,stream);
    ptr+=size;
  }
  return n;
}

/** Write 1 byte **/
void write1(FILE *fout, uint8_t d) {
  fwrite(&d,sizeof(uint8_t),1,fout);
}

/** Write 2 bytes (big-endian) **/
void write2(FILE *fout, uint16_t d) {
  big_fwrite(&d,sizeof(uint16_t),1,fout);
}

/** Write 4 bytes (big-endian) **/
void write4(FILE *fout, uint32_t d) {
  big_fwrite(&d,sizeof(uint32_t),1,fout);
}

/** Write 8 bytes (big-endian) **/
void write8(FILE *fout, uint64_t d) {
  big_fwrite(&d,sizeof(uint64_t),1,fout);
}

/** Write 4B record header **/
void write_record(FILE *fout, uint8_t type, uint8_t datatype, uint16_t len) {
  write2(fout,len+4); // NOTE: len includes length of header
  write1(fout,type);
  write1(fout,datatype);
}

/** Create a GDS library and start a GDS structure **/
void start_gds(FILE *fout, char *libName, char *cellName) {
  uint16_t d;
  uint16_t time[6] = {0,0,0,0,0,0}; // ignore timestamp
  uint8_t units[16] = {0x3e,0x41,0x89,0x37,0x4b,0xc6,0xa7,0xf0,  // 1e-3
                       0x38,0x6d,0xf3,0x7f,0x67,0x5e,0xf6,0x48}; // 1e-10
  write_record(fout,GDS_HEADER,GDS_INT2,sizeof(uint16_t));
  write2(fout,5); // version 5
  write_record(fout,GDS_BGNLIB,GDS_INT2,24);
  fwrite(time,sizeof(uint16_t),6,fout); // modified
  fwrite(time,sizeof(uint16_t),6,fout); // accessed
  int len=2*((strlen(libName)+1)/2);
  write_record(fout,GDS_LIBNAME,GDS_STRING,len);
  fwrite(libName,1,len,fout);
  write_record(fout,GDS_UNITS,GDS_REAL8,16);
  fwrite(units,1,16,fout);
  write_record(fout,GDS_BGNSTR,GDS_INT2,24);
  fwrite(time,sizeof(uint16_t),6,fout); // modified
  fwrite(time,sizeof(uint16_t),6,fout); // accessed
  len=2*((strlen(cellName)+1)/2);
  write_record(fout,GDS_STRNAME,GDS_STRING,len);
  fwrite(cellName,1,len,fout);
}

/** Write GDS rectangle  **/
void write_rect(FILE *fout, layer_map *lpp, int x0, int y0, int x1, int y1) {
  write_record(fout,GDS_BOUNDARY,GDS_NODATA,0);
  write_record(fout,GDS_LAYER,GDS_INT2,2);
  write2(fout,lpp->layer);
  write_record(fout,GDS_DATATYPE,GDS_INT2,2);
  write2(fout,lpp->purpose);
  write_record(fout,GDS_XY,GDS_INT4,10*sizeof(uint32_t));
  write4(fout,x0);
  write4(fout,y0);
  write4(fout,x1);
  write4(fout,y0);
  write4(fout,x1);
  write4(fout,y1);
  write4(fout,x0);
  write4(fout,y1);
  write4(fout,x0);
  write4(fout,y0);
  write_record(fout,GDS_ENDEL,GDS_NODATA,0);
}

/** Write GDS label **/
void write_label(FILE *fout, char *net, layer_map *lpp, int x, int y) {
  uint8_t mag[8] = {0x3f,0x51,0xeb,0x85,0x1e,0xb8,0x51,0xec}; // 0.02
  write_record(fout,GDS_TEXT,GDS_NODATA,0);
  write_record(fout,GDS_LAYER,GDS_INT2,2);
  write2(fout,lpp->layer);
  write_record(fout,GDS_TEXTTYPE,GDS_INT2,2);
  write2(fout,lpp->purpose);
  write_record(fout,GDS_PRESENTATION,GDS_BITFIELD,2);
  write2(fout,0x5); // center
  write_record(fout,GDS_MAG,GDS_REAL8,8);
  fwrite(mag,1,8,fout);
  int len=2*((strlen(net)+1)/2);
  write_record(fout,GDS_LABEL,GDS_STRING,len);
  fwrite(net,1,len,fout);
  write_record(fout,GDS_XY,GDS_INT4,8);
  write4(fout,x);
  write4(fout,y);
  write_record(fout,GDS_ENDEL,GDS_NODATA,0);
}

/** Finish GDS structure and GDS library file **/
void finish_gds(FILE *fout) {
  write_record(fout,GDS_ENDSTR,GDS_NODATA,0);
  write_record(fout,GDS_ENDLIB,GDS_NODATA,0);
}

/** Convert LGF to GDS **/
int main(int argc, char **argv) {
  FILE *fin,*fout;

  // parse command-line arguments
  if (argc!=3) { fprintf(stderr,"USAGE: lgf2gds in.lgf out.gds\n"); exit(1); }
  fin  = fopen(argv[1],"rt");
  if (!fin) { fprintf(stderr,"ERROR: can't read %s\n",argv[1]); exit(1); }
  fout = fopen(argv[2],"wb");
  if (!fout) { fprintf(stderr,"ERROR: can't write %s\n",argv[2]); exit(1); }

  // process LGF line by line
  char line[MAX_STR];
  while (fgets(line,MAX_STR,fin)) {
    char cell[MAX_STR],net[MAX_STR],layer[MAX_STR],model[MAX_STR],type,f;
    char device[MAX_STR],drn[MAX_STR],gate[MAX_STR],src[MAX_STR],bulk[MAX_STR],payload[MAX_STR];
    int x0,y0,x1,y1,invisible=0,ported=0,r,x,y,w,l;
    if (sscanf(line,"Cell %s bbox=%d:%d:%d:%d",cell,&x0,&y0,&x1,&y1)==5) {
      start_gds(fout,"laygen",cell);
      layer_map *lpp=find_layer(draw_map,"prb");
      if (!lpp) { fprintf(stderr,"ERROR: prb layer not mapped\n"); exit(1); }
      write_rect(fout,lpp,x0,y0,x1,y1);
    }
    else if (sscanf(line,"Device %s nets=[ %s %s %s %s ] type=%c model=%s w=%d l=%d r=%d x=%d y=%d f=%c",
                    device,drn,gate,src,bulk,&type,model,&w,&l,&r,&x,&y,&f)==13) {
      // TODO: compute diffusion shape more elegantly
      x0 = (x+1)*540-440;
      x1 = (x+1)*540+440;
      y0 = r*3400 + 1700 + y*340*(type=='p' ? 1 : -1) + 340*(type=='p' ? 1 : 0);
      y1 = y0 + w*(type=='p' ? 1 : -1);
      char *layer=type=='p' ? "pdiff" : "ndiff";
      layer_map *lpp=find_layer(draw_map,layer);
      if (!lpp) { fprintf(stderr,"ERROR: %s layer not mapped\n",layer); exit(1); }
      write_rect(fout,lpp,x0,y0,x1,y1);
    }
    else if (sscanf(line,"Wire net=%s layer=%s rect=%d:%d:%d:%d payload=%s ported=%d invisible=%d",
                    net,layer,&x0,&y0,&x1,&y1,payload,&ported,&invisible)==9 ||
             sscanf(line,"Wire net=%s layer=%s rect=%d:%d:%d:%d payload=%s invisible=%d",
                    net,layer,&x0,&y0,&x1,&y1,payload,&invisible)==8 ||
             sscanf(line,"Wire net=%s layer=%s rect=%d:%d:%d:%d payload=%s ported=%d",
                    net,layer,&x0,&y0,&x1,&y1,payload,&ported)==8 ||
             sscanf(line,"Wire net=%s layer=%s rect=%d:%d:%d:%d payload=%s",
                    net,layer,&x0,&y0,&x1,&y1,payload)==7) {
      layer_map *lpp=find_layer(draw_map,layer);
      if (!lpp) { fprintf(stderr,"ERROR: %s layer not mapped\n",layer); exit(1); }
#ifdef PORT_SUPPLY
      if ((strcmp(net,PowerNet)==0) || (strcmp(net,GroundNet)==0)) ported=1;
#endif
      if (invisible);
      else if (ported) {
        layer_map *pin_lpp=find_layer(pin_map,layer);
        write_rect(fout,lpp,x0,y0,x1,y1); // always create drawing shape
        write_label(fout,net,pin_lpp?pin_lpp:lpp,(x0+x1)/2,(y0+y1)/2); // label it
        if (pin_lpp) write_rect(fout,pin_lpp?pin_lpp:lpp,x0,y0,x1,y1);
      } else if (net[0]=='!') { // prefer fill purpose, no label
        layer_map *fill_lpp=find_layer(fill_map,layer);
        write_rect(fout,fill_lpp?fill_lpp:lpp,x0,y0,x1,y1);
      } else { // drawing shape with label
        write_rect(fout,lpp,x0,y0,x1,y1);
        write_label(fout,net,lpp,(x0+x1)/2,(y0+y1)/2);
      }
    }
  }

  // finish up
  finish_gds(fout);
  fclose(fin);
  fclose(fout);
}
