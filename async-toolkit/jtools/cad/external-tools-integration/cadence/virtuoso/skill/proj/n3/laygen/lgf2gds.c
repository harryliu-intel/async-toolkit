#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define MAX_STR 1024

typedef struct {
  char name[MAX_STR];
  int layer;
  int purpose;
} layer_map;

/** Table of GDS layer:purpose for 1274.13 **/
layer_map map[] = {
  {"nwell",11,0},
  {"ndiff",1,0},
  {"pdiff",8,0},
  {"nv1",94,0},
  {"pv1",97,0},
  {"wirepoly",2,0},
  {"diffcon",5,0},
  {"fti",184,0},
  {"vcg",32,0},
  {"vct",31,0},
  {"metal0",120,0},
  {"metals0",120,0},
  {"metalc0",120,0},
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
  {"metal0",120,250},
  {"metals0",120,250},
  {"metalc0",120,250},
  {"metal1",4,250},
  {"metal2",14,250},
  {"metalc2",14,250},
  {"",0,0} // terminate
};

/** translate layer name to layer:purpose pair **/
layer_map *translate_layer(char *name, int fill) {
  if (fill) {
    for (layer_map *p=fill_map; p->name[0]!=0; p++)
      if (strcmp(p->name,name)==0) return p;
  }
  for (layer_map *p=map; p->name[0]!=0; p++)
    if (strcmp(p->name,name)==0) return p;
  fprintf(stderr,"ERROR: unknown layer %s\n",name);
  exit(1);
}

/** Write 1 byte **/
void write1(FILE *fout, uint8_t d) {
  fwrite(&d,sizeof(uint8_t),1,fout);
}

/** Write 2 bytes (big-endian) **/
void write2(FILE *fout, uint16_t d) {
  uint16_t d2=(d>>8) | (d<<8);
  fwrite(&d2,sizeof(uint16_t),1,fout);
}

/** Write 4 bytes (big-endian) **/
void write4(FILE *fout, uint32_t d) {
  uint32_t d2=(d>>24)&0xFF | ((d>>16)&0xFF)<<8 | ((d>>8)&0xFF)<<16 | (d&0xFF)<<24;
  fwrite(&d2,sizeof(uint32_t),1,fout);
}

/** Write 8 bytes (big-endian) **/
void write8(FILE *fout, uint64_t d) {
  uint64_t d2=(d>>56)&0xFF | ((d>>48)&0xFF)<<8 | ((d>>40)&0xFF)<<16 | ((d>>32)&0xFF)<<24 |
    ((d>>24)&0xFF)<<32 | ((d>>16)&0xFF)<<40 | ((d>>8)&0xFF)<<48 | (d&0xFF)<<56;
  fwrite(&d2,sizeof(uint64_t),1,fout);
}

/** Write 4B record header **/
void write_record(FILE *fout, uint8_t type, uint8_t datatype, uint16_t len) {
  write2(fout,len+4);
  write1(fout,type);
  write1(fout,datatype);
}

/** write preamble records **/
void start_gds(FILE *fout, char *libName, char *cellName) {
  uint16_t d;
  uint16_t time[6] = {0,0,0,0,0,0}; // TODO?
  uint8_t units[16] = {0x3e,0x41,0x89,0x37,0x4b,0xc6,0xa7,0xf0,0x38,0x6d,0xf3,0x7f,0x67,0x5e,0xf6,0x48};

  // header, int16
  write_record(fout,0x0,0x2,sizeof(uint16_t));
  write2(fout,5); // version 5

  // bgnlib, int16
  write_record(fout,0x1,0x2,24);
  fwrite(time,sizeof(uint16_t),6,fout); // modified
  fwrite(time,sizeof(uint16_t),6,fout); // accessed

  // libname, string
  int len=2*((strlen(libName)+1)/2);
  write_record(fout,0x2,0x6,len);
  fwrite(libName,1,len,fout);

  // units, double
  write_record(fout,0x3,0x5,16);
  fwrite(units,1,16,fout);

  // bgnstr, int16
  write_record(fout,0x5,0x2,24);
  fwrite(time,sizeof(uint16_t),6,fout); // modified
  fwrite(time,sizeof(uint16_t),6,fout); // accessed

  // strname, string
  len=2*((strlen(cellName)+1)/2);
  write_record(fout,0x6,0x6,len);
  fwrite(cellName,1,len,fout);
}

/** write rectangle  **/
void write_rect(FILE *fout, layer_map *lpp, int x0, int y0, int x1, int y1) {
  // boundary, no_data
  write_record(fout,0x8,0,0);

  // layer, int16
  write_record(fout,0xd,0x2,2);
  write2(fout,lpp->layer);

  // purpose, int16
  write_record(fout,0xe,0x2,2);
  write2(fout,lpp->purpose);

  // xy, int32
  write_record(fout,0x10,0x3,10*sizeof(uint32_t));
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

  // endel, no_data
  write_record(fout,0x11,0x0,0);
}

/** write label **/
void write_label(FILE *fout, char *net, layer_map *lpp, int x, int y) {
  uint8_t mag[8] = {0x3f,0x51,0xeb,0x85,0x1e,0xb8,0x51,0xec}; // 0.02
  write_record(fout,0xc,0x0,0); // text, no_data
  write_record(fout,0x10,0x3,8); // xy, int4
  write4(fout,x);
  write4(fout,y);
  write_record(fout,0xd,0x2,2);  // layer, int16
  write2(fout,lpp->layer);
  write_record(fout,0xe,0x2,2);  // purpose, int16
  write2(fout,lpp->purpose);
  write_record(fout,0x17,0x1,2); // presentation, bitfield
  write2(fout,0x5); // center
  write_record(fout,0x1b,0x5,8); // mag, double
  fwrite(mag,1,8,fout);
  int len=2*((strlen(net)+1)/2);
  write_record(fout,0x19,0x6,len); // string, string
  fwrite(net,1,len,fout);
  write_record(fout,0x11,0x0,0);  // endel, no_data
}

/** finish GDS file **/
void finish_gds(FILE *fout) {
  // endstr, no_data
  write_record(fout,0x7,0,0);

  // endlib, no_data
  write_record(fout,0x4,0,0);
}

/** user interface **/
int main(int argc, char **argv) {
  // parse command-line arguments
  FILE *fin,*fout;
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
    int x0,y0,x1,y1,invisible,r,x,y,w,l;
    if (sscanf(line,"Cell %s bbox=%d:%d:%d:%d",cell,&x0,&y0,&x1,&y1)==5) {
      start_gds(fout,"laygen",cell);
      layer_map *lpp=translate_layer("prb",0);
      write_rect(fout,lpp,x0,y0,x1,y1);
    }
    else if (sscanf(line,"Device %s nets=[ %s %s %s %s ] type=%c model=%s w=%d l=%d r=%d x=%d y=%d f=%c",
                    device,drn,gate,src,bulk,&type,model,&w,&l,&r,&x,&y,&f)==13) {
      // TODO: compute diffusion shape more elegantly
      x0 = (x+1)*540-440;
      x1 = (x+1)*540+440;
      y0 = r*3400 + 1700 + y*340*(type=='p' ? 1 : -1) + 340*(type=='p' ? 1 : 0);
      y1 = y0 + w*(type=='p' ? 1 : -1);
      layer_map *lpp=translate_layer(type=='p' ? "pdiff" : "ndiff",0);
      write_rect(fout,lpp,x0,y0,x1,y1);
    }
    else if (sscanf(line,"Wire net=%s layer=%s rect=%d:%d:%d:%d payload=%s invisible=%d",
                    net,layer,&x0,&y0,&x1,&y1,payload,&invisible)==7) {
      layer_map *lpp=translate_layer(layer,net[0]=='!');
      write_rect(fout,lpp,x0,y0,x1,y1);
      if (net[0]!='!') write_label(fout,net,lpp,(x0+x1)/2,(y0+y1)/2);
    }
  }

  // finish up
  finish_gds(fout);
  fclose(fin);
  fclose(fout);
}
