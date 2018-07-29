#include <stdio.h>
#include "../m3/genviews/src/build_c/mby_c/src/mby_top_map.h"
#include "../m3/genviews/src/build_c/mby_c/src/mby_top_map_main.h"
#include "../m3/model_server/src/model_c_write.h" // pull in write_field
#include "mby_parser.h"

// we implement the interface required of us by the model_server

void
mby_top_map_Setup     (const mby_top_map       *r,
                       const mby_top_map__addr *w)
{
  printf("Hello from the mby_top_map_Setup!\n");

  printf("field is %d\n", r->mpt[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);
  write_field(w->mpt[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR, 12);
  printf("field is %d\n", r->mpt[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);
}

void
mby_top_map_SendPacket(const mby_top_map       *r,
                       const mby_top_map__addr *w,
                       int                      port,
                       unsigned char           *packet,
                       unsigned int             length)
{

}
