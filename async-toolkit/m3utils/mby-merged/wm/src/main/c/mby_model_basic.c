#include <stdio.h>
#include "../m3/genviews/src/build_c/mby_c/src/mby_top_map.h"
#include "../m3/genviews/src/build_c/mby_c/src/mby_top_map_main.h"
#include "../m3/model_server/src/model_c_write.h" // pull in write_field
#include "mby_pipeline.h"

// we implement the interface required of us by the model_server

void
mby_top_map_Setup     (const mby_top_map       *r,
                       const mby_top_map__addr *w)
{
  printf("Hello from the mby_top_map_Setup!\n");

  printf("field is %d\n", r->mpp.mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);
  write_field(w->mpp.mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR, 12);
  printf("field is %d\n", r->mpp.mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);
}

void
mby_top_map_SendPacket(const mby_top_map       *r,
                       const mby_top_map__addr *w,
                       int                      port,
                       unsigned char           *packet,
                       unsigned int             length)
{
  // mbyParser(&(r->mpp.mgp[0].rx_ppe.parser), &mac2par, &par2map);
  // mbyMapper(&(r->mpp.mgp[0].rx_ppe.mapper), &par2map, &map2cla, &par2mod);

  mby_ppe_rx_top_map const * const rx_top_map = &(r->mpp.mgp[0].rx_ppe);
  mby_shm_map        const * const shm_map    = &(r->mpp.shm);

  // Input struct:
  mbyRxMacToParser mac2par;

  // TODO add persistent store for RX output and TX input:
  mbyRxStatsToRxOut rxs2rxo;

  // Populate input:
  mac2par.RX_DATA   = (fm_byte *) packet;
  mac2par.RX_LENGTH = (fm_uint32) length;
  mac2par.RX_PORT   = (fm_uint32) port;

  // Call RX pipeline:
  RxPipeline(rx_top_map, shm_map, &mac2par, &rxs2rxo);
}
