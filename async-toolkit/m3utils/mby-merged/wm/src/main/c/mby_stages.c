#include "mby_common.h"
#include "mby_stages.h"

STAGE_DEFINE(Parser)
{
  Parser(&(r->mpp[0].mgp[0].rx_ppe.parser), in, out);
}

STAGE_DEFINE(Mapper)
{
  Mapper(&(r->mpp[0].mgp[0].rx_ppe.mapper), in, out);
}

STAGE_DEFINE(Classifier)
{
  Classifier(&(r->mpp[0].mgp[0].rx_ppe.cgrp_a),
             &(r->mpp[0].mgp[0].rx_ppe.cgrp_b),
             &(r->mpp[0].shm),
             in, out);
}

STAGE_DEFINE(Modifier)
{
  varchar_builder_t txd_builder;

  varchar_builder_init(&txd_builder, tx_data, malloc, free);

  Modifier(&(r->mpp[0].mgp[0].tx_ppe.modify),
           &(r->mpp[0].shm),
           rx_data,
           in,
           out,
           &txd_builder);
}

// more to come ...

REGISTRAR_PROTO()
{
  STAGE_REGISTER(Parser);
}
