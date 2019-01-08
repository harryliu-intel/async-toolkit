#include "mby_common.h"
#include "mby_stages.h"

STAGE_PROTO(Parser)
{
  Parser(&(r->mpp[0].mgp[0].rx_ppe.parser), in, out);
}

STAGE_PROTO(Mapper)
{
  Mapper(&(r->mpp[0].mgp[0].rx_ppe.mapper), in, out);
}

STAGE_PROTO(Classifier)
{
  Classifier(&(r->mpp[0].mgp[0].rx_ppe.cgrp_a),
             &(r->mpp[0].mgp[0].rx_ppe.cgrp_b),
             &(r->mpp[0].shm),
             in, out);
}

// more to come ...
