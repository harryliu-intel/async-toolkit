#ifndef _MBY_STAGES_H
#define _MBY_STAGES_H

#include "mby_common.h"
#include "model_stages.h" // mby_common.h must be included BEFORE this

// RX-PPE stages
#include "mby_parser.h"
#include "mby_mapper.h"
#include "mby_classifier.h"

// TX-PPE stages
#include "mby_modifier.h"

STAGE_PROTO(Parser);
STAGE_PROTO(Mapper);
STAGE_PROTO(Classifier);

STAGE_PROTO(Modifier);

REGISTRAR_PROTO();

#endif
