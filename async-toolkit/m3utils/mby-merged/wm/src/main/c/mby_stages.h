#ifndef _MBY_STAGES_H
#define _MBY_STAGES_H

#include "model_stages.h"

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
