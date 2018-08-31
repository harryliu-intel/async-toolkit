// This file is auto generated with tools/dump_gen/dump_gen.py
#ifndef _MBY_LOG_DUMP_H_
#define _MBY_LOG_DUMP_H_
#include "mby_pipeline.h"
void DUMP_mbyParserToMapper(mbyParserToMapper const * const s);
void DUMP_mbyMapperToClassifier(mbyMapperToClassifier const * const s);
void DUMP_mbyClassifierToHash(mbyClassifierToHash const * const s);
void DUMP_mbyHashToNextHop(mbyHashToNextHop const * const s);
void DUMP_mbyNextHopToMaskGen(mbyNextHopToMaskGen const * const s);
void DUMP_mbyMaskGenToTriggers(mbyMaskGenToTriggers const * const s);
void DUMP_mbyTriggersToCongMgmt(mbyTriggersToCongMgmt const * const s);
void DUMP_mbyCongMgmtToRxStats(mbyCongMgmtToRxStats const * const s);
void DUMP_mbyRxStatsToRxOut(mbyRxStatsToRxOut const * const s);
void DUMP_mbyTxInToModifier(mbyTxInToModifier const * const s);
void DUMP_mbyModifierToTxStats(mbyModifierToTxStats const * const s);
#endif /* _MBY_LOG_DUMP_H_ */
