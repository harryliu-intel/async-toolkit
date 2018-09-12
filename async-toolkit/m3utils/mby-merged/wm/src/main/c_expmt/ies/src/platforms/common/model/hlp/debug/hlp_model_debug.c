/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_debug.c
 * Creation Date:   August 29, 2016
 * Description:     HLP white model packet state debug functions.
 *
 *                  DO NOT MODIFY THIS FILE - IT IS AUTOMATICALLY GENERATED
 *                  FROM hlp/hlp_debug.tt.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2017 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#include <stdarg.h>
#include <setjmp.h>

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

typedef struct {
  jmp_buf       abort_env;  /* longjmp hither to abort a concatenation */
  const char   *srcFile;
  const char   *srcFunction;
  fm_uint32     srcLine;
  hlp_model    *model;
  fm_text       buff;       /* concatenation buffer                  */
  fm_uint32     sz;         /* size of above                         */
  fm_uint32     p;          /* next position at which to write       */
} hlp_dbg_state_t;

static int frameNo = 0;

static fm_text get_buffer(hlp_dbg_state_t *state)
{
  return state->buff;
}

static hlp_dbg_state_t *reset_stream(hlp_dbg_state_t *state)
{
  state->buff[0] = '\0';
  state->p       = 0;
  return state;
}

static int setupDbgState(hlp_dbg_state_t *state,
                          const char *  srcFile,
                          const char *  srcFunction,
                          fm_uint32     srcLine,
                          hlp_model *model)
{
  const int initSz = 16;
  state->srcFile     = srcFile;
  state->srcFunction = srcFunction;
  state->srcLine     = srcLine;
  state->model       = model;
  state->buff        = fmAlloc(initSz);
  state->sz          = initSz;
  state->p           = 0;

  return !!(state->buff);
}

static void cleanupDbgState(hlp_dbg_state_t *state)
{
  fmFree(state->buff);
  state->buff = NULL; /* be paranoid */
}

static void dlog(hlp_dbg_state_t *state, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  /* NOTE TO ANYONE PORTING THIS CODE:

     Fulcrum API didn't have a varargs fmLogMessage as of API 3.x.

     I added this to the API myself.  It is the obvious extension
     of fmLogMessage.

     Later versions of Fulcrum API **may** have the requisite VLogMessage.
     If they do not, make one yourself and try to have it upstreamed.

     Mika 9/1/2016 
   */
  fmVLogMessage(0, FM_LOG_LEVEL_PRINT,
                state->srcFile, state->srcFunction, state->srcLine, fmt, ap);
  va_end(ap);
}

#define HLP_DBG_MAXBUFFSZ 1*1024*1024

static void abort_jmp(hlp_dbg_state_t *sp)
{
  /* jump out to the abort point set at every entry point to the library */
  longjmp(sp->abort_env, -1);
}

static void abort_jmp_msg(hlp_dbg_state_t *sp, const char *msg)
{
  FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
               "%s in %s at %s:%d\n",
               msg, sp->srcFunction, sp->srcFile, sp->srcLine);
  abort_jmp(sp);
}

static void grow_buff_or_abort(hlp_dbg_state_t *sp)
{
  /* grow the buffer inside sp or abort using abort_jmp(_msg) */
  void *newbuff;
  fm_uint32 newsz = sp->sz * 2;
  if (newsz > HLP_DBG_MAXBUFFSZ) 
    abort_jmp_msg(sp, "Debug printing buffer exceeded maximum");
      
  if(!(newbuff = fmAlloc(newsz))) 
    abort_jmp_msg(sp, "Debug printing out of memory");

  memcpy(newbuff, sp->buff, sp->sz);

  fmFree(sp->buff);

  sp->buff = (fm_text)newbuff;
  sp->sz = newsz;
}

static void hlp_dprintf(hlp_dbg_state_t *sp,
                        const char      *fmt,
                        ...)
{
  /* hlp_dprintf prints into the buffer inside sp.

     If the string doesnt fit, it attempts to increase the buffer size
     and tries again 
   */
  int asz /* size to attempt       */;
  int psz /* size actually printed */;
  va_list ap;
  va_start(ap,fmt);

  /* 
     NOTE TO ANYONE PORTING THIS CODE:

     vsnprintf is used below because the FM_SNPRINTF_S is BROKEN in API 3.x.

     Verified this with Derek Foster.

     If you port this, go ahead and use FM_VSNPRINTF_S.

     You will have to change the handling of psz if you do---return values
     of FM_VSNPRINTF_S are not the same as for vsnprintf, and you can 
     introduce a buffer overflow if you don't know what you're doing.

     Mika 9/1/16
   */

  do {
    va_list apc; 
    va_copy(apc,ap); /* copy ap into apc so we can reuse it */
    asz = sp->sz - sp->p;
    psz = vsnprintf(sp->buff + sp->p, asz, fmt, apc);
    va_end(apc);
    
    /* if the entire buffer was used, grow the buffer and try again */
    
    if (psz >= asz)
      grow_buff_or_abort(sp);
  } while (psz >= asz);

  sp->p += psz; /* advance index pointer to the EOS null */
  
  va_end(ap);
}

/**********************************************************************/

static void genDumpArray(hlp_dbg_state_t     *sp,
                         const void          *array,
                         fm_int               n,
                         ssize_t              z,
                         const char          *infix,
                         fm_int               width,
                         void               (*formatter)(hlp_dbg_state_t *,
                                                         const void *,
                                                         fm_int))
/* generic code to dump an array of <anything> in memory */
/* width is the number of entries to dump per line of output text */
/* formatter is called with: sp, 
                            the object to format, and 
                            the index in the array, to use if desired */
{
  int i;
  const void *q;
  if (array == NULL) return; /* this is undisciplined but follows old version */

  for(i=0, q=array; i<n; ++i, q=(const char *)q+z) {
    if (i%width == 0 && i != 0)
      hlp_dprintf(sp, "\n%32s", " ");
    formatter(sp, q, i);
    if (i != n-1)
      hlp_dprintf(sp, "%s", infix);
  }
}

/* INST_ARR_DUMPER instantiates dumping code using the genDumpArray function
   coded above.

   It takes a type name NM, the type of entry T, an infix separator (usually
   a comma), and the width in terms of entries per line.

   VA_ARGS contains the code used to format the output.  It is evaluated
   in an environment set up as follows
   -- sp an hlp_dbg_state_t initialized debug data structure (w/ output buffer)
   -- q a pointer to the object to print 
   -- i the index in the array being printed
*/
   
#define INST_ARR_DUMPER(NM, T, infix, width, ...)                         \
  static void format ## NM (hlp_dbg_state_t *sp, const void *x, fm_int i) \
  { FM_NOT_USED(i); T *q=(T *)x; __VA_ARGS__ }                                            \
  static void hlpModelDbgDumpArray ## NM (hlp_dbg_state_t *sp,            \
                                          T               *array,         \
                                          fm_int           n)             \
  { genDumpArray(sp, (void *) array, n, sizeof(T), infix, width, format ## NM); }

INST_ARR_DUMPER(32, fm_uint32, ", ", 4, hlp_dprintf(sp, "0x%08x", *q);)

INST_ARR_DUMPER(16, fm_uint16, ", ", 6, hlp_dprintf(sp, "0x%04x", *q);)

INST_ARR_DUMPER(8 , fm_byte  , ", ", 8, hlp_dprintf(sp, "0x%02x", *q);)

INST_ARR_DUMPER(Bool   , fm_bool  , ", ", 8,
                if ((*q!=0) && (*q!=1))
                  abort_jmp_msg(sp, "fm_bool contained illegal value");
                hlp_dprintf(sp, "0b%01x",*q);)

INST_ARR_DUMPER(Act1, hlp_modelPrecVal, ", ", 4,
                if ((q->val!=0) && (q->val!=1))
                  abort_jmp_msg(sp, "FfuActions contained illegal value");
                hlp_dprintf(sp, "0b%01x(Prec=%d)",q->val, q->prec);)

INST_ARR_DUMPER(Act4, hlp_modelPrecVal, ", ", 4,
                hlp_dprintf(sp, "0b%01x(Prec=%d)",q->val, q->prec);)

INST_ARR_DUMPER(Act24, hlp_modelPrecVal, ", ", 3,
                hlp_dprintf(sp, "0b%06x(Prec=%d)",q->val, q->prec);)

static void hlpModelDbgDumpFfuKeys(hlp_dbg_state_t *sp,
                                   hlp_modelFfuKeys *keys)
{
    if (keys == NULL) return;

    hlp_dprintf(sp,"KEY32:\n%32s", " ");
    hlpModelDbgDumpArray32(sp, keys->key32, HLP_MODEL_FFU_N_KEY32);

    hlp_dprintf(sp, "\n%32sKEYS16:\n%32s", " ", " ");
    hlpModelDbgDumpArray16(sp, keys->key16, HLP_MODEL_FFU_N_KEY16);

    hlp_dprintf(sp, "\n%32sKEYS8:\n%32s", " ", " ");
    hlpModelDbgDumpArray8 (sp, keys->key8 , HLP_MODEL_FFU_N_KEY8 );
    
}   /* end hlpModelDbgDumpFfuKeys */

INST_ARR_DUMPER(FfuKeys, hlp_modelFfuKeys, "", 9999,
                hlp_dprintf(sp,"\n%32sGRP%d_KEYS_OUT:\n%32s"," ",i," ");
                hlpModelDbgDumpFfuKeys(sp,q);)

static void hlpModelDbgDumpParserInfo(hlp_dbg_state_t *sp,
                                      hlp_modelParserInfo  *parserInfo)
{
  if (parserInfo == NULL)
    return;
  
  hlp_dprintf(sp,
          "otr_l2_len=%1d, otr_l2_vlan1=%1d, "
          "otr_l2_vlan2=%1d, otr_l2_v2first=%1d\n"
          "%31s otr_mpls_len=%1d, otr_l3_len=%1d, "
          "otr_l3_v6=%1d, otr_l4_udp=%1d, otr_l4_tcp=%1d\n"
          "%31s otr_tun_len=%1d, inr_l2_len=%1d, "
          "inr_l2_vlan1=%1d, inr_l2_vlan2=%1d, "
          "inr_l2_v2first=%1d\n"
          "%31s inr_mpls_len=%1d, inr_l3_len=%1d, "
          "inr_l3_v6=%1d, inr_l4_udp=%1d, inr_l4_tcp=%1d, "
          "window_parse_v=%1d",
          parserInfo->otr_l2_len,
          parserInfo->otr_l2_vlan1,
          parserInfo->otr_l2_vlan2,
          parserInfo->otr_l2_v2first,
          " ",
          parserInfo->otr_mpls_len,
          parserInfo->otr_l3_len,
          parserInfo->otr_l3_v6,
          parserInfo->otr_l4_udp,
          parserInfo->otr_l4_tcp,
          " ",
          parserInfo->otr_tun_len,
          parserInfo->inr_l2_len,
          parserInfo->inr_l2_vlan1,
          parserInfo->inr_l2_vlan2,
          parserInfo->inr_l2_v2first,
          " ",
          parserInfo->inr_mpls_len,
          parserInfo->inr_l3_len,
          parserInfo->inr_l3_v6,
          parserInfo->inr_l4_udp,
          parserInfo->inr_l4_tcp,
          parserInfo->window_parse_v);
}  /* end hlpModelDbgDumpParserInfo */

static void hlpModelDbgDumpFfuActions(hlp_dbg_state_t *sp,
                                     hlp_modelFfuActions *actions)
{
  if (actions == NULL) return;
    
  hlp_dprintf(sp, "ACT24:\n%32s", " ");
  hlpModelDbgDumpArrayAct24(sp, actions->act24, HLP_MODEL_FFU_N_ACT24);
  
  hlp_dprintf(sp, "\n%32sACT4:\n%32s", " ", " ");
  hlpModelDbgDumpArrayAct4(sp, actions->act4, HLP_MODEL_FFU_N_ACT4);
  
  hlp_dprintf(sp, "\n%32sACT1:\n%32s", " ", " ");
  hlpModelDbgDumpArrayAct1(sp, actions->act1, HLP_MODEL_FFU_N_ACT1);
}   /* end hlpModelDbgDumpFfuActions */

INST_ARR_DUMPER(FfuActions, hlp_modelFfuActions, "", 9999,
                hlp_dprintf(sp, "\n%32sGRP%d_ACTIONS_OUT:\n%32s"," ",i," ");
                hlpModelDbgDumpFfuActions(sp, q);)

INST_ARR_DUMPER(FghashActions, hlp_modelFghashActions, "", 9999,
                hlp_dprintf(sp, "\n%32sFGHASH%d_ACTIONS_OUT:\n%32s"," ",i," ");
                hlpModelDbgDumpArray32(sp, q->action, 4);)

static void hlpModelDbgDumpFfuMuxedAction(hlp_dbg_state_t *sp,
                                     hlp_modelFfuMuxedAction *muxedAction)
{
  if (muxedAction == NULL) return;

  hlp_dprintf(sp,"ECN=%1d AQM_MARK_EN=%1d SWPRI=0x%1x TTL01=0x%1x DSCP=0x%02x VPRI=0x%1x",
          muxedAction->ecn,
          muxedAction->aqm_mark_en,
          muxedAction->swpri,
          muxedAction->ttl01,
          muxedAction->dscp,
          muxedAction->vpri);
}   /* end hlpModelDbgDumpFfuMuxedAction */

static void hlpModelDbgDumpFfuHits(hlp_dbg_state_t *sp,
                                   fm_bool *hits,
                                   fm_int n)
{
  /* this code looks a lot like array printing, but it's not quite,
     because it skips empty entries, so we code it by hand */
  fm_int  i, k;

  if (hits == NULL) return;
  
  /* The printf(3) format used to print each array entry index is 0x%04x f
   * value is 1. Each entry is separated by a comma followed by a
   * whitespace character and the resulting string is NUL-terminated. */
  
  for (k = 0, i = 0; k < n; ++k) {       
    if(hits[k])
      {
        if (i%8 == 0 && i != 0)
          hlp_dprintf(sp, "\n%32s0x%03x", " ", k);
        else
          hlp_dprintf(sp, "0x%03x", k);
        
        if (i < n) /* this condition doesnt seem right to me, its always true */
          hlp_dprintf(sp, "%s", " ");
        ++i;
      }      
  }
}   /* end hlpModelDbgDumpFfuHits */

static void hlpModelDbgDumpFfuFlags(hlp_dbg_state_t *sp,
                                    hlp_modelFfuFlags *flags)
{
  if (flags == NULL) return;

  hlp_dprintf(sp,
              "drop=%1d trap=%1d "
              "log=%1d no_route=%1d "
              "rx_mirror=%1d capture_time=%1d "
              "tx_tag=0x%x",
              flags->drop,
              flags->trap,
              flags->log,
              flags->no_route,
              flags->rx_mirror,
              flags->capture_time,
              flags->tx_tag);
}   /* end hlpModelDbgDumpFfuFlags */

static void hlpModelDbgDumpRxTag(hlp_dbg_state_t *sp,
                                 hlp_modelRxTag *rxTag)
{
  if (rxTag == NULL) return;
  hlp_dprintf(sp, 
          "custom=%1d mpls=%1d "
          "ipv6=%1d ipv4=%1d "
          "v2first=0x%1d vlan2=%1d "
          "vlan1=%1d",
          rxTag->custom,
          rxTag->mpls,
          rxTag->ipv6,
          rxTag->ipv4,
          rxTag->v2first,
          rxTag->vlan2,
          rxTag->vlan1);
}   /* end hlpModelDbgDumpRxTag */

static void hlpModelDbgDumpHashKeys(hlp_dbg_state_t *sp,
                                    hlp_modelHashKeys *keys)
{
  if (keys == NULL) return;
  hlp_dprintf(sp,
              "crc34=0x%llx, crc234=0x%llx,\n%32sl234Key=",
          keys->crc34,
          keys->crc234,
          " ");
  hlpModelDbgDumpArray8(sp, keys->l234Key, sizeof(keys->l234Key));
  hlp_dprintf(sp,
          " \n%32szeroL2=%1d zeroL34=%1d useL34=%1d "
          "rotA=%1d rotB=%1d \n%32sarpEcmpCfg=%1d",
          " ",
          keys->zeroL2,
          keys->zeroL34,
          keys->useL34,
          keys->rotA,
          keys->rotB,
          " ",
          keys->arpEcmpCfg);
}   /* end hlpModelDbgDumpHashKeys */

static void hlpModelDbgDumpMaEntry(hlp_dbg_state_t *sp,
                                   hlpMaTable *maEntry)
{
  if (maEntry == NULL) return;
  hlp_dprintf(sp,
          "MACAddress=" FM_FORMAT_ADDR " VID=%-4d "
          "L2Domain=0x%03x SGlort=0x%04x "
          "DGlort=0x%04x TrigID=%-2d "
          "EntryType=%1d, OldPort=%1d, NewPort=%1d",
          maEntry->MAC_ADDRESS,
          maEntry->VID,
          maEntry->L2_DOMAIN,
          maEntry->S_GLORT,
          maEntry->D_GLORT,
          maEntry->TRIG_ID,
          maEntry->ENTRY_TYPE,
          maEntry->OLD_PORT,
          maEntry->NEW_PORT);
}   /* end hlpModelDbgDumpMaEntry */

static void
hlpModelDbgDumpTriggerResults(hlp_dbg_state_t *sp,
                              hlp_modelTriggerResults *results)
{
  if (results == NULL) return;
  hlp_dprintf(sp,
          "forwardingAction=%1d trapAction=%1d "
          "mirroringAction0=%1d mirroringAction1=%1d TCAction=%1d\n"
          "%31s vlanAction=%1d learningAction=%1d "
          "rateLimitAction=%1d action=%-2d\n"
          "%31s destGlort=0x%04x destMask=0x%012llx "
          "filterDestMask=%1d\n"
          "%31s cpuCode=0x%02x logAction=%1d "
          "rxMirror=%1d \n"
          "%31s mirror0ProfileV=%1d mirror1ProfileV=%1d\n"
          "%31s mirror0ProfileIdx=%-3d mirror1ProfileIdx=%-3d\n"
          "%31s TC=%-2d vlan=%-4d rateLimitNum=%-2d\n"
          "%31s egressL3DomainAction=%1d egressL2DomainAction=%1d\n"
          "%31s MetadataTrigNum[0]=%-2d MetadataAction[0]=%1d "
          "MetadataTrigNum[1]=%-2d MetadataAction[1]=%1d\n"
          "%31s MetadataTrigNum[2]=%-2d MetadataAction[2]=%1d "
          "MetadataTrigNum[3]=%-2d MetadataAction[3]=%1d\n",
          results->forwardingAction,
          results->trapAction,
          results->mirroringAction0,
          results->mirroringAction1,
          results->TCAction,
          " ",
          results->vlanAction,
          results->learningAction,
          results->rateLimitAction,
          results->action,
          " ",
          results->destGlort,
          results->destMask,
          results->filterDestMask,
          " ",
          results->cpuCode,
          results->logAction,
          results->rxMirror,
          " ",
          results->mirror0ProfileV,
          results->mirror1ProfileV,
          " ",
          results->mirror0ProfileIdx,
          results->mirror1ProfileIdx,
          " ",
          results->TC,
          results->vlan,
          results->rateLimitNum,
          " ",
          results->egressL3DomainAction,
          results->egressL2DomainAction,
          " ",
          results->metadataTrigNum[0],
          results->metadataAction[0],
          results->metadataTrigNum[1],
          results->metadataAction[1],
          " ",
          results->metadataTrigNum[2],
          results->metadataAction[2],
          results->metadataTrigNum[3],
          results->metadataAction[3]);
}   /* end hlpModelDbgDumpTriggerActions */

/**********************************************************************/
/* machinery for declaring fields in state data structure */
#define NM_FORMAT "%-30s: " /* indentation format */

/* declare a simple (numerical) field */
#define DCL_SMPL_ST( NM, FMT ) /* declare simple field */               \
  static void DLOG_ ## NM (hlp_dbg_state_t *dsp,                        \
                           const hlp_modelState *state)                 \
  {                                                                     \
    dlog(dsp, NM_FORMAT FMT "\n", #NM, state->NM);                      \
  }

/* declare really an array field */
#define DCL_CPLX_ST( NM, FMTC,  ... ) /* declare complex field */       \
  static void DLOG_ ## NM (hlp_dbg_state_t *dsp,                        \
                           hlp_modelState *state)                       \
  {                                                                     \
    FMTC ( reset_stream(dsp), state-> NM, __VA_ARGS__ ); \
    dlog (dsp, NM_FORMAT "%s\n", #NM , get_buffer(dsp));                \
  }

/* declare field that is a pointer to some object */
#define DCL_CPLX_STR( NM, FMTC ) /* declare complex field in array */   \
  static void DLOG_ ## NM (hlp_dbg_state_t *dsp,                        \
                           hlp_modelState *state)                       \
  {                                                                     \
    FMTC ( reset_stream(dsp), &state-> NM);          \
    dlog (dsp, NM_FORMAT "%s\n", #NM , get_buffer(dsp));                \
  }

/* actual declarations generated by perl */
DCL_SMPL_ST(RX_LENGTH, "%u")
DCL_SMPL_ST(RX_PORT, "%d")
DCL_CPLX_ST (PKT_META, hlpModelDbgDumpArray8, 32)
DCL_CPLX_ST (TX_PKT_META, hlpModelDbgDumpArray8, 32)
DCL_SMPL_ST(RX_EOP, "%1d")
DCL_SMPL_ST(RX_SOP, "%1d")
DCL_SMPL_ST(RX_FLAGS, "0x%01x")
DCL_SMPL_ST(SEG_META_ERR, "%1d")
DCL_CPLX_ST (PARSER_PKT_META, hlpModelDbgDumpArray8, 32)
DCL_SMPL_ST(PA_ADJ_SEG_LEN, "%03d")
DCL_CPLX_ST (PA_KEYS, hlpModelDbgDumpArray16, 84)
DCL_CPLX_ST (PA_KEYS_VALID, hlpModelDbgDumpArrayBool, 84)
DCL_CPLX_ST (PA_FLAGS, hlpModelDbgDumpArrayBool, 48)
DCL_CPLX_ST (PA_PTRS, hlpModelDbgDumpArray8, 8)
DCL_CPLX_ST (PA_PTRS_VALID, hlpModelDbgDumpArrayBool, 8)
DCL_SMPL_ST(PA_CSUM_OK, "0x%01x")
DCL_SMPL_ST(PA_EX_STAGE, "0x%01x")
DCL_SMPL_ST(PA_EX_DEPTH_EXCEED, "%1d")
DCL_SMPL_ST(PA_EX_TRUNC_HEADER, "%1d")
DCL_SMPL_ST(PA_EX_PARSING_DONE, "%1d")
DCL_SMPL_ST(PA_FORCE_SAF, "%1d")
DCL_SMPL_ST(PA_DROP, "%1d")
DCL_SMPL_ST(PA_CSUM_ERR, "%1d")
DCL_SMPL_ST(PA_L3LEN_ERR, "%1d")
DCL_SMPL_ST(TAIL_CSUM_LEN, "0x%09"FM_FORMAT_64"x")
DCL_CPLX_STR(PARSER_INFO, hlpModelDbgDumpParserInfo)
DCL_CPLX_STR(FFU_KEYS, hlpModelDbgDumpFfuKeys)
DCL_CPLX_STR(FFU_ACTIONS, hlpModelDbgDumpFfuActions)
DCL_SMPL_ST(FFU_SCENARIO, "0x%02x")
DCL_SMPL_ST(FFU_VRID, "0x%02x")
DCL_CPLX_ST (IP_OPTION, hlpModelDbgDumpArrayBool, 2)
DCL_SMPL_ST(PRIORITY_PROFILE, "0x%02x")
DCL_SMPL_ST(NO_PRI_ENC, "%1d")
DCL_CPLX_ST (FFU_GRP_KEYS, hlpModelDbgDumpArrayFfuKeys, HLP_FFU_TCAM_ENTRIES_2-1)
DCL_CPLX_ST (FFU_GRP_ACTIONS, hlpModelDbgDumpArrayFfuActions, HLP_FFU_TCAM_ENTRIES_2-1)
DCL_CPLX_ST (FGHASH_ACTIONS, hlpModelDbgDumpArrayFghashActions, HLP_FFU_HASH_CFG_ENTRIES_1)
DCL_CPLX_ST (FFU_GRP_SCENARIO, hlpModelDbgDumpArray8, HLP_FFU_TCAM_ENTRIES_2-1)
DCL_CPLX_STR(FFU_MUXED_ACTION, hlpModelDbgDumpFfuMuxedAction)
DCL_SMPL_ST(ECMP_HASH, "0x%06x")
DCL_SMPL_ST(HQM_HASH, "0x%04x")
DCL_SMPL_ST(HQM_HASH_V, "%1d")
DCL_SMPL_ST(MOD_META, "0x%012"FM_FORMAT_64"x")
DCL_SMPL_ST(L34_HASH, "0x%09"FM_FORMAT_64"x")
DCL_SMPL_ST(L2_EDOMAIN, "0x%04x")
DCL_SMPL_ST(L3_EDOMAIN, "0x%02x")
DCL_SMPL_ST(LEARN_MODE, "%1d")
DCL_SMPL_ST(MOD_IDX, "0x%05x")
DCL_SMPL_ST(L2_ETYPE, "0x%04x")
DCL_SMPL_ST(QOS_SWPRI, "%-2d")
DCL_SMPL_ST(SGLORT, "0x%04x")
DCL_SMPL_ST(IDGLORT, "0x%04x")
DCL_SMPL_ST(L2_DMAC, FM_FORMAT_ADDR)
DCL_SMPL_ST(L2_SMAC, FM_FORMAT_ADDR)
DCL_SMPL_ST(DMAC_FROM_IPV6, FM_FORMAT_ADDR)
DCL_SMPL_ST(IS_IPV4, "%1d")
DCL_SMPL_ST(IS_IPV6, "%1d")
DCL_SMPL_ST(TRAP_IP_OPTIONS, "0x%1d")
DCL_SMPL_ST(L3_LENGTH, "%-5d")
DCL_SMPL_ST(OUTER_L3_LENGTH, "%-5d")
DCL_SMPL_ST(INNER_L3_LENGTH, "%-5d")
DCL_SMPL_ST(DROP_TTL, "%1d")
DCL_SMPL_ST(TRAP_ICMP, "%1d")
DCL_SMPL_ST(TRAP_IGMP, "%1d")
DCL_SMPL_ST(TTL_CTRL, "%1d")
DCL_SMPL_ST(PARSER_ERROR, "%1d")
DCL_CPLX_STR(FFU_FLAGS, hlpModelDbgDumpFfuFlags)
DCL_SMPL_ST(FFU_ROUTE, "0x%08x")
DCL_SMPL_ST(NO_LEARN, "%1d")
DCL_SMPL_ST(L2_IVID1, "0x%03x")
DCL_SMPL_ST(TX_TAG, "0x%01x")
DCL_SMPL_ST(QOS_L2_VPRI1, "0x%01x")
DCL_SMPL_ST(FFU_TRIG, "%-2d")
DCL_SMPL_ST(QOS_L3_DSCP, "0x%02x")
DCL_SMPL_ST(FLOOD_GLORT, "0x%04x")
DCL_CPLX_ST (POLICER_ACTION, hlpModelDbgDumpArray32, 4)
DCL_SMPL_ST(PARITY_ERROR, "%1d")
DCL_SMPL_ST(ECN, "%1d")
DCL_SMPL_ST(AQM_MARK_EN, "%1d")
DCL_SMPL_ST(VLAN_COUNTER, "%04d")
DCL_CPLX_STR(HASH_KEYS, hlpModelDbgDumpHashKeys)
DCL_CPLX_ST (ARP_HASH, hlpModelDbgDumpArray8, 16)
DCL_SMPL_ST(RAW_HASH, "0x%08x")
DCL_SMPL_ST(HASH_ROT_A, "0x%08x")
DCL_SMPL_ST(HASH_ROT_B, "0x%08x")
DCL_SMPL_ST(HASH_ROT_A_PTABLE_INDEX, "0x%04x")
DCL_SMPL_ST(HASH_ROT_B_PTABLE_INDEX, "0x%04x")
DCL_SMPL_ST(L2_EVID1, "0x%03x")
DCL_SMPL_ST(MARK_ROUTED, "%1d")
DCL_SMPL_ST(ARP_TABLE_INDEX, "0x%04x")
DCL_SMPL_ST(MTU_INDEX, "0x%01x")
DCL_SMPL_ST(FLOOD_SET, "%1d")
DCL_SMPL_ST(L2_IDOMAIN, "0x%04x")
DCL_SMPL_ST(L3_IDOMAIN, "0x%02x")
DCL_SMPL_ST(L2_IVLAN1_REFLECT, "%1d")
DCL_SMPL_ST(L2_IVLAN1_CNT_INDEX, "0x%04x")
DCL_SMPL_ST(L2_IVLAN1_MEMBERSHIP, "%1d")
DCL_SMPL_ST(L2_EVLAN1_MEMBERSHIP, "0x%04x")
DCL_SMPL_ST(L2_EVLAN1_TRIG, "%-2d")
DCL_SMPL_ST(MTU_VIOLATION, "%1d")
DCL_SMPL_ST(DA_INDEX, "0x%04x")
DCL_SMPL_ST(DA_SET, "0x%01x")
DCL_SMPL_ST(DA_HIT, "%1d")
DCL_CPLX_STR(DA_RESULT, hlpModelDbgDumpMaEntry)
DCL_SMPL_ST(SA_INDEX, "0x%04x")
DCL_SMPL_ST(SA_SET, "0x%01x")
DCL_SMPL_ST(FREE_INDEX, "0x%x")
DCL_SMPL_ST(FREE_SET, "%1d")
DCL_SMPL_ST(SA_HIT, "%1d")
DCL_CPLX_STR(SA_RESULT, hlpModelDbgDumpMaEntry)
DCL_SMPL_ST(L2_IFID1_STATE, "%1d")
DCL_SMPL_ST(L2_EFID1_STATE, "0x%06x")
DCL_SMPL_ST(GLORT_FORWARDED, "%1d")
DCL_SMPL_ST(FLOOD_FORWARDED, "%1d")
DCL_SMPL_ST(PORT_FIELD_SIZE, "0x%01x")
DCL_SMPL_ST(CSGLORT, "0x%04x")
DCL_SMPL_ST(MOVED_PROV_ENTRY_EXISTED, "%1d")
DCL_SMPL_ST(SECURED_PORT, "%1d")
DCL_SMPL_ST(PROV_INDEX, "0x%x")
DCL_SMPL_ST(PROV_SET, "%1d")
DCL_SMPL_ST(SV_DROP, "%1d")
DCL_SMPL_ST(DROP_PROVISIONAL, "%1d")
DCL_SMPL_ST(DA_LOOKUP_PERFORMED, "%1d")
DCL_SMPL_ST(GLORT_CAM_MISS, "%1d")
DCL_SMPL_ST(AMASK, "0x%012"FM_FORMAT_64"x")
DCL_SMPL_ST(STRICT_GLORT_ROUTING, "%1d")
DCL_SMPL_ST(TARGETED_DETERMINISTIC, "%1d")
DCL_SMPL_ST(GLORT_DMASK, "0x%06x")
DCL_SMPL_ST(IP_MCAST_IDX, "%-5d")
DCL_SMPL_ST(LOG_AMASK, "0x%01x")
DCL_SMPL_ST(DMASK, "0x%06x")
DCL_SMPL_ST(RX_MIRROR, "%1d")
DCL_SMPL_ST(QCN_MIRROR0_PROFILE_V, "%1d")
DCL_SMPL_ST(QCN_MIRROR1_PROFILE_V, "%1d")
DCL_SMPL_ST(MIRROR1_PROFILE_V, "%1d")
DCL_SMPL_ST(MIRROR1_PROFILE_IDX, "%-2d")
DCL_SMPL_ST(PRE_RESOLVE_ACTION, "%-2d")
DCL_SMPL_ST(PRE_RESOLVE_DMASK, "0x%06x")
DCL_SMPL_ST(PRE_RESOLVE_DGLORT, "0x%04x")
DCL_SMPL_ST(ACTION, "%-2d")
DCL_SMPL_ST(CPU_CODE, "0x%01x")
DCL_SMPL_ST(FNMASK, "0x%06x")
DCL_SMPL_ST(MAC_MOVED, "0x%1d")
DCL_SMPL_ST(LEARNING_ENABLED, "0x%1d")
DCL_SMPL_ST(MCAST_EPOCH, "0x%1d")
DCL_SMPL_ST(FCLASS, "0x%-2d")
DCL_SMPL_ST(LOGGING_HIT, "0x%1d")
DCL_SMPL_ST(MIRROR0_PORT, "%-2d")
DCL_SMPL_ST(MIRROR1_PORT, "%-2d")
DCL_SMPL_ST(CPU_TRAP, "0x%1d")
DCL_SMPL_ST(POLICER_DROP, "0x%1d")
DCL_SMPL_ST(OPERATOR_ID, "0x%01x")
DCL_SMPL_ST(STORE_TRAP_ACTION, "0x%1d")
DCL_SMPL_ST(LOG_MIRROR_PROFILE_IDX, "%-2d")
DCL_SMPL_ST(XCAST, "0x%02x")
DCL_SMPL_ST(TRIG_HIT_MASK_RESOLVED_HI, "0x%016"FM_FORMAT_64"x")
DCL_SMPL_ST(TRIG_HIT_MASK_RESOLVED_LO, "0x%016"FM_FORMAT_64"x")
DCL_SMPL_ST(MIRROR0_PROFILE_V, "%1d")
DCL_SMPL_ST(MIRROR0_PROFILE_IDX, "%-2d")
DCL_CPLX_STR(TRIGGERS, hlpModelDbgDumpTriggerResults)
DCL_SMPL_ST(NO_MODIFY, "%1d")
DCL_SMPL_ST(L2_S_KEY_LEARN, "%1d")
DCL_SMPL_ST(L2_S_MA_TABLE_OVERWRITE, "%1d")
DCL_SMPL_ST(L2_S_MA_TABLE_INDEX, "%04d")
DCL_SMPL_ST(TC, "%-2d")
DCL_SMPL_ST(SMP_MEMBERSHIP, "%1d")
DCL_SMPL_ST(PAUSE_TC_XOFF, "%1d")
DCL_SMPL_ST(SAF_ERROR, "%1d")
DCL_SMPL_ST(PM_ERR, "%1d")
DCL_SMPL_ST(FSCHED_INIT, "%01d")
DCL_SMPL_ST(FSCHED_L2_PORTNUM, "%02d")
DCL_SMPL_ST(FSCHED_LEN_TABLE_OFFSET, "%04d")
DCL_SMPL_ST(TX_PORT, "%-2d")
DCL_SMPL_ST(EDGLORT, "0x%04x")
DCL_SMPL_ST(MOD_IP_MCAST_IDX, "0x%04x")
DCL_SMPL_ST(MIRROR0_SENT, "%01d")
DCL_SMPL_ST(MIRROR1_SENT, "%01d")
DCL_SMPL_ST(MIRTYP, "%1d")
DCL_SMPL_ST(IS_TIMEOUT, "%1d")
DCL_SMPL_ST(DCTCP_MARK, "%1d")
DCL_SMPL_ST(LFSR, "0x%2x")
DCL_SMPL_ST(TX_LENGTH, "%-5d")
DCL_SMPL_ST(TX_STATS_LENGTH, "%-5d")
DCL_SMPL_ST(TX_STATS_LAST_LEN, "%-5d")
DCL_SMPL_ST(TX_REASONCODE, "0x%0x")
DCL_SMPL_ST(TX_DISP, "%1d")
DCL_SMPL_ST(TX_DROP, "%1d")
DCL_SMPL_ST(SEG_DROP, "%1d")
DCL_SMPL_ST(MPLS_POP, "0x%02x")
DCL_SMPL_ST(ENCAP, "%1d")
DCL_SMPL_ST(DECAP, "%1d")
DCL_SMPL_ST(SKIP_DGLORT_DEC, "%1d")
DCL_SMPL_ST(TS_1588_CAPTURE, "%1d")
DCL_SMPL_ST(PTOT, "0x%02x")
DCL_SMPL_ST(L2_COUNT, "0x%02x")
DCL_SMPL_ST(DRR_PHASE, "%1d")
DCL_SMPL_ST(TX_FREE, "%1d")
DCL_SMPL_ST(OOM, "%1d")
DCL_SMPL_ST(PM_ERR_NONSOP, "%1d")
DCL_SMPL_ST(ADDR, "0x%04x")
DCL_SMPL_ST(TX_PHYS_PORT, "%02d")
DCL_SMPL_ST(CALLED_FRM_UNIT, "%1d")
DCL_SMPL_ST(DISABLE_DBG_DUMP, "%1d")


/* utility macro EMIT requires specific naming within your code */
#define EMIT( FLD ) \
  DLOG_ ## FLD ( dsp, state );

#define DO_PROLOG(DSP)                                                  \
    hlp_modelState *state = &(DSP)->model->packetState;                 

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

void do_hlpModelDbgDump(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( RX_LENGTH );
   EMIT( RX_PORT );
   EMIT( PKT_META );
   EMIT( TX_PKT_META );
   EMIT( RX_EOP );
   EMIT( RX_SOP );
   EMIT( RX_FLAGS );
   EMIT( SEG_META_ERR );
   EMIT( PARSER_PKT_META );
   EMIT( PA_ADJ_SEG_LEN );
   EMIT( PA_KEYS );
   EMIT( PA_KEYS_VALID );
   EMIT( PA_FLAGS );
   EMIT( PA_PTRS );
   EMIT( PA_PTRS_VALID );
   EMIT( PA_CSUM_OK );
   EMIT( PA_EX_STAGE );
   EMIT( PA_EX_DEPTH_EXCEED );
   EMIT( PA_EX_TRUNC_HEADER );
   EMIT( PA_EX_PARSING_DONE );
   EMIT( PA_FORCE_SAF );
   EMIT( PA_DROP );
   EMIT( PA_CSUM_ERR );
   EMIT( PA_L3LEN_ERR );
   EMIT( TAIL_CSUM_LEN );
   EMIT( PARSER_INFO );
   EMIT( FFU_KEYS );
   EMIT( FFU_ACTIONS );
   EMIT( FFU_SCENARIO );
   EMIT( FFU_VRID );
   EMIT( IP_OPTION );
   EMIT( PRIORITY_PROFILE );
   EMIT( NO_PRI_ENC );
   EMIT( FFU_GRP_KEYS );
   EMIT( FFU_GRP_ACTIONS );
   EMIT( FGHASH_ACTIONS );
   EMIT( FFU_GRP_SCENARIO );
   EMIT( FFU_MUXED_ACTION );
   EMIT( ECMP_HASH );
   EMIT( HQM_HASH );
   EMIT( HQM_HASH_V );
   EMIT( MOD_META );
   EMIT( L34_HASH );
   EMIT( L2_EDOMAIN );
   EMIT( L3_EDOMAIN );
   EMIT( LEARN_MODE );
   EMIT( MOD_IDX );
   EMIT( L2_ETYPE );
   EMIT( QOS_SWPRI );
   EMIT( SGLORT );
   EMIT( IDGLORT );
   EMIT( L2_DMAC );
   EMIT( L2_SMAC );
   EMIT( DMAC_FROM_IPV6 );
   EMIT( IS_IPV4 );
   EMIT( IS_IPV6 );
   EMIT( TRAP_IP_OPTIONS );
   EMIT( L3_LENGTH );
   EMIT( OUTER_L3_LENGTH );
   EMIT( INNER_L3_LENGTH );
   EMIT( DROP_TTL );
   EMIT( TRAP_ICMP );
   EMIT( TRAP_IGMP );
   EMIT( TTL_CTRL );
   EMIT( PARSER_ERROR );
   EMIT( FFU_FLAGS );
   EMIT( FFU_ROUTE );
   EMIT( NO_LEARN );
   EMIT( L2_IVID1 );
   EMIT( TX_TAG );
   EMIT( QOS_L2_VPRI1 );
   EMIT( FFU_TRIG );
   EMIT( QOS_L3_DSCP );
   EMIT( FLOOD_GLORT );
   EMIT( POLICER_ACTION );
   EMIT( PARITY_ERROR );
   EMIT( ECN );
   EMIT( AQM_MARK_EN );
   EMIT( VLAN_COUNTER );
   EMIT( HASH_KEYS );
   EMIT( ARP_HASH );
   EMIT( RAW_HASH );
   EMIT( HASH_ROT_A );
   EMIT( HASH_ROT_B );
   EMIT( HASH_ROT_A_PTABLE_INDEX );
   EMIT( HASH_ROT_B_PTABLE_INDEX );
   EMIT( L2_EVID1 );
   EMIT( MARK_ROUTED );
   EMIT( ARP_TABLE_INDEX );
   EMIT( MTU_INDEX );
   EMIT( FLOOD_SET );
   EMIT( L2_IDOMAIN );
   EMIT( L3_IDOMAIN );
   EMIT( L2_IVLAN1_REFLECT );
   EMIT( L2_IVLAN1_CNT_INDEX );
   EMIT( L2_IVLAN1_MEMBERSHIP );
   EMIT( L2_EVLAN1_MEMBERSHIP );
   EMIT( L2_EVLAN1_TRIG );
   EMIT( MTU_VIOLATION );
   EMIT( DA_INDEX );
   EMIT( DA_SET );
   EMIT( DA_HIT );
   EMIT( DA_RESULT );
   EMIT( SA_INDEX );
   EMIT( SA_SET );
   EMIT( FREE_INDEX );
   EMIT( FREE_SET );
   EMIT( SA_HIT );
   EMIT( SA_RESULT );
   EMIT( L2_IFID1_STATE );
   EMIT( L2_EFID1_STATE );
   EMIT( GLORT_FORWARDED );
   EMIT( FLOOD_FORWARDED );
   EMIT( PORT_FIELD_SIZE );
   EMIT( CSGLORT );
   EMIT( MOVED_PROV_ENTRY_EXISTED );
   EMIT( SECURED_PORT );
   EMIT( PROV_INDEX );
   EMIT( PROV_SET );
   EMIT( SV_DROP );
   EMIT( DROP_PROVISIONAL );
   EMIT( DA_LOOKUP_PERFORMED );
   EMIT( GLORT_CAM_MISS );
   EMIT( AMASK );
   EMIT( STRICT_GLORT_ROUTING );
   EMIT( TARGETED_DETERMINISTIC );
   EMIT( GLORT_DMASK );
   EMIT( IP_MCAST_IDX );
   EMIT( LOG_AMASK );
   EMIT( DMASK );
   EMIT( RX_MIRROR );
   EMIT( QCN_MIRROR0_PROFILE_V );
   EMIT( QCN_MIRROR1_PROFILE_V );
   EMIT( MIRROR1_PROFILE_V );
   EMIT( MIRROR1_PROFILE_IDX );
   EMIT( PRE_RESOLVE_ACTION );
   EMIT( PRE_RESOLVE_DMASK );
   EMIT( PRE_RESOLVE_DGLORT );
   EMIT( ACTION );
   EMIT( CPU_CODE );
   EMIT( FNMASK );
   EMIT( MAC_MOVED );
   EMIT( LEARNING_ENABLED );
   EMIT( MCAST_EPOCH );
   EMIT( FCLASS );
   EMIT( LOGGING_HIT );
   EMIT( MIRROR0_PORT );
   EMIT( MIRROR1_PORT );
   EMIT( CPU_TRAP );
   EMIT( POLICER_DROP );
   EMIT( OPERATOR_ID );
   EMIT( STORE_TRAP_ACTION );
   EMIT( LOG_MIRROR_PROFILE_IDX );
   EMIT( XCAST );
   EMIT( TRIG_HIT_MASK_RESOLVED_HI );
   EMIT( TRIG_HIT_MASK_RESOLVED_LO );
   EMIT( MIRROR0_PROFILE_V );
   EMIT( MIRROR0_PROFILE_IDX );
   EMIT( TRIGGERS );
   EMIT( NO_MODIFY );
   EMIT( L2_S_KEY_LEARN );
   EMIT( L2_S_MA_TABLE_OVERWRITE );
   EMIT( L2_S_MA_TABLE_INDEX );
   EMIT( TC );
   EMIT( SMP_MEMBERSHIP );
   EMIT( PAUSE_TC_XOFF );
   EMIT( SAF_ERROR );
   EMIT( PM_ERR );
   EMIT( FSCHED_INIT );
   EMIT( FSCHED_L2_PORTNUM );
   EMIT( FSCHED_LEN_TABLE_OFFSET );
   EMIT( TX_PORT );
   EMIT( EDGLORT );
   EMIT( MOD_IP_MCAST_IDX );
   EMIT( MIRROR0_SENT );
   EMIT( MIRROR1_SENT );
   EMIT( MIRTYP );
   EMIT( IS_TIMEOUT );
   EMIT( DCTCP_MARK );
   EMIT( LFSR );
   EMIT( TX_LENGTH );
   EMIT( TX_STATS_LENGTH );
   EMIT( TX_STATS_LAST_LEN );
   EMIT( TX_REASONCODE );
   EMIT( TX_DISP );
   EMIT( TX_DROP );
   EMIT( SEG_DROP );
   EMIT( MPLS_POP );
   EMIT( ENCAP );
   EMIT( DECAP );
   EMIT( SKIP_DGLORT_DEC );
   EMIT( TS_1588_CAPTURE );
   EMIT( PTOT );
   EMIT( L2_COUNT );
   EMIT( DRR_PHASE );
   EMIT( TX_FREE );
   EMIT( OOM );
   EMIT( PM_ERR_NONSOP );
   EMIT( ADDR );
   EMIT( TX_PHYS_PORT );
   EMIT( CALLED_FRM_UNIT );
   EMIT( DISABLE_DBG_DUMP );

}   /* end hlpModelDbgDump */

void do_hlpModelDbgDumpMacRx(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( RX_LENGTH );
   EMIT( RX_FLAGS );
   EMIT( SEG_META_ERR );

}   /* end hlpModelDbgDumpMacRx */

void do_hlpModelDbgDumpXbarRx(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( RX_PORT );

}   /* end hlpModelDbgDumpXbarRx */

void do_hlpModelDbgDumpParser(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( RX_PORT );
   EMIT( PARSER_PKT_META );
   EMIT( PA_ADJ_SEG_LEN );
   EMIT( PA_KEYS );
   EMIT( PA_KEYS_VALID );
   EMIT( PA_FLAGS );
   EMIT( PA_PTRS );
   EMIT( PA_PTRS_VALID );
   EMIT( PA_CSUM_OK );
   EMIT( PA_EX_STAGE );
   EMIT( PA_EX_DEPTH_EXCEED );
   EMIT( PA_EX_TRUNC_HEADER );
   EMIT( PA_EX_PARSING_DONE );
   EMIT( PA_FORCE_SAF );
   EMIT( PA_DROP );
   EMIT( PA_CSUM_ERR );
   EMIT( PA_L3LEN_ERR );
   EMIT( TAIL_CSUM_LEN );

}   /* end hlpModelDbgDumpParser */

void do_hlpModelDbgDumpMapper(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( PKT_META );
   EMIT( PARSER_INFO );
   EMIT( FFU_KEYS );
   EMIT( FFU_ACTIONS );
   EMIT( FFU_SCENARIO );
   EMIT( FFU_VRID );
   EMIT( IP_OPTION );
   EMIT( PRIORITY_PROFILE );
   EMIT( NO_PRI_ENC );

}   /* end hlpModelDbgDumpMapper */

void do_hlpModelDbgDumpFfuClassifier(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( FFU_KEYS );
   EMIT( FFU_ACTIONS );
   EMIT( FFU_SCENARIO );
   EMIT( FFU_GRP_KEYS );
   EMIT( FFU_GRP_ACTIONS );
   EMIT( FGHASH_ACTIONS );
   EMIT( FFU_GRP_SCENARIO );

}   /* end hlpModelDbgDumpFfuClassifier */

void do_hlpModelDbgDumpFfuFinalActions(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( PKT_META );
   EMIT( PRIORITY_PROFILE );
   EMIT( FFU_MUXED_ACTION );
   EMIT( ECMP_HASH );
   EMIT( HQM_HASH );
   EMIT( HQM_HASH_V );
   EMIT( MOD_META );
   EMIT( L34_HASH );
   EMIT( LEARN_MODE );
   EMIT( MOD_IDX );
   EMIT( L2_ETYPE );
   EMIT( QOS_SWPRI );
   EMIT( SGLORT );
   EMIT( IDGLORT );
   EMIT( L2_DMAC );
   EMIT( L2_SMAC );
   EMIT( DMAC_FROM_IPV6 );
   EMIT( IS_IPV4 );
   EMIT( IS_IPV6 );
   EMIT( TRAP_IP_OPTIONS );
   EMIT( L3_LENGTH );
   EMIT( OUTER_L3_LENGTH );
   EMIT( INNER_L3_LENGTH );
   EMIT( DROP_TTL );
   EMIT( TRAP_ICMP );
   EMIT( TRAP_IGMP );
   EMIT( TTL_CTRL );
   EMIT( PARSER_ERROR );
   EMIT( FFU_FLAGS );
   EMIT( FFU_ROUTE );
   EMIT( NO_LEARN );
   EMIT( L2_IVID1 );
   EMIT( TX_TAG );
   EMIT( QOS_L2_VPRI1 );
   EMIT( FFU_TRIG );
   EMIT( QOS_L3_DSCP );
   EMIT( FLOOD_GLORT );
   EMIT( POLICER_ACTION );
   EMIT( PARITY_ERROR );
   EMIT( ECN );
   EMIT( AQM_MARK_EN );
   EMIT( VLAN_COUNTER );
   EMIT( MPLS_POP );

}   /* end hlpModelDbgDumpFfuFinalActions */

void do_hlpModelDbgDumpNextHop(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( L2_EDOMAIN );
   EMIT( L3_EDOMAIN );
   EMIT( MOD_IDX );
   EMIT( IDGLORT );
   EMIT( L2_DMAC );
   EMIT( FFU_FLAGS );
   EMIT( L2_EVID1 );
   EMIT( MARK_ROUTED );
   EMIT( ARP_TABLE_INDEX );
   EMIT( MTU_INDEX );
   EMIT( FLOOD_SET );
   EMIT( L2_IDOMAIN );
   EMIT( L3_IDOMAIN );
   EMIT( ENCAP );
   EMIT( DECAP );

}   /* end hlpModelDbgDumpNextHop */

void do_hlpModelDbgDumpHash(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( HASH_KEYS );
   EMIT( ARP_HASH );
   EMIT( RAW_HASH );
   EMIT( HASH_ROT_A );
   EMIT( HASH_ROT_B );
   EMIT( HASH_ROT_A_PTABLE_INDEX );
   EMIT( HASH_ROT_B_PTABLE_INDEX );

}   /* end hlpModelDbgDumpHash */

void do_hlpModelDbgDumpL2Lookup(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( IDGLORT );
   EMIT( DROP_TTL );
   EMIT( TRAP_ICMP );
   EMIT( TRAP_IGMP );
   EMIT( NO_LEARN );
   EMIT( PARITY_ERROR );
   EMIT( L2_EVID1 );
   EMIT( L2_IVLAN1_REFLECT );
   EMIT( L2_IVLAN1_CNT_INDEX );
   EMIT( L2_IVLAN1_MEMBERSHIP );
   EMIT( L2_EVLAN1_MEMBERSHIP );
   EMIT( L2_EVLAN1_TRIG );
   EMIT( MTU_VIOLATION );
   EMIT( DA_INDEX );
   EMIT( DA_SET );
   EMIT( DA_HIT );
   EMIT( DA_RESULT );
   EMIT( SA_INDEX );
   EMIT( SA_SET );
   EMIT( FREE_INDEX );
   EMIT( FREE_SET );
   EMIT( SA_HIT );
   EMIT( SA_RESULT );
   EMIT( L2_IFID1_STATE );
   EMIT( L2_EFID1_STATE );
   EMIT( GLORT_FORWARDED );
   EMIT( FLOOD_FORWARDED );
   EMIT( PORT_FIELD_SIZE );
   EMIT( CSGLORT );
   EMIT( MOVED_PROV_ENTRY_EXISTED );
   EMIT( SECURED_PORT );
   EMIT( PROV_INDEX );
   EMIT( PROV_SET );
   EMIT( SV_DROP );
   EMIT( DROP_PROVISIONAL );
   EMIT( DA_LOOKUP_PERFORMED );

}   /* end hlpModelDbgDumpL2Lookup */

void do_hlpModelDbgDumpGlort(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( PARITY_ERROR );
   EMIT( GLORT_CAM_MISS );
   EMIT( STRICT_GLORT_ROUTING );
   EMIT( TARGETED_DETERMINISTIC );
   EMIT( GLORT_DMASK );
   EMIT( IP_MCAST_IDX );

}   /* end hlpModelDbgDumpGlort */

void do_hlpModelDbgDumpGenMask1(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( QOS_SWPRI );
   EMIT( AMASK );
   EMIT( LOG_AMASK );
   EMIT( DMASK );
   EMIT( RX_MIRROR );
   EMIT( QCN_MIRROR0_PROFILE_V );
   EMIT( QCN_MIRROR1_PROFILE_V );
   EMIT( MIRROR1_PROFILE_V );
   EMIT( MIRROR1_PROFILE_IDX );
   EMIT( PRE_RESOLVE_ACTION );
   EMIT( PRE_RESOLVE_DMASK );
   EMIT( PRE_RESOLVE_DGLORT );
   EMIT( ACTION );
   EMIT( CPU_CODE );
   EMIT( MAC_MOVED );
   EMIT( LEARNING_ENABLED );
   EMIT( FCLASS );
   EMIT( CPU_TRAP );
   EMIT( POLICER_DROP );
   EMIT( OPERATOR_ID );
   EMIT( STORE_TRAP_ACTION );
   EMIT( XCAST );

}   /* end hlpModelDbgDumpGenMask1 */

void do_hlpModelDbgDumpCm(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( TC );
   EMIT( SMP_MEMBERSHIP );
   EMIT( PAUSE_TC_XOFF );

}   /* end hlpModelDbgDumpCm */

void do_hlpModelDbgDumpTriggers(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( PKT_META );
   EMIT( QOS_SWPRI );
   EMIT( IDGLORT );
   EMIT( L2_EVID1 );
   EMIT( DMASK );
   EMIT( MIRROR1_PROFILE_V );
   EMIT( MIRROR1_PROFILE_IDX );
   EMIT( ACTION );
   EMIT( LEARNING_ENABLED );
   EMIT( TRIG_HIT_MASK_RESOLVED_HI );
   EMIT( TRIG_HIT_MASK_RESOLVED_LO );
   EMIT( MIRROR0_PROFILE_V );
   EMIT( MIRROR0_PROFILE_IDX );
   EMIT( TRIGGERS );
   EMIT( NO_MODIFY );

}   /* end hlpModelDbgDumpTriggers */

void do_hlpModelDbgDumpGenMask2(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( PKT_META );
   EMIT( QOS_SWPRI );
   EMIT( IDGLORT );
   EMIT( L2_EVID1 );
   EMIT( MARK_ROUTED );
   EMIT( IP_MCAST_IDX );
   EMIT( LOG_AMASK );
   EMIT( DMASK );
   EMIT( RX_MIRROR );
   EMIT( MIRROR1_PROFILE_V );
   EMIT( MIRROR1_PROFILE_IDX );
   EMIT( ACTION );
   EMIT( CPU_CODE );
   EMIT( FNMASK );
   EMIT( MCAST_EPOCH );
   EMIT( LOGGING_HIT );
   EMIT( MIRROR0_PORT );
   EMIT( MIRROR1_PORT );
   EMIT( LOG_MIRROR_PROFILE_IDX );
   EMIT( MIRROR0_PROFILE_V );
   EMIT( MIRROR0_PROFILE_IDX );

}   /* end hlpModelDbgDumpGenMask2 */

void do_hlpModelDbgDumpLearning(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( L2_S_KEY_LEARN );
   EMIT( L2_S_MA_TABLE_OVERWRITE );
   EMIT( L2_S_MA_TABLE_INDEX );

}   /* end hlpModelDbgDumpLearning */

void do_hlpModelDbgDumpFsched(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( FSCHED_INIT );
   EMIT( FSCHED_L2_PORTNUM );
   EMIT( FSCHED_LEN_TABLE_OFFSET );
   EMIT( TX_PORT );
   EMIT( EDGLORT );
   EMIT( MOD_IP_MCAST_IDX );
   EMIT( MIRROR0_SENT );
   EMIT( MIRROR1_SENT );
   EMIT( MIRTYP );
   EMIT( IS_TIMEOUT );
   EMIT( TX_LENGTH );

}   /* end hlpModelDbgDumpFsched */

void do_hlpModelDbgDumpModify(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( RX_LENGTH );
   EMIT( RX_PORT );
   EMIT( PKT_META );
   EMIT( TX_PKT_META );
   EMIT( SEG_META_ERR );
   EMIT( MOD_META );
   EMIT( MOD_IDX );
   EMIT( QOS_SWPRI );
   EMIT( SGLORT );
   EMIT( L2_DMAC );
   EMIT( DROP_TTL );
   EMIT( TTL_CTRL );
   EMIT( L2_IVID1 );
   EMIT( TX_TAG );
   EMIT( QOS_L2_VPRI1 );
   EMIT( QOS_L3_DSCP );
   EMIT( ECN );
   EMIT( AQM_MARK_EN );
   EMIT( VLAN_COUNTER );
   EMIT( L2_EVID1 );
   EMIT( MARK_ROUTED );
   EMIT( MIRROR1_PROFILE_IDX );
   EMIT( MCAST_EPOCH );
   EMIT( MIRROR0_PROFILE_IDX );
   EMIT( NO_MODIFY );
   EMIT( TC );
   EMIT( SMP_MEMBERSHIP );
   EMIT( SAF_ERROR );
   EMIT( PM_ERR );
   EMIT( TX_PORT );
   EMIT( EDGLORT );
   EMIT( MOD_IP_MCAST_IDX );
   EMIT( MIRTYP );
   EMIT( IS_TIMEOUT );
   EMIT( DCTCP_MARK );
   EMIT( LFSR );
   EMIT( TX_LENGTH );
   EMIT( TX_STATS_LENGTH );
   EMIT( TX_STATS_LAST_LEN );
   EMIT( TX_REASONCODE );
   EMIT( TX_DISP );
   EMIT( TX_DROP );
   EMIT( SEG_DROP );
   EMIT( MPLS_POP );
   EMIT( ENCAP );
   EMIT( DECAP );
   EMIT( SKIP_DGLORT_DEC );
   EMIT( TS_1588_CAPTURE );
   EMIT( PTOT );
   EMIT( L2_COUNT );
   EMIT( DRR_PHASE );
   EMIT( TX_FREE );
   EMIT( OOM );
   EMIT( PM_ERR_NONSOP );
   EMIT( ADDR );
   EMIT( TX_PHYS_PORT );

}   /* end hlpModelDbgDumpModify */

void do_hlpModelDbgDumpXbarTx(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( TX_PORT );

}   /* end hlpModelDbgDumpXbarTx */

void do_hlpModelDbgDumpMacTx(hlp_dbg_state_t *dsp)
{
  DO_PROLOG(dsp);
   EMIT( TX_LENGTH );

}   /* end hlpModelDbgDumpMacTx */

static void genDbgDump(const char *srcFile,
                       const char *srcFunction,
                       fm_uint32   srcLine,
                       hlp_model  *model,
                       void (*f)(hlp_dbg_state_t *),
                       const char *myName)
/* generic debug dumper --

   code calling this library may ONLY enter through this function,
   because it allocates the initial memory, sets up the abort point,
   and deallocates the memory used.

 */
{
    hlp_dbg_state_t ds;

    /* allocate debug structure (including output buffer) */
    if(!setupDbgState(&ds, srcFile, srcFunction, srcLine, model))
      return;

    dlog(&ds,                                                           
         "--------------------------------------------------\n");       
    if (model->packetState.SB_DATA != NULL)                                 
      dlog(&ds,                                                     
           "%s: ID:%08x\n", myName, model->packetState.SB_DATA->idTag);         
    
    /* set an abort point and call f

       When you set the abort point, setjmp returns 0, so call f.

       If f succeeds, it falls through.

       If f fails,  setjmp returns again, with -1 (see abort_jmp).

       Nothing special is done on failure.
     */
         
    if (!setjmp(ds.abort_env))
      f(&ds);

    dlog(&ds,                                                            
         "--------------------------------------------------\n\n")        ;

    /* deallocate everything */
    cleanupDbgState(&ds);
}

#define HLP_INSTANTIATE_DBG_DUMPER(Z)                     \
void               Z(const char *srcFile,                 \
                     const char *srcFunction,             \
                     fm_uint32   srcLine,                 \
                     hlp_model  *model)                   \
{ genDbgDump(srcFile, srcFunction, srcLine, model, do_ ## Z, #Z); }

HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDump)
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpMacRx )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpXbarRx )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpParser )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpMapper )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpFfuClassifier )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpFfuFinalActions )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpNextHop )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpHash )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpL2Lookup )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpGlort )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpGenMask1 )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpCm )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpTriggers )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpGenMask2 )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpLearning )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpFsched )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpModify )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpXbarTx )
HLP_INSTANTIATE_DBG_DUMPER( hlpModelDbgDumpMacTx )

void hlpModelDbgDumpParserState(hlp_model *model, fm_byte *hit_idx,
                                fm_bool *hit_v) {

    hlp_modelState *state = &model->packetState;

    FILE *fp;
    int idx;
    int i;
    fp = fopen("parser.dump", "a+");

    if (!fp)
    {
        printf("Unable to open/create parser.dump file\n");
        return;
    }
    
    fprintf(fp, "===========\nPACKET: %d\n===========\n", frameNo++);
    
    for (i = 0; i < 32; i++) {
        idx = hit_v[i] ? hit_idx[i] : -1;
        fprintf(fp, "HIT_IDX[%d]: 0x%x\n", i, idx);
    }

    for(i = 0; i < 32; i++)
    	fprintf(fp, "PARSER_PKT_META[%d]: 0x%x\n", i, state->PARSER_PKT_META[i]);
    fprintf(fp, "PA_ADJ_SEG_LEN: 0x%x\n", state->PA_ADJ_SEG_LEN);

    for(i = 0; i < 84; i++)
    	fprintf(fp, "PA_KEYS[%d]: 0x%x\n", i, state->PA_KEYS[i]);
    for(i = 0; i < 84; i++)
    	fprintf(fp, "PA_KEYS_VALID[%d]: 0x%x\n", i, state->PA_KEYS_VALID[i]);
    for(i = 0; i < 48; i++)
    	fprintf(fp, "PA_FLAGS[%d]: 0x%x\n", i, state->PA_FLAGS[i]);
    for(i = 0; i < 8; i++)
    	fprintf(fp, "PA_PTRS[%d]: 0x%x\n", i, state->PA_PTRS[i]);
    for(i = 0; i < 8; i++)
    	fprintf(fp, "PA_PTRS_VALID[%d]: 0x%x\n", i, state->PA_PTRS_VALID[i]);
    fprintf(fp, "PA_CSUM_OK: 0x%x\n", state->PA_CSUM_OK);
    fprintf(fp, "PA_EX_STAGE: 0x%x\n", state->PA_EX_STAGE);
    fprintf(fp, "PA_EX_DEPTH_EXCEED: 0x%x\n", state->PA_EX_DEPTH_EXCEED);
    fprintf(fp, "PA_EX_TRUNC_HEADER: 0x%x\n", state->PA_EX_TRUNC_HEADER);
    fprintf(fp, "PA_EX_PARSING_DONE: 0x%x\n", state->PA_EX_PARSING_DONE);
    fprintf(fp, "PA_DORCE_SAF: 0x%x\n", state->PA_FORCE_SAF);
    fprintf(fp, "PA_DROP: 0x%x\n", state->PA_DROP);
    fprintf(fp, "PA_L3LEN_ERR: 0x%x\n", state->PA_L3LEN_ERR);
    fprintf(fp, "TAIL_CSUM_LEN: 0x%llx\n", state->TAIL_CSUM_LEN);
    //fprintf(fp, "PARSER_INFO: %p\n", &state->PARSER_INFO);

    fclose(fp);
}

