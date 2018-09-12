/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/*****************************************************************************
 * File:            sdk_library.i
 * Creation Date:   01/21/08
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2013 Intel Corporation. All Rights Reserved. 
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

%{

#include <sys/wait.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#include "fm_sdk.h"

%}

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

%{

#define FM_SWIG_PATH_BSHELL         "/bin/sh"

#define FM_SWIG_N_MACADDR_BYTES     6

/*****************************************************************************/
/** FM_SWIG_SNPRINTF
 *
 * \desc        prints output to a character buffer according to the specified
 *              format
 *
 * \param[in]   buffer the character buffer to print to
 *
 * \param[in]   size the size of the character buffer
 *
 * \param[in]   format the format of the output to be printed to the character
 *              buffer
 *
 * \param[in]   ... the list of variables to form the output with
 *
 *****************************************************************************/
#define FM_SWIG_SNPRINTF(/* char* */        buffer,                           \
                         /* size_t */       size,                             \
                         /* const char* */  format,                           \
                                            ...)                              \
{                                                                             \
    if (((size_t) snprintf(buffer, size, format, __VA_ARGS__)) >= size)       \
    {                                                                         \
        croak("%s: Out of memory", __func__);                                 \
    }                                                                         \
}

/*****************************************************************************/
/** fmSWIGSetCounter
 *
 * \desc            updates a key in a Perl hash with the correct statistics
 *                  counter value
 *
 * \param[in,out]   hash the Perl hash to modify
 *
 * \param[in]       counters the fm_[port|switch|vlan]Counters structure from
 *                  which to retrieve the statistics counter value
 *
 * \param[in]       member the name of the fm_(port|switch|vlan)Counters member
 *                  whose value is to be assigned to the identically named Perl
 *                  hash key
 *
 *****************************************************************************/
#define fmSWIGSetCounter(/* HV* */                              hash,         \
                         /* fm_(port|switch|vlan)Counters* */   counters,     \
                                                                member)       \
{                                                                             \
    SV      **sv;                                                             \
                                                                              \
    if ((sv = fmSWIGGetHashKey(hash, #member)) == NULL)                       \
    {                                                                         \
        croak("%s: " #member ": Invalid key", __func__);                      \
    }                                                                         \
    fmSWIGSetScalarUINT64(*sv, counters->member);                             \
}

/*****************************************************************************/
/** fmSWIGSetIndexedCounter
 *
 * \desc            updates a vectorized key in a Perl hash with the correct
 *                  statistics counter values 
 *
 * \param[in,out]   hash the Perl hash to modify
 *
 * \param[in]       counters the fm_portCounters structure from which to
 *                  retrieve the statistics counter values
 *
 * \param[in]       member the name of the fm_portCounters member whose values
 *                  are to be assigned to the identically named vectorized Perl
 *                  hash key
 *
 *****************************************************************************/
#define fmSWIGSetIndexedCounter(/* HV* */               hash,                 \
                                /* fm_portCounters* */  counters,             \
                                                        member,               \
                                                        size)                 \
{                                                                             \
    AV      *array;                                                           \
    SV      **sv;                                                             \
    I32     n;                                                                \
    int     i;                                                                \
                                                                              \
    if ((sv = fmSWIGGetHashKey(hash, #member)) == NULL)                       \
    {                                                                         \
        croak("%s: " #member ": Invalid key", __func__);                      \
    }                                                                         \
    if (!SvROK(*sv) || SvTYPE(SvRV(*sv)) != SVt_PVAV)                         \
    {                                                                         \
        croak("%s: " #member " key is not an ARRAY reference",                \
              __func__);                                                      \
    }                                                                         \
    array = (AV *) SvRV(*sv);                                                 \
    n = av_len(array) + 1;                                                    \
    if (n != size)                                                            \
    {                                                                         \
        croak("%s: Invalid size for " #member " ARRAY", __func__);            \
    }                                                                         \
    for (i = 0; i < size; i++)                                                \
    {                                                                         \
        if ((sv = av_fetch(array, i, 0)) == NULL)                             \
        {                                                                     \
            croak("%s: %d: Invalid index for key " #member, __func__, i);     \
        }                                                                     \
        fmSWIGSetScalarUINT64(*sv, counters->member[i]);                      \
        SvREFCNT_inc(*sv);                                                    \
        if (av_store(array, i, *sv) == NULL)                                  \
        {                                                                     \
            SvREFCNT_dec(*sv);                                                \
            croak("%s: Could not store SV at index %d for key " #member,      \
                  __func__, i);                                               \
        }                                                                     \
    }                                                                         \
}


typedef enum
{
    FM_SWIG_TYPE_UNKNOWN = 0,
    FM_SWIG_TYPE_BIT_ARRAY,
    FM_SWIG_TYPE_FIBM_SWITCH_CONFIG,
    FM_SWIG_TYPE_IP_ADDR,
    FM_SWIG_TYPE_MTU_ENTRY,
    FM_SWIG_TYPE_POLICER_BANK_DROP_MASK,
    FM_SWIG_TYPE_ACL_RULE_POLICER_INFO_VALUE,
    FM_SWIG_TYPE_MAC_ADDR,
    FM_SWIG_TYPE_UINT64_ARRAY,
    FM_SWIG_TYPE_UINT64,
    FM_SWIG_TYPE_UINT32,
    FM_SWIG_TYPE_UINT16,
    FM_SWIG_TYPE_UINT,
    FM_SWIG_TYPE_INT_ARRAY,
    FM_SWIG_TYPE_INT32,
    FM_SWIG_TYPE_INT,
    FM_SWIG_TYPE_BOOL,
    FM_SWIG_TYPE_CHAR,
    FM_SWIG_TYPE_HASH_ROTATION_VALUE,
    FM_SWIG_TYPE_ETH_MODE,
    FM_SWIG_TYPE_FFU_SLICE_ALLOCATIONS,
    FM_SWIG_TYPE_MAPPER_SOURCE_VALUE,
    FM_SWIG_TYPE_MAPPER_SOURCE_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE,
    FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_L4PORT_VALUE,
    FM_SWIG_TYPE_MAPPER_L4PORT_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_MAC_VALUE,
    FM_SWIG_TYPE_MAPPER_MAC_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE,
    FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE,
    FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_IPADDR_VALUE,
    FM_SWIG_TYPE_MAPPER_IPADDR_VALUE_ARRAY,
    FM_SWIG_TYPE_MAPPER_VLAN_VALUE,
    FM_SWIG_TYPE_MAPPER_VLAN_VALUE_ARRAY,
    FM_SWIG_TYPE_LBG_DISTRIBUTION_MAP_RANGE,
    FM_SWIG_TYPE_LBG_DISTRIBUTION_MAP_SINGLE,
    FM_SWIG_TYPE_L2_HASH_KEY,
    FM_SWIG_TYPE_L2_HASH_ROT,
    FM_SWIG_TYPE_L3_HASH_CONFIG,
    FM_SWIG_TYPE_ACL_COMPILER_TRY_ALLOC,
    FM_SWIG_TYPE_PAUSE_PACING_TIME,


} fmSWIG_dataType;


typedef struct _fmSWIG_void
{
    char * type;

    STRLEN length;

    SV *   value;

} fmSWIG_void;


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/** FM_SWIG_errSv
 *
 * \desc            Perl SCALAR value used to communicate error messages.
 *
 *****************************************************************************/
SWIGINTERN SV *FM_SWIG_errSv = NULL;


/** FM_SWIG_void
 *
 * \desc            Perl HASH used to store the action lookup table for void *
 *                  arguments.
 *
 *****************************************************************************/
SWIGINTERN HV *FM_SWIG_void = NULL;


/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

SWIGINTERN SV** fmSWIGAddHashKey(HV*, char*);
SWIGINTERN void fmSWIGCreatePortCountersStructure(int, SV*, fm_uint64);
SWIGINTERN void fmSWIGCreateSwitchCountersStructure(int, SV*, fm_uint64);
SWIGINTERN void fmSWIGCreateVLANCountersStructure(int, SV*, fm_uint64);
SWIGINTERN SV** fmSWIGGetHashKey(HV*, char*);
SWIGINTERN void fmSWIGGetMacAddr(fm_macaddr*, char*);
SWIGINTERN fm_uint64 fmSWIGGetScalarUINT64(SV*);
SWIGINTERN SV** fmSWIGSetHashKey(HV*, char*, SV*);
SWIGINTERN void fmSWIGSetMacAddr(char*, size_t, fm_macaddr);
SWIGINTERN void fmSWIGSetScalarUINT64(SV*, fm_uint64);
SWIGINTERN int fmSWIGVerifyVoidStructure(SV*, fmSWIG_void*);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

%}

%insert(init)
{
    /* Initialize FM_SWIG_errSv at SWIG module boot time.  */
    FM_SWIG_errSv = newSV(0);

    /* Initialize FM_SWIG_void at SWIG module boot time.  */
    {
        SV **sv;

        FM_SWIG_void = newHV();
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_bitArray");
        sv_setiv(*sv, FM_SWIG_TYPE_BIT_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_fibmSwitchConfig");
        sv_setiv(*sv, FM_SWIG_TYPE_FIBM_SWITCH_CONFIG);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ipAddr");
        sv_setiv(*sv, FM_SWIG_TYPE_IP_ADDR);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_mtuEntry");
        sv_setiv(*sv, FM_SWIG_TYPE_MTU_ENTRY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_policerBankDropMask");
        sv_setiv(*sv, FM_SWIG_TYPE_POLICER_BANK_DROP_MASK);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_aclRulePolicerInfo");
        sv_setiv(*sv, FM_SWIG_TYPE_ACL_RULE_POLICER_INFO_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ffuSliceAllocations");
        sv_setiv(*sv, FM_SWIG_TYPE_FFU_SLICE_ALLOCATIONS);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_sourceMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_SOURCE_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_sourceMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_SOURCE_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_pausePacingTime");
        sv_setiv(*sv, FM_SWIG_TYPE_PAUSE_PACING_TIME);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_protocolMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_protocolMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_l4PortMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_L4PORT_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_l4PortMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_L4PORT_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_macMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_MAC_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_macMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_MAC_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ethTypeValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ethTypeValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ipLengthMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ipLengthMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ipAddrMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_IPADDR_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ipAddrMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_IPADDR_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_vlanMapperValue");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_VLAN_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_vlanMapperValue[]");
        sv_setiv(*sv, FM_SWIG_TYPE_MAPPER_VLAN_VALUE_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_LBGDistributionMapRange");
        sv_setiv(*sv, FM_SWIG_TYPE_LBG_DISTRIBUTION_MAP_RANGE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_macaddr");
        sv_setiv(*sv, FM_SWIG_TYPE_MAC_ADDR);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_uint64[]");
        sv_setiv(*sv, FM_SWIG_TYPE_UINT64_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_uint64");
        sv_setiv(*sv, FM_SWIG_TYPE_UINT64);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_uint32");
        sv_setiv(*sv, FM_SWIG_TYPE_UINT32);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_uint16");
        sv_setiv(*sv, FM_SWIG_TYPE_UINT16);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_uint");
        sv_setiv(*sv, FM_SWIG_TYPE_UINT);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_int[]");
        sv_setiv(*sv, FM_SWIG_TYPE_INT_ARRAY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_int32");
        sv_setiv(*sv, FM_SWIG_TYPE_INT32);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_int");
        sv_setiv(*sv, FM_SWIG_TYPE_INT);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_bool");
        sv_setiv(*sv, FM_SWIG_TYPE_BOOL);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_char");
        sv_setiv(*sv, FM_SWIG_TYPE_CHAR);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_hashRotationValue");
        sv_setiv(*sv, FM_SWIG_TYPE_HASH_ROTATION_VALUE);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_L2HashKey");
        sv_setiv(*sv, FM_SWIG_TYPE_L2_HASH_KEY);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_L2HashRot");
        sv_setiv(*sv, FM_SWIG_TYPE_L2_HASH_ROT);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_L3HashConfig");
        sv_setiv(*sv, FM_SWIG_TYPE_L3_HASH_CONFIG);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_aclCompilerTryAlloc");
        sv_setiv(*sv, FM_SWIG_TYPE_ACL_COMPILER_TRY_ALLOC);
        sv = fmSWIGAddHashKey(FM_SWIG_void, "fm_ethMode");
        sv_setiv(*sv, FM_SWIG_TYPE_ETH_MODE);
    }
}

%{


/*****************************************************************************/
/** fmSWIGAddHashKey
 *
 * \desc            adds a new key to a Perl hash
 *
 * \param[in,out]   hash points to the Perl hash to add the new key to
 *
 * \param[in]       key the name of the key to add
 *
 *****************************************************************************/
SWIGINTERN SV**
fmSWIGAddHashKey(HV *hash, char *key)
{
    return hv_store(hash, key, (I32) strlen(key), newSV(0), 0);
}


/*****************************************************************************/
/** fmSWIGCreatePortCountersStructure
 *
 * \desc            creates a Perl hash corresponding to a particular version
 *                  of the fm_portCounters structure
 *
 * \param[in]       argnum the argument for which the Perl hash is to be
 *                  created
 *
 * \param[in,out]   rv points to the Perl reference to which the Perl hash is
 *                  assigned
 *
 * \param[in]       version the fm_portCounters structure version for which to
 *                  create the corresponding Perl hash
 *
 *****************************************************************************/
SWIGINTERN void
fmSWIGCreatePortCountersStructure(int argnum, SV *rv, fm_uint64 version)
{
    AV          *array;
    HE          *entry;
    HV          *hv = NULL;
    SV          *cntVersion, *e, *value;
    SV          **sv;
    I32         length, n;
    fm_uint64   mask, variant;
    int         i, j;
    fm_bool     error = FALSE;
    I32         size;

    if (!SvROK(rv))
    {
        croak("%s: Argument %d is not a reference", __func__, argnum);
    }
    /* Dereference the original reference and replace the resulting scalar with
     * a hash reference.  */
    sv_setsv(SvRV(rv), sv_2mortal(newRV_noinc((SV *) newHV())));
    /* Retrieve the hash.  */
    hv = (HV *) SvRV(SvRV(rv));
    variant = version;
    mask = 1ULL;
    while (variant > 0ULL)
    {
        switch (variant & mask)
        {
            /* FM6000 related statistics counters.  */
            case 8ULL:
                if (   fmSWIGAddHashKey(hv, "cntRxCBPausePkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxCBPauseOctets"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxFramingErrorPkts"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxFramingErrorOctets") == NULL
                    || fmSWIGAddHashKey(hv, "cntSTPIngressDropsPkts" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntSTPEgressDropsPkts"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxFramingErrorPkts"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxFramingErrorOctets") == NULL
                    || fmSWIGAddHashKey(hv, "cntTxOutOfMemErrPkts"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxOutOfMemErrOctets" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxUnrepairEccPkts"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxUnrepairEccOctets" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxLoopbackPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxLoopbackOctets"    ) == NULL
                    || fmSWIGAddHashKey(hv, "cntGlortSwitchedPkts"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntGlortRoutedPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntLoopbackDropsPkts"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntInvalidDropPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntGlobalWMDropPkts"    ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRXMPDropPkts"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxHogDropPkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxHogDropPkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntOtherPkts"           ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxUcstPktsNonIP"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxBcstPktsNonIP"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxMcstPktsNonIP"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxUcstPktsIP"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxBcstPktsIP"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxMcstPktsIP"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxMirrorPkts"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntParseErrDropPkts"    ) == NULL
                    || fmSWIGAddHashKey(hv, "cntParityErrorPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTrappedPkts"         ) == NULL
                    || fmSWIGAddHashKey(hv, "cntFFUDropPkts"         ) == NULL
                    || fmSWIGAddHashKey(hv, "cntPolicerDropPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntFloodControlDropPkts") == NULL
                    || fmSWIGAddHashKey(hv, "cntTxFCSErrDropPkts"    ) == NULL
                    || fmSWIGAddHashKey(hv, "cntCodeErrors"          ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxCMDropPkts"        ) == NULL
                    || fmSWIGAddHashKey(hv, "timestamp"              ) == NULL)
                {
                    error = TRUE;
                }
                break;

            /* FM4000 related statistics counters.  */
            case 4ULL:
                if (   fmSWIGAddHashKey(hv, "cntRxCBPausePkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxFrameSizeErrors"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxLoopbackPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxErrorOctets"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntSpeciallyHandledPkts") == NULL
                    || fmSWIGAddHashKey(hv, "cntParseErrDropPkts"    ) == NULL
                    || fmSWIGAddHashKey(hv, "cntParityErrorPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTrappedPkts"         ) == NULL
                    || fmSWIGAddHashKey(hv, "cntPauseDropPkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntGlortMissDropPkts"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntFFUDropPkts"         ) == NULL
                    || fmSWIGAddHashKey(hv, "cntPolicerDropPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntCmPrivDropPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntSmp0DropPkts"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntSmp1DropPkts"        ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxHog0DropPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxHog1DropPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxHog0DropPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxHog1DropPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRateLimit0DropPkts"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRateLimit1DropPkts"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntBadSmpDropPkts"      ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTriggerDropPkts"     ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTriggerRedirPkts"    ) == NULL
                    || fmSWIGAddHashKey(hv, "timestamp"              ) == NULL)
                {
                    error = TRUE;
                }
                break;
            /* FM4000 TCP/IP related statistics counters.  */
            case 2ULL:
                if (   fmSWIGAddHashKey(hv, "cntRxUcstPktsNonIP") == NULL
                    || fmSWIGAddHashKey(hv, "cntRxUcstPktsIPv4" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxUcstPktsIPv6" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxBcstPktsNonIP") == NULL
                    || fmSWIGAddHashKey(hv, "cntRxBcstPktsIPv4" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxBcstPktsIPv6" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxMcstPktsNonIP") == NULL
                    || fmSWIGAddHashKey(hv, "cntRxMcstPktsIPv4" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxMcstPktsIPv6" ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxOctetsNonIp"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxOctetsIPv4"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntRxOctetsIPv6"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTTLDropPkts"    ) == NULL)
                {
                    error = TRUE;
                }
                break;
            /* FM2000 related statistics counters.  */
            case 1ULL:
                if (   fmSWIGAddHashKey(hv, "cntRxCMDropPkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTxCMDropPkts"       ) == NULL
                    || fmSWIGAddHashKey(hv, "cntReservedTrapPkts"   ) == NULL
                    || fmSWIGAddHashKey(hv, "cntTriggerMirroredPkts") == NULL
                    || fmSWIGAddHashKey(hv, "cntBroadcastDropPkts"  ) == NULL
                    || fmSWIGAddHashKey(hv, "cntDLFDropPkts"        ) == NULL
                    || fmSWIGAddHashKey(hv, "timestamp"             ) == NULL)
               {
                    error = TRUE;
                }
                break;
            default:
                break;
        }
        variant = variant & ~mask;
        mask = mask << 1;
    }
    if (   fmSWIGAddHashKey(hv, "cntVersion"              ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxUcstPkts"           ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxBcstPkts"           ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxMcstPkts"           ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxPausePkts"          ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxFCSErrors"          ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxSymbolErrors"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxMinTo63Pkts"        ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx64Pkts"             ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx65to127Pkts"        ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx128to255Pkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx256to511Pkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx512to1023Pkts"      ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx1024to1522Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx1523to2047Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx2048to4095Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx4096to8191Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx8192to10239Pkts"    ) == NULL
        || fmSWIGAddHashKey(hv, "cntRx10240toMaxPkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxFragmentPkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxUndersizedPkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxJabberPkts"         ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxOversizedPkts"      ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxGoodOctets"         ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxBadOctets"          ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxPriorityPkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxInvalidPriorityPkts") == NULL
        || fmSWIGAddHashKey(hv, "cntRxPriorityOctets"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntRxInvalidPriorityOctets") == NULL
        || fmSWIGAddHashKey(hv, "cntTxPriorityPkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxPriorityOctets"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxUcstPkts"           ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxBcstPkts"           ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxMcstPkts"           ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxPausePkts"          ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxFCSErroredPkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxErrorDropPkts"      ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxTimeOutPkts"        ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxMinTo63Pkts"        ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx64Pkts"             ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx65to127Pkts"        ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx128to255Pkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx256to511Pkts"       ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx512to1023Pkts"      ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx1024to1522Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx1523to2047Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx2048to4095Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx4096to8191Pkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx8192to10239Pkts"    ) == NULL
        || fmSWIGAddHashKey(hv, "cntTx10240toMaxPkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTxOctets"             ) == NULL
        || fmSWIGAddHashKey(hv, "cntFIDForwardedPkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntFloodForwardedPkts"   ) == NULL
        || fmSWIGAddHashKey(hv, "cntSTPDropPkts"          ) == NULL
        || fmSWIGAddHashKey(hv, "cntSecurityViolationPkts") == NULL
        || fmSWIGAddHashKey(hv, "cntVLANTagDropPkts"      ) == NULL
        || fmSWIGAddHashKey(hv, "cntVLANIngressBVPkts"    ) == NULL
        || fmSWIGAddHashKey(hv, "cntVLANEgressBVPkts"     ) == NULL
        || fmSWIGAddHashKey(hv, "cntTriggerDropRedirPkts" ) == NULL
        || fmSWIGAddHashKey(hv, "cntUnderrunPkts"         ) == NULL
        || fmSWIGAddHashKey(hv, "cntOverrunPkts"          ) == NULL
        || fmSWIGAddHashKey(hv, "cntCorruptedPkts"        ) == NULL)
    {
        error = TRUE;
    }
    if (!error)
    {
        /* Replace the scalar values with array references for the
         * cntRxPriorityPkts and cntRxPriorityOctets key / value pairs.  */
        sv = fmSWIGGetHashKey(hv, "cntRxPriorityPkts");
        sv_setsv(*sv, sv_2mortal(newRV_noinc((SV *) newAV())));
        sv = fmSWIGGetHashKey(hv, "cntRxPriorityOctets");
        sv_setsv(*sv, sv_2mortal(newRV_noinc((SV *) newAV())));

        /* Do the same for cntTxPriorityPkts and cntTxPriorityOctets */
        sv = fmSWIGGetHashKey(hv, "cntTxPriorityPkts");
        sv_setsv(*sv, sv_2mortal(newRV_noinc((SV *) newAV())));
        sv = fmSWIGGetHashKey(hv, "cntTxPriorityOctets");
        sv_setsv(*sv, sv_2mortal(newRV_noinc((SV *) newAV())));
    }

    if (error)
    {
        hv_undef(hv);
        croak("%s: Could not create Perl fm_portCounters structure",
              __func__);
    }
    else
    {
        /* Initialize all hash key / value pairs.  */
        n = hv_iterinit(hv);
        for (i = 0; i < n; i++)
        {
            entry = hv_iternext(hv);
            value = hv_iterval(hv, entry);
            if (SvROK(value))
            {
                if (SvTYPE(SvRV(value)) != SVt_PVAV)
                {
                    croak("%s: %s key is not an ARRAY reference",
                          __func__, hv_iterkey(entry, &length));
                }
                array = (AV *) SvRV(value);
                /* cnt*xMirror*xPkts */
                if (strncmp(hv_iterkey(entry, &length) + 5, "Mirror", 6) == 0)
                {
                    size = 4;
                }
                /* cnt*xPriority**** */
                else if (strncmp(hv_iterkey(entry, &length) + 5, "Priority", 8) == 0)
                {
                    size = 16;
                }
                else
                {
                    croak("%s: %s key size is not defined",
                          __func__, hv_iterkey(entry, &length));
                }
                for (j = 0; j < size; j++)
                {
                    e = newSV(0);
                    fmSWIGSetScalarUINT64(e, (fm_uint64) 0);
                    av_push(array, e);
                }
            }
            else
            {
                fmSWIGSetScalarUINT64(value, (fm_uint64) 0);
            }
        }
        /* Initialize the 'cntVersion' key to the correct fm_portCounters
         * structure version.  */
        cntVersion = *(fmSWIGGetHashKey(hv, "cntVersion"));
        fmSWIGSetScalarUINT64(cntVersion, version);
    }
}

/*****************************************************************************/
/** fmSWIGCreateSwitchCountersStructure
 *
 * \desc            creates a Perl hash corresponding to a particular version
 *                  of the fm_switchCounters structure
 *
 * \param[in]       argnum the argument for which the Perl hash is to be
 *                  created
 *
 * \param[in,out]   rv points to the Perl reference to which the Perl hash is
 *                  assigned
 *
 * \param[in]       version the fm_switchCounters structure version for which
 *                  to create the corresponding Perl hash
 *
 *****************************************************************************/
SWIGINTERN void
fmSWIGCreateSwitchCountersStructure(int argnum, SV *rv, fm_uint64 version)
{
    HE      *entry;
    HV      *hash;
    SV      *cntVersion, *value;
    I32     length, n;
    int     i;
    fm_bool error = FALSE;

    if (!SvROK(rv))
    {
        croak("%s: Argument %d is not a reference", __func__, argnum);
    }
    /* Dereference the original reference and replace the resulting scalar with
     * a hash reference.  */
    sv_setsv(SvRV(rv), sv_2mortal(newRV_noinc((SV *) newHV())));
    /* Retrieve the hash.  */
    hash = (HV *) SvRV(SvRV(rv));
    switch (version)
    {
        case 1:
            if (   fmSWIGAddHashKey(hash, "cntGlobalLowDropPkts"      ) == NULL
                || fmSWIGAddHashKey(hash, "cntGlobalHighDropPkts"     ) == NULL
                || fmSWIGAddHashKey(hash, "cntGlobalPrivilegeDropPkts") == NULL)
            {
                error = TRUE;
            }
            break;
        default:
            croak("%s: Version %lld: Unsupported fm_switchCounters structure",
                  __func__, version);
    }
    if (fmSWIGAddHashKey(hash, "cntVersion") == NULL)
    {
        error = TRUE;
    }

    if (error)
    {
        hv_undef(hash);
        croak("%s: Could not create Perl fm_switchCounters structure",
              __func__);
    }
    else
    {
        /* Initialize all hash key / value pairs.  */
        n = hv_iterinit(hash);
        for (i = 0; i < n; i++)
        {
            entry = hv_iternext(hash);
            value = hv_iterval(hash, entry);
            if (!SvROK(value))
            {
                fmSWIGSetScalarUINT64(value, (fm_uint64) 0);
            }
            else
            {
                croak("%s: %s key is not a SCALAR",
                      __func__, hv_iterkey(entry, &length));
            }
        }
        /* Initialize the 'cntVersion' key to the correct fm_portCounters
         * structure version.  */
        cntVersion = *(fmSWIGGetHashKey(hash, "cntVersion"));
        fmSWIGSetScalarUINT64(cntVersion, version);
    }
}

/*****************************************************************************/
/** fmSWIGCreateVLANCountersStructure
 *
 * \desc            creates a Perl hash corresponding to a particular version
 *                  of the fm_vlanCounters structure
 *
 * \param[in]       argnum the argument for which the Perl hash is to be
 *                  created
 *
 * \param[in,out]   rv points to the Perl reference to which the Perl hash is
 *                  assigned
 *
 * \param[in]       version the fm_vlanCounters structure version for which to
 *                  create the corresponding Perl hash
 *
 *****************************************************************************/
SWIGINTERN void
fmSWIGCreateVLANCountersStructure(int argnum, SV *rv, fm_uint64 version)
{
    HE      *entry;
    HV      *hash;
    SV      *cntVersion, *value;
    I32     length, n;
    int     i;
    fm_bool error = FALSE;

    if (!SvROK(rv))
    {
        croak("%s: Argument %d is not a reference", __func__, argnum);
    }
    /* Dereference the original reference and replace the resulting scalar with
     * a hash reference.  */
    sv_setsv(SvRV(rv), sv_2mortal(newRV_noinc((SV *) newHV())));
    /* Retrieve the hash.  */
    hash = (HV *) SvRV(SvRV(rv));

    /* FM4000 or FM2000 */
    if (version & 5)
    {
        if (   fmSWIGAddHashKey(hash, "cntUcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntXcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntUcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntXcstPkts"  ) == NULL)
        {
            error = TRUE;
        }
    }
    else if (version & 8)
    {
        if (   fmSWIGAddHashKey(hash, "cntRxUcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntRxMcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntRxBcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntRxDropOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntRxUcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntRxMcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntRxBcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntRxDropPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntTxUcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntTxMcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntTxBcstOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntTxDropOctets") == NULL
            || fmSWIGAddHashKey(hash, "cntTxUcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntTxMcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntTxBcstPkts"  ) == NULL
            || fmSWIGAddHashKey(hash, "cntTxDropPkts"  ) == NULL)
        {
            error = TRUE;
        }
    }
    else
    {
        croak("%s: Version %lld: Unsupported fm_vlanCounters structure",
              __func__, version);
    }
    if (fmSWIGAddHashKey(hash, "cntVersion") == NULL)
    {
        error = TRUE;
    }

    if (error)
    {
        hv_undef(hash);
        croak("%s: Could not create Perl fm_vlanCounters structure",
              __func__);
    }
    else
    {
        /* Initialize all hash key / value pairs.  */
        n = hv_iterinit(hash);
        for (i = 0; i < n; i++)
        {
            entry = hv_iternext(hash);
            value = hv_iterval(hash, entry);
            if (!SvROK(value))
            {
                fmSWIGSetScalarUINT64(value, (fm_uint64) 0);
            }
            else
            {
                croak("%s: %s key is not a SCALAR",
                      __func__, hv_iterkey(entry, &length));
            }
        }
        /* Initialize the 'cntVersion' key to the correct fm_portCounters
         * structure version.  */
        cntVersion = *(fmSWIGGetHashKey(hash, "cntVersion"));
        fmSWIGSetScalarUINT64(cntVersion, version);
    }
}

/*****************************************************************************/
/** fmSWIGGetHashKey
 *
 * \desc            retrieves the value corresponding to the specified Perl
 *                  hash key
 *
 * \param[in]       hash points to the Perl hash to retrieve the key from
 *
 * \param[in]       key the name of the key to add
 *
 *****************************************************************************/
SWIGINTERN SV**
fmSWIGGetHashKey(HV *hash, char *key)
{
    return hv_fetch(hash, key, (I32) strlen(key), FALSE);
}

/*****************************************************************************/
/** fmSWIGGetMacAddr
 *
 * \desc            converts the string representation of a fm_macaddr variable
 *                  into its original representation
 *
 * \param[out]      macAddress points to the fm_macaddr variable to be restored
 *
 * \param[in]       address points to the character buffer containing the
 *                  string representation of the fm_macaddr variable to be
 *                  restored
 *
 *****************************************************************************/
SWIGINTERN void
fmSWIGGetMacAddr(fm_macaddr *macAddress, char *address)
{
    int   i;
    int   oerrno;
    int   shift;
    char *buffer;
    char *end;

    oerrno = errno;
    errno = 0;
    buffer = address;
    *macAddress = 0ULL;
    for (i = 0; i < FM_SWIG_N_MACADDR_BYTES; i++)
    {
        shift = 8 * (FM_SWIG_N_MACADDR_BYTES - (i + 1));
        *macAddress |= ((strtoull(buffer, &end, 16) & 255ULL) << shift);
        if (*end == '\0')
        {
            break;
        }
        buffer = ++end;
    }
    if (errno != 0)
    {
        warn("%s: %s: Invalid MAC address", __func__, address);
    }
    errno = oerrno;
}

/*****************************************************************************/
/** fmSWIGGetScalarUINT64
 *
 * \desc            converts the string representation of a fm_uint64 variable
 *                  into its original representation
 *
 * \param[in]       sv points to the Math::BigInt Perl object to be converted
 *
 * \return          the unsigned 64-bit integer
 *
 *****************************************************************************/
SWIGINTERN fm_uint64
fmSWIGGetScalarUINT64(SV *sv)
{
    fm_uint64   x;
    int         temporary;

    temporary = errno;
    errno = 0;
    /* The SvPV_nolen function handles 'get magic' meaning that it will
     * retrieve the string representation of the Math::BigInt Perl object that
     * is used to represent the unsigned 64-bit integer.  */
    x = (fm_uint64) strtoull(SvPV_nolen(sv), NULL, 10);
    if (errno != 0)
    {
        warn("%s: Could not convert %s into an unsigned 64-bit integer",
              __func__, SvPV_nolen(sv));
    }
    errno = temporary;
    return x;
}

/*****************************************************************************/
/** fmSWIGSetHashKey
 *
 * \desc            sets the value for the specified Perl hash key
 *
 * \param[in,out]   hash points to the Perl hash to modify
 *
 * \param[in]       key the name of the key to modify
 *
 *****************************************************************************/
SWIGINTERN SV**
fmSWIGSetHashKey(HV *hash, char *key, SV *value)
{
    SvREFCNT_inc(value);
    return hv_store(hash, key, strlen(key), value, 0);
}

/*****************************************************************************/
/** fmSWIGSetMacAddr
 *
 * \desc            converts a fm_macaddr variable into its string
 *                  representation
 *
 * \param[out]      macAddress points to the character buffer to store the
 *                  string representation of the fm_macaddr variable in
 *
 * \param[in]       size the size of the character buffer
 *
 * \param[in]       address the fm_macaddr variable to be converted
 *
 *****************************************************************************/
SWIGINTERN void
fmSWIGSetMacAddr(char *macAddress, size_t size, fm_macaddr address)
{
    int         x[2 * FM_SWIG_N_MACADDR_BYTES];
    int         i, shift;
    static char ASCII[16] = {
                                '0', '1', '2', '3', '4', '5', '6', '7',
                                '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
                            };
    char        *buffer;

    if (size < ((size_t) (3 * FM_SWIG_N_MACADDR_BYTES)))
    {
        croak("%s: Invalid buffer size", __func__);
    }
    for (i = 0; i < (2 * FM_SWIG_N_MACADDR_BYTES); i++)
    {
        shift = 8 * FM_SWIG_N_MACADDR_BYTES - 4 * (i + 1);
        x[i] = (int) ((address >> shift) & 15ULL);
    }
    buffer = macAddress;
    i = 0;
    *(buffer++) = ASCII[x[i++]];
    *(buffer++) = ASCII[x[i++]];
    while (i < (2 * FM_SWIG_N_MACADDR_BYTES))
    {
        *(buffer++) = ':';
        *(buffer++) = ASCII[x[i++]];
        *(buffer++) = ASCII[x[i++]];
    }
    *buffer = '\0';
}

/*****************************************************************************/
/** fmSWIGSetScalarUINT64
 *
 * \desc            converts an unsigned 64-bit integer variable into the
 *                  proper form for instantiation of a Perl Math::BigInt
 *                  variable
 *
 * \param[out]      sv points to the Math::BigInt Perl object to be set
 *
 * \param[in]       x the unsigned 64-bit integer variable to be converted
 *
 *****************************************************************************/
SWIGINTERN void
fmSWIGSetScalarUINT64(SV *sv, fm_uint64 x)
{
    /* Declare and initialize a local copy of the Perl stack pointer.  */
    dSP;
    SV      *value;
    I32     count;
    char    buffer[BUFSIZ];

    /* Create a boundary such that only the temporary variables (i.e. the
     * variables that are created using the sv_newmortal and sv_2mortal Perl
     * API function calls) that are created after the ENTER / SAVETMPS pair
     * will get destroyed.  */
    ENTER;
    SAVETMPS;
    /* Create a temporary variable to store the string representation of the
     * unsigned 64-bit variable that is to be converted.  */
    value = sv_newmortal();
    FM_SWIG_SNPRINTF(buffer, sizeof(buffer), "%llu", x);
    sv_setpv(value, buffer);
    /* Record the current position of the local copy of the Perl stack pointer.
     */
    PUSHMARK(SP);
    /* Push the class name, i.e. Math::BigInt, and the value that is to be
     * assigned to the newly created Math::BigInt Perl object onto the Perl
     * stack.  */
    XPUSHs(sv_2mortal(newSVpv("Math::BigInt", 0)));
    XPUSHs(value);
    /* Synchronize the global copy of the stack pointer with the local copy of
     * the stack pointer.  */
    PUTBACK;
    /* Create a new Math::BigInt Perl object.  */
    count = call_pv("Math::BigInt::new", G_SCALAR);
    if (count != 1)
    {
        croak("%s: Expected a SCALAR value", __func__);
    }
    /* Refresh the local copy of the stack pointer.  */
    SPAGAIN;
    /* Pop the first argument from the stack and assign this argument to the
     * Perl data structure that is to be set.  */
    sv_setsv(sv, POPs);
    /* Synchronize the global copy of the stack pointer with the local copy of
     * the stack pointer.  */
    PUTBACK;
    /* Destroy all previously created temporary variables.  */
    FREETMPS;
    LEAVE;
}

/*****************************************************************************/
/** fmSWIGVerifyVoidStructure
 *
 * \desc            verifies that the specified Perl data structure is a Perl
 *                  hash reference and contains a 'type' and 'value' key /
 *                  value pair, and upon successful verification, retrieves the
 *                  values for the two keys.
 *
 * \param[in]       rv points to the Perl data structure to be verified
 *
 * \param[in]       pv points to the SWIG void data structure to be assigned to
 *
 * \return          SWIG_OK if successful
 * \return          SWIG_ERROR otherwise
 *
 *****************************************************************************/
SWIGINTERN int
fmSWIGVerifyVoidStructure(SV *rv, fmSWIG_void *pv)
{
    HV  *hash;
    SV  **sv;

    /* Verify that the specified Perl data structure is a Perl hash reference
     * and contains a 'type' and 'value' key / value pair.  */
    if (!SvROK(rv))
    {
        sv_catpv(ERRSV, "is not a reference");
        return SWIG_ERROR;
    }
    if (SvTYPE(SvRV(rv)) != SVt_PVHV)
    {
        sv_catpv(ERRSV, "is not a HASH reference");
        return SWIG_ERROR;
    }
    hash = (HV *) SvRV(rv);
    /* Retrieve the `type' key. */
    if (!hv_exists(hash, "type", 4)
        || (sv = hv_fetch(hash, "type", 4, FALSE)) == NULL)
    {
        sv_catpv(ERRSV, "is missing the `type' key");
        return SWIG_ERROR;
    }
    pv->type = SvPV(*sv, pv->length);
    /* Retrieve the 'value' key. */
    if (!hv_exists(hash, "value", 5)
        || (sv = hv_fetch(hash, "value", 5, FALSE)) == NULL)
    {
        sv_catpv(ERRSV, "is missing the `value' key");
        return SWIG_ERROR;
    }
    pv->value = *sv;
    return SWIG_OK;
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** clone_readpipe
 *
 *****************************************************************************/
XS(clone_readpipe)
{
    SV * volatile result;
    SV *          program;
    FILE *        fp;
    pid_t         pid;
    int           fd[2];
    int           status;
    char          buffer[BUFSIZ];

    dXSARGS;

    result = &PL_sv_undef;

    if (items != 1)
    {
        goto ABORT;
    }

    program = ST(0);
    if (!SvPOK(program))
    {
        goto ABORT;
    }

    if (pipe(fd) < 0)
    {
        goto ABORT;
    }

    if ((pid = vfork()) == 0)
    {
        int w_clone = fd[1];

        if (w_clone != STDOUT_FILENO)
        {
            /* Redirect STDOUT to the file descriptor pointed to by w_clone. */
            dup2(w_clone, STDOUT_FILENO);
            close(w_clone);
            w_clone = STDOUT_FILENO;
        }

        /* Prevent the child process from accessing the read end of the pipe.
         */
        close(fd[0]);

        execl(FM_SWIG_PATH_BSHELL, "sh", "-c", SvPV_nolen(program), NULL);

        _exit(127);
    }
    else if (pid > 0)
    {
        fp = fdopen(fd[0], "r");

        /* Prevent the parent from accessing the write end of the pipe.  */
        close(fd[1]);

        /* Wait for the child process to exit.  */
        waitpid(pid, &status, 0);

        if (WIFEXITED(status))
        {
            result = newSVpvn("", 0);
            while (fgets(buffer, sizeof(buffer) - 1, fp))
            {
                buffer[sizeof(buffer) - 1] = '\0';
                sv_catpv(result, buffer);
            }
        }

        /* Close both the stream pointed to by fp and the read end of the pipe.
         */
        fclose(fp);
    }
    else
    {
        goto ABORT;
    }

ABORT:
    ST(0) = result;
    XSRETURN(1);
}

/*****************************************************************************/
/** clone_system
 *
 *****************************************************************************/
XS(clone_system)
{
    SV * volatile result;
    SV *          program;
    pid_t         pid;
    int           status;

    dXSARGS;

    result = &PL_sv_undef;

    if (items != 1)
    {
        goto ABORT;
    }

    program = ST(0);
    if (!SvPOK(program))
    {
        goto ABORT;
    }

    if ((pid = vfork()) == 0)
    {
        execl(FM_SWIG_PATH_BSHELL, "sh", "-c", SvPV_nolen(program), NULL);

        _exit(127);
    }
    else if (pid > 0)
    {
        /* Wait for the child process to exit.  */
        waitpid(pid, &status, 0);

        result = newSViv(WEXITSTATUS(status));
    }
    else
    {
        goto ABORT;
    }

ABORT:
    ST(0) = result;
    XSRETURN(1);
}

/*****************************************************************************/
/** FM_SWIG_Free
 *
 * \desc        Frees the memory space pointed to by the pointer that is stored
 *              in a SWIG::malloc Perl object whenever control reverts back to
 *              Perl
 *
 * \param[in]   sv points to a SWIG::malloc Perl object containing the pointer
 *              to the memory space that is to be freed
 *
 * \note        FM_SWIG_Free is an XSUB
 *
 *****************************************************************************/
XS(FM_SWIG_Free)
{
    SV *  sv;
    void *ptr;

    dXSARGS;

    if (items != 1)
    {
        goto ABORT;
    }

    sv = ST(0);
    if (sv_isobject(sv)
        && sv_isa(sv, "SWIG::malloc")
        && SvROK(sv)
        && SvTYPE(SvRV(sv)) == SVt_PVMG)
    {
        if (SvTRUE(SvRV(sv)))
        {
            ptr = (void *) SvIV(SvRV(sv));
            free(ptr);
#ifdef SWIG_DEBUG
            printf("deallocated %p\n", (void *) ptr);
#endif
            sv_setsv(SvRV(sv), &PL_sv_undef);
        }
    }

ABORT:
    XSRETURN(0);

}   /* end FM_SWIG_Free */

%}

%insert(init)
{
    newXS("SDK::Clone::readpipe", clone_readpipe, __FILE__);
    newXS("SDK::Clone::system", clone_system, __FILE__);
    newXS("SWIG::malloc::DESTROY", FM_SWIG_Free, __FILE__);
}
