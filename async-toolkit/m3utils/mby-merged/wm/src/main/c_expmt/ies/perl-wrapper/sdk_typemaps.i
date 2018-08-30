/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/*****************************************************************************
 * File:            sdk_typemaps.i
 * Creation Date:   August 15, 2007
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2013 Intel Corporation. All Rights Reserved. 
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

/******************************************************************************
 *
 *                                ASSUMPTIONS
 *
 * (I)  The Fulcrum Microsystems SDK types are mapped to Perl types as follows:
 *
 *          (a) The fm_int, fm_int16 and fm_int32 types are mapped to the Perl
 *              signed integer type.
 *
 *          (b) The fm_uint, fm_uint16, fm_uint32, fm_byte and fm_bool types
 *              are mapped to the Perl unsigned integer type.
 *
 *          (c) The fm_uint64 type is mapped to a Math::BigInt Perl object.
 *
 *          (d) The fm_macaddr type is mapped to its string representation in
 *              the form XX:XX:XX:XX:XX:XX.
 *
 *          (e) The fm_portCounters, fm_switchCounters and fm_vlanCounters
 *              structures are mapped to Perl hashes that only contain those
 *              keys that are defined to be valid as specified by the
 *              cntVersion structure member.
 *
 *****************************************************************************/

%{

#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>
#include <sys/time.h>

%}

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_SWIG_ABORT(/* sv *  */ sv)                                         \
{                                                                             \
    sv_setpvf(ERRSV, "Argument $argnum%s", SvPV_nolen((sv)));                 \
    sv_setpvf((sv), "");                                                      \
    SWIG_fail;                                                                \
}


%{

/** FM_SWIG_ASSERT
 *
 *****************************************************************************/
#define FM_SWIG_ASSERT(assertion)                                             \
    switch (0) {case 0: case assertion:;}

/** FM_SWIG_ASSERT_BYTE_SIZE
 *
 *****************************************************************************/
#define FM_SWIG_ASSERT_BYTE_SIZE(x, size)                                     \
    FM_SWIG_ASSERT(sizeof(x) == (size))

/** FM_SWIG_ASSERT_TYPE
 *
 *****************************************************************************/
#define FM_SWIG_ASSERT_TYPE(x, type)                                          \
    FM_SWIG_ASSERT_BYTE_SIZE(x, sizeof(type));

%}


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

%insert(init)
{
    FM_NOT_USED(items);
}




%define FM_SWIG_ARRAY(f_type, p5_type, fm_type, sv_get, sv_set)
%{

/*****************************************************************************/
/** fmSWIGGetArray(INT|INT16|INT32|UINT|UINT16|UINT32)
 *
 * \desc        converts the Perl representation of a fm_type array back to its
 *              original representation
 *
 * \param[out]  x points to the fm_type array to be restored
 *
 * \param[in]   rv points to a Perl ARRAY reference to be restored to its
 *              original representation
 *
 * \param[in]   size the size of the array
 *
 *****************************************************************************/
SWIGINTERNINLINE void
fmSWIGGetArray ## f_type (fm_type *x, SV *rv, int size)
{
    AV  *av;
    SV  **sv;
    int i;

    if (!SvROK(rv) || SvTYPE(SvRV(rv)) != SVt_PVAV)
    {
        croak("%s: Expected an ARRAY reference", __func__);
    }
    av = (AV *) SvRV(rv);
    for (i = 0; i < size; i++)
    {
        if ((sv = av_fetch(av, i, 0)) == NULL)
        {
            croak("%s: %d: Invalid index", __func__, i);
        }
        x[i] = (fm_type) sv_get(*sv);
    }
}




/*****************************************************************************/
/** fmSWIGSetArray(INT|INT16|INT32|UINT|UINT16|UINT32)
 *
 * \desc        converts the fm_type array into its Perl representation
 *
 * \param[out]  rv points to a Perl ARRAY reference in which to store the
 *              result
 *
 * \param[in]   x points to a fm_type array to be converted
 *
 * \param[in]   size the size of the array
 *
 *****************************************************************************/
SWIGINTERNINLINE void
fmSWIGSetArray ## f_type (SV *rv, fm_type *x, int size)
{
    AV  *av;
    SV  *sv;
    int i;

    av = newAV();
    for (i = 0; i < size; i++)
    {
        sv = newSV(0);
        sv_set(sv, (p5_type) x[i]);
        av_push(av, sv);
    }
    sv_setsv(rv, sv_2mortal(newRV_noinc((SV *) av)));
}

%}
%enddef /* FM_SWIG_ARRAY */




%define FM_SWIG_ARRAY_IV(f_type, fm_type)
    FM_SWIG_ARRAY(f_type, IV, fm_type, SvIV, sv_setiv);
%enddef /* FM_SWIG_ARRAY_IV */





%define FM_SWIG_ARRAY_UV(f_type, fm_type)
    FM_SWIG_ARRAY(f_type, UV, fm_type, SvUV, sv_setuv);
%enddef /* FM_SWIG_ARRAY_UV */

FM_SWIG_ARRAY_IV(INT, fm_int);
FM_SWIG_ARRAY_IV(INT16, fm_int16);
FM_SWIG_ARRAY_IV(INT32, fm_int32);
FM_SWIG_ARRAY_UV(UINT, fm_uint);
FM_SWIG_ARRAY_UV(UINT8, fm_byte);
FM_SWIG_ARRAY_UV(UINT16, fm_uint16);
FM_SWIG_ARRAY_UV(UINT32, fm_uint32);


/*****************************************************************************
 * Typemaps
 *****************************************************************************/

%{

/* Ensure that the Perl IV API type can hold all possible values of the fm_int,
 * fm_int16 and fm_int32 types. Furthermore, ensure that the Perl UV API type
 * can hold all possible value of the fm_uint, fm_uint16, fm_uint32, fm_byte
 * and fm_bool types.  */
void fmSWIGTypeAssertions(void)
{
    enum { FM_SWIG_TEST_ENUM };

    FM_SWIG_ASSERT(sizeof(IV) >= sizeof(fm_int));
    FM_SWIG_ASSERT(sizeof(IV) >= sizeof(fm_int16));
    FM_SWIG_ASSERT(sizeof(IV) >= sizeof(fm_int32));
    FM_SWIG_ASSERT(sizeof(UV) >= sizeof(fm_uint));
    FM_SWIG_ASSERT(sizeof(UV) >= sizeof(fm_uint16));
    FM_SWIG_ASSERT(sizeof(UV) >= sizeof(fm_uint32));
    FM_SWIG_ASSERT(sizeof(UV) >= sizeof(fm_byte));
    FM_SWIG_ASSERT(sizeof(UV) >= sizeof(fm_bool));
    FM_SWIG_ASSERT(sizeof(FM_SWIG_TEST_ENUM) == sizeof(fm_int));
}

%}

%define FM_SWIG_REFERENCE_TYPEMAP_IN(p5_type,
                                     fm_qualifier,
                                     fm_type)

FM_SWIG_FRAGMENT_ASPTR(p5_type, fm_type);

%typemap(in,fragment=SWIG_AsPtr_frag(fm_type)) fm_qualifier fm_type *
{
    /* typemap(in) $1_type */
    if (!SWIG_IsOK(SWIG_AsPtr(fm_type)($input, (fm_type **) &($1))))
    {
        FM_SWIG_ABORT(FM_SWIG_errSv);
    }
}

%enddef /* FM_SWIG_REFERENCE_TYPEMAP_IN */

%define FM_SWIG_REFERENCE_TYPEMAP_MEMBERIN(p5_type,
                                           fm_qualifier,
                                           fm_type)

%typemap(memberin) fm_qualifier fm_type *
%{
do {
    /* typemap(memberin) $1_type */
    $1 = ($1_ltype) $input;
    $input = NULL;
} while (0);
%}

%enddef /* FM_SWIG_REFERENCE_TYPEMAP_MEMBERIN */

%define FM_SWIG_REFERENCE_TYPEMAP_ARGOUT(p5_type,
                                         fm_qualifier,
                                         fm_type)

FM_SWIG_FRAGMENT_FROMPTR(p5_type, fm_type);

%typemap(argout,fragment=SWIG_FromPtr_frag(fm_type)) fm_qualifier fm_type *
{
    /* typemap(argout) $1_type */
    if (!SWIG_IsOK(SWIG_FromPtr(fm_type)($input, (fm_type *) $1)))
    {
        FM_SWIG_ABORT(FM_SWIG_errSv);
    }
}

%enddef /* FM_SWIG_REFERENCE_TYPEMAP_ARGOUT */

/** typemap(in,out) fm_uint64
 *
 *****************************************************************************/
FM_SWIG_FRAGMENT_ASVAL(UV64, fm_uint64);
FM_SWIG_FRAGMENT_FROMVAL(UV64, fm_uint64);

%typemap(in,fragment=SWIG_AsVal_frag(fm_uint64)) fm_uint64
{
    /* typemap(in) $1_type */
    if (!SWIG_IsOK(SWIG_AsVal(fm_uint64)($input, &($1))))
    {
        FM_SWIG_ABORT(FM_SWIG_errSv);
    }
}

%typemap(out,fragment=SWIG_FromVal_frag(fm_uint64)) fm_uint64
{
    /* typemap(out) $1_type */
    $result = SWIG_FromVal(fm_uint64)($1);
    argvi++;
}

/** typemap(in,out) fm_macaddr
 *
 *****************************************************************************/
%typemap(in,fragment=SWIG_AsVal_frag(fm_macaddr)) fm_macaddr
{
    /* typemap(in) $1_type */
    if (!SWIG_IsOK(SWIG_AsVal(fm_macaddr)($input, &($1))))
    {
        FM_SWIG_ABORT(FM_SWIG_errSv);
    }
}

%typemap(out,fragment=SWIG_FromVal_frag(fm_macaddr)) fm_macaddr
{
    /* typemap(out) $1_type */
    $result = SWIG_FromVal(fm_macaddr)($1);
    argvi++;
}

/** typemap(in,out,freearg) fm_int     [ANY]
 *                          fm_int16   [ANY]
 *                          fm_int32   [ANY]
 *                          fm_uint    [ANY]
 *                          fm_uint16  [ANY]
 *                          fm_uint32  [ANY]
 *
 *****************************************************************************/
%define FM_SWIG_ARRAY_TYPEMAP(f_type, fm_type, cast_type)
%typemap(arginit) fm_type [ANY] "$1 = NULL;"

%typemap(in) fm_type [ANY]
%{
do {
    /* typemap(in) $1_type */
    cast_type *x;

    x = (cast_type *) calloc(sizeof(fm_type), (size_t) $1_dim0);
    fmSWIGGetArray ## f_type (x, $input, $1_dim0);
    $1 = (fm_type *) x;
} while (0);
%}

%typemap(out) fm_type [ANY]
%{
do {
    /* typemap(out) $1_type */

    $result = sv_newmortal();
    fmSWIGSetArray ## f_type ($result, (cast_type *) $1, $1_dim0);
    argvi++;
} while (0);
%}

%typemap(freearg) fm_type [ANY]
%{
    free($1);
%}
%enddef /* FM_SWIG_ARRAY_TYPEMAP */

FM_SWIG_ARRAY_TYPEMAP(INT, fm_int, fm_int);
FM_SWIG_ARRAY_TYPEMAP(INT16, fm_int16, fm_int16);
FM_SWIG_ARRAY_TYPEMAP(INT32, fm_int32, fm_int32);
FM_SWIG_ARRAY_TYPEMAP(UINT, fm_uint, fm_uint);
FM_SWIG_ARRAY_TYPEMAP(UINT16, fm_uint16, fm_uint16);
FM_SWIG_ARRAY_TYPEMAP(UINT32, fm_uint32, fm_uint32);
FM_SWIG_ARRAY_TYPEMAP(UINT8, fm_byte, fm_byte);
FM_SWIG_ARRAY_TYPEMAP(UINT8, fm_bool, fm_bool);
FM_SWIG_ARRAY_TYPEMAP(INT, fm_aclBankType, fm_int);

/** typemap(in,memberin,argout,freearg) fm_int *
 *                                      fm_int16 *
 *                                      fm_int32 *
 *                                      fm_uint *
 *                                      fm_uint16 *
 *                                      fm_uint32 *
 *                                      fm_uint64 *
 *                                      fm_byte *
 *                                      fm_bool *
 *
 *****************************************************************************/

%define FM_SWIG_REFERENCE_TYPEMAP(p5_type, fm_type)
    FM_SWIG_REFERENCE_TYPEMAP_IN(p5_type, , fm_type);
    FM_SWIG_REFERENCE_TYPEMAP_MEMBERIN(p5_type, , fm_type);
    FM_SWIG_REFERENCE_TYPEMAP_ARGOUT(p5_type, , fm_type);
%enddef /* FM_SWIG_REFERENCE_TYPEMAP */

%define FM_SWIG_REFERENCE_TYPEMAP_IV(fm_type)
    FM_SWIG_REFERENCE_TYPEMAP(IV, fm_type);
%enddef /* FM_SWIG_REFERENCE_TYPEMAP_IV */

%define FM_SWIG_REFERENCE_TYPEMAP_UV(fm_type)
    FM_SWIG_REFERENCE_TYPEMAP(UV, fm_type);
%enddef /* FM_SWIG_REFERENCE_TYPEMAP_UV */

%define FM_SWIG_REFERENCE_TYPEMAP_UV64(fm_type)
    FM_SWIG_REFERENCE_TYPEMAP(UV64, fm_uint64);
%enddef /* FM_SWIG_REFERENCE_TYPEMAP_UV64 */

FM_SWIG_REFERENCE_TYPEMAP_IV(fm_int);
FM_SWIG_REFERENCE_TYPEMAP_IV(fm_int16);
FM_SWIG_REFERENCE_TYPEMAP_IV(fm_int32);
FM_SWIG_REFERENCE_TYPEMAP_UV(fm_uint);
FM_SWIG_REFERENCE_TYPEMAP_UV(fm_uint16);
FM_SWIG_REFERENCE_TYPEMAP_UV(fm_uint32);
FM_SWIG_REFERENCE_TYPEMAP_UV64(fm_uint64);
FM_SWIG_REFERENCE_TYPEMAP_UV(fm_byte);
FM_SWIG_REFERENCE_TYPEMAP_UV(fm_bool);

/** typemap(in,memberin,argout,freearg) const fm_byte *
 *
 *****************************************************************************/

%define FM_SWIG_CONST_REFERENCE_TYPEMAP(p5_type, fm_type)
    FM_SWIG_REFERENCE_TYPEMAP_IN(p5_type, const, fm_type);
    FM_SWIG_REFERENCE_TYPEMAP_MEMBERIN(p5_type, const, fm_type);
%typemap(argout) const fm_type *
%{
    /* $1_type is declared as being constant. It is therefore not necessary to
     * apply any typemap(argout) transformations to the associated variable(s).
     */
%}
%enddef /* FM_SWIG_CONST_REFERENCE_TYPEMAP */

%define FM_SWIG_CONST_REFERENCE_TYPEMAP_UV(fm_type)
    FM_SWIG_CONST_REFERENCE_TYPEMAP(UV, fm_type);
%enddef /* FM_SWIG_CONST_REFERENCE_TYPEMAP_UV */

FM_SWIG_CONST_REFERENCE_TYPEMAP_UV(fm_byte);

/** typemap (in) char *
 *
 ****************************************************************************/

%typemap(in,fragment=SWIG_AsPtr_frag(char)) char * (int status)
{
    /* typemap(in) $1_type */
    status = SWIG_AsPtr(char)($input, &($1));
    if (!SWIG_IsOK(status))
    {
        warn("%s: Argument $argnum is not an ASCII SCALAR value"
             " or a reference to an ASCII SCALAR value",
             __func__);
        SWIG_fail;
    }
}

%typemap(freearg) char *
{
}

/** typemap (in) fm_voidptr *
 *
 ****************************************************************************/
%typemap(in) fm_voidptr *
%{
do {
    fm_voidptrW     *ptrW;
    swig_type_info  *type = $descriptor(fm_voidptrW *);

    if (SWIG_ConvertPtr($input, (void **) &ptrW, type, 0) == SWIG_ERROR)
    {
        croak("%s: Argument $argnum is not a SDK::fm_voidptrW", __func__);
    }
    $1 = &(ptrW->ptr);
} while (0);
%}

/** typemap(in,memberin,argout,freearg) fm_arpEntry *
 *                                      fm_macAddressEntry *
 *                                      fm_routeEntry *
 *                                      fm_stormAction *
 *                                      fm_stormCondition *
 *
 *****************************************************************************/
%define FM_SWIG_REFERENCE_TYPEMAP_RV(fm_type)
    FM_SWIG_REFERENCE_TYPEMAP_IN(RV, , fm_type);
    FM_SWIG_REFERENCE_TYPEMAP_MEMBERIN(RV, , fm_type);
    FM_SWIG_REFERENCE_TYPEMAP_ARGOUT(RV, , fm_type);
/* See also SWIG-1.3 Documentation, Section 13.5.  */
%feature("action") fm_type ## ::~ ## fm_type {
    free(arg1);
    arg1 = NULL;
};
%enddef /* FM_SWIG_REFERENCE_TYPEMAP_RV */

FM_SWIG_REFERENCE_TYPEMAP_RV(fm_arpEntry);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_macAddressEntry);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_routeEntry);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_stormAction);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_stormCondition);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_sourceMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_pausePacingTime);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_protocolMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_l4PortMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_macMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_ethTypeValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_ipLengthMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_LBGDistributionMapRange);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_ipAddrMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_vlanMapperValue);
FM_SWIG_REFERENCE_TYPEMAP_RV(fm_aclRulePolicerInfo);

/** typemap(in,argout,freearg) fm_portCounters *
 *
 *****************************************************************************/
/* Prevent any fm_portCounters struct wrappers from being generated.  */
%ignore _fm_portCounters;

%typemap(in) fm_portCounters *
%{
do {
    $1 = (fm_portCounters *) malloc(sizeof(fm_portCounters));
} while (0);
%}

%typemap(argout) fm_portCounters *
%{
do {
    HV              *hv;
    fm_portCounters *counters;
    fm_status       status;
    fm_uint64       mask, variant;

    /* The return status of the API function call is stored in the first
     * argument on the XSUB stack.  */
    status = (fm_status) SvIV(ST(0));
    /* The fm_portCounters C structure is only properly initialized and set iff
     * the API function call completed successfully.  */
    if (status == FM_OK)
    {
        counters = $1;
        fmSWIGCreatePortCountersStructure($argnum, $input,
                                          counters->cntVersion);
        hv = (HV *) SvRV(SvRV($input));
        variant = counters->cntVersion;
        mask = 1ULL;
        while (variant > 0ULL)
        {
            switch (variant & mask)
            {
                /* FM6000 related statistics counters.  */
                case 8ULL:
                    fmSWIGSetCounter(hv, counters, cntRxCBPausePkts);
                    fmSWIGSetCounter(hv, counters, cntRxCBPauseOctets);
                    fmSWIGSetCounter(hv, counters, cntSTPIngressDropsPkts);
                    fmSWIGSetCounter(hv, counters, cntSTPEgressDropsPkts);
                    fmSWIGSetCounter(hv, counters, cntRxFramingErrorPkts);
                    fmSWIGSetCounter(hv, counters, cntRxFramingErrorOctets);
                    fmSWIGSetCounter(hv, counters, cntTxFramingErrorPkts);
                    fmSWIGSetCounter(hv, counters, cntTxFramingErrorOctets);
                    fmSWIGSetCounter(hv, counters, cntTxOutOfMemErrPkts);
                    fmSWIGSetCounter(hv, counters, cntTxOutOfMemErrOctets);
                    fmSWIGSetCounter(hv, counters, cntTxUnrepairEccPkts);
                    fmSWIGSetCounter(hv, counters, cntTxUnrepairEccOctets);
                    fmSWIGSetCounter(hv, counters, cntTxLoopbackPkts);
                    fmSWIGSetCounter(hv, counters, cntTxLoopbackOctets);
                    fmSWIGSetCounter(hv, counters, cntGlortSwitchedPkts);
                    fmSWIGSetCounter(hv, counters, cntGlortRoutedPkts);
                    fmSWIGSetCounter(hv, counters, cntLoopbackDropsPkts);
                    fmSWIGSetCounter(hv, counters, cntInvalidDropPkts);
                    fmSWIGSetCounter(hv, counters, cntGlobalWMDropPkts);
                    fmSWIGSetCounter(hv, counters, cntRXMPDropPkts);
                    fmSWIGSetCounter(hv, counters, cntRxHogDropPkts);
                    fmSWIGSetCounter(hv, counters, cntTxHogDropPkts);
                    fmSWIGSetCounter(hv, counters, cntOtherPkts);
                    fmSWIGSetCounter(hv, counters, cntTxUcstPktsNonIP);
                    fmSWIGSetCounter(hv, counters, cntTxBcstPktsNonIP);
                    fmSWIGSetCounter(hv, counters, cntTxMcstPktsNonIP);
                    fmSWIGSetCounter(hv, counters, cntTxUcstPktsIP);
                    fmSWIGSetCounter(hv, counters, cntTxBcstPktsIP);
                    fmSWIGSetCounter(hv, counters, cntTxMcstPktsIP);
                    fmSWIGSetCounter(hv, counters, cntTxMirrorPkts);
                    fmSWIGSetCounter(hv, counters, cntParseErrDropPkts);
                    fmSWIGSetCounter(hv, counters, cntParityErrorPkts);
                    fmSWIGSetCounter(hv, counters, cntTrappedPkts);
                    fmSWIGSetCounter(hv, counters, cntFFUDropPkts);
                    fmSWIGSetCounter(hv, counters, cntPolicerDropPkts);
                    fmSWIGSetCounter(hv, counters, cntFloodControlDropPkts);
                    fmSWIGSetCounter(hv, counters, cntTxFCSErrDropPkts);
                    fmSWIGSetCounter(hv, counters, cntCodeErrors);
                    fmSWIGSetCounter(hv, counters, timestamp);
		            fmSWIGSetCounter(hv, counters, cntTxCMDropPkts);
                    break;

                /* FM4000 related statistics counters.  */
                case 4ULL:
                    fmSWIGSetCounter(hv, counters, cntRxCBPausePkts);
                    fmSWIGSetCounter(hv, counters, cntRxFrameSizeErrors);
                    fmSWIGSetCounter(hv, counters, cntTxLoopbackPkts);
                    fmSWIGSetCounter(hv, counters, cntTxErrorOctets);
                    fmSWIGSetCounter(hv, counters, cntSpeciallyHandledPkts);
                    fmSWIGSetCounter(hv, counters, cntParseErrDropPkts);
                    fmSWIGSetCounter(hv, counters, cntParityErrorPkts);
                    fmSWIGSetCounter(hv, counters, cntTrappedPkts);
                    fmSWIGSetCounter(hv, counters, cntPauseDropPkts);
                    fmSWIGSetCounter(hv, counters, cntGlortMissDropPkts);
                    fmSWIGSetCounter(hv, counters, cntFFUDropPkts);
                    fmSWIGSetCounter(hv, counters, cntPolicerDropPkts);
                    fmSWIGSetCounter(hv, counters, cntCmPrivDropPkts);
                    fmSWIGSetCounter(hv, counters, cntSmp0DropPkts);
                    fmSWIGSetCounter(hv, counters, cntSmp1DropPkts);
                    fmSWIGSetCounter(hv, counters, cntRxHog0DropPkts);
                    fmSWIGSetCounter(hv, counters, cntRxHog1DropPkts);
                    fmSWIGSetCounter(hv, counters, cntTxHog0DropPkts);
                    fmSWIGSetCounter(hv, counters, cntTxHog1DropPkts);
                    fmSWIGSetCounter(hv, counters, cntRateLimit0DropPkts);
                    fmSWIGSetCounter(hv, counters, cntRateLimit1DropPkts);
                    fmSWIGSetCounter(hv, counters, cntTriggerDropPkts);
                    fmSWIGSetCounter(hv, counters, cntTriggerRedirPkts);
                    fmSWIGSetCounter(hv, counters, timestamp);
                   break;
                /* FM4000 TCP/IP related statistics counters.  */
                case 2ULL:
                    fmSWIGSetCounter(hv, counters, cntRxUcstPktsNonIP);
                    fmSWIGSetCounter(hv, counters, cntRxUcstPktsIPv4);
                    fmSWIGSetCounter(hv, counters, cntRxUcstPktsIPv6);
                    fmSWIGSetCounter(hv, counters, cntRxBcstPktsNonIP);
                    fmSWIGSetCounter(hv, counters, cntRxBcstPktsIPv4);
                    fmSWIGSetCounter(hv, counters, cntRxBcstPktsIPv6);
                    fmSWIGSetCounter(hv, counters, cntRxMcstPktsNonIP);
                    fmSWIGSetCounter(hv, counters, cntRxMcstPktsIPv4);
                    fmSWIGSetCounter(hv, counters, cntRxMcstPktsIPv6);
                    fmSWIGSetCounter(hv, counters, cntRxOctetsNonIp);
                    fmSWIGSetCounter(hv, counters, cntRxOctetsIPv4);
                    fmSWIGSetCounter(hv, counters, cntRxOctetsIPv6);
                    fmSWIGSetCounter(hv, counters, cntTTLDropPkts);
                    break;
                /* FM2000 related statistics counters.  */
                case 1ULL:
                    fmSWIGSetCounter(hv, counters, cntRxCMDropPkts);
                    fmSWIGSetCounter(hv, counters, cntTxCMDropPkts);
                    fmSWIGSetCounter(hv, counters, cntReservedTrapPkts);
                    fmSWIGSetCounter(hv, counters, cntTriggerMirroredPkts);
                    fmSWIGSetCounter(hv, counters, cntBroadcastDropPkts);
                    fmSWIGSetCounter(hv, counters, cntDLFDropPkts);
                    fmSWIGSetCounter(hv, counters, timestamp);
                    break;
                default:
                    break;
            }
            variant = variant & ~mask;
            mask = mask << 1;
        }
        fmSWIGSetCounter(hv, counters, cntRxUcstPkts);
        fmSWIGSetCounter(hv, counters, cntRxBcstPkts);
        fmSWIGSetCounter(hv, counters, cntRxMcstPkts);
        fmSWIGSetCounter(hv, counters, cntRxPausePkts);
        fmSWIGSetCounter(hv, counters, cntRxFCSErrors);
        fmSWIGSetCounter(hv, counters, cntRxSymbolErrors);
        fmSWIGSetCounter(hv, counters, cntRxMinTo63Pkts);
        fmSWIGSetCounter(hv, counters, cntRx64Pkts);
        fmSWIGSetCounter(hv, counters, cntRx65to127Pkts);
        fmSWIGSetCounter(hv, counters, cntRx128to255Pkts);
        fmSWIGSetCounter(hv, counters, cntRx256to511Pkts);
        fmSWIGSetCounter(hv, counters, cntRx512to1023Pkts);
        fmSWIGSetCounter(hv, counters, cntRx1024to1522Pkts);
        fmSWIGSetCounter(hv, counters, cntRx1523to2047Pkts);
        fmSWIGSetCounter(hv, counters, cntRx2048to4095Pkts);
        fmSWIGSetCounter(hv, counters, cntRx4096to8191Pkts);
        fmSWIGSetCounter(hv, counters, cntRx8192to10239Pkts);
        fmSWIGSetCounter(hv, counters, cntRx10240toMaxPkts);
        fmSWIGSetCounter(hv, counters, cntRxFragmentPkts);
        fmSWIGSetCounter(hv, counters, cntRxUndersizedPkts);
        fmSWIGSetCounter(hv, counters, cntRxJabberPkts);
        fmSWIGSetCounter(hv, counters, cntRxOversizedPkts);
        fmSWIGSetCounter(hv, counters, cntRxGoodOctets);
        fmSWIGSetCounter(hv, counters, cntRxBadOctets);
        fmSWIGSetIndexedCounter(hv, counters, cntRxPriorityPkts, 16);
        fmSWIGSetCounter(hv, counters, cntRxInvalidPriorityPkts);
        fmSWIGSetIndexedCounter(hv, counters, cntRxPriorityOctets, 16);
        fmSWIGSetCounter(hv, counters, cntRxInvalidPriorityOctets);
        fmSWIGSetIndexedCounter(hv, counters, cntTxPriorityOctets, 16)
        fmSWIGSetCounter(hv, counters, cntTxUcstPkts);
        fmSWIGSetCounter(hv, counters, cntTxBcstPkts);
        fmSWIGSetCounter(hv, counters, cntTxMcstPkts);
        fmSWIGSetCounter(hv, counters, cntTxPausePkts);
        fmSWIGSetCounter(hv, counters, cntTxFCSErroredPkts);
        fmSWIGSetCounter(hv, counters, cntTxErrorDropPkts);
        fmSWIGSetCounter(hv, counters, cntTxTimeOutPkts);
        fmSWIGSetCounter(hv, counters, cntTxMinTo63Pkts);
        fmSWIGSetCounter(hv, counters, cntTx64Pkts);
        fmSWIGSetCounter(hv, counters, cntTx65to127Pkts);
        fmSWIGSetCounter(hv, counters, cntTx128to255Pkts);
        fmSWIGSetCounter(hv, counters, cntTx256to511Pkts);
        fmSWIGSetCounter(hv, counters, cntTx512to1023Pkts);
        fmSWIGSetCounter(hv, counters, cntTx1024to1522Pkts);
        fmSWIGSetCounter(hv, counters, cntTx1523to2047Pkts);
        fmSWIGSetCounter(hv, counters, cntTx2048to4095Pkts);
        fmSWIGSetCounter(hv, counters, cntTx4096to8191Pkts);
        fmSWIGSetCounter(hv, counters, cntTx8192to10239Pkts);
        fmSWIGSetCounter(hv, counters, cntTx10240toMaxPkts);
        fmSWIGSetCounter(hv, counters, cntTxOctets);
        fmSWIGSetCounter(hv, counters, cntFIDForwardedPkts);
        fmSWIGSetCounter(hv, counters, cntFloodForwardedPkts);
        fmSWIGSetCounter(hv, counters, cntSTPDropPkts);
        fmSWIGSetCounter(hv, counters, cntSecurityViolationPkts);
        fmSWIGSetCounter(hv, counters, cntVLANTagDropPkts);
        fmSWIGSetCounter(hv, counters, cntVLANIngressBVPkts);
        fmSWIGSetCounter(hv, counters, cntVLANEgressBVPkts);
        fmSWIGSetCounter(hv, counters, cntTriggerDropRedirPkts);
        fmSWIGSetCounter(hv, counters, cntUnderrunPkts);
        fmSWIGSetCounter(hv, counters, cntOverrunPkts);
        fmSWIGSetCounter(hv, counters, cntCorruptedPkts);
    }
} while (0);
%}

%typemap(freearg) fm_portCounters *
%{
    free($1);
%}

/** typemap(in,argout,freearg) fm_routeEntry *
 *
 *****************************************************************************/


/** typemap(in,argout,freearg) fm_switchCounters *
 *
 *****************************************************************************/
/* Prevent any fm_switchCounters struct wrappers from being generated.  */
%ignore _fm_switchCounters;

%typemap(in) fm_switchCounters *
%{
do {
    $1 = (fm_switchCounters *) malloc(sizeof(fm_switchCounters));
} while (0);
%}

%typemap(argout) fm_switchCounters *
%{
do {
    HV                  *hash;
    fm_switchCounters   *counters;
    fm_status           status;

    /* The return status of the API function call is stored in the first
     * argument on the XSUB stack.  */
    status = (fm_status) SvIV(ST(0));
    /* The fm_portCounters C structure is only properly initialized and set iff
     * the API function call completed successfully.  */
    if (status == FM_OK)
    {
        counters = $1;
        fmSWIGCreateSwitchCountersStructure($argnum, $input,
                                            counters->cntVersion);
        hash = (HV *) SvRV(SvRV($input));
        switch (counters->cntVersion)
        {
            case 1:
                fmSWIGSetCounter(hash, counters, cntGlobalLowDropPkts);
                fmSWIGSetCounter(hash, counters, cntGlobalHighDropPkts);
                fmSWIGSetCounter(hash, counters, cntGlobalPrivilegeDropPkts);
                break;
            default:
                croak("%s: Version %lld: Unsupported fm_switchCounters structure",
                      __func__, counters->cntVersion);
        }
    }
} while (0);
%}

%typemap(freearg) fm_switchCounters *
%{
    free($1);
%}

/** typemap(in,argout,freearg) fm_vlanCounters *
 *
 *****************************************************************************/
/* Prevent any fm_vlanCounters struct wrappers from being generated.  */
%ignore _fm_vlanCounters;

%typemap(in) fm_vlanCounters *
%{
do {
    $1 = (fm_vlanCounters *) malloc(sizeof(fm_vlanCounters));
} while (0);
%}

%typemap(argout) fm_vlanCounters *
%{
do {
    HV              *hash;
    fm_vlanCounters *counters;
    fm_status       status;

    /* The return status of the API function call is stored in the first
     * argument on the XSUB stack.  */
    status = (fm_status) SvIV(ST(0));
    /* The fm_vlanCounters C structure is only properly initialized and set iff
     * the API function call completed successfully.  */
    if (status == FM_OK)
    {
        counters = $1;
        fmSWIGCreateVLANCountersStructure($argnum, $input,
                                          counters->cntVersion);
        hash = (HV *) SvRV(SvRV($input));
        switch (counters->cntVersion)
        {
            case 1:
            case 4:
                fmSWIGSetCounter(hash, counters, cntUcstOctets);
                fmSWIGSetCounter(hash, counters, cntXcstOctets);
                fmSWIGSetCounter(hash, counters, cntUcstPkts);
                fmSWIGSetCounter(hash, counters, cntXcstPkts);
                break;
            case 8:
                fmSWIGSetCounter(hash, counters, cntRxUcstOctets);
                fmSWIGSetCounter(hash, counters, cntRxMcstOctets);
                fmSWIGSetCounter(hash, counters, cntRxBcstOctets);
                fmSWIGSetCounter(hash, counters, cntRxDropOctets);
                fmSWIGSetCounter(hash, counters, cntRxUcstPkts);
                fmSWIGSetCounter(hash, counters, cntRxMcstPkts);
                fmSWIGSetCounter(hash, counters, cntRxBcstPkts);
                fmSWIGSetCounter(hash, counters, cntRxDropPkts);
                fmSWIGSetCounter(hash, counters, cntTxUcstOctets);
                fmSWIGSetCounter(hash, counters, cntTxMcstOctets);
                fmSWIGSetCounter(hash, counters, cntTxBcstOctets);
                fmSWIGSetCounter(hash, counters, cntTxDropOctets);
                fmSWIGSetCounter(hash, counters, cntTxUcstPkts);
                fmSWIGSetCounter(hash, counters, cntTxMcstPkts);
                fmSWIGSetCounter(hash, counters, cntTxBcstPkts);
                fmSWIGSetCounter(hash, counters, cntTxDropPkts);
                break;
            default:
                croak("%s: Version %lld: Unsupported fm_vlanCounters structure",
                      __func__, counters->cntVersion);
        }
    }
} while (0);
%}

%typemap(freearg) fm_vlanCounters *
%{
    free($1);
%}

/** typemap(constcode) unsigned long long
 *
 * \desc        maps a 64-bit macro definition to a Math::BigInt Perl object
 *
 *****************************************************************************/
%typemap(constcode) unsigned long long
%{
do {
    SV  *sv;

    sv = get_sv((char *) SWIG_prefix "$1_name", TRUE);
    fmSWIGSetScalarUINT64(sv, $1);
} while (0);
%}

/** typemap(in,argout,freearg) void *
 *
 *****************************************************************************/
%typemap(in,fragment=SWIG_AsPtr_frag(void)) void *
{
    if (!SWIG_IsOK(SWIG_AsPtr(void)($input, &($1))))
    {
        FM_SWIG_ABORT(FM_SWIG_errSv);
    }
}

%typemap(argout,fragment=SWIG_FromPtr_frag(void)) void *
{
    if (!SWIG_IsOK(SWIG_FromPtr(void)($input, $1)))
    {
        FM_SWIG_ABORT(FM_SWIG_errSv);
    }
}

/** typemap(in,argout) enum SWIGTYPE *
 *
 *****************************************************************************/
%apply fm_int * { enum SWIGTYPE * };

/******************************************************************************
 * Extensions
 *****************************************************************************/

%extend _fm_buffer {
    ~fm_buffer()
    {
        free((void *) $self->data);
        free((void *) $self);
    }
}

%extend _fm_ffuSliceInfo {
    ~fm_ffuSliceInfo()
    {
        free((void *) $self->selects);
        free((void *) $self);
    }
}

%extend _fm_scatterGatherListEntry {
    ~fm_scatterGatherListEntry()
    {
        free((void *) $self->data);
        free((void *) $self);
    }
}
