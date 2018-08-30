/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/*****************************************************************************
 * File:            sdk_fragments.i
 * Creation Date:   12/20/07
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
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

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#ifdef %descriptor
#   undef %descriptor
#endif
#define %descriptor(Type...) SWIGTYPE_p__ ## #@Type

#define SWIG_FromVal_frag(Type...)      %fragment_name(FromVal, Type)
#define SWIG_FromVal_name(Type...)      %symbol_name(FromVal, Type)
#define SWIG_FromVal_dec(Type...)                                             \
    SWIG_FromVal_name(Type) SWIG_FROM_DECL_ARGS
#define SWIG_FromVal(Type...)                                                 \
    SWIG_FromVal_name(Type) SWIG_FROM_CALL_ARGS

#define SWIG_FromPtr_frag(Type...)      %fragment_name(FromPtr, Type)
#define SWIG_FromPtr_name(Type...)      %symbol_name(FromPtr, Type)
#define SWIG_FromPtr_dec(Type...)                                             \
    SWIG_FromPtr_name(Type) SWIG_AS_DECL_ARGS
#define SWIG_FromPtr(Type...)                                                 \
    SWIG_FromPtr_name(Type) SWIG_AS_CALL_ARGS

#define FM_SWIG_SvFETCH_frag(Type...)   %fragment_name(SvFETCH, Type)
#define FM_SWIG_SvFETCH_name(Type...)   %symbol_name(SvFETCH, Type)
#define FM_SWIG_SvFETCH_dec(Type...)                                          \
    FM_SWIG_SvFETCH_name(Type) SWIG_AS_DECL_ARGS
#define FM_SWIG_SvFETCH(Type...)                                              \
    FM_SWIG_SvFETCH_name(Type) SWIG_AS_CALL_ARGS

#define FM_SWIG_SvSTORE_frag(Type...)   %fragment_name(SvSTORE, Type)
#define FM_SWIG_SvSTORE_name(Type...)   %symbol_name(SvSTORE, Type)
#define FM_SWIG_SvSTORE_dec(Type...)                                          \
    FM_SWIG_SvSTORE_name(Type) SWIG_AS_DECL_ARGS
#define FM_SWIG_SvSTORE(Type...)                                              \
    FM_SWIG_SvSTORE_name(Type) SWIG_AS_CALL_ARGS


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************/
/** fragment("FM_SWIG_Malloc")
 *
 *****************************************************************************/
%fragment("FM_SWIG_Malloc","header") {

/*****************************************************************************/
/** FM_SWIG_Calloc
 *
 * \desc        Allocates memory for an array of \c nmemb elements of \c size
 *              bytes each. The memory is set to zero. The pointer to the
 *              allocated memory is stored in a SWIG::malloc Perl object that
 *              is destroyed when control reverts back to Perl
 *
 * \param[in]   nmemb is the number of elements to be allocated
 *
 * \param[in]   size is the size of each element to be allocated
 *
 * \return      SWIG::malloc Perl object containing a pointer to the allocated
 *              memory
 *
 *****************************************************************************/
SWIGINTERN SV *
FM_SWIG_Calloc(size_t nmemb, size_t size)
{
    HV *  stash;
    SV *  self;
    void *ptr;

    self = sv_newmortal();
    ptr = calloc(nmemb, size);
#ifdef SWIG_DEBUG
    printf("  allocated %p (size: %d B)\n",
           (void *) ptr,
           (int) (nmemb * size));
#endif
    sv_setref_pv(self, NULL, ptr);
    stash = gv_stashpv("SWIG::malloc", GV_ADD);
    sv_bless(self, stash);

    return self;

}   /* end FM_SWIG_Calloc */




/*****************************************************************************/
/** FM_SWIG_Ptr
 *
 * \desc        Retrieves a pointer to the memory space previously allocated
 *              by FM_SWIG_Calloc()
 *
 * \param[in]   sv points to a SWIG::malloc Perl object containing the pointer
 *              to the memory space
 *
 * \return      a pointer to the memory space previously allocated by
 *              FM_SWIG_Calloc()
 *
 *****************************************************************************/
SWIGINTERN void *
FM_SWIG_Ptr(SV *sv)
{
    void *ptr = NULL;

    if (sv_isobject(sv)
        && sv_isa(sv, "SWIG::malloc")
        && SvROK(sv)
        && SvTYPE(SvRV(sv)) == SVt_PVMG)
    {
        if (SvTRUE(SvRV(sv)))
        {
            ptr = (void *) SvIV(SvRV(sv));
        }
    }

    return ptr;

}   /* end FM_SWIG_Ptr */



}




/*****************************************************************************/
/** fragment(FM_SWIG_SvFETCH)
 *
 *****************************************************************************/
%define FM_SWIG_FRAGMENT_SV_FETCH_IV(fm_type)

%fragment(FM_SWIG_SvFETCH_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvFETCH_dec(fm_type)(SV *sv, fm_type *x)
{
    if (SvROK(sv) || !looks_like_number(sv))
    {
        sv_catpv(FM_SWIG_errSv, " is not numeric");
        return SWIG_ERROR;
    }
    *x = (fm_type) SvIV(sv); 
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_FETCH_IV */


%define FM_SWIG_FRAGMENT_SV_FETCH_UV(fm_type)

%fragment(FM_SWIG_SvFETCH_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvFETCH_dec(fm_type)(SV *sv, fm_type *x)
{
    if (SvROK(sv) || !looks_like_number(sv))
    {
        sv_catpv(FM_SWIG_errSv, " is not numeric");
        return SWIG_ERROR;
    }
    *x = (fm_type) SvUV(sv);
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_FETCH_UV */


%define FM_SWIG_FRAGMENT_SV_FETCH_UV64(fm_type)

%fragment(FM_SWIG_SvFETCH_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvFETCH_dec(fm_type)(SV *sv, fm_type *x)
{
    if (!sv_isa(sv, "Math::BigInt"))
    {
        sv_catpv(FM_SWIG_errSv, " is not a Math::BigInt");
        return SWIG_ERROR;
    }
    *x = fmSWIGGetScalarUINT64(sv);
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_FETCH_UV64 */


%define FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_type)

%fragment(FM_SWIG_SvFETCH_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvFETCH_dec(fm_type)(SV *sv, fm_type *x)
{
    swig_type_info *info = %descriptor(fm_type);
    fm_type *       ptr;

    if (!SWIG_IsOK(SWIG_ConvertPtr(sv, (void **) &ptr, info, 0)))
    {
        sv_catpvf(FM_SWIG_errSv, " does not represent a %s", info->str);
        return SWIG_ERROR;
    }
    memcpy((void *) x, (const void *) ptr, sizeof(fm_type));
    return SWIG_OK;
}

SWIGINTERN int
FM_SWIG_SvFETCH_dec(fm_type *)(SV *sv, fm_type **x)
{
    swig_type_info *info = %descriptor(fm_type);

    if (!SWIG_IsOK(SWIG_ConvertPtr(sv, (void **) x, info, 0)))
    {
        sv_catpvf(FM_SWIG_errSv, " does not represent a %s", info->str);
        return SWIG_ERROR;
    }
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_FETCH_RV */




/*****************************************************************************/
/** fragment(FM_SWIG_SvSTORE)
 *
 *****************************************************************************/
%define FM_SWIG_FRAGMENT_SV_STORE_IV(fm_type)

%fragment(FM_SWIG_SvSTORE_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvSTORE_dec(fm_type)(SV *sv, const fm_type *x)
{
    sv_setiv(sv, (IV) (*x));
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_STORE_IV */


%define FM_SWIG_FRAGMENT_SV_STORE_UV(fm_type)

%fragment(FM_SWIG_SvSTORE_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvSTORE_dec(fm_type)(SV *sv, const fm_type *x)
{
    sv_setuv(sv, (UV) (*x));
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_STORE_UV */


%define FM_SWIG_FRAGMENT_SV_STORE_UV64(fm_type)

%fragment(FM_SWIG_SvSTORE_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvSTORE_dec(fm_type)(SV *sv, const fm_type *x)
{
    fmSWIGSetScalarUINT64(sv, *x);
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_STORE_UV64 */


%define FM_SWIG_FRAGMENT_SV_STORE_RV(fm_type)

%fragment(FM_SWIG_SvSTORE_frag(fm_type), "header") {

SWIGINTERN int
FM_SWIG_SvSTORE_dec(fm_type)(SV *sv, const fm_type *x)
{
    swig_type_info *info = %descriptor(fm_type);
    fm_type *       ptr;

    if (!SWIG_IsOK(SWIG_ConvertPtr(sv, (void **) &ptr, info, 0)))
    {
        sv_catpvf(FM_SWIG_errSv, " does not represent a %s", info->str);
        return SWIG_ERROR;
    }
    /* ptr and x might both point to the same memory space.  */
    memmove((void *) ptr, (const void *) x, sizeof(fm_type));
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_SV_STORE_RV */




/*****************************************************************************/
/** fragment(SWIG_AsPtr)
 *
 *****************************************************************************/
%define FM_SWIG_FRAGMENT_ASPTR(p5_type, fm_type)

#if   #p5_type == "IV"
    FM_SWIG_FRAGMENT_SV_FETCH_IV(fm_type);
#elif #p5_type == "UV"
    FM_SWIG_FRAGMENT_SV_FETCH_UV(fm_type);
#elif #p5_type == "UV64"
    FM_SWIG_FRAGMENT_SV_FETCH_UV64(fm_type);
#elif #p5_type == "RV"
    FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_type);
#endif

%fragment(SWIG_AsPtr_frag(fm_type),
          "header",
          fragment=FM_SWIG_SvFETCH_frag(fm_type),
          fragment="FM_SWIG_Malloc") {

SWIGINTERN int
SWIG_AsPtr_dec(fm_type)(SV *sv, fm_type **x)
{
    AV * av;
    SV * ptr;
    SV **entry;
    int  i;
    int  n;
    int  status;

    if (!SvROK(sv))
    {
        sv_catpv(FM_SWIG_errSv, " is not a reference");
        return SWIG_ERROR;
    }
    if (SvTYPE(SvRV(sv)) == SVt_PVAV)
    {
        av = (AV *) SvRV(sv);
        n = (int) av_len(av) + 1;
        ptr = sv_newmortal();
        sv_setsv(ptr, FM_SWIG_Calloc((size_t) n, sizeof(fm_type)));
        *x = (fm_type *) FM_SWIG_Ptr(ptr);
        if (*x == NULL)
        {
            sv_catpv(FM_SWIG_errSv, ": Out of memory");
            return SWIG_ERROR;
        }
        for (i = 0; i < n; i++)
        {
            if ((entry = av_fetch(av, (I32) i, 0)) != NULL)
            {
                if (SvTRUE(*entry))
                {
                    status = FM_SWIG_SvFETCH(fm_type)(*entry, &((*x)[i]));
                    if (!SWIG_IsOK(status))
                    {
                        return status;
                    }
                }
            }
        }
    }
    else
    {
#if #p5_type == "RV"
        if (!SWIG_IsOK((status = FM_SWIG_SvFETCH(fm_type *)(sv, x))))
        {
            return status;
        }
#else
        sv = SvRV(sv);
        ptr = sv_newmortal();
        sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_type)));
        *x = (fm_type *) FM_SWIG_Ptr(ptr);
        if (*x == NULL)
        {
            sv_catpv(FM_SWIG_errSv, ": Out of memory");
            return SWIG_ERROR;
        }
        if (SvTRUE(sv))
        {
            if (!SWIG_IsOK((status = FM_SWIG_SvFETCH(fm_type)(sv, *x))))
            {
                return status;
            }
        }
#endif /* #p5_type == "RV" */
    }
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_ASPTR */




/*****************************************************************************/
/** fragment(SWIG_FromPtr)
 *
 *****************************************************************************/
%define FM_SWIG_FRAGMENT_FROMPTR(p5_type, fm_type)

#if   #p5_type == "IV"
    FM_SWIG_FRAGMENT_SV_STORE_IV(fm_type);
#elif #p5_type == "UV"
    FM_SWIG_FRAGMENT_SV_STORE_UV(fm_type);
#elif #p5_type == "UV64"
    FM_SWIG_FRAGMENT_SV_STORE_UV64(fm_type);
#elif #p5_type == "RV"
    FM_SWIG_FRAGMENT_SV_STORE_RV(fm_type);
#endif

%fragment(SWIG_FromPtr_frag(fm_type),
          "header",
          fragment=FM_SWIG_SvSTORE_frag(fm_type)) {

SWIGINTERN int
SWIG_FromPtr_dec(fm_type)(SV *sv, fm_type *x)
{
    AV * av;
    SV **entry;
    int  i;
    int  n;
    int  status;

    if (sv == NULL || x == NULL)
    {
        /* Either sv or x has been explicitly set to NULL to avoid applying the
         * argout typemap.  */
        return SWIG_OK;
    }
    if (!SvROK(sv))
    {
        sv_catpv(FM_SWIG_errSv, " is not a reference");
        return SWIG_ERROR;
    }
    if (SvTYPE(SvRV(sv)) == SVt_PVAV)
    {
        av = (AV *) SvRV(sv);
        n = (int) av_len(av) + 1;
        for (i = 0; i < n; i++)
        {
            if ((entry = av_fetch(av, (I32) i, 0)) != NULL)
            {
                status = FM_SWIG_SvSTORE(fm_type)(*entry, &(x[i]));
                if (!SWIG_IsOK(status))
                {
                    return status;
                }
            }
        }
    }
    else
    {
#if #p5_type == "RV"
        status = FM_SWIG_SvSTORE(fm_type)(sv, x);
#else
        status = FM_SWIG_SvSTORE(fm_type)(SvRV(sv), x);
#endif /* #p5_type == "RV" */
        if (!SWIG_IsOK(status))
        {
            return status;
        }
    }
    return SWIG_OK;
}

}

%enddef /* FM_SWIG_FRAGMENT_FROMPTR */




/*****************************************************************************/
/** fragment(SWIG_AsVal)
 *
 *****************************************************************************/
%define FM_SWIG_FRAGMENT_ASVAL(p5_type, fm_type)

#if   #p5_type == "IV"
    FM_SWIG_FRAGMENT_SV_FETCH_IV(fm_type);
#elif #p5_type == "UV"
    FM_SWIG_FRAGMENT_SV_FETCH_UV(fm_type);
#elif #p5_type == "UV64"
    FM_SWIG_FRAGMENT_SV_FETCH_UV64(fm_type);
#endif

%fragment(SWIG_AsVal_frag(fm_type),
          "header",
          fragment=FM_SWIG_SvFETCH_frag(fm_type)) {

SWIGINTERN int
SWIG_AsVal_dec(fm_type)(SV *sv, fm_type *x)
{
    return FM_SWIG_SvFETCH(fm_type)(sv, x);
}

}

%enddef /* FM_SWIG_FRAGMENT_ASVAL */




/*****************************************************************************/
/** fragment(SWIG_FromVal)
 *
 *****************************************************************************/
%define FM_SWIG_FRAGMENT_FROMVAL(p5_type, fm_type)

#if   #p5_type == "IV"
    FM_SWIG_FRAGMENT_SV_FETCH_IV(fm_type);
#elif #p5_type == "UV"
    FM_SWIG_FRAGMENT_SV_FETCH_UV(fm_type);
#elif #p5_type == "UV64"
    FM_SWIG_FRAGMENT_SV_FETCH_UV64(fm_type);
#endif

%fragment(SWIG_FromVal_frag(fm_type),
          "header",
          fragment=FM_SWIG_SvSTORE_frag(fm_type)) {

SWIGINTERN SV *
SWIG_FromVal_dec(fm_type)(fm_type x)
{
    SV *result;

    result = sv_newmortal();
    if (!SWIG_IsOK(FM_SWIG_SvSTORE(fm_type)(result, &x)))
    {
        return &PL_sv_undef;
    }
    return result;
}

}

%enddef /* FM_SWIG_FRAGMENT_FROMVAL */




/*****************************************************************************/
/** fragment(AsVal,FromVal) fm_macaddr
 *
 *****************************************************************************/
%fragment(SWIG_AsVal_frag(fm_macaddr), "header") {

SWIGINTERN int
SWIG_AsVal_dec(fm_macaddr)(SV *sv, fm_macaddr *x)
{
    if (!SvPOK(sv))
    {
        sv_catpvf(FM_SWIG_errSv, " is not an ASCII SCALAR value");
        return SWIG_ERROR;
    }
    fmSWIGGetMacAddr(x, SvPV_nolen(sv));
    return SWIG_OK;
}

}


%fragment(SWIG_FromVal_frag(fm_macaddr), "header") {

SWIGINTERN SV*
SWIG_FromVal_dec(fm_macaddr)(fm_macaddr x)
{
    SV * result;
    char buffer[3 * FM_SWIG_N_MACADDR_BYTES];

    result = sv_newmortal();
    fmSWIGSetMacAddr(buffer, sizeof(buffer), x);
    sv_setpv(result, buffer);
    return result;
}

}




/*****************************************************************************/
/** fragment(AsPtr) char *
 *
 *****************************************************************************/
%fragment(SWIG_AsPtr_frag(char), "header") {

SWIGINTERN int
SWIG_AsPtr_dec(char)(SV *sv, char **x)
{
    if ((!SvROK(sv) && !SvPOK(sv)) || (SvROK(sv) && !SvPOK(SvRV(sv))))
    {
        return SWIG_ERROR;
    }
    *x = SvPV_nolen(!SvROK(sv) ? sv : SvRV(sv));
    return SWIG_OK;
}

}




/*****************************************************************************/
/** fragment(AsPtr,FromPtr) void *
 *
 *****************************************************************************/

%fragment(SWIG_AsPtr_frag(void),
          "header",
          fragment=FM_SWIG_SvFETCH_frag(fm_bitArray),
          fragment=FM_SWIG_SvFETCH_frag(fm_fibmSwitchConfig),
          fragment=FM_SWIG_SvFETCH_frag(fm_ipAddr),
          fragment=FM_SWIG_SvFETCH_frag(fm_mtuEntry),
          fragment=FM_SWIG_SvFETCH_frag(fm_policerBankDropMask),
          fragment=FM_SWIG_SvFETCH_frag(fm_aclRulePolicerInfo),
          fragment=FM_SWIG_SvFETCH_frag(fm_ffuSliceAllocations),
          fragment=FM_SWIG_SvFETCH_frag(fm_sourceMapperValue),
          fragment=SWIG_AsPtr_frag(fm_sourceMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_pausePacingTime),
          fragment=SWIG_AsPtr_frag(fm_pausePacingTime),
          fragment=FM_SWIG_SvFETCH_frag(fm_protocolMapperValue),
          fragment=SWIG_AsPtr_frag(fm_protocolMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_l4PortMapperValue),
          fragment=SWIG_AsPtr_frag(fm_l4PortMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_macMapperValue),
          fragment=SWIG_AsPtr_frag(fm_macMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_ethTypeValue),
          fragment=SWIG_AsPtr_frag(fm_ethTypeValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_ipLengthMapperValue),
          fragment=SWIG_AsPtr_frag(fm_ipLengthMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_ipAddrMapperValue),
          fragment=SWIG_AsPtr_frag(fm_ipAddrMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_vlanMapperValue),
          fragment=SWIG_AsPtr_frag(fm_vlanMapperValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_LBGDistributionMapRange),
          fragment=SWIG_AsPtr_frag(fm_uint64),
          fragment=FM_SWIG_SvFETCH_frag(fm_uint64),
          fragment=FM_SWIG_SvFETCH_frag(fm_uint32),
          fragment=FM_SWIG_SvFETCH_frag(fm_uint16),
          fragment=FM_SWIG_SvFETCH_frag(fm_uint),
          fragment=SWIG_AsPtr_frag(fm_int),
          fragment=FM_SWIG_SvFETCH_frag(fm_int32),
          fragment=FM_SWIG_SvFETCH_frag(fm_int),
          fragment=FM_SWIG_SvFETCH_frag(fm_bool),
          fragment=FM_SWIG_SvFETCH_frag(fm_hashRotationValue),
          fragment=FM_SWIG_SvFETCH_frag(fm_L2HashKey),
          fragment=FM_SWIG_SvFETCH_frag(fm_L2HashRot),
          fragment=FM_SWIG_SvFETCH_frag(fm_L3HashConfig),
          fragment=FM_SWIG_SvFETCH_frag(fm_aclCompilerTryAlloc),
          fragment="FM_SWIG_Malloc") {

SWIGINTERN int
SWIG_AsPtr_dec(void)(SV *sv, void **x)
{
    fmSWIG_void     pv;
    fmSWIG_dataType type = FM_SWIG_TYPE_UNKNOWN;
    SV **           value;
    SV *            ptr;
    size_t          length;
    size_t          size;
    int             status;

    if (!SWIG_IsOK((status = fmSWIGVerifyVoidStructure(sv, &pv))))
    {
        return status;
    }

    /* Process the 'type' and 'value' keys.  */
    ptr = sv_newmortal();
    if ((value = fmSWIGGetHashKey(FM_SWIG_void, pv.type)) != NULL)
    {
        type = (fmSWIG_dataType) SvIV(*value);
    }
    switch (type)
    {
        case FM_SWIG_TYPE_BIT_ARRAY:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_bitArray)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_bitArray)(pv.value, (fm_bitArray *) *x);

        case FM_SWIG_TYPE_FIBM_SWITCH_CONFIG:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_fibmSwitchConfig)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_fibmSwitchConfig)(pv.value,
                                                        (fm_fibmSwitchConfig *) *x);

        case FM_SWIG_TYPE_IP_ADDR:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_ipAddr)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_ipAddr)(pv.value, (fm_ipAddr *) *x);

        case FM_SWIG_TYPE_MTU_ENTRY:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_mtuEntry)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_mtuEntry)(pv.value, (fm_mtuEntry *) *x);

        case FM_SWIG_TYPE_POLICER_BANK_DROP_MASK:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_policerBankDropMask)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_policerBankDropMask)(pv.value, (fm_policerBankDropMask *) *x);

        case FM_SWIG_TYPE_ACL_RULE_POLICER_INFO_VALUE:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_aclRulePolicerInfo)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_aclRulePolicerInfo)(pv.value, (fm_aclRulePolicerInfo *) *x);

        case FM_SWIG_TYPE_FFU_SLICE_ALLOCATIONS:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_ffuSliceAllocations)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_ffuSliceAllocations)(pv.value,
                                                        (fm_ffuSliceAllocations *) *x);

        case FM_SWIG_TYPE_MAPPER_SOURCE_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_sourceMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_sourceMapperValue)(pv.value,
                                                        (fm_sourceMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_SOURCE_VALUE_ARRAY:
            return SWIG_AsPtr(fm_sourceMapperValue)(pv.value, (fm_sourceMapperValue **) x);

        case FM_SWIG_TYPE_PAUSE_PACING_TIME:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_pausePacingTime)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_pausePacingTime)(pv.value, (fm_pausePacingTime *) *x);

        case FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_protocolMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_protocolMapperValue)(pv.value,
                                                        (fm_protocolMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE_ARRAY:
            return SWIG_AsPtr(fm_protocolMapperValue)(pv.value, (fm_protocolMapperValue **) x);

        case FM_SWIG_TYPE_MAPPER_L4PORT_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_l4PortMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_l4PortMapperValue)(pv.value,
                                                        (fm_l4PortMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_L4PORT_VALUE_ARRAY:
            return SWIG_AsPtr(fm_l4PortMapperValue)(pv.value, (fm_l4PortMapperValue **) x);

        case FM_SWIG_TYPE_MAPPER_MAC_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_macMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_macMapperValue)(pv.value,
                                                        (fm_macMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_MAC_VALUE_ARRAY:
            return SWIG_AsPtr(fm_macMapperValue)(pv.value, (fm_macMapperValue **) x);

        case FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_ethTypeValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_ethTypeValue)(pv.value,
                                                        (fm_ethTypeValue *) *x);

        case FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE_ARRAY:
            return SWIG_AsPtr(fm_ethTypeValue)(pv.value, (fm_ethTypeValue **) x);

        case FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_ipLengthMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_ipLengthMapperValue)(pv.value,
                                                        (fm_ipLengthMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE_ARRAY:
            return SWIG_AsPtr(fm_ipLengthMapperValue)(pv.value, (fm_ipLengthMapperValue **) x);

        case FM_SWIG_TYPE_MAPPER_IPADDR_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_ipAddrMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_ipAddrMapperValue)(pv.value,
                                                        (fm_ipAddrMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_IPADDR_VALUE_ARRAY:
            return SWIG_AsPtr(fm_ipAddrMapperValue)(pv.value, (fm_ipAddrMapperValue **) x);

        case FM_SWIG_TYPE_MAPPER_VLAN_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_vlanMapperValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_vlanMapperValue)(pv.value,
                                                      (fm_vlanMapperValue *) *x);

        case FM_SWIG_TYPE_MAPPER_VLAN_VALUE_ARRAY:
            return SWIG_AsPtr(fm_vlanMapperValue)(pv.value, (fm_vlanMapperValue **) x);

        case FM_SWIG_TYPE_LBG_DISTRIBUTION_MAP_RANGE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_LBGDistributionMapRange)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_LBGDistributionMapRange)(pv.value,
                                                              (fm_LBGDistributionMapRange *) *x);

        case FM_SWIG_TYPE_MAC_ADDR:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_macaddr)));
            *x = FM_SWIG_Ptr(ptr);
            fmSWIGGetMacAddr((fm_macaddr *) *x, SvPV_nolen(pv.value));
            break;

        case FM_SWIG_TYPE_UINT64_ARRAY:
             /*return SWIG_AsPtr(fm_uint64)(pv.value, (fm_uint64 **) x);*/
             return      SWIG_AsPtr_fm_uint64 SWIG_PERL_CALL_ARGS_2(pv.value, (fm_uint64 **) x);

        case FM_SWIG_TYPE_UINT64:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_uint64)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_uint64)(pv.value, (fm_uint64 *) *x);

        case FM_SWIG_TYPE_UINT32:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_uint32)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_uint32)(pv.value, (fm_uint32 *) *x);

        case FM_SWIG_TYPE_UINT16:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_uint16)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_uint16)(pv.value, (fm_uint16 *) *x);

        case FM_SWIG_TYPE_UINT:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_uint)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_uint)(pv.value, (fm_uint *) *x);

        case FM_SWIG_TYPE_INT_ARRAY:
            return SWIG_AsPtr(fm_int)(pv.value, (fm_int **) x);

        case FM_SWIG_TYPE_INT32:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_int32)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_int32)(pv.value, (fm_int32 *) *x);

        case FM_SWIG_TYPE_INT:
        case FM_SWIG_TYPE_ETH_MODE:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_int)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_int)(pv.value, (fm_int *) *x);

        case FM_SWIG_TYPE_BOOL:
            sv_setsv(ptr, FM_SWIG_Calloc((size_t) 1, sizeof(fm_bool)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_bool)(pv.value, (fm_bool *) *x);

        case FM_SWIG_TYPE_CHAR:
            length = (size_t) SvLEN(pv.value);
            size = FM_API_ATTR_TEXT_MAX_LENGTH;

            sv_setsv(ptr, FM_SWIG_Calloc(size, sizeof(fm_char)));
            *x = FM_SWIG_Ptr(ptr);
            if (length >= size)
            {
                warn("Copying %zu characters of a %zu character buffer",
                     size - 1,
                     length);
                length = size;
            }
            strncpy((fm_char *) *x, SvPV_nolen(pv.value), length - 1);
            *(((fm_char *) *x) + (length - 1) * sizeof(fm_char)) = '\0';
            break;

        case FM_SWIG_TYPE_HASH_ROTATION_VALUE:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_hashRotationValue)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_hashRotationValue)(pv.value,
                                                        (fm_hashRotationValue *) *x);

        case FM_SWIG_TYPE_L2_HASH_KEY:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_L2HashKey)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_L2HashKey)(pv.value,
                                                (fm_L2HashKey *) *x);

        case FM_SWIG_TYPE_L2_HASH_ROT:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_L2HashRot)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_L2HashRot)(pv.value,
                                                (fm_L2HashRot *) *x);

        case FM_SWIG_TYPE_L3_HASH_CONFIG:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_L3HashConfig)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_L3HashConfig)(pv.value,
                                                   (fm_L3HashConfig *) *x);

        case FM_SWIG_TYPE_ACL_COMPILER_TRY_ALLOC:
            sv_setsv(ptr,
                     FM_SWIG_Calloc((size_t) 1, sizeof(fm_aclCompilerTryAlloc)));
            *x = FM_SWIG_Ptr(ptr);
            return FM_SWIG_SvFETCH(fm_aclCompilerTryAlloc)(pv.value,
                                                          (fm_aclCompilerTryAlloc *) *x);

        default:
            sv_catpvf(FM_SWIG_errSv, ": %s: Invalid `type' key", pv.type);
            return SWIG_ERROR;

    }   /* end switch (type) */

    return SWIG_OK;
}

}   /* end fragment(SWIG_AsPtr_frag(void)) */


%fragment(SWIG_FromPtr_frag(void),
          "header",
          fragment=FM_SWIG_SvSTORE_frag(fm_bitArray),
          fragment=FM_SWIG_SvSTORE_frag(fm_fibmSwitchConfig),
          fragment=FM_SWIG_SvSTORE_frag(fm_ipAddr),
          fragment=FM_SWIG_SvSTORE_frag(fm_mtuEntry),
          fragment=FM_SWIG_SvSTORE_frag(fm_policerBankDropMask),
          fragment=FM_SWIG_SvSTORE_frag(fm_aclRulePolicerInfo),
          fragment=FM_SWIG_SvSTORE_frag(fm_ffuSliceAllocations),
          fragment=FM_SWIG_SvSTORE_frag(fm_sourceMapperValue),
          fragment=SWIG_FromPtr_frag(fm_sourceMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_pausePacingTime),
          fragment=SWIG_FromPtr_frag(fm_pausePacingTime),
          fragment=FM_SWIG_SvSTORE_frag(fm_protocolMapperValue),
          fragment=SWIG_FromPtr_frag(fm_protocolMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_l4PortMapperValue),
          fragment=SWIG_FromPtr_frag(fm_l4PortMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_macMapperValue),
          fragment=SWIG_FromPtr_frag(fm_macMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_ethTypeValue),
          fragment=SWIG_FromPtr_frag(fm_ethTypeValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_ipLengthMapperValue),
          fragment=SWIG_FromPtr_frag(fm_ipLengthMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_ipAddrMapperValue),
          fragment=SWIG_FromPtr_frag(fm_ipAddrMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_vlanMapperValue),
          fragment=SWIG_FromPtr_frag(fm_vlanMapperValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_LBGDistributionMapRange),
          fragment=SWIG_FromPtr_frag(fm_uint64),
          fragment=FM_SWIG_SvSTORE_frag(fm_uint64),
          fragment=FM_SWIG_SvSTORE_frag(fm_uint32),
          fragment=FM_SWIG_SvSTORE_frag(fm_uint16),
          fragment=FM_SWIG_SvSTORE_frag(fm_uint),
          fragment=SWIG_FromPtr_frag(fm_int),
          fragment=FM_SWIG_SvSTORE_frag(fm_int32),
          fragment=FM_SWIG_SvSTORE_frag(fm_int),
          fragment=FM_SWIG_SvSTORE_frag(fm_bool),
          fragment=FM_SWIG_SvSTORE_frag(fm_hashRotationValue),
          fragment=FM_SWIG_SvSTORE_frag(fm_L2HashKey),
          fragment=FM_SWIG_SvSTORE_frag(fm_L2HashRot),
          fragment=FM_SWIG_SvSTORE_frag(fm_L3HashConfig),
          fragment=FM_SWIG_SvSTORE_frag(fm_aclCompilerTryAlloc)) {

SWIGINTERN int
SWIG_FromPtr_dec(void)(SV *sv, void *x)
{
    fmSWIG_void     pv;
    fmSWIG_dataType type = FM_SWIG_TYPE_UNKNOWN;
    SV **           value;
    int             status;
    char            buffer[3 * FM_SWIG_N_MACADDR_BYTES];

    if (!SWIG_IsOK((status = fmSWIGVerifyVoidStructure(sv, &pv))))
    {
        return status;
    }

    /* Process the 'type' key and assign the appropriate value to the `value'
     * key.  */
    if ((value = fmSWIGGetHashKey(FM_SWIG_void, pv.type)) != NULL)
    {
        type = (fmSWIG_dataType) SvIV(*value);
    }
    switch (type)
    {
        case FM_SWIG_TYPE_BIT_ARRAY:
            return FM_SWIG_SvSTORE(fm_bitArray)(pv.value, (fm_bitArray *) x);

        case FM_SWIG_TYPE_FIBM_SWITCH_CONFIG:
            return FM_SWIG_SvSTORE(fm_fibmSwitchConfig)(pv.value,
                                                        (fm_fibmSwitchConfig *) x);

        case FM_SWIG_TYPE_IP_ADDR:
            return FM_SWIG_SvSTORE(fm_ipAddr)(pv.value, (fm_ipAddr *) x);

        case FM_SWIG_TYPE_MTU_ENTRY:
            return FM_SWIG_SvSTORE(fm_mtuEntry)(pv.value, (fm_mtuEntry *) x);

        case FM_SWIG_TYPE_POLICER_BANK_DROP_MASK:
            return FM_SWIG_SvSTORE(fm_policerBankDropMask)(pv.value, (fm_policerBankDropMask *) x);

        case FM_SWIG_TYPE_ACL_RULE_POLICER_INFO_VALUE:
            return FM_SWIG_SvSTORE(fm_aclRulePolicerInfo)(pv.value, (fm_aclRulePolicerInfo *) x);

        case FM_SWIG_TYPE_FFU_SLICE_ALLOCATIONS:
            return FM_SWIG_SvSTORE(fm_ffuSliceAllocations)(pv.value,
                                                        (fm_ffuSliceAllocations *) x);

        case FM_SWIG_TYPE_MAPPER_SOURCE_VALUE:
            return FM_SWIG_SvSTORE(fm_sourceMapperValue)(pv.value,
                                                        (fm_sourceMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_SOURCE_VALUE_ARRAY:
            return SWIG_FromPtr(fm_sourceMapperValue)(pv.value, (fm_sourceMapperValue *) x);

        case FM_SWIG_TYPE_PAUSE_PACING_TIME:
            return FM_SWIG_SvSTORE(fm_pausePacingTime)(pv.value, 
                                                       (fm_pausePacingTime *) x);

        case FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE:
            return FM_SWIG_SvSTORE(fm_protocolMapperValue)(pv.value,
                                                        (fm_protocolMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_PROTOCOL_VALUE_ARRAY:
            return SWIG_FromPtr(fm_protocolMapperValue)(pv.value, (fm_protocolMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_L4PORT_VALUE:
            return FM_SWIG_SvSTORE(fm_l4PortMapperValue)(pv.value,
                                                        (fm_l4PortMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_L4PORT_VALUE_ARRAY:
            return SWIG_FromPtr(fm_l4PortMapperValue)(pv.value, (fm_l4PortMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_MAC_VALUE:
            return FM_SWIG_SvSTORE(fm_macMapperValue)(pv.value,
                                                        (fm_macMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_MAC_VALUE_ARRAY:
            return SWIG_FromPtr(fm_macMapperValue)(pv.value, (fm_macMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE:
            return FM_SWIG_SvSTORE(fm_ethTypeValue)(pv.value,
                                                        (fm_ethTypeValue *) x);

        case FM_SWIG_TYPE_MAPPER_ETHTYPE_VALUE_ARRAY:
            return SWIG_FromPtr(fm_ethTypeValue)(pv.value, (fm_ethTypeValue *) x);

        case FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE:
            return FM_SWIG_SvSTORE(fm_ipLengthMapperValue)(pv.value,
                                                        (fm_ipLengthMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_IPLENGTH_VALUE_ARRAY:
            return SWIG_FromPtr(fm_ipLengthMapperValue)(pv.value, (fm_ipLengthMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_IPADDR_VALUE:
            return FM_SWIG_SvSTORE(fm_ipAddrMapperValue)(pv.value,
                                                        (fm_ipAddrMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_IPADDR_VALUE_ARRAY:
            return SWIG_FromPtr(fm_ipAddrMapperValue)(pv.value, (fm_ipAddrMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_VLAN_VALUE:
            return FM_SWIG_SvSTORE(fm_vlanMapperValue)(pv.value,
                                                      (fm_vlanMapperValue *) x);

        case FM_SWIG_TYPE_MAPPER_VLAN_VALUE_ARRAY:
            return SWIG_FromPtr(fm_vlanMapperValue)(pv.value, (fm_vlanMapperValue *) x);

        case FM_SWIG_TYPE_LBG_DISTRIBUTION_MAP_RANGE:    
            return FM_SWIG_SvSTORE(fm_LBGDistributionMapRange)(pv.value, 
                                                              (fm_LBGDistributionMapRange *) x);

        case FM_SWIG_TYPE_MAC_ADDR:
            fmSWIGSetMacAddr(buffer, sizeof(buffer), *((fm_macaddr *) x));
            sv_setpv(pv.value, buffer);
            break;

        case FM_SWIG_TYPE_UINT64_ARRAY:
            /*return SWIG_FromPtr(fm_uint64)(pv.value, (fm_uint64 *) x);*/
            return SWIG_FromPtr_fm_uint64 SWIG_PERL_CALL_ARGS_2(pv.value, (fm_uint64 *) x);

        case FM_SWIG_TYPE_UINT64:
            return FM_SWIG_SvSTORE(fm_uint64)(pv.value, (fm_uint64 *) x);

        case FM_SWIG_TYPE_UINT32:
            return FM_SWIG_SvSTORE(fm_uint32)(pv.value, (fm_uint32 *) x);

        case FM_SWIG_TYPE_UINT16:
            return FM_SWIG_SvSTORE(fm_uint16)(pv.value, (fm_uint16 *) x);

        case FM_SWIG_TYPE_UINT:
            return FM_SWIG_SvSTORE(fm_uint)(pv.value, (fm_uint *) x);

        case FM_SWIG_TYPE_INT_ARRAY:
            return SWIG_FromPtr(fm_int)(pv.value, (fm_int *) x);

        case FM_SWIG_TYPE_INT32:
            return FM_SWIG_SvSTORE(fm_int32)(pv.value, (fm_int32 *) x);

        case FM_SWIG_TYPE_INT:
        case FM_SWIG_TYPE_ETH_MODE:
           return FM_SWIG_SvSTORE(fm_int)(pv.value, (fm_int *) x); 

        case FM_SWIG_TYPE_BOOL:
            return FM_SWIG_SvSTORE(fm_bool)(pv.value, (fm_bool *) x);

        case FM_SWIG_TYPE_CHAR:
            sv_setpv(pv.value, (fm_char *) x);
            break;

        case FM_SWIG_TYPE_HASH_ROTATION_VALUE:
            return FM_SWIG_SvSTORE(fm_hashRotationValue)(pv.value,
                                                        (fm_hashRotationValue *) x);

        case FM_SWIG_TYPE_L2_HASH_KEY:
            return FM_SWIG_SvSTORE(fm_L2HashKey)(pv.value,
                                                (fm_L2HashKey *) x);

        case FM_SWIG_TYPE_L2_HASH_ROT:
            return FM_SWIG_SvSTORE(fm_L2HashRot)(pv.value,
                                                (fm_L2HashRot *) x);

        case FM_SWIG_TYPE_L3_HASH_CONFIG:
            return FM_SWIG_SvSTORE(fm_L3HashConfig)(pv.value,
                                                   (fm_L3HashConfig *) x);

        case FM_SWIG_TYPE_ACL_COMPILER_TRY_ALLOC:
            return FM_SWIG_SvSTORE(fm_aclCompilerTryAlloc)(pv.value,
                                                          (fm_aclCompilerTryAlloc *) x);

        default:
            sv_catpvf(FM_SWIG_errSv, ": %s: Invalid `type' key", pv.type);
            return SWIG_ERROR;

    }   /* end switch (type) */

    return SWIG_OK;
}

}   /* end fragment(SWIG_FromPtr_frag(void)) */


FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_bitArray);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_fibmSwitchConfig);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_ipAddr);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_mtuEntry);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_policerBankDropMask);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_aclRulePolicerInfo);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_ffuSliceAllocations);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_sourceMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_pausePacingTime);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_protocolMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_l4PortMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_macMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_ethTypeValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_ipLengthMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_ipAddrMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_vlanMapperValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_LBGDistributionMapRange);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_hashRotationValue);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_L2HashKey);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_L2HashRot);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_L3HashConfig);
FM_SWIG_FRAGMENT_SV_FETCH_RV(fm_aclCompilerTryAlloc);


FM_SWIG_FRAGMENT_SV_STORE_RV(fm_bitArray);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_ipAddr);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_fibmSwitchConfig);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_mtuEntry);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_policerBankDropMask);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_aclRulePolicerInfo);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_ffuSliceAllocations);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_sourceMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_pausePacingTime);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_protocolMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_l4PortMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_macMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_ethTypeValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_ipLengthMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_ipAddrMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_vlanMapperValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_LBGDistributionMapRange);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_hashRotationValue);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_L2HashKey);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_L2HashRot);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_L3HashConfig);
FM_SWIG_FRAGMENT_SV_STORE_RV(fm_aclCompilerTryAlloc);

