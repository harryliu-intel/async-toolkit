/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */

/******************************************************************************
 * File:            sdk_callback.i
 * Creation Date:   September 19, 2008
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2011 Intel Corporation. All Rights Reserved. 
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

/*****************************************************************************/
/** FM_SWIG_CALLBACK_PTR
 *
 * \desc            Generates the function name for a callback proxy.
 *
 * \param[in]       Symname is the function name to base the function name of
 *                  the callback proxy on.
 *
 *****************************************************************************/
#define FM_SWIG_CALLBACK_PTR(Symname...)    __fmscbp_ ## Symname




/*****************************************************************************/
/** %formacro_3
 *
 * \desc            Calls the specified macro processing the variable argument
 *                  list three arguments at a time.
 *
 * \param[in]       macro is the SWIG macro to call.
 *
 * \param[in]       ... is the variable argument list. Its length must be a
 *                   multiple of three.
 *
 *****************************************************************************/
%define %formacro_3(macro, ...)
%_formacro_3(macro, __VA_ARGS__, __FM_SWIG_macro_end__)
%enddef /* end %formacro_3 */




/*****************************************************************************/
/** %_formacro_3
 *
 * \desc            Implements the backend of the %formacro_3 macro. The
 *                  %_formacro_3 is only to be called via its fronted,
 *                  %formacro_3.
 *
 * \param[in]       macro is the SWIG macro to call.
 *
 * \param[in]       arg1 is the first argument to feed \a macro.
 *
 * \param[in]       arg2 is the second argument to feed \a macro.
 *
 * \param[in]       arg3 is the third argument to feed \a macro.
 *
 * \param[in]       ... is a variable argument list. Its length must be a
 *                  multiple of three.
 *
 *****************************************************************************/
%define %_formacro_3(Macro, arg1, arg2, arg3, ...)
Macro(arg1, arg2, arg3, __VA_ARGS__)
#if #__VA_ARGS__ != "__FM_SWIG_macro_end__"
%_formacro_3(Macro, __VA_ARGS__)
#endif
%enddef /* end %_formacro_3 */




/*****************************************************************************/
/** %FM_SWIG_PrivVar
 *
 * \desc            Privatizes the specified variable name to avoid name
 *                  clashes.
 *
 * \param[in]       Variable is the variable name to privatize.
 *
 *****************************************************************************/
%define %FM_SWIG_PrivVar(Variable) __ ## Variable %enddef




/*****************************************************************************/
/** %FM_SWIG_CALLBACK_Fragments
 *
 * \desc            Generates the set of SWIG fragments that are needed to
 *                  convert from and to the specified function argument.
 *
 * \param[in]       Type is the basetype of the argument.
 *
 * \param[in]       Pointer indicates whether the argument points to a variable
 *                  of the specified basetype.
 *
 * \param[in]       Variable is the argument name.
 *
 * \note            This macro is to be used in conjunction with the
 *                  %formacro_3 SWIG macro.
 *
 *****************************************************************************/
%define %FM_SWIG_CALLBACK_Fragments(Type, Pointer, Variable, ...)
#if #Pointer == "*"
#if #Type != "void"
    %fragment(SWIG_AsPtr_frag(Type));
    %fragment(SWIG_FromPtr_frag(Type));
#endif  /* end #if #Type != "void" */
#else
    %fragment(SWIG_AsVal_frag(Type));
    %fragment(SWIG_FromVal_frag(Type));
#endif  /* end #if #Pointer == "*" */
%enddef /* end FM_SWIG_CALLBACK_Fragments */




/*****************************************************************************/
/** %FM_SWIG_CALLBACK_Arg
 *
 * \desc            Generates a function argument declaration for the specified
 *                  function argument.
 *
 * \param[in]       Type is the basetype of the argument.
 *
 * \param[in]       Pointer indicates whether the argument points to a variable
 *                  of the specified basetype.
 *
 * \param[in]       Variable is the argument name.
 *
 * \note            This macro is to be used in conjunction with the
 *                  %formacro_3 SWIG macro.
 *
 *****************************************************************************/
%define %FM_SWIG_CALLBACK_Arg(Type, Pointer, Variable, ...)
#if #__VA_ARGS__ == "__FM_SWIG_macro_end__"
Type Pointer ## Variable
#else
Type Pointer ## Variable,
#endif  /* end #if #__VA_ARGS__ == "__FM_SWIG_macro_end__" */
%enddef /* end %FM_SWIG_CALLBACK_Arg */




/*****************************************************************************/
/** %FM_SWIG_CALLBACK_Var
 *
 * \desc            Generates a Perl variable declaration for the specified
 *                  function argument.
 *
 * \param[in]       Type is the basetype of the argument.
 *
 * \param[in]       Pointer indicates whether the argument points to a variable
 *                  of the specified basetype.
 *
 * \param[in]       Variable is the argument name.
 *
 * \note            This macro is to be used in conjunction with the
 *                  %formacro_3 SWIG macro.
 *
 *****************************************************************************/
%define %FM_SWIG_CALLBACK_Var(Type, Pointer, Variable, ...)
SV *%FM_SWIG_PrivVar(Variable);
%enddef /* end %FM_SWIG_CALLBACK_Var */




/*****************************************************************************/
/** %FM_SWIG_CALLBACK_From
 *
 * \desc            Converts the specified function argument to its Perl
 *                  representation.
 *
 * \param[in]       Type is the basetype of the argument.
 *
 * \param[in]       Pointer indicates whether the argument points to a variable
 *                  of the specified basetype.
 *
 * \param[in]       Variable is the argument name.
 *
 * \note            This macro is to be used in conjunction with the
 *                  %formacro_3 SWIG macro.
 *
 *****************************************************************************/
%define %FM_SWIG_CALLBACK_From(Type, Pointer, Variable, ...)
#if #Pointer == "*"
#if #Type == "void"
    %FM_SWIG_PrivVar(Variable) = SWIG_NewPointerObj(%as_voidptr(Variable),
                                                    SWIGTYPE_p_void,
                                                    SWIG_POINTER_OWN | SWIG_SHADOW);
#else
    %FM_SWIG_PrivVar(Variable) = sv_newmortal();
    if (!SWIG_IsOK(SWIG_FromPtr(Type)(%FM_SWIG_PrivVar(Variable), Variable)))
    {
        sv_setpvf(ERRSV, %str('Variable') "%s", SvPV_nolen(FM_SWIG_errSv));
        SWIG_fail;
    }
#endif  /* end #if #Type == "void" */
#else
    %FM_SWIG_PrivVar(Variable) = SWIG_FromVal(Type)(Variable);
#endif  /* end #if #Pointer == "*" */
    XPUSHs(%FM_SWIG_PrivVar(Variable));
%enddef /* end %FM_SWIG_CALLBACK_From */




/*****************************************************************************/
/** %FM_SWIG_CALLBACK_As
 *
 * \desc            Converts the Perl representation of the specified function
 *                  argument back to its original representation.
 *
 * \param[in]       Type is the basetype of the argument.
 *
 * \param[in]       Pointer indicates whether the argument points to a variable
 *                  of the specified basetype.
 *
 * \param[in]       Variable is the argument name.
 *
 * \note            This macro is to be used in conjunction with the
 *                  %formacro_3 SWIG macro.
 *
 *****************************************************************************/
%define %FM_SWIG_CALLBACK_As(Type, Pointer, Variable, ...)
#if #Type != "void"
#if #Pointer == "*"
    if (!SWIG_IsOK(SWIG_AsPtr(Type)(%FM_SWIG_PrivVar(Variable), &Variable)))
#else
    if (!SWIG_IsOK(SWIG_AsVal(Type)(%FM_SWIG_PrivVar(Variable), &Variable)))
#endif  /* end #if #Pointer == "*" */
    {
        sv_setpvf(ERRSV, %str('Variable') "%s", SvPV_nolen(FM_SWIG_errSv));
        SWIG_fail;
    }
#endif  /* end #if #Type != "void" */
%enddef /* end %FM_SWIG_CALLBACK_As */




%types(void *);


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

%{

/*****************************************************************************/
/** fmSWIGMasterInterp
 *
 *****************************************************************************/
SWIGINTERN PerlInterpreter *fmSWIGMasterInterp = NULL;

/*****************************************************************************/
/** fmSWIGCallbackStore
 *
 * \desc            Perl HASH used to store all registered Perl callback
 *                  functions.
 *
 *****************************************************************************/
SWIGINTERN HV *fmSWIGCallbackStore = NULL;

%}

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

%{

#ifdef USE_ITHREADS

SWIGINTERNINLINE void
fmSWIGClonePerlInterpreter(void)
{
    PerlInterpreter *thisInterp = NULL;

    if ((thisInterp = PERL_GET_CONTEXT) == NULL)
    {
        PERL_SET_CONTEXT(fmSWIGMasterInterp);
        thisInterp = perl_clone(fmSWIGMasterInterp, CLONEf_KEEP_PTR_TABLE);
        if (thisInterp == NULL)
        {
            SWIG_exception_fail(SWIG_RuntimeError,
                                "cannot clone Perl interpreter");
        }
    }

    return;

fail:
    SWIG_croak_null();

}   /* end fmSWIGClonePerlInterpreter */

#endif  /* end USE_ITHREADS */

%}

/*****************************************************************************/
/** FM_SWIG_CALLBACK
 *
 * \desc            Generates a callback proxy and a function pointer typemap
 *                  for the specified function.
 *
 * \param[in]       Symname is the name of the function for which to create a
 *                  callback proxy.
 *
 * \param[in]       Type is the function pointer type definition. It must point
 *                  to a type definition created using typedef.
 *
 * \param[in]       ReturnType is the return type of the callback function.
 *
 * \param[in]       Prototype contains a list of callback function arguments.
 *                  Each argument consist of three parts: (1) the argument
 *                  basetype, (2) a pointer indicator and (3) the argument
 *                  name.
 *
 * <b>Example I</b>                                                      \lb\lb
 *
 * Say, \c g is a function of the form:
 * \code
 *
 * void g(void (*f)(int) h);
 *
 * \endcode
 *
 * To allow the typemap that is generated by the FM_SWIG_CALLBACK macro to
 * match on the function pointer argument of \c g, we need to define a SWIG
 * only type definition:
 * \code
 * %{
 *
 * typedef void (*f_int_void_t)(int);
 *
 * %}
 * \endcode
 *
 * This type definition is only needed if the function pointer argument of the
 * function that is to be wrapped, is not already a <c>typedef</c>fed type
 * definition.                                                           \lb\lb
 *
 * Having added a type definition, we can call the FM_SWIG_CALLBACK macro to
 * generate a callback proxy for the function pointer argument of \c g as
 * follows:
 * \code
 *
 * FM_SWIG_CALLBACK(g, f_int_void_t, void, int, , x);
 *
 * \endcode
 *
 * <b>Example II</b>                                                     \lb\lb
 *
 * Say, \c g is a function of the form:
 * \code
 *
 * typedef int (*f_p_double_int_t)(double *);
 * void g(f_p_double_int_t h);
 *
 * \endcode
 *
 * A callback proxy for the function pointer argument of \c g can be generated
 * as follows:
 * \code
 *
 * FM_SWIG_CALLBACK(g, f_p_double_int_t, int, double, *, x);
 *
 * \encode
 *
 *****************************************************************************/
%define FM_SWIG_CALLBACK(Symname, Type, ReturnType, Prototype...)

#if #ReturnType == "void"
%{ #define FM_SWIG_CALLBACK_void 1 %}
#else
%{ #define FM_SWIG_CALLBACK_void 0 %}
#endif  /* end #if #ReturnType == "void" */

%formacro_3(%FM_SWIG_CALLBACK_Fragments, Prototype);

%{

SWIGINTERN ReturnType
FM_SWIG_CALLBACK_PTR(Symname)(%formacro_3(%FM_SWIG_CALLBACK_Arg, Prototype))
{
#ifdef USE_ITHREADS
    fmSWIGClonePerlInterpreter();
#endif  /* end USE_ITHREADS */
    {
    /* Declare and initialize a local copy of the Perl stack pointer.  */
    dSP;
#ifdef USE_ITHREADS
    CLONE_PARAMS cloneParams;
#endif  /* end USE_ITHREADS */
    SV **        sv;
    SV *         cv;
    /* For each function argument, declare an equivalent local Perl variable.
     */
    %formacro_3(%FM_SWIG_CALLBACK_Var, Prototype)
    I32          count;
#if FM_SWIG_CALLBACK_void == 0
    ReturnType   result = (ReturnType) 0;
#endif  /* end #if FM_SWIG_CALLBACK_void == 0 */

    ENTER;
    SAVETMPS;

    /* Retrieve a CODE reference to the Perl callback routine.  */
    if (   (sv = fmSWIGGetHashKey(fmSWIGCallbackStore, %str(Symname))) == NULL
        || !SvTRUE(*sv))
    {
        SWIG_exception_fail(SWIG_RuntimeError,
                            %str(Symname) ": no callback function registered");
    }

    /* Duplicate the retrieved CODE reference to force the CODE reference to
     * run within the thread context of SWIG callback proxy.  */
#ifdef USE_ITHREADS
    cloneParams.flags = 0;
    cv = SvREFCNT_inc(sv_dup(*sv, &cloneParams));
#else
    cv = *sv;
#endif  /* end USE_ITHREADS */

    /* Convert all function arguments into their Perl equivalents and push
     * these converted function arguments onto the Perl stack.  */
    PUSHMARK(SP);
    %formacro_3(%FM_SWIG_CALLBACK_From, Prototype)
    PUTBACK;

    /* Flush the standard out and standard error streams to prevent
     * interleaving of the SDK messages and Perl messages. */
    fflush(stdout);
    fflush(stderr);

    /* Call the Perl callback routine.  */
#if FM_SWIG_CALLBACK_void == 0
    count = call_sv(cv, G_SCALAR);
    if (count != 1)
#else
    count = call_sv(cv, G_VOID);
    if (count != 0)
#endif
    {
        SWIG_exception_fail(SWIG_RuntimeError,
                            %str(Symname) ": invalid number of arguments returned");
    }

    /* Flush the Perl IO standard out and standard error streams. */
    PerlIO_flush(PerlIO_stdout());
    PerlIO_flush(PerlIO_stderr());

    /* Refresh the local copy of the stack pointer.  */
    SPAGAIN;

#if FM_SWIG_CALLBACK_void == 0
    if (!SWIG_IsOK(SWIG_AsVal(ReturnType)(POPs, &result)))
    {
        sv_setpvf(ERRSV,
                  %str(Symname) ": return value%s",
                  SvPV_nolen(FM_SWIG_errSv));
        SWIG_fail;
    }
#endif  /* end #if FM_SWIG_CALLBACK_void == 0 */

    /* Propagate any changes made to the function arguments by the Perl
     * callback routine back to the caller.  */
    %formacro_3(%FM_SWIG_CALLBACK_As, Prototype)

    PUTBACK;
    FREETMPS;
    LEAVE;

#if FM_SWIG_CALLBACK_void == 0
    return result;
#else
    return;
#endif  /* end #if FM_SWIG_CALLBACK_void == 0 */

fail:
    PUTBACK;
    FREETMPS;
    LEAVE;

    SWIG_croak_null();
    }
}

#undef FM_SWIG_CALLBACK_void

%}

#ifndef __FM_SWIG_CALLBACK_TYPEMAP_Type
#define __FM_SWIG_CALLBACK_TYPEMAP_Type 1

%typemap(in) (Type)
{
    SV **sv;
    char key[] = %str($symname);

    /* Add a new key to the callback store if necessary.  */
    if ((sv = fmSWIGGetHashKey(fmSWIGCallbackStore, key)) == NULL)
    {
        if ((sv = fmSWIGAddHashKey(fmSWIGCallbackStore, key)) == NULL)
        {
            sv_setpvf(ERRSV,
                      %str($symname) ": cannot add a callback entry for '%s'",
                      key);
            SWIG_fail;
        }
    }

    /* Store the code reference.  */
    sv_setsv(*sv, $input);

    /* A compile error caused by below line of code is due to the absence of a
     * SWIG callback proxy function. This proxy function can be generated using
     * the SWIG FM_SWIG_CALLBACK macro. See the FM_SWIG_CALLBACK description
     * for an example of how to use this macro.  */
    $1 = FM_SWIG_CALLBACK_PTR($symname);
}

#endif  /* __FM_SWIG_CALLBACK_TYPEMAP_Type */

%enddef /* FM_SWIG_CALLBACK */




%init
{
    /* Initialize fmSWIGMasterInterp at SWIG module boot time.  */
    fmSWIGMasterInterp = PERL_GET_CONTEXT;
    /* Initialize fmSWIGCallbackStore at SWIG module boot time.  */
    fmSWIGCallbackStore = newHV();
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

%perlcode
%{

###############################################################################
##@method public static T& cast(Object &o, T)
#
# @desc             Casts the Perl object reference \a o to an object of type
#                   \a T.
#
# @param[in]        o points to the Perl object that is to be casted.
#
# @param[in]        T is the type \a o is to be casted to.
#
# @return           \a o casted to an object of type \a T.
#
###############################################################################
sub cast
{
    my ($o, $T) = @_;

    bless(tied(%{$o}), $T);
    bless($o, $T);
    return $o;

}   # end

%}

