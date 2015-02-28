/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef FULCRUM_EXCLUDED_GENERATED_INCLUDES
#include "Sigscan.h"
#endif
#include <sdi2.h>

//For asynchronous signal enumeration
static char** signals=NULL;
static int signals_length;

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_openDatabase(
        JNIEnv *env, jclass clazz, jstring filename,
        jboolean useOpen, jint timeScale) {

    char * c_filename_p;
    jboolean isCopy;
    bool user = true;
    
    c_filename_p = (char *)
        env->GetStringUTFChars(filename,&isCopy);

    if (useOpen == JNI_FALSE) user = false;
    
    sdiT* tmp_sdi_p = new sdiT(c_filename_p,user,timeScale);
//    printf("Hello, you called me %s %d %d\n",c_filename_p,useOpen, timeScale);

    if (isCopy == JNI_TRUE)
        env->ReleaseStringUTFChars(filename,c_filename_p);

    if (tmp_sdi_p && tmp_sdi_p->isOK() != sdiT::enumsT::STATUS_OK) {
        delete tmp_sdi_p;
        tmp_sdi_p = NULL;
        jclass exc =
            env->FindClass("com/avlsi/tools/sigscan/SigscanException");
        if (exc != NULL) {
            env->ThrowNew(exc, "Cannot create signal scan database");
        }
    }

    return (jlong) tmp_sdi_p;
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_pauseDatabase(
        JNIEnv *env, jclass clazz, jlong sdiHandle) {
    ((sdiT *) sdiHandle)->simulationPaused();
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_resumeDatabase(
        JNIEnv *env, jclass clazz, jlong sdiHandle) {
    ((sdiT *) sdiHandle)->simulationResumed();
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_closeDatabase(
        JNIEnv *env, jclass clazz, jlong sdiHandle) {
    delete (sdiT *) sdiHandle;
}

/** frees the memory used by whats pointed to by handle
 * Does not call that object's destructor **/
JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_freeHandle(
        JNIEnv *env, jclass clazz, jlong handle) {
    free( (void *)handle);
}


JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_setTime(
        JNIEnv *env, jclass clazz, jlong sdiHandle, jlong time) {
    /*printf("Setting [%d]=%s to %ld\n",
           sdiHandle,
           ((sdiT *) sdiHandle)->getSdiNameP(),
           time);*/
    ((sdiT *) sdiHandle)->setTime((sdiT::enumsT::timeT) time);
}

jlong newVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname,
        sdiT::enumsT::dataTypeT type,int numLiterals=0,
        char *const* literalNameList=NULL,
        jint lsb=0, jint msb=0) {
    
    sdiT::variableT* tmp_var_p;
    char *c_scopename_p, *c_varname_p;
    jboolean isCopy1, isCopy2;

    c_scopename_p = (char *)
        env->GetStringUTFChars(scopename,&isCopy1);
    c_varname_p = (char *)
        env->GetStringUTFChars(varname,&isCopy2);

    tmp_var_p = new sdiT::variableT(c_scopename_p,
                                    c_varname_p,
                                    type,
                                    NULL, lsb, msb,
                                    numLiterals,
                                    literalNameList,
                                    (sdiT *)sdiHandle);
    if (isCopy1 == JNI_TRUE)
        env->ReleaseStringUTFChars(scopename,c_scopename_p);
    if (isCopy2 == JNI_TRUE)
        env->ReleaseStringUTFChars(varname,c_varname_p);
    return (jlong) tmp_var_p;
}
    //
    //Signals for Analog Logging
    //
    
JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newDoubleVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::REAL64);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordDoubleChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jdouble data) {
    ((sdiT::variableT *)varHandle)->recordValueChange(&data);
}   

    //
    //Set of methods for Asynchronous Signals
    //
    
JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_setAsyncEnumeration(
        JNIEnv *env, jclass clazz, jobjectArray sigs) {
    jboolean isCopy;
    jstring temp;
    int loop;
    
    signals_length = env->GetArrayLength(sigs);
    signals = new char *[signals_length];
    for (loop=0;loop<signals_length;loop++) {
        temp = (jstring) env->GetObjectArrayElement(sigs, loop);
        signals[loop] = (char *)
            env->GetStringUTFChars(temp,&isCopy);
    }   
    //Note that this array is not released, shouldn't cause a 
    //serious problem
}

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newAsyncVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::ENUMERATION, signals_length,
                       signals);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordAsyncChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jint value) {
    ((sdiT::variableT *)varHandle)->recordValueChange(&value);
}

    //
    //Synchronous Logic Functionality
    //

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newLogicVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::LOGIC, 0, NULL);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordLogicChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jint value) {
    ((sdiT::variableT *)varHandle)->recordValueChange(&value);
}  

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newLogicArrayVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname, jint lsb, jint msb) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::LOGIC, 0, NULL, lsb, msb);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordLogicArrayChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jbyteArray value) {
    jbyte *buf = env->GetByteArrayElements(value, NULL);
    ((sdiT::variableT *)varHandle)->recordValueChange(buf);
    env->ReleaseByteArrayElements(value, buf, JNI_ABORT);
}

    //
    //Standard Int and String Signal Logging Support
    //
    
JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newIntVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::INT32, 0, NULL);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordIntChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jint value) {
    ((sdiT::variableT *)varHandle)->recordValueChange(&value);
}  
  
JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newLongVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::INT64, 0, NULL);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordLongChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jlong value) {
    ((sdiT::variableT *)varHandle)->recordValueChange(&value);
}  
  
JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newStringVariable(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring varname) {
    return newVariable(env, clazz, sdiHandle, scopename, varname,
                       sdiT::enumsT::STRING, 0, NULL);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_recordStringChange(
        JNIEnv *env, jclass clazz, jlong varHandle, jstring value) {
    char * c_value_p;
    jboolean isCopy;

    if (value == NULL) return;

    c_value_p = (char *)
        env->GetStringUTFChars(value,&isCopy);
    
    ((sdiT::variableT *)varHandle)->recordValueChange(c_value_p);

    if (isCopy == JNI_TRUE)
        env->ReleaseStringUTFChars(value,c_value_p);
    
}  
  

//
//Transaction Logging Support
//

//Builds a new transaction fiber and returns its pointer
//Don't forget to delete!
JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newTransactionFiber(
        JNIEnv *env, jclass clazz, jlong sdiHandle,
        jstring scopename, jstring fibername) {
    sdiT::fiberT *fiber_p;
    char *c_scopename_p, *c_fibername_p;
    jboolean isCopy1, isCopy2;
    
    c_scopename_p = (char *)
        env->GetStringUTFChars(scopename,&isCopy1);
    c_fibername_p = (char *)
        env->GetStringUTFChars(fibername,&isCopy2);

    fiber_p = new sdiT::fiberT(c_scopename_p,
                               c_fibername_p,
                               sdiT::enumsT::TVM_FIBER,
                               (sdiT *)sdiHandle);
    if (isCopy1 == JNI_TRUE)
        env->ReleaseStringUTFChars(scopename,c_scopename_p);
    if (isCopy2 == JNI_TRUE)
        env->ReleaseStringUTFChars(fibername,c_fibername_p);

    return (jlong) fiber_p;
}
    
JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_newTransactionType(
        JNIEnv *env, jclass clazz, jlong fiberHandle,
        jstring name, jint trankind) {
    
    sdiT::transactionTypeT *type_p;
    char *c_typename_p;
    jboolean isCopy;
    sdiT::enumsT::transactionKindT kind = sdiT::enumsT::UNSPECIFIED;
    
    c_typename_p = (char *)
        env->GetStringUTFChars(name,&isCopy);

    switch(trankind) {
        //XXX: Hacked, be sure that the java only send 1-3
      case 1:   kind = sdiT::enumsT::BEGINEND;
                break;
      case 2:   kind = sdiT::enumsT::EVENT;
                break;
      case 3:   kind = sdiT::enumsT::ERROR;
                break;
    }
    
    type_p= new sdiT::transactionTypeT(c_typename_p,
                               kind,
                               (sdiT::fiberT *)fiberHandle);
    if (isCopy == JNI_TRUE)
        env->ReleaseStringUTFChars(name,c_typename_p);
    
    //printf("Made : %s\n", type_p->getTransactionTypeNameP());
    return (jlong) type_p;
}

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_beginTransaction(
        JNIEnv *env, jclass clazz,
        jlong fiberHandle, jlong tranTypeHandle,
        jstring label, jstring description) {
    char *c_label_p, *c_desc_p;
    jboolean isCopy1, isCopy2;
    sdiT::transactionHandleT *tranhandleT_p;
    sdiT::transactionHandleT tranhandleT;

    //tranhandleT_p = (sdiT::transactionHandleT *)
    //    malloc(sizeof(sdiT::transactionHandleT));
    c_label_p= (char *)
        env->GetStringUTFChars(label,&isCopy1);
    c_desc_p = (char *)
        env->GetStringUTFChars(description,&isCopy2);
    
    sdiT::transactionTypeT ptype = *(sdiT::transactionTypeT *) tranTypeHandle;
    //printf("Made : %s %d\n", c_label_p,isCopy1);
    
    tranhandleT =((sdiT::fiberT *) fiberHandle)->beginTransaction(ptype,
            c_label_p, c_desc_p);
    tranhandleT_p = new sdiT::transactionHandleT(tranhandleT);
    if (isCopy1 == JNI_TRUE)
        env->ReleaseStringUTFChars(label,c_label_p);
    if (isCopy2 == JNI_TRUE)
        env->ReleaseStringUTFChars(description,c_desc_p);
    return (jlong) tranhandleT_p;
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_linkTransactions(
        JNIEnv *env, jclass clazz, jlong linkHandle,
        jlong srcHandle, jlong dstHandle) {
    ((sdiT::relationT *) linkHandle)->
        link(*(sdiT::transactionHandleT *) srcHandle,
             *(sdiT::transactionHandleT *) dstHandle);
}

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_getPredecessorHandle(
        JNIEnv *env, jclass clazz,
        jlong sdiHandle) {
    sdiT::relationT *relationP =
        new sdiT::relationT("predecessor",(sdiT *) sdiHandle);
    return (jlong) relationP;
}

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_getSuccessorHandle(
        JNIEnv *env, jclass clazz,
        jlong sdiHandle) {
    sdiT::relationT *relationP =
        new sdiT::relationT("successor",(sdiT *) sdiHandle);
    return (jlong) relationP;
}

/** Ends the most recent transaction **/
JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_endLastTransaction(
        JNIEnv *env, jclass clazz,
        jlong fiberHandle) {
    ((sdiT::fiberT *) fiberHandle)->
        endTransaction();
}

/** Ends the transaction with handle tranHandle **/
JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_endTransaction(
        JNIEnv *env, jclass clazz,
        jlong fiberHandle, jlong tranHandle) {
    sdiT::transactionHandleT *handle_p = 
        (sdiT::transactionHandleT *) tranHandle;
    ((sdiT::fiberT *) fiberHandle)->
        endTransaction(*handle_p);
    //delete(handle_p);
}

jlong newAttribute(
        JNIEnv *env, jclass clazz, jlong tranTypeHandle,
        jstring attrname,sdiT::enumsT::dataTypeT type,
        const char* formatStringP=NULL, bool notGenetic=true,
        unsigned int numLiterals=0, char *const *literalNameList=NULL) {
    sdiT::attributeT *attr;
    char *c_attrname_p;
    jboolean isCopy;
    
    c_attrname_p = (char *)
        env->GetStringUTFChars(attrname,&isCopy);
    attr = new sdiT::attributeT(
            (sdiT::transactionTypeT *) tranTypeHandle,
            c_attrname_p,
            type,
            formatStringP,//default display format
            -1,false, notGenetic, 0,0,
            numLiterals, literalNameList); 
    if (isCopy == JNI_TRUE)
        env->ReleaseStringUTFChars(attrname,c_attrname_p);
    return (jlong) attr;
}

JNIEXPORT jlong JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_attachAttribute(
        JNIEnv *env, jclass clazz, jint type, jlong tranTypeHandle,
        jstring attrname) {
    return newAttribute(env, clazz, tranTypeHandle, attrname,
                        (sdiT::enumsT::dataTypeT) type);
}

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_setIntAttribute(
        JNIEnv *env, jclass clazz, jlong attrHandle, jint value) {
    ((sdiT::attributeT *)attrHandle)->setValue(&value);
}  

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_setLongAttribute(
        JNIEnv *env, jclass clazz, jlong attrHandle, jlong value) {
    ((sdiT::attributeT *)attrHandle)->setValue(&value);
}  

JNIEXPORT void JNICALL
Java_com_avlsi_tools_sigscan_Sigscan_setStringAttribute(
        JNIEnv *env, jclass clazz, jlong attrHandle, jstring attrval) {
    jboolean isCopy;
    char *c_attrval_p;
    
    if (attrval == NULL) return;
    
    c_attrval_p = (char *)
        env->GetStringUTFChars(attrval,&isCopy);
        
    ((sdiT::attributeT *)attrHandle)->setValue(c_attrval_p);
    
    if (isCopy == JNI_TRUE)
        env->ReleaseStringUTFChars(attrval,c_attrval_p);
}  


