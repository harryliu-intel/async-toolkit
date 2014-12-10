// vim:sw=4:expandtab:ai:cin:ts=4
#include "Wrapper.h"

/*
 * Class:     Readline
 * Method:    rl_complete_internal
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1complete_1internal (JNIEnv *env, jclass cls, jint what_todo)
{
    return (jint) rl_complete_internal((int) what_todo);
}

/*
 * Class:     Readline
 * Method:    rl_complete
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1complete (JNIEnv *env, jclass cls, jint ignore, jint key) {
    return (jint) rl_complete((int) ignore, (int) key);
}

/*
 * Class:     Readline
 * Method:    rl_possible_completions
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1possible_1completions (JNIEnv *env, jclass cls,
                                         jint count, jint key) {
    return (jint) rl_possible_completions((int) count, (int) key);
}

/*
 * Class:     Readline
 * Method:    rl_insert_completions
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1insert_1completions (JNIEnv *env, jclass cls,
                                       jint count, jint key) {
    return (jint) rl_insert_completions((int) count, (int) key);
}

/* A CompletionFunction */
static jobject completion_func;
static jobject completion_class;
static jobject attempt_completion_func; 
static jobject attempt_completion_class; 

int useCompletionClassToMakeCompilerHappy() {
    return completion_class!=attempt_completion_class; 
}
void cvalidate(void* object, char* msg) {
    if (!object) {
        fprintf(stderr, "Fatal error Completion: %s not found\n", msg);
        fflush(stderr);
        exit(1);
    }
}
static char **c_attempt_completion_func(char *ctext, int start, int end) {
    JNIEnv *env;
    jstring text, cur;
    jarray result;
    const char *cresult;
    char **rcopy=NULL;
    int i, status;
    status = (*jvm)->AttachCurrentThread(jvm, (void **) &env, NULL);
    assert (status == 0);
    // May need to replace ctext with "rl_line_buffer" to get the whole command line...
    //text = (*env)->NewStringUTF(env, ctext);
    if (rl_line_buffer) { text = (*env)->NewStringUTF(env, rl_line_buffer); }
    else { text = NULL; }
    result = (jarray) (*env)->CallObjectMethod(env, attempt_completion_func, 
            Attempt_completion_run, text, (jint)start, (jint)end);
    if (result) {
        int sz = (*env)->GetArrayLength(env, result);
        if (sz) {
            rcopy = (char**)malloc(sizeof(char*)*(sz+1));
            rcopy[sz]=NULL;
            for (i=0; i<sz; i++) {
                rcopy[i]=NULL;
                cur = (jstring)(*env)->GetObjectArrayElement(env, result, (jsize)i);
                if (cur) {
                    cresult = CSTR(cur);
                    if (cresult) { 
                        int len = strlen(cresult)+1;
                        if (len>1) {
                            rcopy[i] = (char*)malloc(len);
                            strncpy(rcopy[i], cresult, len);
                        }
                    }
                }
                DELSTR(cur, cresult);
            }
        }
    //} else {
    //    rcopy = (char**)malloc(sizeof(char* ));
    //    rcopy[0]=NULL;
    }
    // Detaching causes a crash, since this thread came from the VM
    //   and Attach/Detach are absolute, not incremental
    //(*jvm)->DetachCurrentThread(jvm);
    return rcopy;
}

static char *default_completion_func(const char *ctext, int state) {
    JNIEnv *env;
    jstring text, result;
    const char *cresult;
    char *rcopy=NULL;
    int status;
    static int doFilename = 0;
    if (!state) { doFilename = 0; }
    if (!doFilename) {
        status = (*jvm)->AttachCurrentThread(jvm, (void **) &env, NULL);
        assert (status == 0);
        text = (*env)->NewStringUTF(env, ctext);
        result = (*env)->CallObjectMethod(env, completion_func, Completion_run, text, (jint) state);
        if (result) {
            cresult = CSTR(result);
            if (cresult) { 
                int len = strlen(cresult)+1;
                if (len>1) {
                    rcopy = (char*)malloc(len);
                    strncpy(rcopy, cresult, len);
                }
            }
            DELSTR(result, cresult);
        }
    }
    if (!(state || rcopy)) { doFilename = 1; }
    if (doFilename || !(state || rcopy)) {
        return rl_filename_completion_function(ctext, state);
    }
    // Detaching causes a crash, since this thread came from the VM
    //   and Attach/Detach are absolute, not incremental
    //(*jvm)->DetachCurrentThread(jvm);
    return rcopy;
}


/*
 * Class:     Readline
 * Method:    completion_matches
 * Signature: (Ljava/lang/String;LReadline$CompletionFunction;)[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL
Java_com_avlsi_util_readline_Readline_completion_1matches (JNIEnv *env, jclass cls, jstring text,
                                   jobject func) {
    int len = 0, i;
    char **list;
    const char *ctext = CSTR(text);
    jarray result;
    completion_func = func;
    list = rl_completion_matches(ctext, default_completion_func);
    DELSTR(text, ctext);
    while (list[len]) ++len;
    result = (*env)->NewObjectArray(env, len, StringClass, (jobject) NULL);
    for (i = 0; i < len; ++i) {
        (*env)->SetObjectArrayElement(env, result, (jsize) i,
                                      (*env)->NewStringUTF(env, list[i]));
    }
    return (jobjectArray) result;
}

/*
 * Class:     Readline
 * Method:    rl_completion_entry_function
 * Signature: (LReadline$CompletionFunction;)V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1completion_1entry_1function (JNIEnv *env, jclass cls,
                                               jobject func) {
    if (func == (jobject) NULL) {
        rl_completion_entry_function = NULL;
    } else {
        jobject oldObj = completion_func; 
        rl_completion_entry_function = default_completion_func;
        completion_func = (*env)->NewGlobalRef(env, func);
        if (oldObj) { (*env)->DeleteGlobalRef(env, oldObj); }
    }
}

/*
 * Class:     com_avlsi_util_readline_Readline
 * Method:    rl_completion_attempt_function
 * Signature: (Lcom/avlsi/util/readline/Readline$AttemptCompletionFunction;)V
 */
JNIEXPORT void JNICALL Java_com_avlsi_util_readline_Readline_rl_1completion_1attempt_1function
  (JNIEnv *env, jclass cls, jobject func) {
    if (func == (jobject) NULL) {
        rl_completion_entry_function = NULL;
    } else {
        jobject oldObj = attempt_completion_func; 
        rl_attempted_completion_function = (CPPFunction *) c_attempt_completion_func;
        attempt_completion_func = (*env)->NewGlobalRef(env, func);
        if (oldObj) { (*env)->DeleteGlobalRef(env, oldObj); }
    }
}

  

