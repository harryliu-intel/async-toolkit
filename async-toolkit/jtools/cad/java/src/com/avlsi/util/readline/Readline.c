#include "Wrapper.h"

JavaVM *jvm;
jclass KeymapClass, StringClass, CompletionInterface, AttemptCompletionInterface;
jmethodID Keymap_makeKeymap, Keymap_getAddress, Keymap_dispatch;
jmethodID Keymap_registerFunction, Keymap_unregisterFunction;
jmethodID Completion_run;
jmethodID Attempt_completion_run;

#include "Table.h"

void validate(void* object, char* msg) {
    if (!object) {
        fprintf(stderr, "Fatal error Readline.native_init: %s not found\n", msg);
        fflush(stderr);
        exit(1);
    }
}

/*
 * Class:     Readline
 * Method:    native_init
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_native_1init (JNIEnv *env, jclass cls) {
    assert((*env)->GetJavaVM(env, &jvm) == 0);
    KeymapClass = (*env)->FindClass(env, "com/avlsi/util/readline/Readline$Keymap");
	validate(KeymapClass, "class Keymap");
    StringClass = (*env)->FindClass(env, "java/lang/String");
	validate(StringClass, "class String");
    CompletionInterface = (*env)->FindClass(env,
			"com/avlsi/util/readline/Readline$CompletionFunction");
	validate(CompletionInterface, "interface Completion");
    AttemptCompletionInterface = (*env)->FindClass(env,
			"com/avlsi/util/readline/Readline$AttemptCompletionFunction");
	validate(CompletionInterface, "interface AttemptCompletion");

    /* Hold global references to the jclasses so they won't be garbage
     * collected.  This makes the caching of jmethodIDs possible.
     */
    KeymapClass = (jclass) (*env)->NewGlobalRef(env, (jobject) KeymapClass);
    StringClass = (jclass) (*env)->NewGlobalRef(env, (jobject) StringClass);
    CompletionInterface = (jclass) (*env)->NewGlobalRef(env, (jobject) CompletionInterface);
    AttemptCompletionInterface = (jclass) (*env)->NewGlobalRef(env, (jobject) AttemptCompletionInterface);

    Keymap_makeKeymap = (*env)->GetStaticMethodID(env, KeymapClass, "makeKeymap",
                                                  "(I)Lcom/avlsi/util/readline/Readline$Keymap;");
	validate(Keymap_makeKeymap, "method makeKeymap");
    Keymap_getAddress = (*env)->GetMethodID(env, KeymapClass, "getAddress", "()I");
	validate(Keymap_getAddress, "method Keymap_getAddress");
    Keymap_dispatch = (*env)->GetStaticMethodID(env, KeymapClass, "dispatch", "(IIII)I");
	validate(Keymap_dispatch, "method Keymap_dispatch");
    Keymap_registerFunction =
        (*env)->GetStaticMethodID(env, KeymapClass, "registerFunction",
                                  "(IILcom/avlsi/util/readline/Readline$KeyboardFunction;)V");
	validate(Keymap_registerFunction, "method Keymap_registerFunction");
    Keymap_unregisterFunction =
        (*env)->GetStaticMethodID(env, KeymapClass, "unregisterFunction", "(II)V");
	validate(Keymap_unregisterFunction, "method Keymap_unregisterFunction");
    Completion_run =
        (*env)->GetMethodID(env, CompletionInterface, "run", "(Ljava/lang/String;I)Ljava/lang/String;");
	validate(Completion_run, "method Completion_run");
    Attempt_completion_run =
        (*env)->GetMethodID(env, AttemptCompletionInterface, "run", "(Ljava/lang/String;II)[Ljava/lang/String;");
	validate(Attempt_completion_run, "method Attempt_completion_run");
}

/*
 * Class:     com_avlsi_util_readline_Readline
 * Method:    rl_set_readline_name
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_avlsi_util_readline_Readline_rl_1set_1readline_1name
  (JNIEnv *env, jclass cls, jstring text) {
    const char *ctext = CSTR(text);
    rl_readline_name=(char*)ctext;
}
/*
 * Class:     com_avlsi_util_readline_Readline
 * Method:    rl_add_history
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL 
Java_com_avlsi_util_readline_Readline_rl_1add_1history (JNIEnv *env, jclass cls, jstring line) {
    const char *cline = CSTR(line);
    add_history((char*)cline);
    DELSTR(line, cline);
}
/* * Class:     Readline
 * Method:    readline
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_com_avlsi_util_readline_Readline_readline (JNIEnv *env, jclass cls, jstring prompt) {
    jstring ret = 0;
    char *cprompt, *pcopy, *cresult;
    cprompt = (char*)CSTR(prompt);
    pcopy = strdup(cprompt);
    DELSTR(prompt, cprompt);
    cresult = readline(pcopy);
    if (cresult) { ret = (*env)->NewStringUTF(env, cresult); }
    free(cresult); free(pcopy);
    return ret;
}
