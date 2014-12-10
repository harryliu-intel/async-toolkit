// vim:sw=4:expandtab:ai:cin:ts=4
#include "Wrapper.h"

/*
 * Class:     Readline
 * Method:    rl_insert_text
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1insert_1text (JNIEnv *env, jclass cls, jstring str) {
    const char *cstr = (*env)->GetStringUTFChars(env, str, 0);
    jint result = (jint) rl_insert_text((char*)cstr);
    (*env)->ReleaseStringUTFChars(env, str, cstr);
    return result;
}

/*
 * Class:     Readline
 * Method:    rl_delete_text
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1delete_1text (JNIEnv *env, jclass cls,
                                jint start, jint end) {
    return (jint) rl_delete_text((int) start, (int) end);
}

/*
 * Class:     Readline
 * Method:    rl_copy_text
 * Signature: (II)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_com_avlsi_util_readline_Readline_rl_1copy_1text (JNIEnv *env, jclass cls,
                              jint start, jint end) {
    char *cstr = rl_copy_text((int) start, (int) end);
    if (cstr) {
        return (*env)->NewStringUTF(env, cstr);
    } else {
        return (jstring) NULL;
    }
}

/*
 * Class:     Readline
 * Method:    rl_kill_text
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1kill_1text (JNIEnv *env, jclass cls,
                              jint start, jint end) {
    return (jint) rl_kill_text((int) start, (int) end);
}
