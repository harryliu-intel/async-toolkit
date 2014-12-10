// vim:sw=4:expandtab:ai:cin:ts=4
#include "Wrapper.h"

/*
 * Class:     Readline
 * Method:    rl_display
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1display (JNIEnv *env, jclass cls) {
    rl_redisplay();
}

/*
 * Class:     Readline
 * Method:    rl_forced_update_display
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1forced_1update_1display (JNIEnv *env, jclass cls) {
    return (jint) rl_forced_update_display();
}

/*
 * Class:     Readline
 * Method:    rl_on_new_line
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1on_1new_1line (JNIEnv *env, jclass cls) {
    return (jint) rl_on_new_line();
}

/*
 * Class:     Readline
 * Method:    rl_reset_line_state
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1reset_1line_1state (JNIEnv *env, jclass cls) {
    return (jint) rl_reset_line_state();
}

/*
 * Class:     Readline
 * Method:    rl_message
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1message (JNIEnv *env, jclass cls, jstring str) {
    const char *cstr = (*env)->GetStringUTFChars(env, str, 0);
    jint result = (jint) rl_message(cstr);
    (*env)->ReleaseStringUTFChars(env, str, cstr);
    return result;
}

/*
 * Class:     Readline
 * Method:    rl_clear_message
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1clear_1message (JNIEnv *env, jclass cls) {
    return (jint) rl_clear_message();
}

/*
 * Class:     Readline
 * Method:    rl_save_prompt
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1save_1prompt (JNIEnv *env, jclass cls) {
    /// _rl_save_prompt();
}

/*
 * Class:     Readline
 * Method:    rl_restore_prompt
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1restore_1prompt (JNIEnv *env, jclass cls) {
    /// _rl_restore_prompt();
}
