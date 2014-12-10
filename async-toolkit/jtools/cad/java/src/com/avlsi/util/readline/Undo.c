// vim:sw=4:expandtab:ai:cin:ts=4
#include "Wrapper.h"

/*
 * Class:     Readline
 * Method:    rl_begin_undo_group
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1begin_1undo_1group (JNIEnv *env, jclass cls) {
    return (jint) rl_begin_undo_group();
}

/*
 * Class:     Readline
 * Method:    rl_end_undogroup
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1end_1undogroup (JNIEnv *env, jclass cls) {
    return (jint) rl_end_undo_group();
}

/*
 * Class:     Readline
 * Method:    rl_add_undo
 * Signature: (IIILjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1add_1undo (JNIEnv *env, jclass cls, jint undo_code,
                             jint start, jint end, jstring text) {
    const char *ctext = CSTR(text);
    rl_add_undo((int) undo_code, (int) start, (int) end, (char*)ctext);
    DELSTR(text, ctext);
}

/*
 * Class:     Readline
 * Method:    free_undo_list
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_free_1undo_1list (JNIEnv *env, jclass cls) {
    rl_free_undo_list();
}

/*
 * Class:     Readline
 * Method:    rl_do_undo
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1do_1undo (JNIEnv *env, jclass cls) {
    return (jint) rl_do_undo();
}

/*
 * Class:     Readline
 * Method:    rl_modifying
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1modifying (JNIEnv *env, jclass cls, jint start, jint end) {
    return (jint) rl_modifying((int) start, (int) end);
}
