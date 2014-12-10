// vim:sw=4:expandtab:ai:cin:ts=4
#include "Wrapper.h"

/*
 * Class:     Readline
 * Method:    rl_make_bare_keymap
 * Signature: ()LReadline$Keymap;
 */
JNIEXPORT jobject JNICALL
Java_com_avlsi_util_readline_Readline_rl_1make_1bare_1keymap (JNIEnv *env, jclass cls) {
    return MK_KEYMAP(rl_make_bare_keymap());
}

/*
 * Class:     Readline
 * Method:    rl_copy_keymap
 * Signature: (LReadline$Keymap;)LReadline$Keymap;
 */
JNIEXPORT jobject JNICALL
Java_com_avlsi_util_readline_Readline_rl_1copy_1keymap (JNIEnv *env, jclass cls, jobject map) {
    GET_KM;
    return MK_KEYMAP(rl_copy_keymap(km));
}

/*
 * Class:     Readline
 * Method:    rl_make_keymap
 * Signature: ()LReadline$Keymap;
 */
JNIEXPORT jobject JNICALL
Java_com_avlsi_util_readline_Readline_rl_1make_1keymap (JNIEnv *env, jclass cls) {
    return MK_KEYMAP(rl_make_keymap());
}

/*
 * Class:     Readline
 * Method:    rl_discard_keymap
 * Signature: (LReadline$Keymap;)V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1discard_1keymap (JNIEnv *env, jclass cls, jobject map) {
    GET_KM;
    rl_discard_keymap(km);
}

/*
 * Class:     Readline
 * Method:    rl_get_keymap
 * Signature: ()LReadline$Keymap;
 */
JNIEXPORT jobject JNICALL
Java_com_avlsi_util_readline_Readline_rl_1get_1keymap (JNIEnv *env, jclass cls) {
    return MK_KEYMAP(rl_get_keymap());
}

/*
 * Class:     Readline
 * Method:    rl_set_keymap
 * Signature: (LReadline$Keymap;)V
 */
JNIEXPORT void JNICALL
Java_com_avlsi_util_readline_Readline_rl_1set_1keymap (JNIEnv *env, jclass cls, jobject map) {
    GET_KM;
    rl_set_keymap(km);
}

/*
 * Class:     Readline
 * Method:    rl_get_keymap_by_name
 * Signature: (Ljava/lang/String;)LReadline$Keymap;
 */
JNIEXPORT jobject JNICALL
Java_com_avlsi_util_readline_Readline_rl_1get_1keymap_1by_1name (JNIEnv *env, jclass cls, jstring str)
{
    const char *cstr = CSTR(str);
    Keymap km = rl_get_keymap_by_name((char*)cstr);
    DELSTR(str, cstr);
    return MK_KEYMAP(km);
}

/*
 * Class:     Readline
 * Method:    rl_get_keymap_name
 * Signature: (LReadline$Keymap;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_com_avlsi_util_readline_Readline_rl_1get_1keymap_1name (JNIEnv *env, jclass cls, jobject map) {
    GET_KM;
    return (*env)->NewStringUTF(env, rl_get_keymap_name(km));
}
