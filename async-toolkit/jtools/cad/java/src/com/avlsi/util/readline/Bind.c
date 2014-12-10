// vim:sw=4:expandtab:ai:cin:ts=4
#include "Wrapper.h"

/*
 * Class:     Readline
 * Method:    rl_bind_key
 * Signature: (ILReadline$KeyboardFunction;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1bind_1key (JNIEnv *env, jclass cls, jint key, jobject func) {
    (*env)->CallStaticVoidMethod(env, KeymapClass, Keymap_registerFunction,
                                 (jint) rl_get_keymap(), key, func);
    return (jint) rl_bind_key((int) key, default_map[key]);
}

/*
 * Class:     Readline
 * Method:    rl_bind_key_in_map
 * Signature: (ILReadline$KeyboardFunction;LReadline$Keymap;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1bind_1key_1in_1map (JNIEnv *env, jclass cls,
                                      jint key, jobject func, jobject map) {
    GET_KM;
    (*env)->CallStaticVoidMethod(env, KeymapClass, Keymap_registerFunction,
                                 km, key, func);
    return (jint) rl_bind_key_in_map((int) key, default_map[key], km);
}

/*
 * Class:     Readline
 * Method:    rl_unbind_key
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1unbind_1key (JNIEnv *env, jclass cls, jint key) {
    (*env)->CallStaticVoidMethod(env, KeymapClass, Keymap_unregisterFunction,
                                 (jint) rl_get_keymap(), key);
    return (jint) rl_unbind_key(key);
}

/*
 * Class:     Readline
 * Method:    rl_unbind_key_in_map
 * Signature: (ILReadline$Keymap;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1unbind_1key_1in_1map (JNIEnv *env, jclass cls,
                                        jint key, jobject map) {
    GET_KM;
    (*env)->CallStaticVoidMethod(env, KeymapClass, Keymap_unregisterFunction,
                                 km, key);
    return (jint) rl_unbind_key_in_map(key, km);
}

/*
 * Class:     Readline
 * Method:    rl_unbind_function_in_map
 * Signature: (LReadline$KeyboardFunction;LReadline$Keymap;)I
XXX: Not implemented yet
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1unbind_1function_1in_1map (JNIEnv *env, jclass cls,
                                             jobject func, jobject map) {
    GET_KM;
}
 */

/*
 * Class:     Readline
 * Method:    rl_unbind_command_in_map
 * Signature: (Ljava/lang/String;LReadline$Keymap;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1unbind_1command_1in_1map (JNIEnv *env, jclass cls,
                                            jstring cmd, jobject map) {
    jint result;
    const char *ccmd = CSTR(cmd);
    GET_KM;
    result = (jint) rl_unbind_command_in_map((char*)ccmd, km);
    DELSTR(cmd, ccmd);
    return result;
}

/*
 * Class:     Readline
 * Method:    rl_generic_bind
 * Signature: (ILjava/lang/String;Ljava/lang/String;LReadline$Keymap;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1generic_1bind (JNIEnv *env, jclass cls, jint type,
                                 jstring keyseq, jstring keydata,
                                 jobject map) {
    jint result;
    const char *ckeyseq = CSTR(keyseq);
    const char *ckeydata = CSTR(keydata);
    GET_KM;
    result = (jint) rl_generic_bind((int) type, (char*)ckeyseq, (char*)ckeydata, km);
    DELSTR(keyseq, ckeyseq);
    DELSTR(keydata, ckeydata);
    return result;
}

/*
 * Class:     Readline
 * Method:    rl_parse_and_bind
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1parse_1and_1bind (JNIEnv *env, jclass cls, jstring line) {
    jint result;
    const char *cline = CSTR(line);
    result = (jint) rl_parse_and_bind((char*)cline);
    DELSTR(line, cline);
    return result;
}

/*
 * Class:     Readline
 * Method:    rl_read_init_file
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_com_avlsi_util_readline_Readline_rl_1read_1init_1file (JNIEnv *env, jclass cls, jstring filename) {
    jint result;
    const char *cfilename = CSTR(filename);
    result = (jint) rl_parse_and_bind((char*)cfilename);
    DELSTR(filename, cfilename);
    return result;
}
