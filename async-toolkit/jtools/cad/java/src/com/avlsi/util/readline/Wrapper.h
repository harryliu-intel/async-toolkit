#ifndef FULCRUM_EXCLUDED_GENERATED_INCLUDES
#include "Readline.h"
#endif

#include "Readline_Keymap.h"
#include <stdlib.h>
#include <assert.h>
#include <readline/readline.h>
#include <readline/history.h>

extern JavaVM *jvm;
extern jclass KeymapClass, StringClass, CompletionInterface, AttemptCompletionInterface;
extern jmethodID Keymap_makeKeymap, Keymap_getAddress, Keymap_dispatch;
extern jmethodID Keymap_registerFunction, Keymap_unregisterFunction;
extern jmethodID Completion_run, Attempt_completion_run;
extern Function *default_map[];

#define GET_KM \
    Keymap km = (Keymap) (*env)->CallIntMethod(env, map, Keymap_getAddress)

#define MK_KEYMAP(map) (*env)->CallStaticObjectMethod(env, KeymapClass, \
                                                      Keymap_makeKeymap, \
                                                      (jint) (map))

#define CSTR(str) (*env)->GetStringUTFChars(env, (str), 0)
#define DELSTR(str, cstr) (*env)->ReleaseStringUTFChars(env, (str), (cstr))
