/* These are common utility routines for JNI manipulations    */
#include <jni.h>

/* Prototypes  */
int JNU_SetNativeIntFromBoolean (JNIEnv *env, jobject jobj, const char *ident, int* target);
int JNU_SetNativeCharFromString (JNIEnv *env, jobject jobj, const char *ident, char* target);
int JNU_SetNativeCharsFromString(JNIEnv *env, jobject jobj, const char * ident, char* target, int maxLen);
int JNU_SetIntegerFromNativeInt (JNIEnv *env, jobject jobj, const char *ident, int value);
int JNU_SetStringFromNativeChars(JNIEnv *env, jobject jobj, const char *ident, char* value);
int JNU_GetNativeCharsFromString(JNIEnv *env, jobject jobj, const char *str, char** target);

jstring JNU_NewStringNative(JNIEnv *env, const char *str);
void JNU_ThrowByName(JNIEnv *env, const char *name, const char *msg);


