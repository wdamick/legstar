/* These are common utility routines for JNI manipulations    */
#include <stdlib.h>
#include <string.h>
#include "com_legstar_schemagen_UTILJNIWrapper.h" /* Utility method prototypes */

/** This function gets a boolean value from a Java object */
int JNU_SetNativeIntFromBoolean (JNIEnv *env, jobject jobj, const char *ident, int* target)
{
	jfieldID jfID;
    jclass objClass;
    jboolean jbool;
    jthrowable exc;
    
    objClass = (*env)->GetObjectClass(env, jobj);
	jfID  = (*env)->GetFieldID(env, objClass, ident, "Z");
	if (jfID == NULL)
		return JNI_ERR;
	jbool = (*env)->GetBooleanField(env, jobj, jfID);
    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	    (*env)->DeleteLocalRef(env, exc);
		return JNI_ERR;
	}
	*target = jbool;
	return JNI_OK; 
}

/** This function gets a char value as the first char of a Java String */
int JNU_SetNativeCharFromString (JNIEnv *env, jobject jobj, const char *ident, char* target)
{
	char* str = NULL;
	if (JNI_OK != JNU_GetNativeCharsFromString(env, jobj, ident, &str))
		return JNI_ERR;
	*target = str[0];
    free(str);
	return JNI_OK; 
}

/* Set a native string from a Java object String field. This method assumes the target
 * is already allocated and is large enough to hold the result.*/
int JNU_SetNativeCharsFromString(JNIEnv *env, jobject jobj, const char * ident, char* target, int maxLen)
{
	char* jStr = NULL;
	
	/* make sure we have a valid target */
	if (target == NULL) {
		JNU_ThrowByName(env, "java/lang/NullPointerException", 0);
		return JNI_ERR;
	}
	
	if (JNI_OK != JNU_GetNativeCharsFromString(env, jobj, ident, &jStr))
		return JNI_ERR;
	
	/* If a null string was passed return an empty char */
	if (jStr == NULL) {
		target[0] = '\0';
		return JNI_OK;
	}

	/* output container may not be large enough */
	if (strlen(jStr) > maxLen) {
		JNU_ThrowByName(env, "java/lang/IndexOutOfBoundsException", 0);
		return JNI_ERR;
	}
	
	if (strlen(jStr) > 0) {
		strcpy(target, jStr);
	}
	else {
		target[0] = '\0';
	}
		
    free(jStr);
    
	return JNI_OK;
}


/** This function translates from a jstring to a native char string 
 * using the current JVM charset assumed to be identical to the C locale. 
 * The idea is to use the Java String getBytes method to get a byte array
 * using whatever local charset the JVM is setup for. This way, we
 * don't need to mess up with iconv and handle local code pages.    */
int JNU_GetNativeCharsFromString(JNIEnv *env, jobject jobj, const char *ident, char** target)
 {
     jclass objClass;
	 jfieldID jfID;
	 jstring  jstr;
     jbyteArray bytes = 0;
     jthrowable exc;
     jclass strClass;
     jmethodID MID_String_getBytes;
     jint len;
     char* result;
     
	 /* first get a reference to the java String field */
	 objClass = (*env)->GetObjectClass(env, jobj); 
	 jfID  = (*env)->GetFieldID(env, objClass, ident, "Ljava/lang/String;"); 
	 if (jfID == NULL)
         return JNI_ERR;
	 jstr = (jstring) (*env)->GetObjectField(env, jobj, jfID); 
	 exc = (*env)->ExceptionOccurred(env);
	 if (exc) {
		(*env)->DeleteLocalRef(env, exc);
		return JNI_ERR;
	 }
	 /* it might be a null string. no need for processing in this case*/
	 if (jstr == NULL)	 {
	 	*target = NULL;
	 	return JNI_OK;
	 }

     /* then get the method ID for the java String method getBytes */
     strClass = (*env)->GetObjectClass(env, jstr);
     MID_String_getBytes = 
         (*env)->GetMethodID(env, strClass, "getBytes", "()[B");
	 exc = (*env)->ExceptionOccurred(env);
	 if (exc) {
		(*env)->DeleteLocalRef(env, exc);
		return JNI_ERR;
	 }
	 
	 /* make sure JNI still has enough internal memory */
     if ((*env)->EnsureLocalCapacity(env, 2) < 0) {
         return JNI_ERR; /* out of memory error */
     }
     
     /* call the getBytes method to translate the String to char */
     bytes = (*env)->CallObjectMethod(env, jstr, MID_String_getBytes);
     exc = (*env)->ExceptionOccurred(env);
	 if (exc) {
		(*env)->DeleteLocalRef(env, exc);
		return JNI_ERR;
	 }
	 
	 /* convert the jbyte array into a native char array*/
     len = (*env)->GetArrayLength(env, bytes);
     result = (char *)malloc(len + 1);
     if (target == 0) {
         JNU_ThrowByName(env, "java/lang/OutOfMemoryError", 0);
         (*env)->DeleteLocalRef(env, bytes);
         return JNI_ERR;
     }
     (*env)->GetByteArrayRegion(env, bytes, 0, len,
                                (jbyte *)result);
     result[len] = 0; /* NULL-terminate */
     *target = result;

     (*env)->DeleteLocalRef(env, bytes);
     return JNI_OK;
 }
 
 /** Creates a jstring from a native char string */
 jstring JNU_NewStringNative(JNIEnv *env, const char *str)
 {
	 jclass Class_java_lang_String;
	 jmethodID MID_String_init;
     jstring result;
     jbyteArray bytes = 0;
     int len;

	 Class_java_lang_String = (*env)->FindClass(env, "java/lang/String");
	 MID_String_init = (*env)->GetMethodID(env, Class_java_lang_String,
	           "<init>", "([B)V");

     if ((*env)->EnsureLocalCapacity(env, 2) < 0) {
         return NULL; /* out of memory error */
     }
     len = strlen(str);
     bytes = (*env)->NewByteArray(env, len);
     if (bytes != NULL) {
         (*env)->SetByteArrayRegion(env, bytes, 0, len,
                                    (jbyte *)str);
         result = (*env)->NewObject(env, Class_java_lang_String,
                                    MID_String_init, bytes);
         (*env)->DeleteLocalRef(env, bytes);
         return result;
     } /* else fall through */
     return NULL;
 }
 
 /** Set a Java integer field from a native int */
 int JNU_SetIntegerFromNativeInt(JNIEnv *env, jobject jobj, const char *ident, int value)
 {
	jclass objClass;
	jfieldID jfID;
    jthrowable exc;

 	objClass = (*env)->GetObjectClass(env, jobj);
	jfID  = (*env)->GetFieldID(env, objClass, ident, "I");
	if (jfID == NULL)
         return JNI_ERR;
	(*env)->SetIntField(env, jobj, jfID, value);
    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	    (*env)->DeleteLocalRef(env, exc);
 		return JNI_ERR; 
	}
	return JNI_OK;
 }
 
 /** Set a Java String field of an object from a native char */
 int JNU_SetStringFromNativeChars(JNIEnv *env, jobject jobj, const char *ident, char* value)
 {
	jclass objClass;
	jfieldID jfID;
	jstring jStr;
    jthrowable exc;

	jStr = JNU_NewStringNative(env,value); 
 	objClass = (*env)->GetObjectClass(env, jobj);
	jfID  = (*env)->GetFieldID(env, objClass, ident, "Ljava/lang/String;");
	if (jfID == NULL)
         return JNI_ERR;
	(*env)->SetObjectField(env, jobj, jfID, jStr);
    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	    (*env)->DeleteLocalRef(env, exc);
 		return JNI_ERR; 
	}
	return JNI_OK;
 }
 
 /* Helper to throw an exception back to the JVM */
 void JNU_ThrowByName(JNIEnv *env, const char *name, const char *msg)
 {
     jclass cls = (*env)->FindClass(env, name);
     /* if cls is NULL, an exception has already been thrown */
     if (cls != NULL) {
         (*env)->ThrowNew(env, cls, msg);
     }
     /* free the local ref */
     (*env)->DeleteLocalRef(env, cls);
 }

 
 
