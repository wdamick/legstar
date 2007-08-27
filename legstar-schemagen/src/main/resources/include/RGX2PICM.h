#ifndef _DEFINED_MREGEX2PICTURE 
#define _DEFINED_MREGEX2PICTURE
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = MREGEX2PICTURE                                       */
/*                                                                    */
/* Description of the methods exported by Regex2Picture.dll.          */
/*                                                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* Prototypes exports                                                */
/*-------------------------------------------------------------------*/
#ifdef COMPILE_REGEX2PICTURE
#ifdef MVS
#pragma export(Picture2Regex)
#endif
#define LIBSPEC
#else
#define LIBSPEC extern
#endif
LIBSPEC    int    Picture2Regex(
           int debug_mode,    /* 1= Debug traces in stdout         */
           char* picture,     /* Input Cobol picture clause        */
           char* regex,       /* Output regular expression         */
           int regexMaxSize,  /* Maximum size of regular expression*/
           enum REGEXSTYLE regexStyle, /* Regular expression style */
           char* msg);        /* Error message if any              */
#undef LIBSPEC

#endif /* _DEFINED_MREGEX2PICTURE */

