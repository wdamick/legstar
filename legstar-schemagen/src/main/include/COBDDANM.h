#ifndef _DEFINED_MCOBDDANALYZE 
#define _DEFINED_MCOBDDANALYZE
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = COBDDANM                                             */
/*                                                                    */
/* Description of the methods exported by COBDDAnalyze.dll.           */
/*                                                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* Prototypes exports                                                */
/*-------------------------------------------------------------------*/
#ifdef COMPILE_COBDDANALYZE
#pragma export(COBDDAnalyze)
#define LIBSPEC
#else
#define LIBSPEC extern
#endif
LIBSPEC    int    COBDDAnalyze(
           int debug_mode,    /* 1= Debug traces in stdout         */
           COBOL_COMPILER_OPTIONS* cobol_options, /* Input options
                                                     in effect     */
           char* usage,       /* Input Cobol usage clause          */
           char* picture,     /* Input Cobol picture clause        */
           int*  signSeparate,/* Input Is sign separated. This has
                                 an influence on zoned decimal byte
                                 length.                           */
           enum DATATYPE* pDataType, /* Output Data item type 
                                 derived from picture clause       */
           int* pTotalDigits, /* Output total number of digits     */
           int* pFractionDigits, /* Output fractional digits       */
           int* pSign,        /* Output sign                       */
           int* pByteLength,  /* Total length of item in bytes     */
           char* msg);        /* Error message if any              */
#undef LIBSPEC

#endif /* _DEFINED_MCOBDDANALYZE */

